# 0. Initiation ##########################################################################################
message("0. Initiation")

library(httr)
library(rlang)
library(sp)
library(sf)
library(maptools)
library(rgeos)
library(rgdal)
library(plyr)
library(dplyr)
library(readxl)
library(googledrive)
library(data.table)
library(raster)
library(DT)
library(stringr)
library(openxlsx)
library(ggplot2)
library(plotly)
library(officer)
library(grid)
library(utils)
library(aws.s3)
library(tibble)
library(RColorBrewer)
library(purrr)
library(tidyr)
library(lubridate)
library(profvis)
library(future)
library(furrr)
#library(tidyverse)
library(leaflet)
library(tictoc)
library(parallel)


Background_path="G:/Tools/DREAMhsi"
Type_of_runing="t"
in_param="t"
Sys.setlocale("LC_ALL", "Hebrew")

if(Type_of_runing=="t"){
  # 0.1 Load Global Metadata File ========================================================================
  message("0.4 Load Global Metadata File")
  ETp_Grid_Path=paste(Background_path,"data/GDB",sep = "/")
  ETp_Grid_st=st_read(paste(ETp_Grid_Path,"ETp_Grid_1000_ll.shp",sep="/"))
  proj_WGS_ll<<-"+proj=longlat +datum=WGS84" 
}


# 1-n. ETP Minnig System #################################################################################
# 1. Load DBs ############################################################################################
# Unit Test
if(Type_of_runing=="u_t"){
  period_v= c(start=as_date("2021-03-22 00:00:00 IDT"),end=as_date("2022-03-05 00:00:00 IDT")) # as_date(Sys.time())-1 -> Not Working
  exoprt2DB=F
  ETp_daily_valus=load_ETp_DBs(period=period_v,exoprt2DB)
}
# 1. Function
load_ETp_DBs=function(period,exoprt2DB){
  # Set Connection to Mteo API ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  token_request_v <- GET(url = 'https://api.floods.online/v1/envista/stations'
                         , add_headers(.headers = c('Authorization'= 'ApiToken 967a4917-4a56-4d6b-83d1-a67f4734814b'
                                                    , 'Envi-data-Source' = 'meteo.co.il')))
  # 1.1  Metadata Connection ==============================================================================
  ETp_md_df= ETp_md_Connection(token_request_v)
  # 1.2 DB Minnig =========================================================================================
  selected_parametrs=c("התאדות שבועית","התאדות תלת יומית","התאדות פנמן","גיגית הדמיה","התאדות נמדדת מגיגית")
  ETp_daily_valus_df = Mteo_ts_miner(md_df=ETp_md_df,
                                  token_request=token_request_v,
                                  parm_id=3,
                                  period=period_v,
                                  skip_stations=c(0),
                                  exoprt2DB=exoprt2DB)
  return(ETp_daily_valus_df)
}

# 1.1  Metadata Connection ================================================================================
# Unit Test
if(Type_of_runing=="u_t"){
  ETp_md_df = ETp_md_Connection(token_request)
}
# Function
ETp_md_Connection=function(token_request){
  message("1.1  Metadata Connection")
  # 1.1.1 Get json file as DF ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  js_meta_data=rjson::fromJSON(rawToChar(token_request$content), method = "C", unexpected.escape = "error", simplify = TRUE )
  meta_data_df=base::Reduce(rbind,js_meta_data)%>%as.data.frame(.)
  n_md=nrow(meta_data_df)
  # 1.1.2 Biuld Meta data file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  lalo=base::Reduce(rbind,meta_data_df$location)%>%as.data.table(.)
  lalo_df=data.frame(latitude=c(1:n_md),longitude=c(1:n_md))
  for (i in 1:nrow(lalo)){
    lalo_df$latitude[i]=as.numeric(as.character(lalo$latitude[i]))
    lalo_df$longitude[i]=as.numeric(as.character(lalo$longitude[i]))
  }
  gdf=cbind(lalo_df,meta_data_df)
  df_meta_data=subset(gdf,,c("latitude","longitude","stationId","name","shortName","timebase","active","owner","regionId","StationTarget"))%>%
    as_tibble(.) %>%  mutate_if(is.list,unlist)%>%
    dplyr::filter(.,is.na(latitude)!=T)%>%
    dplyr::rename(.,lat=latitude,lon=longitude)
  # xy=LongLatToUTM(df_meta_data$longitude,df_meta_data$latitude)
  # gdf_meta_data=cbind(xy,df_meta_data)
  
  return(df_meta_data)
}
# 1.2 TS  DB Minnig ===========================================================================
# Function
Mteo_ts_miner=function(md_df,token_request,parm_id,period,skip_stationsv,exoprt2DB){
  message("1.2 Last Received DB Minnig")
  Sys.setlocale("LC_ALL", "Hebrew")
  if(length(period)==1){target="MEASURE"}
  if(length(period)==2){
    if(as_date(period[2])<=as_date(Sys.time())+1){target="MEASURE"}
    if(as_date(period[2])>as_date(Sys.time())){target="FORECAST"}
  }
  print(target)
  # Data extraction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(target=="MEASURE"){
    message("Get Measured Data frome close list")
    ETp_dfi=read_excel(paste0(Background_path,"/data/tabular/ETp_stations_V1.xlsx"))
  } else{
    message("Get Model Data by string detection")
    ETp_dfi=subset(ETp_md_df,,c("stationId","name","lon","lat","StationTarget"))%>%
      mutate(mnt=str_detect(StationTarget,target)) %>%
      filter(.,mnt==T)
  }
  
  ETp_df=ETp_dfi %>%
    add_column(status_code=-999,
               datetime=NA,
               value=-999,
               channel_name="Unclassified")  %>%
    setDT(.)
  # Set Selected Storage ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  selected_parametrs=data.frame(parm_n=c("התאדות שבועית","התאדות תלת יומית","התאדות פנמן","גיגית הדמיה","התאדות נמדדת מגיגית"))
  parm=as.character(selected_parametrs$parm_n[parm_id])
  
  for (i in 1:NROW(ETp_df)){
    # Set Connection to API ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    St_IDX=as.numeric(ETp_df$stationId[i])
    print(St_IDX)
    if(length(period)==1){
      mod_url=paste0("https://api.floods.online/v1/envista/stations/",St_IDX,"/data/",period)
    }
    if(length(period)==2){
      mod_url=paste0("https://api.floods.online/v1/envista/stations/",St_IDX,"/data?from=",period[1],"&to=",period[2])
    }
    
    token_request <- GET(url = mod_url
                         , add_headers(.headers = c('Authorization'= 'ApiToken 967a4917-4a56-4d6b-83d1-a67f4734814b'
                                                    , 'Envi-data-Source' = 'meteo.co.il')))
    
    
    status_code=token_request[["status_code"]]
    skip=F
    # Get Connection to status ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if(status_code!=200 || skip==T){print(paste0("i-",i," ;St.-",St_IDX," ;Status_code-",status_code," Skip-",skip))
      ETp_df$status_code[i]=status_code}
    if(status_code==200 & skip==F){
      # Get dynamic data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      dynamic_st_data=rjson::fromJSON(rawToChar(token_request$content), method = "C", unexpected.escape = "error", simplify = TRUE )
      ETp_df$status_code[i]=status_code
      if(length(period)==1){
        ETp_df$datetime[i]=dynamic_st_data[["data"]][[1]][["datetime"]]
        ETp_df$value[i]=data_colector(dynamic_st_data,parm)
      }
      if(length(period)==2){
        
        ts_i_data=data_ts_colector(dynamic_st_data,parm)
        max_v=max(ts_i_data$value)
        if(max_v>0){
          datetime=nest(subset(ts_i_data,,c("datetime")))%>%dplyr::rename(.,"datetime"="data")
          value=nest(subset(ts_i_data,,c("value")))%>%dplyr::rename(.,"value"="data") 
          ETp_df$datetime[i]=datetime
          ETp_df$value[i]=value
        }
      }
      ETp_df$channel_name[i]=parm
    }
  }
  if(length(period)==1){
    ETp_df=ETp_df %>% mutate(datetime=as.Date(datetime),
                             measure=value)
  }
  if(length(period)==2){
    ETp_df=unnest(unnest(as_tibble(ETp_df[is.na(datetime)==F])))%>%
      mutate(datetime=as.Date(datetime)) %>%
      dplyr::filter(.,value>=0) %>%
      dplyr::filter(.,lon>0) %>%
      dplyr::filter(.,lat>0) %>%
      dplyr::group_by(stationId,name,lon,lat,status_code,channel_name,datetime)%>%
      dplyr::summarise(measure=sum(value)) # mean or sum ????
  }
  # Export to external DB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(isTRUE(exoprt2DB)){
    t_idx=dplyr::distinct(ETp_df[,c("datetime")])
    for (i in 1:nrow(t_idx)){
      print(i)
      ETp_df_i=subset(ETp_df,datetime %in% t_idx$datetime[i],c("lon","lat","measure"))
      # Tests
      if (nrow(ETp_df_i)==0){warning(paste0("No Data in: ",t_idx$datetime[i]))}
      if (max(ETp_df_i$measure)>15){warning(paste0("Exceptionally high values on the date: ",t_idx$datetime[i]))}
      # Export 
      fwrite(ETp_df_i,paste0("G:/Data/ETP_DB/RAW/ETP_",t_idx$datetime[i],"_source.csv"),row.names=F)
    }
    
  }
  return(ETp_df)
}

# 1.2.1 data colector -----------------------------------------------------------------------------------
# Unit Test
if(Type_of_runing=="u_t"){
  aa=data_colector(dynamic_st_data,"התאדות פנמן")
}
# Function
data_colector=function(source_db,parm){
  n_channels=length(source_db$data[[1]]$channels)
  data_st_value=-888
  for (j in 1:n_channels){
    data_st_name=source_db$data[[1]]$channels[[j]]$name
    if(data_st_name==parm){
      data_st_value=source_db$data[[1]]$channels[[j]]$value
    }
  }
  return(data_st_value)
}
# 1.2.2 data ts colector -------------------------------------------------------------------------------
# Unit Test
if(Type_of_runing=="u_t"){
  ts_i_nest=data_ts_colector(source_db=dynamic_st_data,parm="התאדות פנמן")
}
# Function
data_ts_colector=function(source_db,parm){
  # Set ts loop
  n_ts=length(source_db[["data"]])
  ts_i_data=data.frame("datetime"=c(1:n_ts),"value"=c(1:n_ts))
  ts_i_data$datetime=NA
  ts_i_data$value=-999
  for (t in 1:n_ts){
    ts_i_data$datetime[t]<-source_db[["data"]][[t]][["datetime"]]
    # Data loop
    n_channels=length(source_db$data[[t]]$channels)
    data_st_value=-888
    for (j in 1:n_channels){
      data_st_name=source_db$data[[t]]$channels[[j]]$name
      if(data_st_name==parm){
        data_st_value=source_db$data[[t]]$channels[[j]]$value
      }
    }
    ts_i_data$value[t]=data_st_value
  }
  return(ts_i_data)
  rm(t);rm(j)
}

# 2. Build ETp Regular Grid ###################################################################
if(Type_of_runing=="u_t"){
  ETp_Grid=ETp_Grid_st
  datetime_df=table(ETp_daily_valus$datetime)%>%as.data.table()
  for (i in 1:NROW(datetime_df)){
    datetime_i=datetime_df$V1[i]
    ETp_daily_valus_i=dplyr::filter(ETp_daily_valus,datetime==datetime_i)
    intETp_i=regular_grid_builder(st_df=ETp_daily_valus,
                                  datetime_i=datetime_i,
                                  ETp_Grid=ETp_Grid_st,
                                  int_type="kriging", # "IDW" # kriging
                                  exoprt2DB=T) 
    #intETp_i$intETp_C
    print(i)
  }
}

regular_grid_builder=function(st_df,datetime_i,ETp_Grid,int_type,exoprt2DB){
  
  # Clean St. values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  print("Clean St. values")
  st_df_clean=subset(st_df,measure>=0 & lon >0 & datetime %in% date(as.Date(datetime_i)),c("lon","lat","measure")) 
  
  # Build St. Layer ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  print("Build St. Layer-12")
  # st_df_shp=df2sf2shp(st_df_clean)
  xy = data.frame(x=st_df_clean$lon,y=st_df_clean$lat)
  st_df_shp=SpatialPointsDataFrame(coords = xy, data = st_df_clean,proj4string = CRS(proj_WGS_ll))
  
  # Build Extent & Grid ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  israel_bounds_st=st_read(paste(Background_path,"data/GDB/insrael_bounds_pnt_V2.shp",sep = "/"))
  israel_bounds_sp=as_Spatial(israel_bounds_st)
  p_coor_df=as_tibble(israel_bounds_sp@coords)
  colnames(p_coor_df)=c("x","y")
  p_coor_lst=list(data.frame(p_coor_df$x,p_coor_df$y))
  ETp_Grid=as.data.frame(ETp_Grid_st)
  
  # Get Masked Raster ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  print("Get Masked Raster")
  etp_monthly_pth=paste(Background_path,"data/GDB/ETp_Rate",sep = "/")
  monthIDX=month(datetime_i)
  ETpeman.file = as.data.table(list.files(path=etp_monthly_pth,
                                          pattern =".tif$", full.names=TRUE)) %>%
    dplyr::filter(.,str_detect(V1,"wgs84")) %>%
    mutate(mnt=as.numeric(str_remove(str_remove(V1,paste0(etp_monthly_pth,"/wgs84_")),".tif"))) %>%
    dplyr::filter(.,mnt==monthIDX)
  ETpeman=raster(ETpeman.file$V1)
  
  ETpeman_p=raster::extract(ETpeman, st_df_shp, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                            fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)%>%as.data.table()
  
  # Extract St. data by Mask ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  print("Extract St. data by Mask")
  st_df_sprv=cbind(as.data.table(st_df_clean),as.data.table(ETpeman_p))%>%
    subset(.,,c("lon","lat","measure","."))%>%
    dplyr::rename(.,"perennial_mean"=".")%>%
    mutate(diftot=round(perennial_mean-measure,2),
           difpre=round(100*diftot/perennial_mean,0),
           etp_filt=round(ifelse(abs(difpre)>60,perennial_mean,measure),2),
           etp_buff=measure,
           etp_buff=round(ifelse(abs(difpre)>60 & abs(difpre)<100,(0.5*perennial_mean+0.5*measure),etp_buff),2),
           etp_buff=ifelse(abs(difpre)>100,perennial_mean,etp_buff))%>%
    na.omit()
  
  st_sprv_shp=df2sf2shp(st_df_sprv)
  
  # Convert to Regular Grid ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  print("Convert to Regular Grid")
  st_df4int=subset(st_df_sprv,,c(etp_buff,lon,lat)) %>% 
    group_by(lon,lat) %>% 
    dplyr::summarise(z=mean(etp_buff,na.rm=T)) %>% 
    dplyr::rename(x=lon,y=lat)
  
  
  z=as.numeric((st_df4int$z))
  x=as.numeric((st_df4int$x))
  y=as.numeric((st_df4int$y))
  # Kriging
  if(int_type=="kriging"){
    intETp_v=crs_kriging(x, y, z, polygons=p_coor_lst, pixels=300)
    intETp_df_all=intETp_v[["map"]] %>%
      transmute(z=ifelse(pred>1,pred,0),
                lon=x,
                lat=y,
                id_krig = row_number()) %>% as.data.table(.,key="id_krig") 
    # fit Interpolation Values to Standart Grid ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    closest=RANN::nn2(subset(intETp_df_all,,c("lon","lat")),
                      subset(ETp_Grid,,c("lon","lat")),
                      k = 1, searchtype = "radius", radius = 0.01)[["nn.idx"]]
    
    Grid_idx=cbind(ETp_Grid,closest)%>%dplyr::rename(.,"id_krig"="closest") %>% as.data.table(.,key="id_krig")
    intETp_df=setDT(left_join(Grid_idx,subset(intETp_df_all,,c("id_krig","z")),by="id_krig")) 
  }
  # IDW
  if(int_type=="IDW"){
    intETp_v=idw_mteo(st_df=st_df_sprv,
                      int_value=st_df_sprv$etp_buff,
                      poly_df=ETp_Grid) %>% dplyr::rename(.,z=Z) # idw_mteo
    intETp_df=data.frame(lon=ETp_Grid$lon,lat=ETp_Grid$lat,z=intETp_v$z)
    
  }
    # Build QA element ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  print("Export QA element")
  intETp_C=ggplot(intETp_df) + geom_point(aes(x=lon,y=lat,color=z))+
    scale_colour_gradient2(low="blue",high="red",mid="yellow",midpoint=mean(st_df_sprv$perennial_mean))+
    geom_point(data=st_df4int,aes(x=x,y=y),color="black",shape=22)+
    ggtitle(paste0("Type: ",int_type," | Date: " ,as_character(datetime_i)))
  
  intETp_list=list("stETp_df"=st_df_clean,"intETp_df"=intETp_df,"intETp_C"=intETp_C)
  
  
  # Export 2 DB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(isTRUE(exoprt2DB)){
    mteopath=paste0("G:/Data/ETP_DB")
    write.csv(intETp_list[[2]],paste0(mteopath,"/ETP_",datetime_i,"_St.csv"),row.names=F)
    ggsave(filename=paste0(mteopath,"/ETP_",datetime_i,"_St.jpg"),plot=intETp_list[[3]],dpi = 200)
  }
  return(intETp_list)
}
# Subfunctions #################################################################
##  Mteo IDW Calculator ========================================================
idw_mteo=function(st_df,int_value,poly_df){
  
  # Get IDW elemnets for each time satpe
  values_measure=as.numeric(unlist(int_value))
  coords_measure=as.data.frame(subset(st_df,,c("lon","lat")))
  coords_poly=as.data.frame(subset(poly_df,,c("lon","lat")))
  n_measure=NROW(st_df)
  
  # Run IDW
  if(n_measure>3){
    tic()
    int_ts_poly = idw_core(values=values_measure,
                           coords=coords_measure,
                           grid=coords_poly,
                           method = "Shepard",
                           p = 2,
                           R = 15000,
                           N = 1000) %>%
      mutate(poly_id=poly_df$poly_id)
    toc()
  }
  else(int_ts_poly=poly_df$poly_id %>% as.data.frame(.) %>%
         transmute(Z=0,poly_id=poly_df$poly_id))
  
  return(int_ts_poly)
  rm(int_ts_poly)
}

### IDW Core -------------------------------------------------------------------
idw_core=function (values, coords, grid, method = "Shepard", p = 2, 
                   R = 2, N = 15, distFUN = geo.dist, ...) 
{
  d.real <- phylin::geo.dist(from = grid, to = coords, ...)
  dimensions <- dim(d.real)
  methods <- c("Shepard", "Modified", "Neighbours")
  method <- agrep(method, methods)
  if (method == 1) {
    w <- 1/d.real^p
  }
  else if (method == 2) {
    w <- ((R - d.real)/(R * d.real))^p
  }
  else if (method == 3) {
    calcneighbours <- function(x, N) {
      x[order(x)][N:length(x)] <- Inf
      return(x)
    }
    newdist <- t(apply(d.real, 1, calcneighbours, N))
    w <- 1/newdist^p
  }
  for (i in 1:nrow(w)) {
    if (sum(is.infinite(w[i, ])) > 0) {
      w[i, !is.infinite(w[i, ])] <- 0
      w[i, is.infinite(w[i, ])] <- 1
    }
  }
  w.sum <- apply(w, 1, sum, na.rm = TRUE)
  wx <- w %*% old_diag(values)
  ux <- apply(wx/w.sum, 1, sum, na.rm = TRUE)
  data.frame(Z = ux)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
old_diag=function (x = 1, nrow, ncol, names = TRUE)
{
  if (is.matrix(x)) {
    if (nargs() > 1L && (nargs() > 2L || any(names(match.call()) %in%
                                             c("nrow", "ncol"))))
      stop("'nrow' or 'ncol' cannot be specified when 'x' is a matrix")
    if ((m <- min(dim(x))) == 0L)
      return(vector(typeof(x), 0L))
    y <- x[1 + 0L:(m - 1L) * (dim(x)[1L] + 1)]
    if (names) {
      nms <- dimnames(x)
      if (is.list(nms) && !any(vapply(nms, is.null, NA)) &&
          identical((nm <- nms[[1L]][seq_len(m)]), nms[[2L]][seq_len(m)]))
        names(y) <- nm
    }
    return(y)
  }
  if (is.array(x) && length(dim(x)) != 1L)
    stop("'x' is an array, but not one-dimensional.")
  if (missing(x))
    n <- nrow
  else if (length(x) == 1L && nargs() == 1L) {
    n <- as.integer(x)
    x <- 1
  }
  else n <- length(x)
  if (!missing(nrow))
    n <- nrow
  if (missing(ncol))
    ncol <- n
  .Internal(diag(x, n, ncol))
}


# 2.2 df to shp by sf ======================================================================
if(Type_of_runing=="u_t"){
  st_df_shp=df2sf2shp(st_df_clean)
}
df2sf2shp=function(df){
  
  # Split df to spatial and Values Elements ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ddf=df
  xy = as.data.frame(subset(ddf,,c(lon,lat)))
  md=as.data.frame(subset(ddf,,-c(lon,lat)))
  
  # Set Dims ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pts = matrix(,nrow(ddf),2)
  pts[,1]=ddf$lon
  pts[,2]=ddf$lat
  
  # Biuld SpatialPointsDataFrame Via st and sf elements ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  st_df_mp = st_sfc(st_multipoint(pts, dim = "XY"))
  st_df_p = st_cast(x = st_df_mp, to = "POINT")
  st_df_pj=st_sf(st_df_p,crs=proj_WGS_ll)
  st_shp=as(st_df_pj, 'Spatial')
  
  # Join Back Values Elements ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  df_shp=spCbind(st_shp,md)
  return(df_shp)
}
# 2.3 Kriging CRS ========================================================================= 
crs_kriging=function(x, y, response, model = "spherical", lags = 10, 
                     pixels = 100, polygons) 
{
  # Sub functions --------------------------------------------------------------
  onedim <- function(x, n) {
    .C("onedimdist", PACKAGE="kriging",
       as.double(x), 
       as.integer(n),
       v = as.double(rep(0, n*n)))$v
  }
  
  twodim <- function(x, y, n) {
    .C("twodimdist", PACKAGE="kriging", 
       as.double(x), 
       as.double(y), 
       as.integer(n),
       d = as.double(rep(0, n*n)))$d
  }
  krig.vg <- function(D, V, cutoff, lags) {
    W <- matrix(0, lags, 3)
    for(i in 1:lags) {
      W[i,1] <- length(D[(D<(i*cutoff/lags)) & (D>((i-1)*cutoff/lags))])/2
      W[i,2] <- mean(D[(D<(i*cutoff/lags)) & (D>((i-1)*cutoff/lags))]) # Average 
      W[i,3] <- sum(V[(D<(i*cutoff/lags)) & (D>((i-1)*cutoff/lags))])/(4*W[i,1]) # Semivariance
    }
    return(W)
  }
  krig.fit <- function(D, nugget, range, sill, model, n) {
    if(model=="spherical") modelID <- 0
    if(model=="exponential") modelID <- 1
    if(model=="gaussian") modelID <- 2
    return(.C("krigfit", PACKAGE="kriging",
              as.double(D),
              as.double(nugget),
              as.double(range),
              as.double(sill),
              as.integer(modelID),
              as.integer(n),
              a = as.double(rep(1, (n+1)*(n+1))))$a)
  }
  
  krig.grid <- function(blx, bly, trx, try, pixels) {
    pixel <- max((trx-blx), (try-bly)) / (pixels-1)
    xpixels <- ceiling((trx-blx)/pixel)
    ypixels <- ceiling((try-bly)/pixel)
    
    G.grid <- .C("kriggrid", PACKAGE="kriging",
                 as.double(blx),
                 as.double(bly),
                 as.double(pixel),
                 as.integer(xpixels),
                 as.integer(ypixels),
                 gx = as.double(rep(0, xpixels * ypixels)),
                 gy = as.double(rep(0, xpixels * ypixels)))
    
    return(data.frame(x = G.grid$gx, y = G.grid$gy, pixel=pixel))
  }
  
  krig.polygons <- function(X, Y, polygons) {
    n <- length(X)
    nlist <- length(polygons)
    npoly <- rep(0, nlist+1)
    
    polygonsx <- polygonsy <- {}
    for(i in 1:nlist) {
      polygonsx <- c(polygonsx, polygons[[i]][,1])
      polygonsy <- c(polygonsy, polygons[[i]][,2])
      npoly[i+1] <- length(polygonsx)
    }
    
    G <- .C("krigpolygons", 
            as.integer(n),
            as.double(X),
            as.double(Y),
            as.integer(nlist),
            as.integer(npoly),
            as.double(polygonsx),
            as.double(polygonsy),
            Gn = as.integer(0),
            Gx = as.double(rep(0, n)),
            Gy = as.double(rep(0, n)))
    
    return(data.frame(x = G$Gx[1:G$Gn], y = G$Gy[1:G$Gn]))
  }
  
  krig.pred <- function(x, y, response, Gx, Gy, invA, nugget, range, sill, model, n) {
    if(model=="spherical") modelID <- 0
    if(model=="exponential") modelID <- 1
    if(model=="gaussian") modelID <- 2
    return(.C("krigpred", PACKAGE="kriging",
              as.double(x),
              as.double(y),
              as.double(response),
              as.double(Gx),
              as.double(Gy),
              as.double(invA),
              as.double(nugget),
              as.double(range),
              as.double(sill),
              as.integer(modelID),
              as.integer(n),
              as.integer(length(Gx)),
              as.integer(n+1),
              as.integer(1),
              G.pred = as.double(rep(0, length(Gx))))$G.pred)
  }
  
  imageloop <- function(x, y, z, a, b) {
    n <- length(z)
    na <- length(a)
    nb <- length(b)
    
    loop <- .C("krigimage",
               as.integer(x),
               as.integer(y),
               as.double(z),
               as.integer(n),
               as.integer(a),
               as.integer(b),
               as.integer(na),
               as.integer(nb),
               i = as.double(rep(0, na*nb)),
               j = as.integer(rep(0, na*nb)))
    
    loop$i[loop$j==0] <- NA
    return(matrix(loop$i, na, nb, byrow=T))
  }
  # Main function --------------------------------------------------------------
  n <- length(response)
  p <- 2
  D <- twodim(x, y, n)
  V <- onedim(response, n)
  cutoff <- sqrt((max(x) - min(x))^2 + (max(y) - min(y))^2)/3
  W <- krig.vg(as.matrix(D, n, n), as.matrix(V, n, n), cutoff, 
               lags)
  fit.vg <- lm(W[, 3] ~ W[, 2])$coefficients
  nugget <- as.numeric(fit.vg[1])
  range <- max(W[, 2])
  sill <- nugget + as.numeric(fit.vg[2]) * range
  a <- 1/3
  A <- krig.fit(D, nugget, range, sill, model, n)
  # CRS update S#############
  # Fit interpolation to polygon grid
  x_grid=polygons[[1]][[1]]
  y_grid=polygons[[1]][[2]]
  G <- krig.grid(min(x_grid), min(y_grid), max(x_grid), max(y_grid), pixels)
  # CRS update E#############
  
  pixel <- unique(G$pixel)
  if (!is.null(polygons)) {
    G <- krig.polygons(G$x, G$y, polygons)
  }
  G.pred <- krig.pred(x, y, response, G$x, G$y, c(solve(matrix(A, 
                                                               n + 1, n + 1))), nugget, range, sill, model, n)
  o <- list(model = model, nugget = nugget, range = range, 
            sill = sill, pixel = pixel, map = data.frame(x = G$x, 
                                                         y = G$y, pred = G.pred), semivariogram = data.frame(distance = W[, 
                                                                                                                          2], semivariance = W[, 3]))
  class(o) <- "kriging"
  o
}






