# Recharge Funcs ##################################################################################
message("Recharge Funcs")
# 0. Initiation ===================================================================================
message("0. Initiation")
# 0.1 Get Running Parameters ----------------------------------------------------------------------
message("0.1 Get Running Parameters")
cs_crs="+proj=longlat +datum=WGS84"
Type_of_runing="t"

# Unit Test =======================================================================================
if(Type_of_runing=="u_t"){
  # Load Libraries ---------------------------------------------------------------------------------
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
}

# founc.1 Nationl2model #############################################################################
# Unit Test
if(Type_of_runing=="u_t"){
  
  model_sp_YT=st_read("G:/Layers/Geohydrology/Geohydrology/code/Recharge_Model/RAW/Inputs/csv_DB/SP_V2.shp")%>%
    dplyr::rename(.,poly_id=ELNO)%>%
    #dplyr::filter(poly_id==3171,) %>%
    mutate(area_km2=as.numeric(st_area(.))/10^6)
  
  
  natioanal_grid_V1=st_read("G:/Layers/Geohydrology/Geohydrology/code/Recharge_Model/RAW/Inputs/csv_DB/ETp_Grid_1000_poly.shp")%>%
    dplyr::select(.,c(lon,lat,geometry)) %>%
    mutate(area_km2=as.numeric(st_area(.))/10^6)
  
  tictoc::tic()
  nat_db=biuldb(
    natioanal_grid=natioanal_grid_V1,
    model_sp=model_sp_YT,
    p_pth="G:/Layers/Geohydrology/Hydrometeorolgy/data/GDB/Tebular/Pdaily_BD/National_Grid",
    etp_pth="G:/Layers/Geohydrology/Hydrometeorolgy/data/GDB/Tebular/ETP_DB",
    st=as_datetime("2003-01-01"),
    ed=as_datetime("2009-01-01")
  )
  tictoc::toc()
  
  tictoc::tic()
  connection_netwotk=conn_builder(
    folder_db=nat_db$folder_db,
    nat_mod_grd=nat_db$nat_mod_grd,
    nat_mod_dt=nat_db$nat_mod_dt,
    save_nw=T
  )
  tictoc::toc()
  
  tictoc::tic()
  P_ETP_YT=nationl2model(
    nat_mod_dt=nat_db$nat_mod_dt,
    folder_db=nat_db$folder_db,
    conn_p=sf::st_read(paste0(getwd(),"/p_ts_md.shp")), # connection_netwotk$conn_p
    conn_etp=sf::st_read(paste0(getwd(),"/etp_ts_md.shp")) # connection_netwotk$conn_etp
  )
  tictoc::toc()
}
# Func 1.1 Build Intersected National DB =======================================================
biuldb=function(natioanal_grid,model_sp,p_pth,etp_pth,st,ed){
  # 1.1 Intersection --------------------------------------------------------------------------------
  message("Intesection")
  sf::sf_use_s2(F)
  nat_mod_grd=st_intersection(natioanal_grid,model_sp) %>% mutate(area_km2=as.numeric(st_area(.))/10^6)
  sf::sf_use_s2(T)
  nat_mod_dt=subset(nat_mod_grd,,c("lon","lat","area_km2","poly_id")) %>% as.data.table(.,key=c("lon","lat"))
  
  # Build Meta Data files ----------------------------------------------------------------------
  message("Build Meta Data files")
  folder_p_df=list.files(path=p_pth,pattern=".csv") %>% as.data.frame() %>% dplyr::rename(.,"filename"=".") %>%
    mutate(p_pth=list.files(path=p_pth,full.names = T,pattern=".csv"),
           date=as.Date(str_sub(str_remove(as.character(filename),"_id-1.csv"),3,13))) %>% as.data.table(.,key="date")
  
  folder_etp_df=list.files(path=etp_pth,pattern=".csv") %>% as.data.frame() %>% dplyr::rename(.,"filename"=".") %>%
    mutate(etp_pth=list.files(path=etp_pth,full.names = T,pattern=".csv"),
           date=as.Date(str_sub(filename,5,14))) %>% as.data.table(.,key="date")
  
  folder_dt=folder_p_df[folder_etp_df][date %between% c(st,ed),]
  
  # Get time serise ----------------------------------------------------------------------------
  message("Get time serise")
  
  file_empt=nat_mod_dt[,c("lon","lat")][,z:=0]
  read_core=function(filepath){
    if(!is.na(filepath)){
      file_i=fread(filepath,select=c("lon","lat","z"),key=c("lon","lat"))
      file_i$lon=round(file_i$lon,5)
      file_i$lat=round(file_i$lat,5)
    } else{file_i=file_empt}
    return(file_i)
  }
  
  future::plan(multicore)
  folder_db=mutate(folder_dt,P=purrr::map(p_pth,.f=read_core),
                   ETP=purrr::map(etp_pth,.f=read_core)) %>%
    dplyr::select(.,c(date,P,ETP))
  
  db_lst=list("folder_db"=folder_db,"nat_mod_grd"=nat_mod_grd,"nat_mod_dt"=nat_mod_dt)
  return(db_lst)
}
# Func 1.2 Biuld Connection Network ============================================================
conn_builder=function(folder_db,nat_mod_grd,nat_mod_dt,save_nw){
  # Get Connection netwotk ---------------------------------------------------------------------
  message("Get Connection netwotk")
  etp_ts_md=folder_db$ETP[[1]][,c("lon","lat")] %>%  tibble::rowid_to_column(., "id")                     
  conn_etp=st_join(nat_mod_grd[,c("area_km2","poly_id","geometry")],
                   st_as_sf(etp_ts_md, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84"),
                   join = nngeo::st_nn, k = 1, maxdist = 100000) %>%
    left_join(.,etp_ts_md) %>%
    subset(.,,c("area_km2","poly_id","lon","lat"))# %>% as.data.table(.,key=c("lon","lat"))
  
  p_ts_md=folder_db$P[[1]][,c("lon","lat")] %>%  tibble::rowid_to_column(., "id")                     
  conn_p=st_join(nat_mod_grd[,c("area_km2","poly_id","geometry")],
                 st_as_sf(p_ts_md, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84"),
                 join = nngeo::st_nn, k = 1, maxdist = 100000) %>%
    left_join(.,p_ts_md) %>%
    subset(.,,c("area_km2","poly_id","lon","lat"))# %>% as.data.table(.,key=c("lon","lat"))
  # Export ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (save_nw==T){
    sf::st_write(conn_etp,paste0(getwd(),"/etp_ts_md.shp"), delete_layer = TRUE)
    sf::st_write(conn_p,paste0(getwd(),"/p_ts_md.shp"), delete_layer = TRUE)
    message(paste0("Connections network saved to external path: ",getwd()))
  }
  conn_lst=list("conn_p"=conn_p,"conn_etp"=conn_etp)
  return(conn_lst)
}
# Func 1.3 Biuld Model DB ============================================================
nationl2model=function(nat_mod_dt,folder_db,conn_p,conn_etp){
  # Get Connection Networks ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  conn_p=as.data.table(conn_p,key=c("lon","lat"))
  conn_p$lon=round(conn_p$lon,5)
  conn_p$lat=round(conn_p$lat,5)
  
  conn_etp=as.data.table(conn_etp,key=c("lon","lat"))
  conn_etp$lon=round(conn_etp$lon,5)
  conn_etp$lat=round(conn_etp$lat,5)
 
  # Calc model grid flow -----------------------------------------------------------------------
  message(" Calc model grid flow")
  ts_empt=nat_mod_dt[,c("area_km2","poly_id")][,.(z=0,area_km2=sum(area_km2)),.(poly_id)]
  setcolorder(ts_empt, c("poly_id", "z", "area_km2"))
  cl <- parallel::makeCluster(46L)
  plan("cluster", workers = cl)
  flow_mdl_etp=mutate(folder_db,ETP=furrr::future_map(.x=ETP,conn_net=conn_etp,ts_empt,.f=flowmdl,.progress = TRUE))[,c("date","ETP")]
  flow_mdl_p=mutate(folder_db,P=furrr::future_map(.x=P,conn_net=conn_p,ts_empt,.f=flowmdl,.progress = TRUE))[,c("date","P")]

  parallel::stopCluster(cl)
  flow_mdl_db=cbind(flow_mdl_p,flow_mdl_etp)[,c("date","P","ETP")]
  return(flow_mdl_db)
}
# founc.1.3.1 Convert National Unit Flow 2 Model Unit Flow -------------------------------------
flowmdl=function(ts,conn_net,ts_empt){
  act_day=max(ts[,"z"],na.rm=T)
  if(act_day>0){
    keycols = c("lon","lat")
    setkeyv(ts,keycols)
    setkeyv(conn_net,keycols)
    # tabular Join try -------------------------------------------------------------------------
    flow_grd=ts[conn_net[,c("lon","lat","area_km2","poly_id")]][,z:=z*area_km2]
    if(any(is.na(flow_grd$z))==T){
      warning("Oh shit, there is no full representation of the model polygon in the input data network")
    }
    # Calc flow Per Model Unit
    vol_mdl=flow_grd[,.(z=sum(z,na.rm = T),area_km2=sum(area_km2,na.rm = T)),.(poly_id)]
    flow_mdl=vol_mdl[,z:=z/area_km2]
  } else{flow_mdl=ts_empt}
  return(flow_mdl)
}

# founc.2.1 Recharge data Constrocation =========================================================
message("founc.2.1 Recharge data Constrocation")
# Unit test
if(Type_of_runing=="u_t"){
  P_ETP=P_ETP_YT
  AMC=fread("G:/Layers/Geohydrology/Geohydrology/code/Recharge_Model/RAW/Inputs/csv_DB/AMC_initial.csv") %>% as.data.table(.,key="poly_id")
  model_sp=st_read("G:/Layers/Geohydrology/Geohydrology/code/Recharge_Model/RAW/Inputs/csv_DB/SP_V2.shp") %>% dplyr::rename(.,poly_id=ELNO) %>% mutate(area_km2=as.numeric(st_area(.))/10^6)
  
  tictoc::tic()
  Recharge_cons_DB = recharge_cons(P_ETP,AMC,model_sp)
  tictoc::toc()
}

# Function
recharge_cons=function(P_ETP,AMC,model_sp){
  
  # Time Serise to Spatial Serise
  P=P_ETP[,c("date","P")] %>% dplyr::rename("z"="P")
  P=tidyr::unnest(P,cols = z) %>% subset(.,,c("poly_id","z"))  %>% dplyr::rename(.,"P"="z") %>% group_by(poly_id) %>%  tidyr::nest() %>% dplyr::rename(.,"P"="data")
  ETP=P_ETP[,c("date","ETP")]   %>% dplyr::rename(.,"z"="ETP") 
  ETP=tidyr::unnest(ETP,cols = z)  %>% dplyr::rename(.,"ETP"="z") %>% group_by(poly_id) %>%  tidyr::nest() %>% dplyr::rename(.,"ETP"="data")
  SP=setDT(model_sp,key="poly_id")[,c("poly_id","b","t_s","t_fc","t_pwp","z","m")]

  recharge_construction=Reduce(left_join, list(P,ETP,SP,AMC))
  return(recharge_construction)
}

# founc.2.2 Run DREAM ===============================================================================
message("founc.2.2 Run DREAM")
# Unit Test

if(Type_of_runing=="u_t"){
  
  Recharge_cons_DB_tst=Recharge_cons_DB[1:100,]
  tictoc::tic()
  Recharge_tst=run_DREAM(Recharge_cons_DB=Recharge_cons_DB_tst)
  tictoc::toc()
}

# Function
run_DREAM=function(Recharge_cons_DB,intail_soil_moisture){
  cl <- parallel::makeCluster(8L)
  plan("cluster", workers = cl) 
  #future::plan(multiprocess)

  Recharge_df=unnest(Recharge_cons_DB,cols = c(P, ETP))
  message("Build Initial DB")
  Recharge_int_ndf=as.data.table(Recharge_df)[,P:=ifelse(!is.na(P)==T,P,0)] %>% mutate( teta=-999,
                                                                                                   teta_1=-999,
                                                                                                   teta_2=-999,
                                                                                                   teta_final=-999,
                                                                                                   ET_1=-999,
                                                                                                   ET_2=-999,
                                                                                                   ET_final=-999,
                                                                                                   Recharge_final=-999) %>% group_by(poly_id) %>% tidyr::nest(.)
  message("Run DREAM")
  tictoc::tic()
  Recharge_ndf=as.data.table(Recharge_int_ndf) %>% mutate(data=furrr::future_map(.x=data,15,.f=DREAM_core))
  tictoc::toc()
  parallel::stopCluster(cl)
  message("Reduse to DF")
  Recharge_dt=as.data.table(unnest(Recharge_ndf))
  return(Recharge_dt)
  gc()
} # run_DREAM Close

# founc.2.2.1 DREAM core ----------------------------------------------------------------------------
DREAM_core=function(data,intail_soil_moisture){
  
  Soil_moisture_j=intail_soil_moisture # Inital Value
  for(j in 1:NROW(data)){
    Recharge_j=data[j,] %>% mutate(
      teta=Soil_moisture_j/z,
      teta_1=ifelse((teta+P/z)<=t_s,(teta+P/z),t_s),
      ET_1=as.numeric(b)*ETP*(teta_1-t_pwp)/(t_fc-t_pwp),
      ET_2=ifelse(teta_1<=t_pwp,0,ET_1),
      ET_final=ifelse(teta_1>=t_fc,as.numeric(b)*ETP,ET_2),
      teta_2=teta_1-(ET_final/z),
      Recharge_final=ifelse(teta_2>=t_fc,teta_2*z*m,0),
      teta_final=ifelse(teta_2>=t_fc,(teta_2-Recharge_final/z),teta_2),
      Soil_moisture=teta_final*z)
    # update
    Soil_moisture_j=Recharge_j$Soil_moisture
    data[j,]=Recharge_j
  }
  return(data)
}

# Edit 17012020 #########################
DREAM_core_dt=function(data){
  Soil_moisture_j=135 # Inital Value 215 =Wither 15 = Summer
  for(j in 1:NROW(data)){
    Recharge_j=as.data.table(data[j,])[,c("teta",
                                          "teta_1",
                                          "ET_1",
                                          "ET_2",
                                          "ET_final",
                                          "teta_2",
                                          "Recharge_final",
                                          "teta_final",
                                          "Soil_moisture"):=
                                         list(Soil_moisture_j/z, # "teta"
                                              ifelse((teta+P/z)<=t_s,(teta+P/z),t_s), # "teta_1"
                                              b*ETP*(teta_1-t_pwp)/(t_fc-t_pwp), # "ET_1"
                                              ifelse(teta_1<=t_pwp,0,ET_1), #"ET_2"
                                              ifelse(teta_1>=t_fc,b*ETP,ET_2), #"ET_final"
                                              teta_1-(ET_final/z), # "teta_2"
                                              ifelse(teta_2>=t_fc,teta_2*z*m,0), # "Recharge_final"
                                              ifelse(teta_2>=t_fc,(teta_2-Recharge_final/z),teta_2), # "teta_final"
                                              teta_final*z)] # "Soil_moisture"
    # update
    Soil_moisture_j=Recharge_j$Soil_moisture
    data[j,]=Recharge_j
  }
  return(data)
}
# Edit 17012020 #########################

# func.3 Export ####################################################################################
message("founc.3 Export")
# func 3.1 Feflow ==================================================================================
message("func 3.1 Feflow")
# Unit test
if(Type_of_runing=="u_t"){
  Recharge_db=Recharge[date %between% c(as_date("2003-10-01"),as_date("2008-10-01"))]
  funcIDX=as.data.table(table(Recharge$poly_id))[, funcid := .I][, poly_id := as.numeric(V1)][,c("funcid","poly_id")]
  tic()
  rec2ff(Recharge_db,
         interval="month",
         funcIDX,
         pth="V:/Pub_Jeru/GW FLOW MODELS/FEFLOW YARKTAN Dafni/test 2020 NA/data/Calibration_Inputs_2020/Recharge_HSI_2003_2008_V3.pow")
  toc()
}
# Func
rec2ff=function(Recharge_db,interval,funcIDX,pth){
  Recharge_db$date=lubridate::as_date(Recharge_db$date)
  t0=as.numeric(days_in_month(Recharge_db$date[1]))
  Recharge_ff=Recharge_db[,c("poly_id","date","Recharge_final")] %>%
    mutate(month_date=floor_date(date, unit = "month")) %>%
    group_by(poly_id,month_date) %>%
    dplyr::summarise(Recharge=mean(Recharge_final,na.rm = T), ## 17/02/2021: The values should be 10 X doubled ##
                     n_days=ifelse(interval=="month",days_in_month(month_date),interval)) %>%
    group_by(poly_id) %>% mutate(days=cumsum(n_days)-t0) %>%
    left_join(.,funcIDX,by="poly_id")
  
  Recharge_ff_sub=Recharge_ff[,c("funcid","days","Recharge")]
  
  Recharge_ff_nst=Recharge_ff_sub %>%
    mutate(Recharge=as.character(round(Recharge,2)),
           days=as.character(days)) %>%
    group_by(funcid) %>%
    nest() %>% mutate(pow_data=map2(.x=data,.y=funcid,.f=head_unit)) %>%
    subset(.,,c(pow_data)) %>%
    tidyr::unnest()
  
  if (!is.null(pth)==T) {
    fwrite(Recharge_ff_nst,pth,
           sep = " ",
           row.names = F)
    
  } else (
    return(Recharge_ff_nst)
  )
  
}

# func.3.1.1 Feflow Head Units --------------------------------------------------------------------
message("func.3.1 Feflow Head Units")

head_unit=function(data,funcid){
  head_row=data.frame("days"=paste0("#",funcid), "Recharge"=NA)
  end_row=data.frame("days"="END", "Recharge"=NA)
  tunc_unit=bind_rows(head_row,data,end_row)
  return(tunc_unit)
} 
# func 3.2 GMS ==================================================================================
message("3.2 GMS")
# Unit test
if(Type_of_runing=="u_t"){
  Recharge_db=Recharge[date %between% c(as_date("2003-10-01"),as_date("2008-10-01"))]
  interval="month"
  pth="G:/Layers/Geohydrology/Geohydrology/RB/Recharge_EastMt_V1.txt"
  
  tic()
  rec2GMS(Recharge_db,
         interval="month",
         funcIDX,
         pth=pth)
  toc()
}
# Func.
rec2GMS=function(Recharge_db,interval,pth){
  if(interval=="month"){
    n=30
  } else if (interval=="week") {
    n=7
  } else if (interval=="day") {
    n=1
  } else if (interval=="quarterly") {
    n=90
  }  else if (interval=="10 days") {
    n=10
  }
  
  
  Recharge_GMS=as_tibble(Recharge_db) %>%
    mutate(Date=lubridate::floor_date(date, unit = interval)) %>% 
    group_by(Date,poly_id) %>% 
    dplyr::summarise(Q_mm=sum(Recharge_final,na.rm=T)) %>% 
    mutate(Q=0.1*Q_mm/n, # Rate per stress period unit in of meter per day
           Date=as.character(Date),
           Time="00:00:00",
           year=str_sub(Date,1,4),
           month=str_sub(Date,6,7),
           day=str_sub(Date,9,10),
           Date=paste(day,month,year,sep="/")) %>% 
    dplyr::select(poly_id,Date,Time,Q) %>% 
    dplyr::arrange(poly_id) %>% 
    dplyr::transmute(Poly_Name=poly_id,
                     Date=as.character(Date),
                     Time=Time,
                     Q=Q)
  
  write.table(Recharge_GMS,pth, append = FALSE, sep = " ", dec = ".",row.names = F)
}


# 4. Visualization & Export #########################################################################
# 4.1 Cell Chart ===================================================================================
Recharge_C=function(Recharge,lab_size,l_limt,u_limit){
  Rech_C=ggplot(data = Recharge, aes(x = date,group = 1)) +
    geom_area(aes(y = -1*Recharge_final), fill = "green",alpha=0.5)+
    geom_line(aes(y = -1*Recharge_final, colour = "Recahrge [mm/d]")) +
    geom_line(aes(y = P, colour = "Rain[mm/d]")) +
    geom_area(aes(y =P), fill = "blue",alpha=0.5)+
    geom_line(aes(y = ET_final, colour = "Evapotranspiration[mm/d]")) +
    geom_area(aes(y = ET_final), fill = "red",alpha=0.5)+
    scale_colour_manual("", 
                        breaks = c("Recharge [mm/d]", "Rain[mm/d]", "Evapotranspiration[mm/d]"),
                        values = c("green", "blue", "red")) +
    theme(legend.position="bottom")+
    xlab(" ") + 
    labs(title=paste0("Recharge balance: ",min(year(Recharge$date),na.rm = T),"-",max(year(Recharge$date),na.rm = T)," in cell: ",Recharge$poly_id[1]))+
    scale_x_date(labels = scales::date_format("%m/%y"), breaks = scales::date_breaks("month"))+
    theme(axis.text.x = element_text(colour="grey20",size=lab_size,angle=45,hjust=.5,vjust=.5,face="plain"),
          axis.title.y = element_text(colour="grey20",size=lab_size,angle=90,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="grey20",size=lab_size,angle=0,hjust=1,vjust=0,face="plain"))+
    scale_y_continuous("Daily flux [mm/d]") # , limits = c(l_limt,u_limit)
  return(Rech_C)
}
# 4.2 Annual summary ================================================================================
annual_summary=function(Recharge,st,ed){
  DATE1 <- as.Date(st, "%d-%m-%Y")
  DATE2 <- as.Date(ed, "%d-%m-%Y")
  budg <- subset(Recharge, date>=DATE1 & date <=DATE2) %>%
    dplyr::mutate(rain_mcm=P*area_km2*10^-3,
                  et_mcm=ET_final*area_km2*10^-3,
                  rec_mcm=Recharge_final*area_km2*10^-3) %>%
    dplyr::summarise(rain=round(sum(rain_mcm, na.rm=T),0),
                     et=round(sum(et_mcm, na.rm=T),0),
                     reac=round(sum(rec_mcm, na.rm=T),0))
  if((budg$et+budg$reac)>budg$rain){warning("The resulting balance is negative, check that something is not screwed")}
  return(budg)
}

