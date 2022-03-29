# Load Libraries ---------------------------------------------------------------------------------
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

Background_path="G:/Data"
Type_of_runing="t"

debugSource("G:/Tools/DREAMhsi/scripts/DB_Builder/ETP_Conncetion_Func_V5.R")

# 1. ET ##################################################################################################
if(Type_of_runing=="u_t"){
  period_v= c(start=as_date("2011-10-01 00:00:00 IDT"),end=as_date("2022-03-05 00:00:00 IDT")) # as_date(Sys.time())-1 -> Not Working
  ## 1.1 Build raw Station DB data and store in DB (Optional) ==================================================
  exoprt2DB=T  # Data is Store in global path: "G:/Data/ETP_DB/RAW"
  ETp_daily_valus=load_ETp_DBs(period=period_v,exoprt2DB)
  ETp_daily_valus$type="source" #  The working assumption is that all data from recent years are based on station data
  
  ## 2.2 Build ETp raster ===================================================================================
  ETp_Grid=ETp_Grid_st
  ### 2.2.1 Option 1: Run directly from the products of step 1.1 ---------------
  datetime_df=table(ETp_daily_valus$datetime) %>% as.data.table()
  
  ### 2.2.1 Option 2: Run from the input database ------------------------------
  folderPath="G:/Data/ETP_DB/RAW/ETP_4_Intrepolation_1970_Updated"
  ptr=".csv"
  folder_df=list.files(path=folderPath,pattern=ptr) %>% as.data.frame() %>%
    dplyr::rename(.,"filename"=".") %>%
    mutate(filepath=list.files(path=folderPath,full.names = T,pattern=ptr),
           datetime=as.Date(stringr::str_sub(filename,5,14)),
           type=stringr::str_extract(filename,paste(c("source","typical"), collapse="|"))) %>% 
    dplyr::filter(datetime %between% period_v)
  # Get Files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  future::plan(multicore)
  folder_db=as.data.frame(dplyr::mutate(folder_df,df=purrr::map(filepath,.f=fread)))
  ETp_daily_all=tidyr::unnest(folder_db)
  ETp_daily_valus=ETp_daily_all[,c("datetime","lon","lat","measure","type")]
  datetime_df=table(ETp_daily_valus$datetime) %>% as.data.table()
  
  for (i in 1:NROW(datetime_df)){
    datetime_i=datetime_df$V1[i]
    ETp_daily_valus_i=dplyr::filter(ETp_daily_valus,datetime==datetime_i)
    intETp_i=regular_grid_builder(st_df=ETp_daily_valus_i,
                                  datetime_i=datetime_i,
                                  ETp_Grid=ETp_Grid_st,
                                  int_type="kriging", # "IDW" # kriging
                                  exoprt2DB=T) # Data is Store in global path: "G:/Data/ETP_DB"
    #intETp_i$intETp_C
    print(i)
  }
}
# 2. Rain ##################################################################################################
## 2.1 Build raw Station DB data ============================================================================
if(Type_of_runing=="u_t"){
  
  ims_db=read.csv(paste0(Background_path,"/Pdaily_BD/Stations/RAW/data_2022_partial.csv"))
  acc_corrections=T
  prod_pth="G:/Data/Pdaily_BD/Stations/RB/"
  tictoc::tic()
  fix_dt=build_raw_db(ims_db,acc_corrections,prod_pth)
  tictoc::toc()
}

## 2.2 Build Rain raster ===================================================================================
if(Type_of_runing=="u_t"){
  data_str="02/05/2021"
  data_end="01/02/2022"
  # Get Meta Data for Stations
  folder_P_st=paste0(Background_path,"/Pdaily_BD/Stations/RAW/")
  Stat_P=as.data.table(readr::read_csv(paste0(folder_P_st,"/MetaData/stations_P.csv"),show_col_types = F) %>%
                         dplyr::rename("idstation"="ID") %>%
                         mutate(data_str=as.Date(data_str,"%d/%m/%Y"),
                                data_end=as.Date(data_end,"%d/%m/%Y")),key = "idstation")
  
  
  P_ts_MetaData=read_excel(paste0(Background_path,"/Pdaily_BD/P_ts_MetaData.xlsx"),sheet="DB2Grid")%>%
    mutate(time=as.Date(time,format="%d/%m/%Y")) %>% as.data.table(.,key = "time") %>% dplyr::filter(.,!is.na(source_id))
  n_days=as.numeric(difftime(max(P_ts_MetaData$time),min(P_ts_MetaData$time),units = "days"))
  
  proj_WGS_ll="+proj=longlat +datum=WGS84"
  Grid_Path=paste0(Background_path,"/ETP_DB/MetaData/") # Same Like ETp Grid
  Grid=as.data.frame(readOGR(paste(Grid_Path,"ETp_Grid_1000_ll.shp",sep="/"))) %>%
    dplyr::rename("x"="coords.x1","y"="coords.x2")
  
  # Set Rain palette
  cols =c(0,1,3,5,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,220,240,260) 
  ncols=length(cols)
  palette=rev(rainbow(ncols))
  edited_palette=c("#aeafb0","#a2cffc","#95b9de","#467bb3","#1adb9b","#1EDC66","#1EDC21","#0FA411",
                   "#ACE409","#DDE409","#F5C907","#F59B07","#F56507","#DC5C47","#D82D12",
                   "#D81254","#174182","#23539e","#722261","#260C21","#260C21","#260C21","#260C21",
                   "#260C21","#260C21","#260C21","#260C21")
  
  
  t_check=as.Date("2022/01/27", formt = "%d/%m/%Y")
  ii=23
  
  st_metadata=subset(Stat_P,,c("idstation","lon","lat","zone_c","zone_n"))%>%setDT(.,key = "idstation")
  P_ts_MetaData_y=P_ts_MetaData%>%mutate(year=year(time))%>%group_by(year)%>%
    summarise(source_id=max(source_id))
  p_meta_zones="G:/code/Recharge_Model/ANA/Rain_Zones_V2.shp"
  
  # Ran function
  Build_National_P_DB(rain_ts_metadata=P_ts_MetaData_y,
                      st_loc_metadata=st_metadata,
                      st=53,
                      en=53,# NROW(rain_ts_metadata)
                      methodology="kriging", # methodology = "IDW" ; "kriging" ; "ensemble"
                      meta_zones="G:/code/Recharge_Model/ANA/Rain_Zones_V2.shp",
                      mod="op",
                      p_pth="G:/Data/Pdaily_BD/RB/P_",#"/Hydrometeorolgy/data/GDB/Tebular/Pdaily_BD/National_Grid/P_",
                      v_pth="G:/Code/Recharge_Model/RB/Vaildation_dt_")#"/Hydrometeorolgy/data/GDB/Tebular/Pdaily_BD/Vaildation_dt_"
  Build_National_P_DB(rain_ts_metadata=P_ts_MetaData_y,
                      st_loc_metadata=st_metadata,
                      st=52,
                      en=52, # NROW(rain_ts_metadata)
                      methodology="IDW", # methodology = "IDW" ; "kriging" ; "ensemble"
                      meta_zones=p_meta_zones,
                      mod="v",
                      p_pth="/RB/rec_qa/P_",#"/Hydrometeorolgy/data/GDB/Tebular/Pdaily_BD/National_Grid/P_",
                      v_pth="/RB/rec_qa/Vaildation_dt_")#"/Hydrometeorolgy/data/GDB/Tebular/Pdaily_BD/Vaildation_dt_"
}
# Func.
Build_National_P_DB=function(rain_ts_metadata,st_loc_metadata,st,en,methodology,meta_zones,mod,p_pth,v_pth){
  tictoc::tic()
  if(mod=="v"){vlidation_lst=list();j=1}
  if(mod=="op"){samp_status="inactive"}
  for (i in st:en){
    print(paste0("i=",i))
    # Define Metadata data for each Month ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    year_i=rain_ts_metadata$year[i]
    source_id_i=1 #rain_ts_metadata$source_id[i]
    
    # Get Time Series for each year ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    p_st_y=fread(paste0(folder_P_st,"/data_",year_i,".csv")) 
    setnames(p_st_y, "measure", "P_measure")
    # Get Active days along the year ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    n_days_i=p_st_y %>% group_by(time) %>% 
      summarise(n_st=n(),
                sum_st=as.numeric(sum(P_measure,na.rm = T)))%>%
      mutate(d_active=ifelse((n_st>10 & sum_st>10),T,F)) %>%
      dplyr::filter(.,d_active==T)
    
    p_st_y_fltr=right_join(p_st_y,subset(n_days_i,,c(time)))%>% setDT(.,key = "idstation")
    
    if(isTRUE(str_detect(p_st_y_fltr$time[1],"/"))){
      p_st_y_fltr$time=as_date(as.Date(p_st_y_fltr$time, format ="%d/%m/%Y"))
    } 
    
    for (ii in 1:NROW(n_days_i)){
      # Get Data From Raster Inactive ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (source_id_i>1){
        
      }
      # Get Data from Stations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (source_id_i==1){
        print(paste0("ii=",ii))
        #time_ii=lubridate::day(p_st_y_fltr$time[ii])
        P_ii=base::unique(p_st_y_fltr[time %in% p_st_y_fltr$time[ii] & !is.na(P_measure),],by="idstation")
        P_ii_f=P_ii[st_loc_metadata][!is.na(lon)][!is.na(P_measure)]
        
        # Separate into populations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if(mod=="v"){
          # Sample Validation op  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          samp_status="try-success"
          plan(multicore)
          P_valid_geo=try(P_ii_f %>% group_by(zone_c) %>%
                            mutate(n=n()) %>%
                            dplyr::filter(.,zone_n!=0 & n>6) %>%
                            tidyr::nest() %>%
                            dplyr::rename(valid_st=data) %>%
                            mutate(P_valid=purrr::pmap(list(valid_st,meta_zones,0.1),.f=zonal_sampler)) %>%
                            unnest(P_valid) %>% as.data.table(.,key="idstation"))
          samp_status=class(P_valid_geo)
          # Sample Main Pop ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          if(samp_status!="try-error"){
            P_valid=as.data.table(subset(P_ii_f,,c("idstation","lon","lat")))[P_valid_geo,on=.(idstation=idstation)][idstation!=-999,]
            P_ii_m=setDT(anti_join(P_ii_f,P_valid),key = "idstation")[,time:=as_date(time)]
            P_ii_f=P_ii_m
            if(mean(P_valid$st_ratio,na.rm=T)>0.2){warning(paste0("Ran In Validation Mode: ",p_st_y_fltr$time[ii]," ;sample-ratio: ",P_valid$st_ratio[1]))}
          }else{warning(paste0("Sampling failed: i=",i,"  ;ii=",ii))}
        }
        
        if(methodology=="IDW"){
          intP_df=plyr::empty(df)
          values_measure=as.numeric(unlist(P_ii_f$P_measure))
          coords_measure = subset(P_ii_f,,c("lon","lat"))
          coords_poly = as.data.frame(subset(Grid,,c("lon","lat")))
          
          # Run IDW ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          int_ts_poly = phylin::idw(values_measure,coords_measure,coords_poly,
                                    method = "Shepard", p = 2, R = Inf, N = Inf) %>%
            mutate(poly_id=Grid$Id)
          intP_df=setDT(cbind(coords_poly,int_ts_poly) %>% dplyr::rename(.,z=Z) %>%
                          mutate(z=ifelse(z>1,z,0)))
          ran_status="try-success"
        }
        if(methodology=="kriging"){
          intP_df=plyr::empty(df)
          # Build Real data to interpolation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          P_ii_ff= unique(as.data.table(P_ii_f), by =c("lon","lat"))
          
          # Calc Face Data for interpolation Borders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          fake_P_st=st_read("G:/code/Recharge_Model/ANA/fake_P_st_V2.shp")
          fake_P_pol=st_buffer(fake_P_st,dist = 0.2)
          P_ii_st= st_as_sf(P_ii_ff, coords = c("lon","lat"),crs = "+proj=longlat +datum=WGS84", agr = "constant")
          intP_face_st=st_centroid(st_join(fake_P_pol,P_ii_st,join = st_nn,k = 1, maxdist = 100,left = T))
          intP_face_ll=st_coordinates(intP_face_st)
          intP_df_face=setDT(cbind(intP_face_st,intP_face_ll))[,c("idstation.x","time","P_measure","flag","X","Y" )][,P_measure:=ifelse(is.na(P_measure)==T,0,P_measure)][,zone_c:=0][,zone_n:=0] #[,time:=p_st_y_fltr$time[ii]]      
          colnames(intP_df_face)=c("idstation","time","P_measure","flag","lon","lat","zone_c","zone_n")
          
          # Combine Points & Build vectors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          Pf_ii_f=rbind(P_ii_ff,intP_df_face)[!is.na(P_measure)]
          z=as.numeric(unlist(Pf_ii_f$P_measure))
          x=as.numeric(unlist(Pf_ii_f$lon))
          y=as.numeric(unlist(Pf_ii_f$lat))
          
          
          # Ran Kriging ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          intP_df=plyr::empty(df)
          kriged=try(kriging::kriging(x, y, z, polygons=NULL, pixels=300,lags=3),silent=T)
          
          if(class(kriged)!="try-error"){
            krig_df=kriged[["map"]] %>%
              transmute(z=ifelse(pred>1,pred,0),
                        lon=x,
                        lat=y,
                        id_krig = row_number()) %>% as.data.table(.,key="id_krig")
            
            # fit Interpolation Values to Standart Grid ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            closest=RANN::nn2(subset(krig_df,,c("lon","lat")),
                              subset(Grid,,c("lon","lat")),
                              k = 1, searchtype = "radius", radius = 0.01)[["nn.idx"]]
            
            Grid_idx=cbind(Grid,closest)%>%dplyr::rename(.,"id_krig"="closest") %>% as.data.table(.,key="id_krig")
            intP_df=setDT(left_join(Grid_idx,subset(krig_df,,c("id_krig","z")),by="id_krig")) 
          }else{
            warning(paste0("Kriging Interpolation failed in date: ",p_st_y_fltr$time[ii]))
          }
          ran_status=class(kriged)
        }
        
        # Ran Validation Analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if(mod=="v" & ran_status!="try-error" & samp_status!="try-error"){ 
          # Biuld Join MAtrix ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          join_dt=RANN::nn2(subset(intP_df,,c("lon","lat")),
                            subset(P_valid,,c("lon","lat")),
                            k = 1, searchtype = "radius", radius = 0.01)[["nn.idx"]] # Distans of ~1 Km (0.01 dgree)
          valid_idx=cbind(P_valid,join_dt) %>%dplyr::rename(.,"IDX"="V1") %>% as.data.table(.,key="IDX")
          
          # Combine Data Bases ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          intP_rdf=rownames_to_column(intP_df,var = "IDX")[,IDX:=as.numeric(IDX)]%>%setkey("IDX")
          valid_P_dt=left_join(valid_idx,intP_rdf,by="IDX")
          intP_v_fltr=as.data.table(valid_P_dt)[!is.na(z), c("idstation","time","zone_c","zone_n","P_measure","z","n","area_km2","tot_valid_area","rep_ratio","st_ratio")]
          pop_ratio=round(NROW(intP_v_fltr)/NROW(valid_P_dt),2)
          if(pop_ratio<0.95){warning(paste0("Validation Population Size: ",pop_ratio, " in date: ", p_st_y_fltr$time[ii]))}
          
          # Pop Validation Matrix ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          vlidation_lst[[j]]=intP_v_fltr
          j=j+1
          
        }
        # Build QA Chart ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if(!is.na(all(intP_df)) & mod=="op"){
          intP_C=ggplot() +
            geom_point(data=intP_df,aes(x=lon,y=lat,color=z),alpha=0.15)+
            geom_point(data=P_ii_f,aes(x=lon,y=lat),color="white",size=1,alpha=0.15)+
            geom_point(data=P_ii_f,aes(x=lon,y=lat,color=P_measure),size=0.5)+
            scale_color_gradientn(colours = edited_palette,breaks =cols,limits = c(0,50))
          intP_C
          
          p_pth_i=paste0(p_pth,p_st_y_fltr$time[ii],"_id-",source_id_i)
          # Export Interpolation Results ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ggsave(filename=paste0(p_pth_i,".jpg"),intP_C,dpi = 100)
          write.csv(intP_df,paste0(p_pth_i,".csv"),row.names=F)
        }
      }
    }
    print(year_i)
  }
  
  if(mod=="v"){
    # Export Interpolation Results ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    validation_dt=Reduce(rbind,vlidation_lst)
    v_pth=paste0(v_pth,methodology,".csv")
    fwrite(validation_dt,v_pth)
  }
  tictoc::toc()
}

### 2.2.1 Zonal Sampler ----------------------------------------------------------------------------------
if(Type_of_runing=="u_t"){
  P_valid_c=dplyr::filter(P_ii_f,zone_c==1)
  meta_zones="G:/code/Recharge_Model/ANA/Rain_Zones_V2.shp"
  zone_envelope=st_read(meta_zones)
  P_valid=zonal_sampler(valid_st=P_valid_c,envelope=zone_envelope,zonal_ratio=0.1)
}

# Func.
zonal_sampler=function(valid_st,envelope,zonal_ratio){
  # biuld Rep's areas for each St's ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  P_valid_st = st_sf(st_as_sf(valid_st, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84"))
  P_valid_vor=st_collection_extract(st_voronoi(st_union(P_valid_st)))
  zone_envelope=st_read(envelope)
  P_valid_are=st_join(st_sf(st_intersection(st_cast(P_valid_vor),zone_envelope)),P_valid_st, join = st_contains) %>%
    mutate(area_km2=as.numeric(st_area(.))/10^6) %>% dplyr::filter(.,!is.na(idstation),) %>% as.data.table(.,key="idstation")
  
  
  # Sample Stations until get rep area ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  P_valid_fltr=P_valid_are[!is.na(idstation),]
  P_valid_rep=P_valid_fltr[,tot_valid_area:=as.numeric(zonal_ratio)*nrow(P_valid_fltr)*round(median(area_km2,na.rm=T),0)][,rep_ratio:=area_km2/tot_valid_area]
  if(nrow(P_valid_rep)>3){
    P_valid_i=sample_n(P_valid_rep, 1)  
    for(i in 1:nrow(P_valid_rep)){
      # whan Achive Rep area in first try with 1 St.
      if(P_valid_i$area_km2 > P_valid_rep$tot_valid_area[1] ){P_valid=as.data.table(P_valid_i)[,st_ratio:=round(nrow(P_valid_i)/nrow(P_valid_rep),2)]
      return(P_valid)
      break}else{
        # whan Achive Rep area in secuned try with 1 St.
        P_valid_ii=sample_n(P_valid_rep[idstation != P_valid_i$idstation], 1)
        if(P_valid_ii$area_km2 > P_valid_rep$tot_valid_area[1] ){P_valid=as.data.table(P_valid_ii)[,st_ratio:=round(nrow(P_valid_ii)/nrow(P_valid_rep),2)]
        return(P_valid)
        rm(P_valid_ii)
        break}
        # whan Achive Rep area in secuned try with many St.
        P_valid_i=rbind(P_valid_i,P_valid_ii)
        if(sum(P_valid_i$area_km2) > P_valid_rep$tot_valid_area[1] ){
          P_valid=P_valid=as.data.table(P_valid_i)[,st_ratio:=round(nrow(P_valid_i)/nrow(P_valid_rep),2)]
          return(P_valid)
          break}
      }
    } 
  }else{
    P_valid=data.frame("idstation"=-999,"time"=P_valid_rep$time[1],
                       "P_measure"=NA,"flag"=NA,"zone_n"=P_valid_rep$zone_n[1],"geometry"=NA,
                       "area_km2"=NA,"rep_ratio"=NA,"st_ratio"=NA)}
}



## 2.3 Rain Validation IDX ==============================================================================================
if(Type_of_runing=="u_t"){
  
  # Get Validation DB  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  methodology="IDW"
  folderPath="G:/Layers/Geohydrology/Hydrometeorolgy/data/GDB/Tebular/Pdaily_BD/Stations/RAW"
  metafile="G:/Layers/Geohydrology/Hydrometeorolgy/data/GDB/Tebular/Pdaily_BD/Stations/RAW/MetaData/stations_P.csv"
  meta_zones="G:/Layers/Geohydrology/Geohydrology/code/Recharge_Model/ANA/Rain_Zones_V2.shp"
  synoptic_status=fread("Q:/Projects/Open/Active/Recharge_Climate_Models/Data/ANA/GDB/Tebular/synoptic_classification_1948-20200630_ANA.csv",header=T)
  synoptic_code=fread("Q:/Projects/Open/Active/Recharge_Climate_Models/Data/ANA/GDB/Tebular/Synoptic_Codes_V2.csv")
  synoptic_dt=synoptic_status_bulder(synoptic_status,synoptic_code)
  synoptic_dt$time=as.Date(synoptic_dt$time)
  pattern="data"
  validation_db=fread(paste0(Background_path,"/Hydrometeorolgy/data/GDB/Tebular/Pdaily_BD/Vaildation_dt_",
                             methodology,".csv")) %>%
    dplyr::filter(.,time!="") %>%
    mutate(time=as.Date(time, format ="%d/%m/%Y")) %>%  as.data.table()
  
  # Get Impacts Elements ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p_net_denstiy=net_denstiy(folderPath,metafile,pattern,meta_zones) %>% 
    mutate(time=as.Date(time, format ="%d/%m/%Y")) %>%
    dplyr::filter(.,!is.na(time)) %>% as.data.table(p_net_denstiy,key=c("zone_c","time")) # See 2.3.1
  
  synoptic_dt=synoptic_status_bulder(synoptic_status,synoptic_code) %>%
    mutate(time=as.Date(time))
  
  p_mean_intensity=mean_intensity(folderPath,metafile,pattern,meta_zones) %>% # See 2.3.4
    mutate(time=as.Date(time))
  # Combine Elements  to validation DB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  validation_db_1=validation_db[,month:=month(time)][,year:=year(time)][,
                                                                        time:=as.Date(time,format="%d/%m/%Y")][
                                                                          p_net_denstiy,on=.(zone_c=zone_c,time=time)]
  validation_db_2=validation_db_1[!is.na(idstation) & zone_n !=0 & zone_n !="",]
  validation_db_3=left_join(validation_db_2,synoptic_dt)
  validation_db_4=left_join(validation_db_3,p_mean_intensity)
  
  # Set Analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ranks=c("zone_n") # ,"synoptic_group"
  methodology="IDW" # methodology = "IDW" ; "kriging"
  zone_dt=validation_idx_bulder(validation_db_4,ranks)
  sum(as.numeric(zone_dt$n))
}
# Func.
validation_idx_bulder=function(validation_db,ranks){
  
  # Set nested function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  n_rmsd=function(data){
    A=as.matrix(subset(data,,"z"))
    B= as.matrix(subset(data,,"P_measure"))
    fungible::rmsd(A,B, Symmetric = F, IncludeDiag = TRUE)
  }
  n_bias=function(data){
    sum((subset(data,,"P_measure")-subset(data,,"z")))/NROW(subset(data,,"P_measure"))
  }
  n_rsq=function(data){
    mod = lm(P_measure ~ z, data = data)
    rsq= summary(mod)$r.squared
  }
  n_slope=function(data){
    mod = lm(P_measure ~ z, data = data)
    slope= summary(mod)$coeff[2]
    
  }
  n_intercept=function(data){
    mod = lm(P_measure ~ z, data = data)
    intercept=summary(mod)$coeff[1]
  }
  n_density=function(data){
    density = mean(data$density,na.rm=T)
  }
  
  # Nest DFs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(length(ranks)==2){
    validation_ndb=validation_db %>% group_by(.[[ranks[1]]],.[[ranks[2]]]) %>% nest()
    colnames(validation_ndb)=c(ranks[1],ranks[2],"data") 
  }
  if(length(ranks)==1){
    validation_ndb=validation_db %>% group_by(.[[ranks[1]]]) %>% nest()
    colnames(validation_ndb)=c(ranks[1],"data") 
  }
  # Ran IDX ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  idx_tbl =mutate(validation_ndb,
                  rmsd=round(as.numeric(purrr::map(data,.f=n_rmsd)),2),
                  bias=round(as.numeric(purrr::map(data,.f=n_bias)),2),
                  rsq=round(as.numeric(purrr::map(data,.f=n_rsq)),2),
                  slope=round(as.numeric(purrr::map(data,.f=n_slope)),2),
                  intercept=round(as.numeric(purrr::map(data,.f=n_intercept)),2),
                  mean_density=round(as.numeric(purrr::map(data,.f=n_density)),2),
                  n=purrr::map(data,.f=NROW)) %>% dplyr::select(.,-data) %>% as.data.table(.)
  return(idx_tbl)
}

### 2.3.1 Get St Net Density --------------------------------------------------------------------------------------------
if(Type_of_runing=="u_t"){
  folderPath="G:/Layers/Geohydrology/Hydrometeorolgy/data/GDB/Tebular/Pdaily_BD/Stations/RAW"
  metafile="G:/Layers/Geohydrology/Hydrometeorolgy/data/GDB/Tebular/Pdaily_BD/Stations/RAW/MetaData/stations_P.csv"
  meta_zones="G:/Layers/Geohydrology/Geohydrology/code/Recharge_Model/ANA/Rain_Zones_V2.shp"
  pattern="data"
  tictoc::tic()
  p_net_denstiy=net_denstiy(folderPath,metafile,pattern,meta_zones)
  tictoc::toc()
}
# Func.
net_denstiy=function(folderPath,metafile,pattern,meta_zones){
  # Filter by Pattern - optional ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(missing(pattern)==F){ptr=pattern}else(ptr = NULL)
  # Build Meta Data file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  folder_df=list.files(path=folderPath,pattern=ptr) %>% as.data.frame() %>% dplyr::rename(.,"filename"=".") %>%
    mutate(filepath=list.files(path=folderPath,full.names = T,pattern=ptr))
  read_core=function(filepath){
    fread(filepath)
  }
  # Get Files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  future::plan(multicore)
  folder_db=mutate(folder_df,df=purrr::map(filepath,.f=fread))%>%
    tidyr::unnest(.) %>% as.data.table(.,key="idstation")
  meta_db=fread(metafile)%>% as.data.table(.,key="ID")
  
  st_db=folder_db[meta_db][!is.na(lat),c("idstation","time","lat","lon","zone_c","zone_n")] %>% as.data.table(.,key="zone_n")  
  meta_zones_db=st_read(meta_zones) %>% group_by(zone) %>%
    summarise(area_10km2=sum(area,na.rm=T)/10^2) %>% as.data.table(.,key="zone")  
  # Get Number of St in area in each time ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  stt_db=st_db[meta_zones_db,on=.(zone_n=zone)] %>% group_by(time,zone_n,zone_c,area_10km2) %>%
    summarise(n=n()) %>%
    mutate(density=(n/area_10km2))
  
  return(stt_db)
  
}

### 2.3.2 Get synoptic_status -------------------------------------------------------------------------------------------
if(Type_of_runing=="u_t"){
  synoptic_status=fread("Q:/Projects/Open/Active/Recharge_Climate_Models/Data/ANA/GDB/Tebular/synoptic_classification_1948-20200630_ANA.csv",header=T)
  synoptic_code=fread("Q:/Projects/Open/Active/Recharge_Climate_Models/Data/ANA/GDB/Tebular/Synoptic_Codes_V2.csv")
  synoptic_dt=synoptic_status_bulder(synoptic_status,synoptic_code)
}
# Func.
synoptic_status_bulder=function(synoptic_status,synoptic_code){
  synoptic_status_melt=melt(as.data.table(synoptic_status),
                            id.vars="ddmm",
                            variable.name = "year",
                            value.name = "code",
                            value.factor = FALSE) %>%
    mutate(time=as.Date(str_remove_all(paste(ddmm,"-",year)," "),format="%d-%m-%Y")) %>% as.data.table(.,key="code")
  synoptic_dt=left_join(synoptic_status_melt,synoptic_code) %>% dplyr::distinct(.,time,.keep_all=T) %>%
    dplyr::filter(.,!is.na(time))%>% 
    subset(.,,c("time","abbr","name","name_h","synoptic_group","synoptic_group_c"))
  fwrite(synoptic_dt,"G:/Layers/Geohydrology/RB/synoptic_dt.csv")
  synoptic_dt=fread("G:/Layers/Geohydrology/RB/synoptic_dt.csv")
  return(synoptic_dt)
}

### 2.3.4 Get Mean intensity ===================================================================
if(Type_of_runing=="u_t"){
  folderPath="G:/Layers/Geohydrology/Hydrometeorolgy/data/GDB/Tebular/Pdaily_BD/Stations/RAW"
  metafile="G:/Layers/Geohydrology/Hydrometeorolgy/data/GDB/Tebular/Pdaily_BD/Stations/RAW/MetaData/stations_P.csv"
  meta_zones="G:/Layers/Geohydrology/Geohydrology/code/Recharge_Model/ANA/Rain_Zones_V2.shp"
  pattern="data"
  tictoc::tic()
  p_mean_intensity=mean_intensity(folderPath,metafile,pattern,meta_zones)
  tictoc::toc()
}
# Func.

mean_intensity=function(folderPath,metafile,pattern,meta_zones){
  # Filter by Pattern - optional ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(missing(pattern)==F){ptr=pattern}else(ptr = NULL)
  # Build Meta Data file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  folder_df=list.files(path=folderPath,pattern=ptr) %>% as.data.frame() %>% dplyr::rename(.,"filename"=".") %>%
    mutate(filepath=list.files(path=folderPath,full.names = T,pattern=ptr))
  # 2.1.1 Build files Colector Function --------------------------------------------------------
  read_core=function(filepath){
    fread(filepath)
  }
  # Get Files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  future::plan(multicore)
  folder_db=mutate(folder_df,df=purrr::map(filepath,.f=fread))%>%
    tidyr::unnest(.) %>% as.data.table(.,key="idstation")
  meta_db=fread(metafile)%>% as.data.table(.,key="ID")
  
  st_db=folder_db[meta_db][!is.na(lat),c("idstation","time","lat","lon","zone_c","zone_n","measure")] %>% as.data.table(.,key="zone_n")  
  st_intensity_dt=st_db %>% group_by(zone_c,zone_n,time) %>%
    mutate(time=as.Date(time, format ="%d/%m/%Y")) %>%
    summarise(mean_intensity=round(mean(measure,na.rm=T),2),
              median_intensity=round(median(measure,na.rm=T),2)) %>% as.data.table(.,key="zone_c")  
  # Get Number of St in area in each time ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  st_intensity_flyr=st_intensity_dt[zone_n!=0 & !is.na(time),]
  st_intensity_flyr$time=as.Date(st_intensity_flyr$time)
  
  fwrite(st_intensity_flyr,"G:/Layers/Geohydrology/RB/st_intensity_flyr.csv")
  st_intensity_flyr=fread("G:/Layers/Geohydrology/RB/st_intensity_flyr.csv") 
  
  
  return(st_intensity_flyr)
}

# 3. Visualization ###########################################################################
## 3.1 Rain DB Visualization =================================================================
if(Type_of_runing=="u_t"){
  achieving_goals_C=goal_vis(dt_IDW=zone_dt_IDW,
                             dt_kriging=zone_dt_kriging,
                             prm="zone_n",
                             cols_vlu=cols,
                             export="achieving_goals_Cp_V2")
}
# Func.
goal_vis=function(dt_IDW,dt_kriging,prm,cols_vlu,export){
  # Chart ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  goals_df=bind_rows(dt_IDW,dt_kriging)
  achieving_goals_C=ggplot(goals_df)+
    geom_rect(aes(xmin = -0.5, xmax = 0.5, ymin = 0.75, ymax = 1),fill="blue",alpha=0.01)+
    geom_rect(aes(xmin = -1.5, xmax = 0.5, ymin = 0.5, ymax = 0.75),fill="green",alpha=0.01)+
    geom_rect(aes(xmin = -1.5, xmax = -0.5, ymin = 0.75, ymax = 1),fill="green",alpha=0.01)+
    geom_rect(aes(xmin = -1.5, xmax = 0.5, ymin = 0, ymax = 0.5),fill="red",alpha=0.01)+
    geom_rect(aes(xmin = -6, xmax = -1.5, ymin = 0, ymax = 1),fill="red",alpha=0.01)+
    geom_point(aes_string(x="bias",y="rsq",color=prm,shape="model"),size=6,alpha=1)+
    labs(x="Bias",y="RSQ")+
    theme_minimal()
  if(is.character(cols_vlu)==T){
    achieving_goals_C=achieving_goals_C+scale_color_manual(values=cols_vlu)
  }
  # Export to PPT
  if(is.character(export)){
    doc=read_pptx()
    doc=add_slide(doc, layout = "Title and Content", master = "Office Theme")
    achieving_goals_Cp=rvg::ph_with_vg_at(x=doc,  ggobj =achieving_goals_C, left=0.1, top=0.1, width=9, height=6)
    print(achieving_goals_Cp, target = paste0("G:/Layers/Geohydrology/Geohydrology/code/Recharge_Model/ANA/",export,".pptx"))  
  }
  return(achieving_goals_C)
}





