#  Recharge Code ##################################################################################
message("Recharge Code")
# 0.Initiation ===================================================================================
message("0.Initiation")
# 0.1 Get Running Parameters ----------------------------------------------------------------------
message("0.1 Get Running Parameters")
cs_crs="+proj=longlat +datum=WGS84"
Type_of_runing="t"
# Unit Test =======================================================================================
if(Type_of_runing=="u_t"){
  # Load Libraries ---------------------------------------------------------------------------------
  library(sp)
  library(sf)
  library(maptools)
  library(rgeos)
  library(rgdal)
  library(readxl)
  library(googledrive)
  library(data.table)
  library(raster)
  library(DT)
  library(stringr)
  library(openxlsx)
  library(plotly)
  library(officer)
  library(grid)
  library(utils)
  library(aws.s3)
  library(tibble)
  library(RColorBrewer)
  library(tidyr)
  library(lubridate)
  library(profvis)
  library(future)
  library(furrr)
  library(tidyverse)
  library(leaflet)
  library(parallel)
  library(doParallel)
  library(furrr)
  # 1. Get Parameters ##########################################################################################
  # 1.1 Grids =================================================================================================
  model_sp_YT=st_read("G:/Layers/Geohydrology/Geohydrology/code/Recharge_Model/RAW/Inputs/csv_DB/SP_V2.shp")%>%
    dplyr::rename(.,poly_id=ELNO)%>%
    mutate(area_km2=as.numeric(st_area(.))/10^6)
  
  
  natioanal_grid_V1=st_read("G:/Layers/Geohydrology/Geohydrology/code/Recharge_Model/RAW/Inputs/csv_DB/ETp_Grid_1000_poly.shp")%>%
    dplyr::select(.,c(lon,lat,geometry)) %>%
    mutate(area_km2=as.numeric(st_area(.))/10^6)
  
  AMC_YT=fread("G:/Layers/Geohydrology/Geohydrology/code/Recharge_Model/RAW/Inputs/csv_DB/AMC_initial.csv") %>% as.data.table(.,key="poly_id")
  
  
  tictoc::tic()
  Recharge_2018_2021_03=rechage_model(natioanal_grid=natioanal_grid_V1,
                                      model_sp=model_sp_YT,
                                      AMC=AMC_YT,
                                      start=as_datetime("2018-10-01"),
                                      end=as_datetime("2021-03-01"),
                                      build_connection_netwotk=F,
                                      conn_p=sf::st_read(paste0(getwd(),"/p_ts_md.shp")),
                                      conn_etp=sf::st_read(paste0(getwd(),"/etp_ts_md.shp")),
                                      solver="DREAM",
                                      export=F,
                                      hydro_model="FeFlow",
                                      interval="month",
                                      split=5, # 2...5
                                      intail_soil_moisture=15,
                                      pth="G:/Layers/Geohydrology/Geohydrology/RB/Recharge_ff_V4.pow"
  )
  tictoc::toc()
  
  fwrite(Recharge_2018_2021_03,"G:/Layers/Geohydrology/Geohydrology/RB/Recharge_2018_2021_03_1.csv",row.names = F)
}
# 2. Run Model ###############################################################################################
rechage_model=function(natioanal_grid,model_sp,AMC,start,end,build_connection_netwotk,conn_p,conn_etp,solver,export,hydro_model,interval,split,intail_soil_moisture,pth,cell4test){
  options(future.globals.maxSize=+Inf)#+Inf # 1500*1024^2
  message("1.1 Build Intersected National DB") # 1.1 Build Intersected National DB ===========================
  nat_db=biuldb(
    natioanal_grid=natioanal_grid,
    model_sp=model_sp,
    p_pth="G:/Tools/DREAMhsi/data/tabular/Pdaily_BD",
    etp_pth="G:/Tools/DREAMhsi/data/tabular/ETP_DB",
    st=start,
    ed=end
  )
  message("1.2 Get Connection Network") # 1.2 Get Connection Network =========================================
  if(build_connection_netwotk==T){
    message("start Buiding of new connection Network")
    connection_netwotk=conn_builder(
      folder_db=nat_db$folder_db,
      nat_mod_grd=nat_db$nat_mod_grd,
      nat_mod_dt=nat_db$nat_mod_dt,
      save_nw=T
    ) 
  } else{
    message("Use exist connection Network")
    connection_netwotk=list("conn_p"=conn_p,"conn_etp"=conn_etp)
  }
  
  message("1.3 Buid Model DB") # 1.3 Biuld Model DB =========================================================
  P_ETP=nationl2model(
    nat_mod_dt=nat_db$nat_mod_dt,
    folder_db=nat_db$folder_db,
    conn_p=connection_netwotk$conn_p,
    conn_etp=connection_netwotk$conn_etp
  )
  message("2.1 Recharge data constrocation") # 2.1 Recharge data constrocation ===============================
  Recharge_cons_DB = recharge_cons(P_ETP,AMC,model_sp)
  
  message("2.2 Run recharge model") # 2.2 Run recharge model =================================================
  if(split==0){
    if(solver=="DREAM"){
      message("Apply DREAM")
      Recharge=run_DREAM(Recharge_cons_DB=Recharge_cons_DB,intail_soil_moisture) 
    }  
  }
  if(split>0){
    # Set spliter
    n=nrow(Recharge_cons_DB)
    jumps=floor(n/split)
    
    if(solver=="DREAM"){
      message("Apply DREAM- by loop")
      # Initail
      Recharge=run_DREAM(Recharge_cons_DB=Recharge_cons_DB[1:jumps,],intail_soil_moisture)
      # Main
      for(i in 2:split-1){
        Recharge_i=run_DREAM(Recharge_cons_DB=Recharge_cons_DB[((i-1)*jumps+1):(i*jumps+1),],intail_soil_moisture)
        Recharge=rbind(Recharge,Recharge_i)
        message(paste0(100*round(((i*jumps+1)/nrow(Recharge_cons_DB)),4) ,"% of the model polygons have been completed"))
      }
      # Rest
      Recharge_i=run_DREAM(Recharge_cons_DB=Recharge_cons_DB[(nrow(table(Recharge$poly_id))+1):n,],intail_soil_moisture)
      Recharge=rbind(Recharge,Recharge_i)
    }  
  }
  Recharge=unique(Recharge)
  # 3. Post prossing  ######################################################################################### 
  message("3.1 Chack Reasults")  # 3.1 Chack Reasults =========================================================
  if(!is.null(cell4test)){
    Recharge_i=Recharge[poly_id==cell4test,c("poly_id","ET_final","Recharge_final","date","P","Soil_moisture","ETP")]
    Recharge_Cp=Recharge_C(Recharge_i,
                           lab_size=5 # ,
                           # l_limt=min(Recharge_i$Recharge_final,na.rm=T),
                           # u_limit=max(Recharge_i$Recharge_final,na.rm=T)
                           ) 
  } else {
    Recharge_Cp=NULL
  }
  Recharge_sum=Recharge %>%
    mutate(year=year(date)) %>%
    group_by(year) %>%
    dplyr::summarise(Recharge_final=sum(Recharge_final,na.rm=T),
                     P=sum(P,na.rm=T),
                     ET=sum(ET,na.rm=T))
  
  message("3.2 Export to Hydrogeologecal model") # 3.2 Export to Hydrogeologecal model =========================
  if(hydro_model=="FeFlow"){
    message('Exporting to FeFlow format')
    rec2ff(Recharge_db=Recharge,
           interval=interval,
           funcIDX=as.data.table(table(Recharge$poly_id))[, funcid := .I][, poly_id := as.numeric(V1)][,c("funcid","poly_id")],
           pth=pth)
  }
  if(hydro_model=="GMS"){
    message('Exporting to GMS format')
    rec2GMS(Recharge_db=Recharge,
            interval=interval,
            pth=pth)
  }
  Recharge_lst=list("Recharge"=Recharge,"Recharge_sum"=Recharge_sum,"Recharge_Cp"=Recharge_Cp)
  return(Recharge_lst)
  message("The model has been successfully finsh and you will receive a kiss") # DONE ========================
}




