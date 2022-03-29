# Load Libraries ###############################################################
library(sp)
library(sf)
library(nngeo)
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
Type_of_runing="t"
# Function.1 Build Geo grid by aggregation to set number of cells ##############
# Unit Test
if(Type_of_runing=="u_t"){
  outcrops_poly=st_read("Q:/Projects/Open/Models/Esat_Mt/data/ANA/initial_recharge_mdl_grid_V1.shp")
  tictoc::tic()  
  outcrops_aggr_joined=fixgeogrid(outcrops_poly=outcrops_poly,ncells_in_unit=125,initial_cell_size=2)
  tictoc::toc()
  plot(outcrops_aggr_joined)
  # st_write(outcrops_aggr_joined,"Q:/Projects/Open/Models/Esat_Mt/data/ANA/recharge_mdl_grid_V2.shp",delete_layer =T)
}
# Func
fixgeogrid=function(outcrops_poly,ncells_in_unit,initial_cell_size){
  units=as.data.frame(table(outcrops_poly$act_lyr))
  outcrops_aggr_joined=outcrops_poly[0,]
  u=1  
  for ( u in 1:nrow(units)){
    print(paste("%%%% Start unit: ",units$Var1[u], " %%%%"))
    outcrops_aggr=outcrops_poly %>% dplyr::filter(.,act_lyr==units$Var1[u])
    
    cells <- nrow(outcrops_aggr)
    i=1
    sz=initial_cell_size
    while (cells >ncells_in_unit) {
      print(cells)
      print(paste("itration: ",i))
      outcrops_aggr=geogrid_builder(raw_grid_poly=outcrops_aggr,area_size=sz)
      if(cells-nrow(outcrops_aggr)<10){
        sz=sz+0.5
        print(paste("New cell size: ",sz))
      }
      cells <- nrow(outcrops_aggr)
      i=i+1
    }
    
    outcrops_aggr_joined=rbind(outcrops_aggr_joined,outcrops_aggr)
    print(paste("%%%% End unit: ",units$Var1[u], " %%%%"))
  }
  return(outcrops_aggr_joined)
}

# Function.1.1 Build Geo grid by aggregation to set number of cells ============
geogrid_builder=function(raw_grid_poly,area_size){
  # Get Nearest Neighbor
  raw_grid_poly$n=seq(1,nrow(raw_grid_poly))
  raw_grid_poly$nn=st_nn(raw_grid_poly,raw_grid_poly,maxdist =Inf)
  
  # Calc sum Area
  area_grid_poly = raw_grid_poly %>% group_by(nn) %>% 
    dplyr::mutate(area_uni=sum(area,na.rm = T))
  
  # Union by barrier of size
  poly4uni= area_grid_poly %>% dplyr::filter(area_uni<=area_size) %>% 
    dplyr::summarise(area_uni=sum(area,na.rm = T))
  
  polyN4uni= area_grid_poly %>% dplyr::filter(area_uni>area_size) %>% 
    subset(.,,c("nn","area_uni","geometry"))
  
  # Reunion
  unin_grid_poly=rbind(poly4uni,polyN4uni) %>%
    mutate(area=round(as.numeric(st_area(.)/10^6),2),
           act_lyr=raw_grid_poly$act_lyr[1])
  
  return(unin_grid_poly)
}

