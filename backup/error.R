
#version
# Fire Probability

library(sp) 
library(raster)
library(rgdal)
library(rgeos)
library(foreign)
library(ncdf4)
library(spatialEco)
library(dplyr)
library(s2)
library(sf)
#install.packages("lubridate")
library(lubridate)
#install.packages("zoo")
library(zoo)
#install.packages("foreach")
library(foreach)
library(data.table)
library(plyr)


for(j in 1:2){
  shapes <- c("WDPA_PI","capacidade")
  print(paste0("Shapefile: ", shapes[j])) 
  
  if( j == 1) {
    print(paste0("Shapefile: ", shapes[j] )) 
    print("C:/Users/wanme/Downloads/ASO_2021/shapefiles/")
    setwd("C:/Users/wanme/Downloads/ASO_2021/shapefiles/")
    
    ap <- shapefile("WDPA_Jul2020_AS.shp")
    ap
    ap <- spTransform(ap, CRS=CRS("+proj=longlat +datum=WGS84"))
    ap
    
    
  } else {
    #rm(ap)
    print(paste0("Shapefile: ", shapes[j] )) 
    print("C:/Users/wanme/Downloads/ASO_2021/shapefiles/")
    setwd("C:/Users/wanme/Downloads/ASO_2021/shapefiles/")
    
    assent <- shapefile("Assentamento_Brasil_Total_albers.shp")
    assent <- spTransform(assent, CRS=CRS("+proj=longlat +datum=WGS84"))
    #shapefile(assent, "Assentamentos_novo.shp")
    
    crs(assent)
    assent$area_sqkm <- area(assent) / 1000000
    head(assent)
    write.table(assent, "Tabela_completa_ASO2021_Assent2.csv", sep=";", dec = ".")
    
    ap <- assent
  }
  
  #for(i in 4:1)
  for(i in 4:1) {
    start_sys <-  floor_date(Sys.Date() %m+% months(1), 'month') - years(i)
    end_sys <-  ceiling_date(Sys.Date() %m+% months(3), 'month') %m-% days(1) - years(i)
    dateFormat <- format(start_sys,"%Y")
    dateFormatName <- format(start_sys,"%Y%m")
    
    print(paste0("Ano: ", dateFormat ))  
    print(paste0("Inicio: ", start_sys, "  fim: ", end_sys))
    
    # Abrir focos de queimadas de cada abn
    setwd("C:/Users/wanme/Downloads/ASO_2021/fires/")
    
    NameOutFilterDateFormat <- paste0("viirs_", dateFormat,".csv")
    print(paste0("******Inicio: ",NameOutFilterDateFormat))
    
    viirs <- read.csv2(NameOutFilterDateFormat, header = T, sep = ";")
    
    viirs$latitude <- as.numeric(as.character(viirs$latitude))
    viirs$longitude <- as.numeric(as.character(viirs$longitude))
    
    CRS.new <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    coordinates(viirs)<-~longitude+latitude # whatever the equivalent is in your
    proj4string(viirs) <- CRS.new 
    
    # convert in column of date
    viirs$acq_date <- as.Date(viirs$acq_date, "%Y-%m-%d")
    
    # selecionar os focos do trimestre de cada ano
    myfunc <- function(x,y){viirs[viirs$acq_date >= x & viirs$acq_date <= y,]}
    viirs <- myfunc(as.Date(start_sys), as.Date(end_sys))  
    
    NameOutFilter <- paste0("viirs_",dateFormatName,"_",shapes[j],"_filtro.csv")
    print(paste0("******Inicio: ",NameOutFilter))
    
    sf_use_s2(FALSE)  
    focos <- point.in.poly(viirs, ap)
    f.filtro <- subset(focos,shapes[j] >= 1)
    
    rm(focos)
    rm(viirs)
    setwd("C:/Users/wanme/Downloads/ASO_2021/20210830/")
    write.table(f.filtro, NameOutFilter, sep=";", dec = ".", row.names = F)
    
    fim <- paste0(" FIM ",shapes[j],"_",dateFormatName)
    print(paste0("******fim: ", fim))
    
    rm(NameOutFilter)
    rm(NameOutFilterDateFormat)
    rm(dateFormatName)
  }
}