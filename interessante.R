
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

#t_limpo_t4.csv

for(j in 1:1){
  shapes <- c("WDPA_PI","capacidade")
  
  for(i in 4:1) {
    #Em start_sys 0 é para quando quer q rode o mês atual q vc esta; 1 para rodar o mês adiante
    #Em end_sys, se colocar 0 em start_sys precisa colocar 2; se colocar 1 em start sys precisa colocar 3
    start_sys <-  floor_date(Sys.Date() %m+% months(0), 'month') - years(i)
    end_sys <-  ceiling_date(Sys.Date() %m+% months(2), 'month') %m-% days(1) - years(i)
    dateFormat <- format(start_sys,"%Y")
    dateFormatName <- format(start_sys,"%Y%m")
    
    CountFocus <- paste0("viirs_",dateFormatName,"_",shapes[j],"_filtro_Count.csv")
    print(paste0("******Inicio: ",CountFocus))
    
    setwd("C:/Users/wanme/Downloads/ASO_2021/20210830/")
    viirs1 <- read.csv2(CountFocus, header = T, sep = ";")
    viirs1
    viirsFrame1 <- as.data.frame(viirs1)
    viirsFrame2 <- subset(viirsFrame1, select= -c(id, acq_date, WDPAID, blw_prc))
    viirsFrame2
    
    limpo <- paste0("viirs_",dateFormatName,"_",shapes[j],"_filtro_Count_cleanForPy.csv")
    
    write.table(viirsFrame2, limpo, sep=";", dec = ".", row.names = F)
    rm(viirs1)
    rm(viirsFrame1)
    rm(viirsFrame2)
  }
}




setwd("C:/Users/wanme/Downloads/ASO_2021/shapefiles/")
ap <- shapefile("WDPA_Jul2020_AS.shp")
ap <- spTransform(ap, CRS=CRS("+proj=longlat +datum=WGS84"))
ap
apDT <- as.data.frame(ap)
apDT1 <- subset(apDT, select= -c(blw_prc,Sum_Frs,Trend,abv_tmp,fc_2017,fc_2018,fc_2019,m_2017_,fc_2020,dif_fcs,rgrs_vl,seca,alerta,name_ps, WDPAID))
apDT1$count <- "0"
apDT1
setwd("C:/Users/wanme/Downloads/ASO_2021/20210830/")
write.table(apDT1, "Teste_para_Python_shape.csv", sep=";", dec = ".", row.names = F)

#trs <- apDT1
#trs

#viirsFrame1$WDPA_PI[2]
#viirsFrame1$count[2]

#342484
#which(viirsFrame1$WDPA_PI == "342484")
#which(apDT1$WDPA_PI == viirsFrame1$WDPA_PI[2])


#which(viirsFrame1$WDPA_PI == "342484") = viirsFrame1$count[477]

#position
#a <- which(viirsFrame2$WDPA_PI == "101761")
##value
#b <- viirsFrame2$count[477]
#positionValue <- paste0("position: ",a," value ",b)
#positionValue


#position <- viirsFrame2[2,]
#position
#apDT1[668,]
#which(apDT1$WDPA_PI == 10747)



write.table(NewDf, "t.csv", sep=";", dec = ".", row.names = F)
#########################################################
#rr <- anti_join(filess5,apDT1, by='WDPA_PI')
#rr



#viirs <- read.csv2("viirs_201709_WDPA_PI_filtro_test3.csv", header = T, sep = ";")
#viirs
#viirsFrame <- as.data.frame(viirs)
#viirsFrame
#filess2 <- subset(viirsFrame, select= -c(seca, alerta, name_ps, coords.x1, coords.x2, confidence, frp, new.att))
#filess3 <- subset(filess2, select= -c(fc_2017,fc_2018,fc_2019,fc_2020,dif_fcs, rgrs_vl))
#filess4 <- subset(filess3, select= -c(Trend, Sum_Frs, abv_tmp,m_2017_))
#filess5 <- subset(filess4, select= -c(id, acq_date,WDPAID))
#filess5
