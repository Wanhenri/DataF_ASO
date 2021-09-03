
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

# *******************************************
# Converter dados de prev. do CPTEC Bin >> Tif

#setwd("C:/Users/wanme/Downloads/ASO_2021/prev/JAS/")

#r <- raster(xmx=-32.5, xmn=-78.5, ymn=-40.5, ymx=5.5, ncol=46, nrow=46, crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#r[] <- readBin("previsao_anomalia_CATEG_cptec1.2_inmet_funceme_2021_tmp.bin", 'double', n=2116, size=4, endian='little')
#x <- flip(r, 'y')
#sr <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#x <- projectRaster(x, crs = sr)
#projection(x)
#writeRaster(x, "prev_anomalia_JAS2021_temp.tif")

# *******************************************
##GEra o viirs
for(j in 2:1){
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
    
    # shapefile das Areas de Protecao
    setwd("C:/Users/wanme/Downloads/ASO_2021/shapefiles/")
    ap <- shapefile("WDPA_Jul2020_AS.shp")
    #ap <- spTransform(ap, CRS=CRS("+proj=longlat +datum=WGS84"))
    
    sh <- read_sf("WDPA_Jul2020_AS.shp")
    sh
    frame1 <- as.data.frame(sh)
    frame1
    
    write.table(frame1[1], "shape.csv", sep=";", dec = ".")
    
    
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
    #Em start_sys 0 é para quando quer q rode o mês atual q vc esta; 1 para rodar o mês adiante
    #Em end_sys, se colocar 0 em start_sys precisa colocar 2; se colocar 1 em start sys precisa colocar 3
    start_sys <-  floor_date(Sys.Date() %m+% months(0), 'month') - years(i)
    end_sys <-  ceiling_date(Sys.Date() %m+% months(2), 'month') %m-% days(1) - years(i)
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
    #head(f.filtro)
    fFile <- na.omit(f.filtro)
    fFile
    
    filess <- as.data.frame(fFile)
    filess2 <- na.omit(filess)
    filess2
    
    #filess2 <- subset(filess2, select= -c(seca, alerta, name_ps, coords.x1, coords.x2))
    #filess2

    
    rm(focos)
    rm(viirs)
    setwd("C:/Users/wanme/Downloads/ASO_2021/20210830/")
    write.table(filess2, NameOutFilter, sep=";", dec = ".", row.names = F)
    #write.table(f.filtro, NameOutFilter, sep=";", dec = ".", row.names = F)
    
    fim <- paste0(" FIM ",shapes[j],"_",dateFormatName)
    print(paste0("******fim: ", fim))
    
    rm(NameOutFilter)
    rm(NameOutFilterDateFormat)
    rm(dateFormatName)
  }
}

###Realiza a contagem

for(j in 1:1){
  shapes <- c("WDPA_PI","capacidade")
  print(paste0("Shapefile: ", shapes[j])) 
  
  if( j == 1) {
    print(paste0("Shapefile: ", shapes[j] )) 
    print("C:/Users/wanme/Downloads/ASO_2021/shapefiles/")
    setwd("C:/Users/wanme/Downloads/ASO_2021/shapefiles/")
    
    ap <- shapefile("WDPA_Jul2020_AS.shp")
    ap <- spTransform(ap, CRS=CRS("+proj=longlat +datum=WGS84"))
    #ap
    apDT <- as.data.frame(ap)
    #apDT
  } else {
    #rm(ap)
    print(paste0("Shapefile: ", shapes[j] )) 
    print("C:/Users/wanme/Downloads/ASO_2021/shapefiles/")
    setwd("C:/Users/wanme/Downloads/ASO_2021/shapefiles/")
    
    assent <- shapefile("Assentamento_Brasil_Total_albers.shp")
    assent <- spTransform(assent, CRS=CRS("+proj=longlat +datum=WGS84"))
    
    
    ap <- assent
  }
  
  for(i in 4:1) {
    #Em start_sys 0 é para quando quer q rode o mês atual q vc esta; 1 para rodar o mês adiante
    #Em end_sys, se colocar 0 em start_sys precisa colocar 2; se colocar 1 em start sys precisa colocar 3
    start_sys <-  floor_date(Sys.Date() %m+% months(0), 'month') - years(i)
    end_sys <-  ceiling_date(Sys.Date() %m+% months(2), 'month') %m-% days(1) - years(i)
    dateFormat <- format(start_sys,"%Y")
    dateFormatName <- format(start_sys,"%Y%m")
    
    print(paste0("Ano: ", dateFormat ))  
    print(paste0("Inicio: ", start_sys, "  fim: ", end_sys))
    # Abrir focos de queimadas de cada abn
    setwd("C:/Users/wanme/Downloads/ASO_2021/20210830/")
    
    # NameOutFilter <- paste0("viirs_",dateFormatName,"_",shapes[j],"_filtro.csv")
    
    NameOutFilterDateFormat <- paste0("viirs_",dateFormatName,"_",shapes[j],"_filtro.csv")
    print(paste0("******Inicio: ",NameOutFilterDateFormat))
    
    viirs <- read.csv2(NameOutFilterDateFormat, header = T, sep = ";")
    #viirs
    filess2 <- subset(viirs, select= -c(seca, alerta, name_ps, coords.x1, coords.x2, confidence, frp, new.att))
    filess3 <- subset(filess2, select= -c(fc_2017,fc_2018,fc_2019,fc_2020,dif_fcs, rgrs_vl))
    filess4 <- subset(filess3, select= -c(Trend, Sum_Frs, abv_tmp,m_2017_))
    
    #op <- ddply(filess4, .(id,acq_date,WDPA_PI,ISO3,WDPAID,ORIG_NA,DESIG_E,ap_sqkm,ps_sqkm,prp_p_p,blw_prc),transform,count=length(WDPA_PI))
    #op <- table(filess4[1:11]) transform(filess4, count(rep(id,acq_date,WDPA_PI,ISO3,WDPAID,ORIG_NA,DESIG_E,ap_sqkm,ps_sqkm,prp_p_p,blw_prc)))
    op <- plyr::ddply(filess4, .(WDPA_PI,ISO3), transform, count=length(WDPA_PI))
    #op 
    ttt <- distinct(op,WDPA_PI, .keep_all =  TRUE )
    ttt
    
    rm(viirs)
    setwd("C:/Users/wanme/Downloads/ASO_2021/20210830/")
    CountFocus <- paste0("viirs_",dateFormatName,"_",shapes[j],"_filtro_Count.csv")
    write.table(ttt, CountFocus, sep=";", dec = ".", row.names = F)
    
    fim <- paste0(" FIM ",shapes[j],"_",dateFormatName)
    print(paste0("******fim: ", fim))
    
    #rm(NameOutFilter)
    rm(CountFocus)
    rm(dateFormatName)
  }
}


# *******************************************
# Media ponderada de prev de chuva e temp em cada AP

# se precisar abrir novamente a o shapefile de AP
secasItem <- c("secas/","ASO_Mapa_Seca.tif","prev_metoffice/","prob_AboveTemp_AS.tif","prob_BelowPrec_AS.tif","Assent_seca_SON.csv","Assent_Prob_aboveprec.csv","Assent_Prob_abovetemp.csv")

aereasProItem <- c("ap/","ASO_Mapa_Seca.tif", "prev/JAS/", "prev_anomalia_ASO2021_temp.tif", "prev_anomalia_ASO2021_prec.tif", "AP_seca_SON.csv", "AP_Prob_aboveprec.csv","AP_Prob_abovetemp.csv")

l <- list(SItem = secasItem, ApITem = aereasProItem)

for(j in 2:1){
  shapes <- c("WDPA_PI","capacidade")
  print(paste0("Shapefile: ", shapes[j])) 
  
  if( j == 1) {
    print(paste0("Shapefile: ", shapes[j] )) 
    setwd("C:/Users/wanme/Downloads/ASO_2021/shapefiles/")
    
    ap <- shapefile("WDPA_Jul2020_AS.shp")
    ap <- spTransform(ap, CRS=CRS("+proj=longlat +datum=WGS84"))
    
  } else {
    #rm(ap)
    print(paste0("Shapefile: ", shapes[j] )) 
    setwd("C:/Users/wanme/Downloads/ASO_2021/shapefiles/")    
    assent <- shapefile("Assentamento_Brasil_Total_albers.shp")
    assent <- spTransform(assent, CRS=CRS("+proj=longlat +datum=WGS84"))
    
    
    ap <- assent
  }
  
  x <- c("SItem","ApITem")
  for (nameItem in x){
    #print(l[[nameItem]][indexItemName])
    print(l[[nameItem]])
    
    setwd("C:/Users/wanme/Downloads/ASO_2021/secas/")
    #secas 
    #'ASO_Mapa_Seca_secas.tif'
    secas <- raster(l[[nameItem]][2])
    
    #secas
    #'C:/Users/wanme/Downloads/ASO_2021/secas/'
    #setwd(NomeCaminhoASO)
    setwd("C:/Users/wanme/Downloads/ASO_2021/20210830/")
    ap_seca <- extract(secas, ap, fun=modal, na.rm=T, df=T, sp = T)
    #secas
    #'Assent_seca_JAS.csv'
    write.table(ap_seca, l[[nameItem]][6], sep=";", dec = ".")
  }
}