
# Fire Probability

library(sp) 
library(raster)
library(rgdal)
library(rgeos)
library(foreign)
library(ncdf4)
library(spatialEco)
library(dplyr)

# *******************************************
# Converter dados de prev. do CPTEC Bin >> Tif

setwd("C:/Dropbox/Bases_Geograficas/Cemaden/Report_Fires_AmericalSul/platform/prev/JAS/")

r <- raster(xmx=-32.5, xmn=-78.5, ymn=-40.5, ymx=5.5, ncol=46, nrow=46, crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
r[] <- readBin("previsao_anomalia_CATEG_cptec1.2_inmet_funceme_2021_tmp.bin", 'double', n=2116, size=4, endian='little')
x <- flip(r, 'y')
sr <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
x <- projectRaster(x, crs = sr)
projection(x)
writeRaster(x, "prev_anomalia_JAS2021_temp.tif")


for(j in 1:2){
  shapes <- c("WDPA_PI","capacidade")

  if( j == 1) {
    setwd("c:/Dropbox/Bases_Geograficas/Cemaden/Report_Fires_AmericalSul/ASO_2021/shapefiles/")
    ap <- shapefile("WDPA_Jul2020_AS.shp")
    ap <- spTransform(ap, CRS=CRS("+proj=longlat +datum=WGS84"))
    #WDPA_PI
  } else {
    # cruzar os pontos de queimadas com as areas de protecao
    setwd("C:/Users/wanme/Downloads/ASO_2021/assent/")
    head(assent)
    ap <- assent
    #capacidade
  }
  
  for(i in 4:1) {
    start_sys <-  floor_date(Sys.Date() %m+% months(1), 'month') - years(i)
    end_sys <-  ceiling_date(Sys.Date() %m+% months(3), 'month') %m-% days(1) - years(i)
    dateFormat <- format(start_sys,"%Y")
  
    print(paste0("Ano: ", dateFormat ))  
    print(paste0("Inicio: ", start_sys, "  fim: ", end_sys))
  
    # Abrir focos de queimadas de cada abn
    setwd("C:/Users/wanme/Downloads/ASO_2021/fires/")
  
    NameOutFilterDateFormat <- paste0("viirs_", dateFormat,".csv")
  
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
 
    NameOutFilter <- paste0("viirs_assent_",dateFormat,"_",shapes[j],"_filtro.csv")

    sf_use_s2(FALSE)  
    focos <- point.in.poly(viirs, ap)
    f.filtro <- subset(focos,shapes[j] >= 1)
    rm(focos)
    write.table(f.filtro, NameOutFilter, sep=";", dec = ".")
  }
}

# *******************************************
# Media ponderada de prev de chuva e temp em cada AP

# se precisar abrir novamente a o shapefile de AP
setwd("C:/Users/wanme/Downloads/ASO_2021/shapefiles/")
ap <- shapefile("WDPA_Jul2020.shp")

secasItem <- c("secas/","ASO_Mapa_Seca_secas.tif","prev_metoffice/","prob_AboveTemp_AS.tif","prob_BelowPrec_AS.tif","Assent_seca_JAS.csv","Assent_Prob_aboveprec.csv","Assent_Prob_abovetemp.csv")

aereasProItem <- c("ap/","ASO_Mapa_Seca_ap.tif", "prev/JAS/", "prev_anomalia_ASO2021_temp.tif", "prev_anomalia_ASO2021_prec.tif", "ap", "AP_seca_JAS.csv", "AP_Prob_aboveprec.csv","AP_Prob_abovetemp.csv")

l <- list(SItem = secasItem, ApITem = aereasProItem)

x <- c("SItem","ApITem")
for (nameItem in x){
    #print(l[[nameItem]][indexItemName])

    NomeCaminhoASO <- paste0("C:/Users/wanme/Downloads/ASO_2021/",l[[nameItem]][1])
    NomeCaminhoASO
    #secas
    #'C:/Users/wanme/Downloads/ASO_2021/secas/'
    setwd( NomeCaminhoASO)
    #secas 
    #'ASO_Mapa_Seca_secas.tif'
    secas <- raster(l[[nameItem]][2])

    NomeCaminhoPrev <- paste0("C:/Users/wanme/Downloads/ASO_2021/",l[[nameItem]][3])
    NomeCaminhoPrev    
    #secas
    #'C:/Users/wanme/Downloads/ASO_2021/prev_metoffice/'
    setwd(NomeCaminhoPrev)
    #secas
    #'prob_AboveTemp_AS.tif'
    #'prob_BelowPrec_AS.tif'
    p.temp <- raster(l[[nameItem]][4])
    p.prec <- raster(l[[nameItem]][5])

    #secas
    #'C:/Users/wanme/Downloads/ASO_2021/secas/'
    setwd(NomeCaminhoASO)
    ap_seca <- extract(secas, ap, fun=modal, na.rm=T, df=T, sp = T)
    #secas
    #'Assent_seca_JAS.csv'
    write.table(ap_seca, l[[nameItem]][6], sep=";", dec = ".")

    ap_prec <- extract(p.prec, ap, fun=mean, df=T, weights = T, sp = T)
    ap_temp <- extract(p.temp, ap, fun=mean, df=T, weights = T, sp = T)
    
    #secas
    #'C:/Users/wanme/Downloads/ASO_2021/secas/'
    setwd(NomeCaminhoASO)
    #secas
    #'Assent_Prob_aboveprec.csv'
    #'Assent_Prob_abovetemp.csv'
    write.table(ap_prec, l[[nameItem]][7], sep=";", dec = ".")
    write.table(ap_temp, l[[nameItem]][8], sep=";", dec = ".")
}