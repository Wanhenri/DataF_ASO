
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



# *******************************************
# Contar o numero de focos de queimadas em AP

# shapefile das Areas de Protecao
setwd("c:/Dropbox/Bases_Geograficas/Cemaden/Report_Fires_AmericalSul/ASO_2021/shapefiles/")
ap <- shapefile("WDPA_Jul2020_AS.shp")
ap <- spTransform(ap, CRS=CRS("+proj=longlat +datum=WGS84"))

#periodoAno <- lubridate::ceiling_date(Sys.Date(), "month") - years(4)
#periodoAno

#periodoMes <- lubridate::ceiling_date(Sys.Date(), "month") + months(3) - 1
#periodoMes

#for(i in 1:4) {
#  periodoMes <- lubridate::ceiling_date(Sys.Date(), "month") + months(i) - 1
#  print(periodoMes )
#}

#for(i in 1:4) {
#  periodoAno <- lubridate::ceiling_date(Sys.Date(), "month") - years(i)
#  print(periodoAno) 
#  #print("****")
#
#}

# shapefile das Areas de Protecao
setwd("c:/Dropbox/Bases_Geograficas/Cemaden/Report_Fires_AmericalSul/ASO_2021/shapefiles/")
ap <- shapefile("WDPA_Jul2020_AS.shp")
ap <- spTransform(ap, CRS=CRS("+proj=longlat +datum=WGS84"))
#WDPA_PI

# cruzar os pontos de queimadas com as areas de protecao
setwd("C:/Users/wanme/Downloads/ASO_2021/assent/")
head(assent)
ap <- assent
#capacidade


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
#for(i in 4:1) {
#  start_sys <-  floor_date(Sys.Date() %m+% months(1), 'month') - years(i)
#  end_sys <-  ceiling_date(Sys.Date() %m+% months(3), 'month') %m-% days(1) - years(i)
#  dateFormat <- format(start_sys,"%Y")
  
#  print(paste0("Ano: ", dateFormat ))  
#  print(paste0("Inicio: ", start_sys, "  fim: ", end_sys))
  
#  # Abrir focos de queimadas de cada abn
#  setwd("C:/Users/wanme/Downloads/ASO_2021/fires/")
  
#  NameOutFilterDateFormat <- paste0("viirs_", dateFormat,".csv")
  
#  viirs <- read.csv2(NameOutFilterDateFormat, header = T, sep = ";")
  
#  viirs$latitude <- as.numeric(as.character(viirs$latitude))
#  viirs$longitude <- as.numeric(as.character(viirs$longitude))
  
#  CRS.new <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#  coordinates(viirs)<-~longitude+latitude # whatever the equivalent is in your
#  proj4string(viirs) <- CRS.new 
  
#  # convert in column of date
#  viirs$acq_date <- as.Date(viirs$acq_date, "%Y-%m-%d")
  
  # selecionar os focos do trimestre de cada ano
#  myfunc <- function(x,y){viirs[viirs$acq_date >= x & viirs$acq_date <= y,]}
#  viirs <- myfunc(as.Date(start_sys), as.Date(end_sys))  
 
#  NameOutFilter <- paste0("viirs_assent_",dateFormat,"_",shapes[j],"_filtro.csv")

#  sf_use_s2(FALSE)  
#  focos <- point.in.poly(viirs, ap)
#  f.filtro <- subset(focos,shapes[j] >= 1)
#  rm(focos)
#  write.table(f.filtro, NameOutFilter, sep=";", dec = ".")
#}


# Nesse momento vamos ter uma tabela pra cada ano. Onde cada ponto de queimadas vai estar associado a uma AP
# No caso eu vou pro Excel e somo o numero de focos por AP em cada trimestre de cada ano e depois junto tudo em uma unica tabela
# Depois disso no Excel eu calculo a tendencia e o acumulado de focos



# *******************************************
# Media ponderada de prev de chuva e temp em cada AP

# se precisar abrir novamente a o shapefile de AP
setwd("c:/Dropbox/Bases_Geograficas/Cemaden/Report_Fires_AmericalSul/ASO_2021/shapefiles/")
ap <- shapefile("WDPA_Jul2020_AS.shp")
ap <- spTransform(ap, CRS=CRS("+proj=longlat +datum=WGS84"))


# tif de prev de temp e chuva
setwd("c:/Dropbox/Bases_Geograficas/Cemaden/Report_Fires_AmericalSul/ASO_2021/prev_metoffice/")
p.temp <- raster("prob_AboveTemp_AS.tif")
p.prec <- raster("prob_BelowPrec_AS.tif")

ap_prec <- extract(p.prec, ap, fun=mean, df=T, weights = T, sp = T)
ap_temp <- extract(p.temp, ap, fun=mean, df=T, weights = T, sp = T)

write.table(ap_prec, "Assent_Prob_aboveprec.csv", sep=";", dec = ".")
write.table(ap_temp, "Assent_Prob_abovetemp.csv", sep=";", dec = ".")


# Dados de secas da Nathi
setwd("c:/Dropbox/Bases_Geograficas/Cemaden/Report_Fires_AmericalSul/ASO_2021/secas/")
secas <- raster("ASO_Mapa_Seca.tif")

ap_seca <- extract(secas, ap, fun=modal, na.rm=T, df=T, sp = T)
write.table(ap_seca, "Assent_seca_JAS.csv", sep=";", dec = ".")

# A partir daqui n�s juntamos todas as tabelas e entao temos todas as colunas com informacoes que precisamos pra rodar as combinacoes de nivel de alerta


# *******************************************
# Classifica��o dos alertas
setwd("c:/Dropbox/Bases_Geograficas/Cemaden/Report_Fires_AmericalSul/ASO_2021/ap/")
list.files()
ap <- read.csv2("AP_Alertas.csv", header = T, sep = ";") # essa tabela foi a tabela que eu criei no excel, depois de juntar tudo
head(ap)

#setwd("c:/Dropbox/Bases_Geograficas/Cemaden/Report_Fires_AmericalSul/ASO_2021/assent/")
#list.files()
#ap <- read.csv2("Assent_ASO_2021.csv", header = T, sep = ";")
#head(ap)


summary(ap$Sum_Frs [ap$Sum_Frs != 0])
tercquartil = as.numeric(summary(ap$Sum_Frs [ap$Sum_Frs != 0])[5])
tercquartil

ap$alerta [
  ap$Sum_Frs >= tercquartil & ap$Trend > 0 & ap$prev_t >= 0.6 & ap$prev_p >= 0.6 & ap$seca == 2] = "High Alert"


ap$alerta [
  (ap$Sum_Frs >= tercquartil & ap$Trend > 0 & ap$prev_t >= 0.6 & ap$prev_p >= 0.6 & ap$seca == 0) |
    (ap$Sum_Frs >= tercquartil & ap$Trend > 0 & ap$prev_t >= 0.6 & ap$prev_p >= 0.6 & ap$seca == 1) |
    (ap$Sum_Frs >= tercquartil & ap$Trend > 0 & ap$prev_t >= 0.6 & ap$prev_p < 0.6 & ap$seca == 2) |
    (ap$Sum_Frs >= tercquartil & ap$Trend > 0 & ap$prev_t < 0.6 & ap$prev_p >= 0.6 & ap$seca == 2) |
    (ap$Sum_Frs >= tercquartil & ap$Trend < 0 & ap$prev_t >= 0.6 & ap$prev_p >= 0.6 & ap$seca == 2) |
    (ap$Sum_Frs >= tercquartil & ap$Trend > 0 & ap$prev_t < 0.6 & ap$prev_p < 0.6 & ap$seca == 2)] = "Alert"


ap$alerta [(ap$Sum_Frs >= tercquartil & ap$Trend > 0 & ap$prev_t < 0.6 & ap$prev_p >= 0.6 & ap$seca == 0) |
             (ap$Sum_Frs >= tercquartil & ap$Trend > 0 & ap$prev_t >= 0.6 & ap$prev_p < 0.6 & ap$seca == 0) |
             (ap$Sum_Frs >= tercquartil & ap$Trend > 0 & ap$prev_t < 0.6 & ap$prev_p >= 0.6 & ap$seca == 1) |
             (ap$Sum_Frs >= tercquartil & ap$Trend > 0 & ap$prev_t >= 0.6 & ap$prev_p < 0.6 & ap$seca == 1) |
             (ap$Sum_Frs < tercquartil & ap$Trend > 0 & ap$prev_t >= 0.6 & ap$prev_p >= 0.6 & ap$seca == 2) |
             (ap$Sum_Frs >= tercquartil & ap$Trend < 0 & ap$prev_t >= 0.6 & ap$prev_p >= 0.6 & ap$seca == 0) |
             (ap$Sum_Frs >= tercquartil & ap$Trend < 0 & ap$prev_t >= 0.6 & ap$prev_p >= 0.6 & ap$seca == 1) |
             (ap$Sum_Frs >= tercquartil & ap$Trend < 0 & ap$prev_t < 0.6 & ap$prev_p >= 0.6 & ap$seca == 2) |
             (ap$Sum_Frs >= tercquartil & ap$Trend < 0 & ap$prev_t >= 0.6 & ap$prev_p < 0.6 & ap$seca == 2) |
             (ap$Sum_Frs < tercquartil & ap$Trend > 0 & ap$prev_t >= 0.6 & ap$prev_p >= 0.6 & ap$seca == 0) |
             (ap$Sum_Frs < tercquartil & ap$Trend > 0 & ap$prev_t >= 0.6 & ap$prev_p >= 0.6 & ap$seca == 1) |
             (ap$Sum_Frs < tercquartil & ap$Trend > 0 & ap$prev_t < 0.6 & ap$prev_p >= 0.6 & ap$seca == 2) |
             (ap$Sum_Frs < tercquartil & ap$Trend > 0 & ap$prev_t >= 0.6 & ap$prev_p < 0.6 & ap$seca == 2) |
             (ap$Sum_Frs < tercquartil & ap$Trend < 0 & ap$prev_t >= 0.6 & ap$prev_p >= 0.6 & ap$seca == 2)] = "Attention"


ap$alerta [(ap$Sum_Frs >= tercquartil & ap$Trend > 0 & ap$prev_t < 0.6 & ap$prev_p < 0.6 & ap$seca == 0) |
             (ap$Sum_Frs >= tercquartil & ap$Trend > 0 & ap$prev_t < 0.6 & ap$prev_p < 0.6 & ap$seca == 1) |
             (ap$Sum_Frs >= tercquartil & ap$Trend < 0 & ap$prev_t < 0.6 & ap$prev_p >= 0.6 & ap$seca == 0) |
             (ap$Sum_Frs >= tercquartil & ap$Trend < 0 & ap$prev_t >= 0.6 & ap$prev_p < 0.6 & ap$seca == 0) |
             (ap$Sum_Frs >= tercquartil & ap$Trend < 0 & ap$prev_t < 0.6 & ap$prev_p >= 0.6 & ap$seca == 1) |
             (ap$Sum_Frs >= tercquartil & ap$Trend < 0 & ap$prev_t >= 0.6 & ap$prev_p < 0.6 & ap$seca == 1) |
             (ap$Sum_Frs >= tercquartil & ap$Trend < 0 & ap$prev_t < 0.6 & ap$prev_p < 0.6 & ap$seca == 2) |
             (ap$Sum_Frs < tercquartil & ap$Trend > 0 & ap$prev_t >= 0.6 & ap$prev_p < 0.6 & ap$seca == 0) |
             (ap$Sum_Frs < tercquartil & ap$Trend > 0 & ap$prev_t < 0.6 & ap$prev_p >= 0.6 & ap$seca == 0) |
             (ap$Sum_Frs < tercquartil & ap$Trend > 0 & ap$prev_t >= 0.6 & ap$prev_p < 0.6 & ap$seca == 1) |
             (ap$Sum_Frs < tercquartil & ap$Trend > 0 & ap$prev_t < 0.6 & ap$prev_p >= 0.6 & ap$seca == 1) |
             (ap$Sum_Frs < tercquartil & ap$Trend > 0 & ap$prev_t < 0.6 & ap$prev_p < 0.6 & ap$seca == 2) |
             (ap$Sum_Frs < tercquartil & ap$Trend < 0 & ap$prev_t >= 0.6 & ap$prev_p >= 0.6 & ap$seca == 0) |
             (ap$Sum_Frs < tercquartil & ap$Trend < 0 & ap$prev_t >= 0.6 & ap$prev_p >= 0.6 & ap$seca == 1) |
             (ap$Sum_Frs < tercquartil & ap$Trend < 0 & ap$prev_t < 0.6 & ap$prev_p >= 0.6 & ap$seca == 2) |
             (ap$Sum_Frs < tercquartil & ap$Trend < 0 & ap$prev_t >= 0.6 & ap$prev_p < 0.6 & ap$seca == 2)] = "Observation"


ap$alerta [(ap$Sum_Frs >= tercquartil & ap$Trend < 0 & ap$prev_t < 0.6 & ap$prev_p < 0.6 & ap$seca == 0) |
             (ap$Sum_Frs >= tercquartil & ap$Trend < 0 & ap$prev_t < 0.6 & ap$prev_p < 0.6 & ap$seca == 1) |
             (ap$Sum_Frs < tercquartil & ap$Trend > 0 & ap$prev_t < 0.6 & ap$prev_p < 0.6 & ap$seca == 0) |
             (ap$Sum_Frs < tercquartil & ap$Trend > 0 & ap$prev_t < 0.6 & ap$prev_p < 0.6 & ap$seca == 1) |
             (ap$Sum_Frs < tercquartil & ap$Trend < 0 & ap$prev_t < 0.6 & ap$prev_p >= 0.6 & ap$seca == 0) |
             (ap$Sum_Frs < tercquartil & ap$Trend < 0 & ap$prev_t >= 0.6 & ap$prev_p < 0.6 & ap$seca == 0) |
             (ap$Sum_Frs < tercquartil & ap$Trend < 0 & ap$prev_t < 0.6 & ap$prev_p < 0.6 & ap$seca == 0) |
             (ap$Sum_Frs < tercquartil & ap$Trend < 0 & ap$prev_t < 0.6 & ap$prev_p >= 0.6 & ap$seca == 1) |
             (ap$Sum_Frs < tercquartil & ap$Trend < 0 & ap$prev_t >= 0.6 & ap$prev_p < 0.6 & ap$seca == 1) |
             (ap$Sum_Frs < tercquartil & ap$Trend < 0 & ap$prev_t < 0.6 & ap$prev_p < 0.6 & ap$seca == 1) |
             (ap$Sum_Frs < tercquartil & ap$Trend < 0 & ap$prev_t < 0.6 & ap$prev_p < 0.6 & ap$seca == 2) |
             (ap$Sum_Frs == 0 & ap$prev_t >= 0.6 & ap$prev_p >= 0.6 & ap$seca == 0) |
             (ap$Sum_Frs == 0 & ap$prev_t < 0.6 & ap$prev_p >= 0.6 & ap$seca == 0) |
             (ap$Sum_Frs == 0 & ap$prev_t >= 0.6 & ap$prev_p < 0.6 & ap$seca == 0) |
             (ap$Sum_Frs == 0 & ap$prev_t < 0.6 & ap$prev_p < 0.6 & ap$seca == 0) |
             (ap$Sum_Frs == 0 & ap$prev_t >= 0.6 & ap$prev_p >= 0.6 & ap$seca == 1) |
             (ap$Sum_Frs == 0 & ap$prev_t < 0.6 & ap$prev_p >= 0.6 & ap$seca == 1) |
             (ap$Sum_Frs == 0 & ap$prev_t >= 0.6 & ap$prev_p < 0.6 & ap$seca == 1) |
             (ap$Sum_Frs == 0 & ap$prev_t < 0.6 & ap$prev_p < 0.6 & ap$seca == 1) |
             (ap$Sum_Frs == 0 & ap$prev_t >= 0.6 & ap$prev_p >= 0.6 & ap$seca == 2) |
             (ap$Sum_Frs == 0 & ap$prev_t < 0.6 & ap$prev_p >= 0.6 & ap$seca == 2) |
             (ap$Sum_Frs == 0 & ap$prev_t >= 0.6 & ap$prev_p < 0.6 & ap$seca == 2) |
             (ap$Sum_Frs == 0 & ap$prev_t < 0.6 & ap$prev_p < 0.6 & ap$seca == 2)] = "Low Probability"

# Ordenar tabela
ap$alerta = factor(ap$alerta, levels = c("High Alert", "Alert", "Attention", "Observation", "Low Probability"))

write.csv (ap, "Tabela_completa_ASO2021_AP.csv", sep = ";", dec = ".")





# ---------------------------
# Nao precisa usar essas linhas abaixo
# Ligacao de tabelas - shapefile
setwd("c:/Dropbox/Bases_Geograficas/Cemaden/Report_Fires_AmericalSul/ASO_2021/ap/")
write.dbf(ap, "WDPA_Jul2020_AS.dbf")


ap2 <- read.csv("Tabela_completa_ASO2021_Assent.csv", header = TRUE, sep=";")

head(ap2)
write.dbf(ap2, "WDPA_Jul2020_AS2.dbf")

write.csv (assent, "assent_lista_ID.txt", sep = ";", dec = ".")



