
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

setwd("C:/Users/wanme/Downloads/ASO_2021/prev/JAS/")

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
setwd("C:/Users/wanme/Downloads/ASO_2021/shapefiles/")
ap <- shapefile("WDPA_Jul2020_AS.shp")
ap <- spTransform(ap, CRS=CRS("+proj=longlat +datum=WGS84"))

# Abrir focos de queimadas de cada abn
setwd("C:/Users/wanme/Downloads/ASO_2021/fires/")
viirs1 <- read.csv2("viirs_2017.csv", header = T, sep = ";")
viirs2 <- read.csv2("viirs_2018.csv", header = T, sep = ";")
viirs3 <- read.csv2("viirs_2019.csv", header = T, sep = ";")
viirs4 <- read.csv2("viirs_2020.csv", header = T, sep = ";")

viirs1$latitude <- as.numeric(as.character(viirs1$latitude))
viirs1$longitude <- as.numeric(as.character(viirs1$longitude))

viirs2$latitude <- as.numeric(as.character(viirs2$latitude))
viirs2$longitude <- as.numeric(as.character(viirs2$longitude))

viirs3$latitude <- as.numeric(as.character(viirs3$latitude))
viirs3$longitude <- as.numeric(as.character(viirs3$longitude))

viirs4$latitude <- as.numeric(as.character(viirs4$latitude))
viirs4$longitude <- as.numeric(as.character(viirs4$longitude))

CRS.new <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
coordinates(viirs1)<-~longitude+latitude # whatever the equivalent is in your
proj4string(viirs1) <- CRS.new

coordinates(viirs2)<-~longitude+latitude # whatever the equivalent is in your
proj4string(viirs2) <- CRS.new

coordinates(viirs3)<-~longitude+latitude # whatever the equivalent is in your
proj4string(viirs3) <- CRS.new

coordinates(viirs4)<-~longitude+latitude # whatever the equivalent is in your
proj4string(viirs4) <- CRS.new

# convert in column of date
viirs1$acq_date <- as.Date(viirs1$acq_date, "%Y-%m-%d")
viirs2$acq_date <- as.Date(viirs2$acq_date, "%Y-%m-%d")
viirs3$acq_date <- as.Date(viirs3$acq_date, "%Y-%m-%d")
viirs4$acq_date <- as.Date(viirs4$acq_date, "%Y-%m-%d")

# selecionar os focos do trimestre de cada ano
# filter dataset between two dates
myfunc <- function(x,y){viirs1[viirs1$acq_date >= x & viirs1$acq_date <= y,]}
viirs1 <- myfunc(as.Date("2017-08-01"), as.Date("2017-10-31"))

myfunc <- function(x,y){viirs2[viirs2$acq_date >= x & viirs2$acq_date <= y,]}
viirs2 <- myfunc(as.Date("2018-08-01"), as.Date("2018-10-31"))

myfunc <- function(x,y){viirs3[viirs3$acq_date >= x & viirs3$acq_date <= y,]}
viirs3 <- myfunc(as.Date("2019-08-01"), as.Date("2019-10-31"))

myfunc <- function(x,y){viirs4[viirs4$acq_date >= x & viirs4$acq_date <= y,]}
viirs4 <- myfunc(as.Date("2020-08-01"), as.Date("2020-10-31"))


# cruzar os pontos de queimadas com as areas de protecao

focos_1 <- point.in.poly(viirs1, ap)
f.filtro1 <- subset(focos_1, capacidade   >= 1)
rm(focos_1)
write.table(f.filtro1, "viirs_assent_BR_1_filtro.csv", sep=";", dec = ".")

focos_2 <- point.in.poly(viirs2, ap)
f.filtro2 <- subset(focos_2, capacidade   >= 1)
rm(focos_2)
write.table(f.filtro2, "viirs_assent_BR_2_filtro.csv", sep=";", dec = ".")

focos_3 <- point.in.poly(viirs3, ap)
f.filtro3 <- subset(focos_3, capacidade   >= 1)
rm(focos_3)
write.table(f.filtro3, "viirs_assent_BR_3_filtro.csv", sep=";", dec = ".")

focos_4 <- point.in.poly(viirs4, ap)
f.filtro4 <- subset(focos_4, capacidade   >= 1)
rm(focos_4)
write.table(f.filtro4, "viirs_assent_BR_4_filtro.csv", sep=";", dec = ".")


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

# A partir daqui nós juntamos todas as tabelas e entao temos todas as colunas com informacoes que precisamos pra rodar as combinacoes de nivel de alerta


# *******************************************
# Classificação dos alertas
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



