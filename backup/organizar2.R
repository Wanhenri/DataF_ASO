# *******************************************
# Media ponderada de prev de chuva e temp em cada AP

# se precisar abrir novamente a o shapefile de AP
setwd("c:/Dropbox/Bases_Geograficas/Cemaden/Report_Fires_AmericalSul/platform/")
ap <- shapefile("WDPA_Jul2020.shp")

setwd("C:/Users/wanme/Downloads/ASO_2021/ap/")
secas <- raster("ASO_Mapa_Seca.tif")

# tif de prev de temp e chuva
setwd("c:/Dropbox/Bases_Geograficas/Cemaden/Report_Fires_AmericalSul/platform/prev/JAS/")
p.temp <- raster("prev_anomalia_ASO2021_temp.tif")
p.prec <- raster("prev_anomalia_ASO2021_prec.tif")

setwd("C:/Users/wanme/Downloads/ASO_2021/ap/")
ap_seca <- extract(secas, ap, fun=modal, na.rm=T, df=T, sp = T)
write.table(ap_seca, "AP_seca_JAS.csv", sep=";", dec = ".")

ap_prec <- extract(p.prec, ap, fun=mean, df=T, weights = T, sp = T)
ap_temp <- extract(p.temp, ap, fun=mean, df=T, weights = T, sp = T)

setwd("C:/Users/wanme/Downloads/ASO_2021/ap/")
write.table(ap_prec, "AP_Prob_aboveprec.csv", sep=";", dec = ".")
write.table(ap_temp, "AP_Prob_abovetemp.csv", sep=";", dec = ".")

{"prev/JAS/", "prev_anomalia_ASO2021_temp.tif", "prev_anomalia_ASO2021_prec.tif", "ap", "AP_seca_JAS.csv", "AP_Prob_aboveprec.csv","AP_Prob_abovetemp.csv"}