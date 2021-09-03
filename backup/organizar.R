# *******************************************
# Media ponderada de prev de chuva e temp em cada AP

# se precisar abrir novamente a o shapefile de AP
setwd("c:/Dropbox/Bases_Geograficas/Cemaden/Report_Fires_AmericalSul/platform/")
ap <- shapefile("WDPA_Jul2020.shp")

setwd("C:/Users/wanme/Downloads/ASO_2021/secas/")
secas <- raster("ASO_Mapa_Seca.tif")

# tif de prev de temp e chuva
setwd("C:/Users/wanme/Downloads/ASO_2021/prev_metoffice/")
p.temp <- raster("prob_AboveTemp_AS.tif")
p.prec <- raster("prob_BelowPrec_AS.tif")

ap_seca <- extract(secas, ap, fun=modal, na.rm=T, df=T, sp = T)
write.table(ap_seca, "Assent_seca_JAS.csv", sep=";", dec = ".")

ap_prec <- extract(p.prec, ap, fun=mean, df=T, weights = T, sp = T)
ap_temp <- extract(p.temp, ap, fun=mean, df=T, weights = T, sp = T)

setwd("C:/Users/wanme/Downloads/ASO_2021/assent/")
write.table(ap_prec, "Assent_Prob_aboveprec.csv", sep=";", dec = ".")
write.table(ap_temp, "Assent_Prob_abovetemp.csv", sep=";", dec = ".")

{"secas/","prev_metoffice/","prob_AboveTemp_AS.tif","prob_BelowPrec_AS.tif","Assent_seca_JAS.csv","Assent_Prob_aboveprec.csv","Assent_Prob_abovetemp.csv"}