# *******************************************
# Media ponderada de prev de chuva e temp em cada AP

# se precisar abrir novamente a o shapefile de AP
setwd("C:/Users/wanme/Downloads/ASO_2021/shapefiles/")
ap <- shapefile("WDPA_Jul2020.shp")

secasItem <- c("secas/","ASO_Mapa_Seca.tif","prev_metoffice/","prob_AboveTemp_AS.tif","prob_BelowPrec_AS.tif","Assent_seca_JAS.csv","Assent_Prob_aboveprec.csv","Assent_Prob_abovetemp.csv")

aereasProItem <- c("ap/","ASO_Mapa_Seca.tif", "prev/JAS/", "prev_anomalia_ASO2021_temp.tif", "prev_anomalia_ASO2021_prec.tif", "ap", "AP_seca_JAS.csv", "AP_Prob_aboveprec.csv","AP_Prob_abovetemp.csv")

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
#Caminhos para

