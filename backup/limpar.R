for(j in 1:2){
  shapes <- c("WDPA_PI","capacidade")
  print(paste0("Shapefile: ", shapes[j])) 
  
 
  #for(i in 4:1)
  for(i in 4:1) {
    start_sys <-  floor_date(Sys.Date() %m+% months(1), 'month') - years(i)
    end_sys <-  ceiling_date(Sys.Date() %m+% months(3), 'month') %m-% days(1) - years(i)
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
    
    
    
    NameOutFilter <- paste0("viirs_",dateFormatName,"_",shapes[j],"_filtro_limpo.csv")
    print(paste0("******Inicio: ",NameOutFilter))
    
    

    rm(viirs)
    setwd("C:/Users/wanme/Downloads/ASO_2021/20210830/")
    write.table(filess2, NameOutFilter, sep=";", dec = ".", row.names = F)
    
    fim <- paste0(" FIM ",shapes[j],"_",dateFormatName)
    print(paste0("******fim: ", fim))
    
    rm(NameOutFilter)
    rm(NameOutFilterDateFormat)
    rm(dateFormatName)
  }
}