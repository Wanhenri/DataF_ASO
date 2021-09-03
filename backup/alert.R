
# Classificação dos alertas
setwd("c:/Dropbox/Bases_Geograficas/Cemaden/Report_Fires_AmericalSul/ASO_2021/ap/")
list.files()
ap <- read.csv2("AP_Alertas.csv", header = T, sep = ";")
head(ap)

setwd("c:/Dropbox/Bases_Geograficas/Cemaden/Report_Fires_AmericalSul/ASO_2021/assent/")
list.files()
ap <- read.csv2("Assent_ASO_2021.csv", header = T, sep = ";")
head(ap)


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

write.csv (ap, "Tabela_completa_ASO2021_Assent.csv", sep = ";", dec = ".")