# Codi per preparar-ho tot!

#setwd("D:/00_PseudoEsciptori/Illesndades/IllesNDades/IllesNDades_V1/web")
#
## metadata<-read_csv("meta_links_dates.csv", locale = locale(encoding = "UTF-8")) # Palma
## metadata_cat <- metadata[, c("SUBJECT.AREA.ca.1", "SUBJECT.AREA.ca.2", "TITLE.ca.", "data.1", "MATRIX")] # Palma
#
#metadata <- read_csv("doc_metadata_links.csv", locale = locale(encoding = "UTF-8")) # IB
#metadata_cat <- metadata[, c("SUBJECT.AREA.ca.1", "SUBJECT.AREA.ca.2", "TITLE.ca.", "REFPERIOD.ca.", "MATRIX")] # IB
#
#metadata_cat <- metadata_cat[!c(is.na(metadata_cat[, "SUBJECT.AREA.ca.1"])),]
#metadata_cat[is.na(metadata_cat)] <-"desconegut"
#colnames(metadata_cat)<- c("Area", "Subarea", "Titol", "Data", "NomBBDD")
#
#metadata_cat <- funcio.netetja.data(metadata_cat, noms_columnes = c("Inici periode", "Fi periode"))
#
#write.csv(metadata_cat, "BBDD_web_05_01_2023.csv", fileEncoding = "UTF-8")

# funcio com a tal:

funcio.netetja.data <- function(metadata_cat, noms_columnes = c("Inici periode", "Fi periode")) {
  ## Perparar data:
  data_cat <- metadata_cat[,"Data"]
  # count(data_cat)
  
  data_cat$Data <- str_remove_all(data_cat$Data, c("Des de 01/2001 a 12/2012. Índex general "))
  data_cat$Data <- str_remove_all(data_cat$Data, c('Des de 01/2001 a 12/2017 Index General s" "ense estacions de servei des de '))
  
  data_cat$Data <- str_remove_all(data_cat$Data, c("Migracions "))
  data_cat$Data <- str_remove_all(data_cat$Data, c("Any "))
  data_cat$Data <- str_remove_all(data_cat$Data, c("Matrimonis "))
  data_cat$Data <- str_remove_all(data_cat$Data, c("Parts "))
  data_cat$Data <- str_remove_all(data_cat$Data, c("Revisió del padró a "))
  data_cat$Data <- str_remove_all(data_cat$Data, c("Revisió del padró "))
  data_cat$Data <- str_remove_all(data_cat$Data, c("desconegut"))
  
  data_cat$Data <- str_remove_all(data_cat$Data, c('\" \"'))
  data_cat$Data <- str_remove_all(data_cat$Data, c("01/01/"))
  
  data_cat$Data <- str_remove_all(data_cat$Data, c("de"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("Des"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("De"))
  
  data_cat$Data <- str_remove_all(data_cat$Data, c("l'any "))
  data_cat$Data <- str_remove_all(data_cat$Data, c("l "))
  
  data_cat$Data <- str_replace_all(data_cat$Data, " a ", "_")
  data_cat$Data <- str_replace_all(data_cat$Data, " al ", "_")
  data_cat$Data <- str_replace_all(data_cat$Data, "fins", "_")
  data_cat$Data <- str_replace_all(data_cat$Data, "hasta", "_")
  
  data_cat$Data <- str_replace_all(data_cat$Data, "primer trimestre", "T1")
  data_cat$Data <- str_replace_all(data_cat$Data, "segon trimestre", "T2")
  data_cat$Data <- str_replace_all(data_cat$Data, "tercer trimestre", "T3")
  data_cat$Data <- str_replace_all(data_cat$Data, "quart trimestre", "T4")
  
  data_cat$Data <- str_remove_all(data_cat$Data, c("curs"))
  
  data_cat$Data <- str_remove_all(data_cat$Data, c("gener"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("febrer"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("març"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("abril"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("maig"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("juny"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("juliol"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("agost"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("setembre"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("octubre"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("novembre"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("desembre"))
  
  data_cat$Data <- str_remove_all(data_cat$Data, c("M01"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("M02"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("M03"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("M04"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("M05"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("M06"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("M07"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("M08"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("M09"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("M10"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("M11"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("M12"))
  
  data_cat$Data <- str_remove_all(data_cat$Data, c("01/"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("02/"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("03/"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("04/"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("05/"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("06/"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("07/"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("08/"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("09/"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("10/"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("11/"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("12/"))
  
  data_cat$Data <- str_remove_all(data_cat$Data, c("T1"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("T2"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("T3"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("T4"))
  
  data_cat$Data <- str_remove_all(data_cat$Data, c("No proceix"))
  
  data_cat$Data <- str_remove_all(data_cat$Data, c("julio"))
  data_cat$Data <- str_remove_all(data_cat$Data, c("#"))
  data_cat$Data <- str_replace_all(data_cat$Data, " a", "_")
  
  data_cat$Data <- str_remove_all(data_cat$Data, c(" "))
  data_cat$Data <- str_replace_all(data_cat$Data, "1990-91__2019-20", "1990__2019")
  data_cat$Data <- str_replace_all(data_cat$Data, "1992-93__2020-21", "1992__2020")
  data_cat$Data <- str_replace_all(data_cat$Data, "1999-00__2021-22", "1999__2021")
  data_cat$Data <- str_replace_all(data_cat$Data, "2000-01__2021-22", "2000__2021")
  data_cat$Data <- str_replace_all(data_cat$Data, "2001-02__2021-22", "2001__2021")
  data_cat$Data <- str_replace_all(data_cat$Data, "2002-2003_2020-2021", "2002_2020")
  data_cat$Data <- str_replace_all(data_cat$Data, "2004-2005", "2004")
  data_cat$Data <- str_replace_all(data_cat$Data, "2005-2006", "2005")
  data_cat$Data <- str_replace_all(data_cat$Data, "2006-2007", "2006")
  data_cat$Data <- str_replace_all(data_cat$Data, "2007-2008", "2007")
  data_cat$Data <- str_replace_all(data_cat$Data, "2008-2009", "2008")
  data_cat$Data <- str_replace_all(data_cat$Data, "2009-2010", "2009")
  data_cat$Data <- str_replace_all(data_cat$Data, "2010-2011", "2010")
  data_cat$Data <- str_replace_all(data_cat$Data, "2011-2012", "2011")
  data_cat$Data <- str_replace_all(data_cat$Data, "2012-2013", "2012")
  data_cat$Data <- str_replace_all(data_cat$Data, "2013-2014", "2013")
  data_cat$Data <- str_replace_all(data_cat$Data, "2014-2015", "2014")
  data_cat$Data <- str_replace_all(data_cat$Data, "2015-2016", "2015")
  data_cat$Data <- str_replace_all(data_cat$Data, "2016-2017", "2016")
  data_cat$Data <- str_replace_all(data_cat$Data, "2017-2018", "2017")
  data_cat$Data <- str_replace_all(data_cat$Data, "2018-2019", "2018")
  data_cat$Data <- str_replace_all(data_cat$Data, "2019-2020", "2019")
  data_cat$Data <- str_replace_all(data_cat$Data, "2020-2021", "2020")
  data_cat$Data <- str_replace_all(data_cat$Data, "2021-2022", "2021")
  data_cat$Data <- str_replace_all(data_cat$Data, "2010-11__2021-22", "2010__2021")
  data_cat$Data <- str_replace_all(data_cat$Data, "2013__2019/20", "2013__2019")
  data_cat$Data <- str_replace_all(data_cat$Data, "2013__2020/21", "2013__2020")
  data_cat$Data <- str_replace_all(data_cat$Data, "2014/15__2020/21", "2014__2020")
  
  data_cat$Data <- str_remove_all(data_cat$Data, c("2001_2012.Ínx_"))
  
  data_cat$Data <- str_replace_all(data_cat$Data, "__", "_")
  data_cat$Data <- str_replace_all(data_cat$Data, "__", "_")
  data_cat$Data <- str_replace_all(data_cat$Data, "__", "_")
  data_cat$Data <- str_replace_all(data_cat$Data, "__", "_")
  
  count(data_cat)
  
  data_cat <- as.matrix(str_split_fixed(data_cat$Data,  "_", 2))
  colnames(data_cat) <- c("Inici periode", "Fi periode")
  data_cat <- as_tibble(data_cat)
  
  data_cat <- data_cat %>% mutate_at(c('Inici periode', 'Fi periode'), as.numeric)
  
  ####
  
  metadata_cat[,c(6,7)] <- data_cat
  metadata_cat[,8] <- metadata_cat[,5]
  metadata_cat <- metadata_cat[,-c(4,5)]
  
  for (i in 1:nrow(metadata_cat)) {
    if(is.na(metadata_cat[i, 5])){
      metadata_cat[i, 5] <- metadata_cat[i, 4] 
    }
  }
  
  
  return(metadata_cat)
}

