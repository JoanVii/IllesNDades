reformat.date <- function(dades) {

  # dades <- all_metadata_links[, c("ID", "REFPERIOD.ca.")]
  
  dades <- as_tibble(dades)
  
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("Des de 01/2001 a 12/2012. Índex general "))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c('Des de 01/2001 a 12/2017 Index General s" "ense estacions de servei des de '))
  
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("Migracions "))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("Any "))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("Matrimonis "))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("Parts "))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("Revisió del padró a "))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("Revisió del padró "))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("desconegut"))
  
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c('\" \"'))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("01/01/"))
  
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("gener"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("febrer"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("març"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("abril"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("maig"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("juny"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("juliol"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("agost"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("setembre"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("octubre"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("novembre"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("desembre"))
  
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("de"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("Des"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("De"))
  
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("l'any "))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("l "))
  
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., " a ", "_")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., " al ", "_")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "fins", "_")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "hasta", "_")
  
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "primer trimestre", "T1")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "segon trimestre", "T2")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "tercer trimestre", "T3")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "quart trimestre", "T4")
  
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("curs"))
  
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("M01"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("M02"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("M03"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("M04"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("M05"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("M06"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("M07"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("M08"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("M09"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("M10"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("M11"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("M12"))
  
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("01/"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("02/"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("03/"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("04/"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("05/"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("06/"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("07/"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("08/"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("09/"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("10/"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("11/"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("12/"))
  
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("T1"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("T2"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("T3"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("T4"))
  
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("No proceix"))
  
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("julio"))
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("#"))
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., " a", "_")
  
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c(" "))
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "1990-91__2019-20", "1990__2019")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "1992-93__2020-21", "1992__2020")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "1999-00__2021-22", "1999__2021")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "2000-01__2021-22", "2000__2021")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "2001-02__2021-22", "2001__2021")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "2002-2003_2020-2021", "2002_2020")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "2004-2005", "2004")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "2005-2006", "2005")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "2006-2007", "2006")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "2007-2008", "2007")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "2008-2009", "2008")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "2009-2010", "2009")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "2010-2011", "2010")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "2011-2012", "2011")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "2012-2013", "2012")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "2013-2014", "2013")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "2014-2015", "2014")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "2015-2016", "2015")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "2016-2017", "2016")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "2017-2018", "2017")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "2018-2019", "2018")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "2019-2020", "2019")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "2020-2021", "2020")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "2021-2022", "2021")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "2010-11__2021-22", "2010__2021")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "2013__2019/20", "2013__2019")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "2013__2020/21", "2013__2020")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "2014/15__2020/21", "2014__2020")
  
  dades$REFPERIOD.ca. <- str_remove_all(dades$REFPERIOD.ca., c("2001_2012.Ínx_"))
  
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "__", "_")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "__", "_")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "__", "_")
  dades$REFPERIOD.ca. <- str_replace_all(dades$REFPERIOD.ca., "__", "_")
  
  #count(dades$REFPERIOD.ca.)
  
  dades <- as_tibble(cbind(dades$"ID", str_split_fixed(dades$REFPERIOD.ca.,  "_", 2)))
  colnames(dades) <- c("ID", "REFPERIOD.start.", "REFPERIOD.end.")
  
  dades <- dades %>% mutate_at(c("REFPERIOD.start.", "REFPERIOD.end."), as.numeric)
  
  return(dades)

}











