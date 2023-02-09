rm.duplicated.data<- function(data){
  # data <- read.csv("Data/all_metadata_links.csv", fileEncoding = "UTF-8")

  data <- data[, !colnames(data) %in% c("Noms_dades", "X.1", "X")]
  
  dupl_MATRIX <- data[duplicated(data$MATRIX), ]$MATRIX
  
  dupl_Nom_BBDD <- data[duplicated(data$Nom_BBDD), ]$Nom_BBDD
  
  data_dupl_MATRIX_original <- data[data$MATRIX %in% dupl_MATRIX, ] # nrow = 78 
  data_dupl_MATRIX <- data_dupl_MATRIX_original
  
  data_dupl_Nom_BBDD <- data[data$Nom_BBDD %in% dupl_Nom_BBDD, ] # nrow = 48
  
  # sum(duplicated(data_dupl_MATRIX$Nom_BBDD))*2 # NO  tots els que tenen Matrix repetit tenen Nom repe
  # sum(duplicated(data_dupl_Nom_BBDD$MATRIX))*2 # tots els que tenen Nom repetit tmb tenen Matrix repe
  
  for(i in 1:nrow(data_dupl_MATRIX)){
    if(data_dupl_MATRIX[i, "MATRIX"] != data_dupl_MATRIX[i, "Nom_BBDD"]){
      data_dupl_MATRIX[i, "MATRIX"] <- data_dupl_MATRIX[i, "Nom_BBDD"]
    }
  }
  
  data_dupl_MATRIX_save <- data_dupl_MATRIX[!data_dupl_MATRIX$MATRIX %in% dupl_MATRIX, ] # aqui hi ha els 30 amb MAtrix erronia!
  
  
  ### arreglat MATRIX, ara quedada arreglar els que tenen el nom repe
  
  dupl_MATRIX <- data_dupl_MATRIX[duplicated(data_dupl_MATRIX$MATRIX), ]$MATRIX
  
  data_dupl_MATRIX <- data_dupl_MATRIX[data_dupl_MATRIX$MATRIX %in% dupl_MATRIX, ] 
  
  key_var <- c("CONTENTS", "CONTENTS.ca.", "CREATION.DATE", "DESCRIPTION", "DESCRIPTION.ca.",
               "INFO", "INFO.ca.", "MATRIX", "REFPERIOD", "REFPERIOD.ca.", "SUBJECT.AREA" ,
               "SUBJECT.AREA.ca.", "SUBJECT.CODE", "SURVEY", "SURVEY.ca.", "TITLE", "TITLE.ca.") # he llevat ID i enllac
  
  data_dupl_MATRIX <- (data_dupl_MATRIX[!duplicated(data_dupl_MATRIX[, key_var]), ]) # elimin files repes
  
  ####
  
  dupl_MATRIX <- data_dupl_MATRIX[duplicated(data_dupl_MATRIX$MATRIX), ]$MATRIX
  
  data_dupl_MATRIX_save_2 <- data_dupl_MATRIX[!data_dupl_MATRIX$MATRIX %in% dupl_MATRIX, ] 
  
  
  data_dupl_MATRIX <- data_dupl_MATRIX[data_dupl_MATRIX$MATRIX %in% dupl_MATRIX, ] # nrow = 34
  nrow(data_dupl_MATRIX)
  
  # paste ID
  
  data_dupl_MATRIX_save <- rbind(data_dupl_MATRIX_save, data_dupl_MATRIX_save_2)
  nrow(data_dupl_MATRIX_save)
  
  data_dupl_MATRIX$Nom_BBDD <- paste0(data_dupl_MATRIX$Nom_BBDD, "!", data_dupl_MATRIX$LAST.UPDATED)
  
  data_dupl_MATRIX_save <- rbind(data_dupl_MATRIX_save, data_dupl_MATRIX)
  
  data <- rbind(setdiff(data, data_dupl_MATRIX_original), data_dupl_MATRIX_save)
}
data <- rm.duplicated.data(all_metadata_links)
