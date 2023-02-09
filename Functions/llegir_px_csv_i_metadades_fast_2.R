llegir.px.csv.i.metadades.fast.2 <- function(links_descarregar = links_descarregar){
  #links_descarregar = links_mini
  temp <- as.character(links_descarregar[, "Nom_BBDD"])
  temp_enllac <- as.character(links_descarregar[, "Enllac"])
  # New 
  temp_ID <- as.character(links_descarregar[, "ID"])
  ###
  
  doc_px <-tibble()  # arxiu temporal on es bolca la info del px
  doc_metadata <-tibble() #arxiu de metadades
  doc_df <-tibble() # arxiu de de la bbdd
  
  error_lectura_px <- c()
  error_lectura_df <- c()
  error_guardat_csv <- c() # eliminar!
  error_i <- c()
  
  # New add "ID"
  cat_unic <- c("Noms_dades", "ID", "AGGREGALLOWED", "AUTOPEN", "AXIS.VERSION", "CHARSET", "CODES", 
                "CODES.ca.", "CONTACT", "CONTACT.ca.", "CONTENTS", "CONTENTS.ca.", 
                "COPYRIGHT", "CREATION.DATE", "DATA", "DECIMALS", "DESCRIPTION", 
                "DESCRIPTION.ca.", "DESCRIPTIONDEFAULT", "ELIMINATION", "ELIMINATION.ca.", 
                "HEADING", "HEADING.ca.", "INFO", "INFO.ca.", "LANGUAGE", "LANGUAGES", 
                "LAST.UPDATED", "LAST.UPDATED.ca.", "MAP", "MAP.ca.", "MATRIX", "NOTE", 
                "NOTE.ca.", "REFPERIOD", "REFPERIOD.ca.", "SHOWDECIMALS", "SOURCE", 
                "SOURCE.ca.", "STUB", "STUB.ca.", "SUBJECT.AREA", "SUBJECT.AREA.ca.", 
                "SUBJECT.CODE", "SURVEY", "SURVEY.ca.", "TITLE", "TITLE.ca.", "UNITS", 
                "UNITS.ca.", "UPDATE.FREQUENCY", "VALUES", "VALUES.ca.", "PRECISION", 
                "PRECISION.ca.", "CONTVARIABLE", "CONTVARIABLE.ca.")
  
  doc_metadata_util <- as_tibble(matrix(NA, ncol=length(cat_unic)))
  colnames(doc_metadata_util) <- cat_unic
  
  n <- 1
  
  for (i in 1:length(temp)){
    skip_to_next_1 <- FALSE
    
    tryCatch(#doc_px <- assign(temp[i], read.px(url(temp_enllac[i]))),
      doc_px <- read.px(url(temp_enllac[i])),
      #close(url(temp_enllac[i])),
      error = function(e){
        skip_to_next_1<<- TRUE
        # print("No s'ha pogut llegir el següent fitxer .px: ")
      }
    ) 
    
    doc_metadata[i,1] <- as.character(temp[i])
    
    if(skip_to_next_1) {
      error_lectura_px <- c(error_lectura_px, temp[i])
      error_i <- c(error_i, i)
      next
    }
    
    for (j in 1:length(names(doc_px))) {
      doc_metadata[i,j+1] <- names(doc_px)[j]
    }
    
    skip_to_next_2 <- FALSE
    
    tryCatch(doc_df <- as_tibble(doc_px),
             #assign(paste0(as.character(temp[i]),"_df"), as_tibble(doc_px)),
             error = function(e){
               skip_to_next_2<<- TRUE
               # print("No s'ha pogut llegir el següent fitxer com a data.frame: ")
             }
    )
    if(skip_to_next_2) {
      error_lectura_df <- c(error_lectura_df, temp[i])
      error_i <- c(error_i, i)
      next
    }
    
    ##
    
    doc_metadata_util[n,1] <- temp[i]
    
    #New 
    doc_metadata_util[n,2] <- temp_ID[i]
    ###
    
    # Canvi de for(j in 2:length(cat_unic)){ a:
    for(j in 3:length(cat_unic)){
      
      if(sum(is.na(doc_px[[ cat_unic[j] ]] [[1]] [2]))){
        
        doc_metadata_util[n,j] <- doc_px[[ cat_unic[j] ]] [[1]] [1]
      }
      
    }
    n <- n+1
    
  }
  
  errors <- list("error_lectura_px"=error_lectura_px, "error_lectura_df"=error_lectura_df, "error_guardat_csv"=error_guardat_csv, "error_i"=error_i)
  
  #printar.errors(error_lectura_px, error_lectura_df, error_guardat_csv)
  
  return(list("doc_metadata_util"=doc_metadata_util, "errors"=errors))
}