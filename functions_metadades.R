### Funcions: API IBESTAT ###

# descarregar.px 
descarregar.px <- function(links_descarregar, directori=getwd()){
  links_no_descarregats <- c()
  
  # és necessari una subcarpeta /PX
  directori<- paste0(directori, "/PX")
  
  if (file.exists(directori)) {
    print("Les dades es guarden a la subcarpeta:")
    print(directori)
  } else {
    dir.create(directori, showWarnings = TRUE, recursive = FALSE, mode = "0777")
    print("Es crea una carpeta on es guardaran les dades amb el nom:")
    print(directori)
  }
  
  for (i in 1:nrow(links_descarregar)){
    skip_to_next <- FALSE
    
    tryCatch(
      download.file(links_descarregar[[i,"Enllac"]], 
                    destfile = paste0(directori, "/", links_descarregar[i,"Nom_BBDD"], ".px", collapse = "")
      ),
      error = function(e){
        skip_to_next<<- TRUE
      }
    ) 
    
    if(skip_to_next) {
      links_no_descarregats <- c(links_no_descarregats, links_descarregar[[i,"Nom_BBDD"]])
      next
    }
  }
  
  # Com es pot veure, els arxius .px es guarden a la sub carpeta "PX"
  if(length(links_no_descarregats) == 0) {
    print("S'han descarregat TOTS els arxius")
  } else {
    print(paste0("No s'han descarregat ", length(links_no_descarregats), " arxius! Aquests són:"))
    print(links_no_descarregats)
  }
  return("links_no_descarregats"=links_no_descarregats)
}
##

# printar.errors
printar.errors<- function(error_lectura_px, error_lectura_df, error_guardat_csv){
  if(length(error_lectura_px) == 0) {
    print("S'han llegit TOTS els arxius .px")
  } else {
    print(paste0("No s'han llegit ", length(error_lectura_px), " arxius .px! Aquests son:"))
    print(error_lectura_px)
  }
  
  if(length(error_lectura_df) == 0) {
    print("De la resta d'arxius s'han llegit TOTS els arxius df")
  } else {
    print(paste0("No s'han llegit ", length(error_lectura_df), " arxius df! Aquests son:"))
    print(error_lectura_df)
  }
  
  if(length(error_guardat_csv) == 0) {
    print("De la resta d'arxius s'han guardat TOTS els arxius com a .csv")
  } else {
    print(paste0("No s'han guardat ", length(error_guardat_csv), " arxius com a df! Aquests son:"))
    print(error_guardat_csv)
  }
}
##

# categories.df
categories.df <- function(doc_metadata=doc_metadata){
cat_unic<- c() # categories a algun data frame No repetides
for (i in 1:dim(doc_metadata)[1]) {
  cat_unic <- c(cat_unic, as.list(doc_metadata[i, -1]))
  cat_unic <- cat_unic[!duplicated(cat_unic)]
}

return(list("cat_unic"=cat_unic))
}
##

# doc.metadata.bolean
doc.metadata.bolean <- function(doc_metadata=doc_metadata, cat_unic=cat_unic, error_i=error_i){
  if(is.null(error_i) == F){
    doc_metadata <- doc_metadata[-error_i,] # Elimina les files que no ha pogut guardar
  }
  
  doc_metadata_bolean <- as_tibble(matrix(NA, nrow=dim(doc_metadata)[1], ncol=length(cat_unic)+1))
  doc_metadata_bolean[,1] <- doc_metadata[,1]
  
  # Indica les dades que te els diferents .px
  
  for (i in 1:nrow(doc_metadata)) {
    skip_to_next <- FALSE
    for (j in 1:length(cat_unic)){
      tryCatch(doc_metadata_bolean[i,j+1] <- cat_unic[[j]] %in% doc_metadata[i,],
               error = function(e){
                 skip_to_next<<- TRUE
               }
      )
      if(skip_to_next) {
        doc_metadata_bolean[i,j+1] <- NA
        next
      }
    }
  }
  
  a <- 0
  noms_var <- c("Noms_dades")
  for (i in 1:length(cat_unic)) {
    noms_var<- c(noms_var, cat_unic[[i]])
    if(is.na(noms_var[i])) a <- i 
  }
  
  colnames(doc_metadata_bolean) <- noms_var
  doc_metadata_bolean <- doc_metadata_bolean[,- a] # a és una columna que té de titol NA
  # noms_var <- colnames(doc_metadata_bolean)
  
  list("doc_metadata_bolean"=doc_metadata_bolean)
  return(list("doc_metadata_bolean"=doc_metadata_bolean))
}
##

# llegir.px.guardar.csv.i.metadades
llegir.px.guardar.csv.i.metadades <- function(directori=getwd()){
  
  if (file.exists(paste0(directori, "/CSV"))) {
    print("Les dades es guarden a la subcarpeta:")
    print(directori)
  } else {
    dir.create(paste0(directori, "/CSV"), showWarnings = TRUE, recursive = FALSE, mode = "0777")
    print("Es crea una carpeta on es guardaran les dades amb el nom:")
    print(paste0(directori, "/CSV"))
  }
  
  setwd(paste0(directori, "/PX"))
  
  temp <- list.files(pattern = ".px")
  
  doc_px <-tibble()  # arxiu temporal on es bolca la info del px
  doc_metadata <-tibble() #arxiu de metadades
  doc_df <-tibble() # arxiu de de la bbdd
  
  error_lectura_px <- c()
  error_lectura_df <- c()
  error_guardat_csv <- c()
  error_i <- c()
  
  
  for (i in 1:length(temp)){
    skip_to_next_1 <- FALSE
    
    tryCatch(doc_px <- assign(temp[i], read.px(temp[i])),
             error = function(e){
               skip_to_next_1<<- TRUE
               # print("No s'ha pogut llegir el següent fitxer .px: ")
               # print(temp[i])
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
    
    tryCatch(doc_df <- assign(paste0(as.character(temp[i]),"_df"), as_tibble(doc_px)),  # Canvi de as.data.frame a as_tibble
             error = function(e){
               skip_to_next_2<<- TRUE
               # print("No s'ha pogut llegir el següent fitxer com a data.frame: ")
               # print(temp[i])
             }
    )
    if(skip_to_next_2) {
      error_lectura_df <- c(error_lectura_df, temp[i])
      error_i <- c(error_i, i)
      next
    }
    
    
    skip_to_next_3 <- FALSE
    
    tryCatch(write.csv(doc_df, paste0(str_replace(getwd(), "PX", "CSV"),"/", str_replace(temp[i], ".px", ".csv")), fileEncoding = "UTF-8"), # Canvi de paste0(getwd(),"/", str_replace(temp[i], ".px",   ".csv")) a ?
             error = function(e){
               skip_to_next_3<<- TRUE
               # print("No s'ha pogut guardar el següent fitxer com a .csv: ")
               # print(temp[i])
             }
    )
    if(skip_to_next_3) {
      error_guardat_csv <- c(error_guardat_csv, temp[i])
      error_i <- c(error_i, i)
      next
    }
    
  }
  
  #errors <- list("error_lectura_px"=error_lectura_px, "error_lectura_df"=error_lectura_df, "error_guardat_csv"=error_guardat_csv, "error_i"=error_i)
  
  print("fase 1")
  printar.errors(error_lectura_px, error_lectura_df, error_guardat_csv)
  
  print("fase 2")
  cat_unic <- categories.df(doc_metadata)$cat_unic
  
  print("fase 3")
  dades_2 <- doc.metadata.bolean(doc_metadata=doc_metadata, cat_unic=cat_unic, error_i=error_i)
  doc_metadata_bolean <- dades_2$doc_metadata_bolean
  
  print("fase 4")
  # dades_3 <- doc.metadata.dim.util(doc_metadata_bolean)
  # doc_metadata_util <- dades_3$doc_metadata_util
  
  # doc_metadata_dim és el nombre de dades que conté cada cel·la
  # doc_metadata_util són totes aquelles que contenen una unica dada i s'omple la cel·la amb aquesta.
  
  doc_metadata_dim <- as_tibble(matrix(NA, nrow=dim(doc_metadata_bolean)[1], ncol=dim(doc_metadata_bolean)[2]))
  doc_metadata_dim[,1] <- doc_metadata_bolean[,1]
  colnames(doc_metadata_dim) <- colnames(doc_metadata_bolean)
  
  doc_metadata_util <- as_tibble(matrix(NA, nrow=dim(doc_metadata_bolean)[1], ncol=dim(doc_metadata_bolean)[2]))
  doc_metadata_util[,1] <- doc_metadata_bolean[,1]
  colnames(doc_metadata_util) <- colnames(doc_metadata_bolean)
  
  
  for (i in 1:nrow(doc_metadata_bolean)) {
    for (j in 2:ncol(doc_metadata_bolean)) {
      if(doc_metadata_bolean[i,j] == T) {
        doc_metadata_dim[i,j] <- length(get(as.character(doc_metadata_bolean[i,1]))[[names(doc_metadata_bolean)[j]]]) 
      }
      if(doc_metadata_dim[i,j] == 1 && is.na(doc_metadata_dim[i,j]) != TRUE && sum(names(doc_metadata_dim)[j] != c("DATA", "STUB", "HEADING"))==3){ # he canvait j+1 per j
        doc_metadata_util[i,j] <- get(as.character(doc_metadata_bolean[i,1]))[[names(doc_metadata_bolean)[j]]] # revisar els j+1 n'hi un de malament!
      }
    }
    # print(i) # Per saber si avança
  }
  
  
  return(list("doc_metadata"=doc_metadata, "doc_metadata_util"=doc_metadata_util))
}
##

## aquesta no guarda res ni requereix res apart dels links i els noms de la bbdd de cada link
llegir.px.csv.i.metadades <- function(links_descarregar = links_descarregar){
  
  #temp <- as.character(paste0(links_descarregar[, "Nom_BBDD"], ".px"))
  temp <- as.character(links_descarregar[, "Nom_BBDD"])
  temp_enllac <- as.character(links_descarregar[, "Enllac"])
  
  
  doc_px <-tibble()  # arxiu temporal on es bolca la info del px
  doc_metadata <-tibble() #arxiu de metadades
  doc_df <-tibble() # arxiu de de la bbdd
  
  error_lectura_px <- c()
  error_lectura_df <- c()
  error_guardat_csv <- c() # eliminar!
  error_i <- c()
  
  
  for (i in 1:length(temp)){
    skip_to_next_1 <- FALSE
    
    tryCatch(doc_px <- assign(temp[i], read.px(url(temp_enllac[i]))),
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
    
    tryCatch(doc_df <- assign(paste0(as.character(temp[i]),"_df"), as_tibble(doc_px)),  # Canvi de as.data.frame a as_tibble
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
    
  }
  
  errors <- list("error_lectura_px"=error_lectura_px, "error_lectura_df"=error_lectura_df, "error_guardat_csv"=error_guardat_csv, "error_i"=error_i)
  
  print("fase 1")
  printar.errors(error_lectura_px, error_lectura_df, error_guardat_csv)
  
  print("fase 2")
  cat_unic <- categories.df(doc_metadata)$cat_unic
  
  print("fase 3")
  dades_2 <- doc.metadata.bolean(doc_metadata=doc_metadata, cat_unic=cat_unic, error_i=error_i)
  doc_metadata_bolean <- dades_2$doc_metadata_bolean
  
  print("fase 4")
  # doc_metadata_dim és el nombre de dades que conté cada cel·la
  # doc_metadata_util són totes aquelles que contenen una unica dada i s'omple la cel·la amb aquesta.
  
  doc_metadata_dim <- as_tibble(matrix(NA, nrow=dim(doc_metadata_bolean)[1], ncol=dim(doc_metadata_bolean)[2]))
  doc_metadata_dim[,1] <- doc_metadata_bolean[,1]
  colnames(doc_metadata_dim) <- colnames(doc_metadata_bolean)
  
  doc_metadata_util <- as_tibble(matrix(NA, nrow=dim(doc_metadata_bolean)[1], ncol=dim(doc_metadata_bolean)[2]))
  doc_metadata_util[,1] <- doc_metadata_bolean[,1]
  colnames(doc_metadata_util) <- colnames(doc_metadata_bolean)
  
  
  for (i in 1:nrow(doc_metadata_bolean)) {
    for (j in 2:ncol(doc_metadata_bolean)) {
      if(doc_metadata_bolean[i,j] == T) {
        doc_metadata_dim[i,j] <- length(get(as.character(doc_metadata_bolean[i,1]))[[names(doc_metadata_bolean)[j]]]) 
      }
      if(doc_metadata_dim[i,j] == 1 && is.na(doc_metadata_dim[i,j]) != TRUE && sum(names(doc_metadata_dim)[j] != c("DATA", "STUB", "HEADING"))==3){ # he canvait j+1 per j
        doc_metadata_util[i,j] <- get(as.character(doc_metadata_bolean[i,1]))[[names(doc_metadata_bolean)[j]]] # revisar els j+1 n'hi un de malament!
      }
    }
  }
  
  
  return(list("doc_metadata"=doc_metadata, "doc_metadata_util"=doc_metadata_util, "errors"=errors))
}


llegir.px.csv.i.metadades.fast <- function(links_descarregar = links_descarregar){
  
  temp <- as.character(links_descarregar[, "Nom_BBDD"])
  temp_enllac <- as.character(links_descarregar[, "Enllac"])
  
  doc_px <-tibble()  # arxiu temporal on es bolca la info del px
  doc_metadata <-tibble() #arxiu de metadades
  doc_df <-tibble() # arxiu de de la bbdd
  
  error_lectura_px <- c()
  error_lectura_df <- c()
  error_guardat_csv <- c() # eliminar!
  error_i <- c()
  
  cat_unic <- c("Noms_dades", "AGGREGALLOWED", "AUTOPEN", "AXIS.VERSION", "CHARSET", "CODES", 
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
    
    for(j in 2:length(cat_unic)){
      
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
