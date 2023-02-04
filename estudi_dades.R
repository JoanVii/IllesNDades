directory <- getwd()

library("tidyverse")
dades <- tibble(read.csv(paste0(directory, "/Data/all_metadata_links.csv"), fileEncoding = "UTF-8"))

col_names <- colnames(dades)

for(i in 1:length(col_names)){
  print(paste0("Nom columna :", col_names[i]))
  unique_data <- unique(dades[col_names[i]])
  num_data <- nrow(unique_data)
  print(paste0("Hi ha ", num_data, " dades uniques a la columna"))
  #print(head(unique(dades[col_names[i]]), 10))
  
}

### son aquelles que no aporten cap info 
eliminar <- c("X", "X.1", "AGGREGALLOWED", "AUTOPEN", "AXIS.VERSION", "CHARSET", "COPYRIGHT",
              "DESCRIPTIONDEFAULT", "CONTVARIABLE", "CONTVARIABLE.ca.", "LANGUAGE", "LANGUAGES")
### son aquelles que no aporten cap info


### no se que hi ha...
maybe_elimiar <- c("CODES", "CODES.ca.", "MAP", "MAP.ca.", "PRECISION", "PRECISION.ca.", "CONTACT", "CONTACT.ca.")
###

col_names_1 <- setdiff(col_names, eliminar)
col_names_1 <- setdiff(col_names_1, maybe_elimiar)


for(i in 1:length(col_names_1)){
  print(paste0("Nom columna :", col_names_1[i]))
  unique_data <- unique(dades[col_names_1[i]])
  num_data <- nrow(unique_data)
  print(paste0("Hi ha ", num_data, " dades uniques a la columna"))
  print(head(unique(dades[col_names_1[i]]), 10))
}


unique(dades[col_names_1[7]])
