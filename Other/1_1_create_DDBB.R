directory <- getwd()
#setwd(paste0(directory, "/Data/"))

library("tidyverse")
all_metadata_links <- tibble(read.csv(paste0(directory, "/all_metadata_links.csv"), fileEncoding = "UTF-8"))

col_names <- colnames(all_metadata_links)

### no info
delete <- c("X", "X.1", "AGGREGALLOWED", "AUTOPEN", "AXIS.VERSION", "CHARSET", "COPYRIGHT",
            "DESCRIPTIONDEFAULT", "CONTVARIABLE", "CONTVARIABLE.ca.", "LANGUAGE", "LANGUAGES")

### trivial info
maybe_delete <- c("CODES", "CODES.ca.", "MAP", "MAP.ca.", "PRECISION", "PRECISION.ca.", "CONTACT", "CONTACT.ca.")
###

col_names <- setdiff(col_names, delete)
col_names <- setdiff(col_names, maybe_delete)


# for(i in 1:length(col_names)){
#   print(paste0("Nom columna :", col_names[i]))
#   unique_data <- unique(dades[col_names[i]])
#   num_data <- nrow(unique_data)
#   print(paste0("Hi ha ", num_data, " dades uniques a la columna"))
#   #print(head(unique(dades[col_names[i]]), 10))
# }


imp_metadata_links <- all_metadata_links[col_names]

write.csv(imp_metadata_links,"imp_metadata_links.csv", fileEncoding = "UTF-8")


######


links_to_actualize <- imp_metadata_links[imp_metadata_links$"UPDATE.FREQUENCY" != "No se actualiza" &
                                           !is.na(imp_metadata_links$"UPDATE.FREQUENCY"), ]

links_to_actualize <- links_to_actualize[c("Titol_cat", "Enllac", "Noms_dades")]
names(links_to_actualize) <- c("Titol_cat", "Enllac", "Nom_BBDD")


write.csv(links_to_actualize, "links_to_actualize.csv", fileEncoding = "UTF-8")

       