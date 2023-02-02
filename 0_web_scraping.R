library(readr)
library(stringr)
library(rvest)
library(tibble)

lectura.links.ibestat <- function(links_ibestat_df){
  
  links_descarregar_all <- tibble()
  
  # clean HTML code from multiple urls:
  
  for (i in 1:nrow(links_ibestat_df)) {
    links_brut <- read_html(links_ibestat_df[i,]) %>% html_nodes("a")
    
    links_tots <- as.data.frame(links_brut %>% html_attr("title"))
    links_tots[,2] <- as.data.frame(links_brut %>% html_attr("href"))
    
    links_descarregar_all<-links_tots[str_ends(links_tots[,2], ".px"),]
  }
  
  links_descarregar_all <- links_descarregar_all[!is.na(links_descarregar_all[, 2]), ]
  
  # getting the urls whit possible data:
  
  for (i in 1:nrow(links_descarregar_all)) {
    posicio <- str_locate_all(links_descarregar_all[i,2], "/")
    posicio_nombbdd <- posicio[[1]][[nrow(posicio[[1]]), 1]]
    posicio_link <- posicio[[1]][[nrow(posicio[[1]])-2, 1]]
    
    # create link
    links_descarregar_all[i,3] <- str_c(
      "https://ibestat.caib.es/ibestat/service/ibestat/pxcontent",
      str_replace(str_sub(links_descarregar_all[i,2], start=posicio_link), "/es/", "/")
    )
    
    # create name of ddbb
    links_descarregar_all[i,4] <- str_sub(links_descarregar_all[i,2], start=posicio_nombbdd+1, end= -4)
    
  }

  links_descarregar<-as_tibble(links_descarregar_all)
  names(links_descarregar) <- c("Titol_cat", "Eliminar", "Enllac", "Nom_BBDD")
  links_descarregar <- links_descarregar[,c("Titol_cat", "Enllac", "Nom_BBDD")]
  
  return(links_descarregar)
}

# a IBestat.txt hi ha els links on hi ha mes links per descarregar les bbdd.
links_ibestat_df <- read_csv("Data/IBestat.txt")
links_ibestat_df <- links_ibestat_df[str_detect(links_ibestat_df$"Links:", "https"), ]
links_ibestat_df <- as.data.frame(links_ibestat_df)

# funció de lectura dels links, hi hem d'introduir unicament els links en format df

links_descarregar <- lectura.links.ibestat(links_ibestat_df)

View(links_descarregar_tibble)

directori <- getwd()

write.csv(
  links_descarregar,
  paste0(directori, "/Data/links_descarregar.csv"),
  fileEncoding = "UTF-8"
)


