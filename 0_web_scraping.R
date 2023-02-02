library(readr)
library(stringr)
library(rvest)
library(tibble)


lectura.links.ibestat <- function(links_ibestat_df){
  
  links_descarregar_all <- data.frame()
  
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
      str_replace(str_sub(links_descarregar_all_2[i,2], start=posicio_link), "/es/", "/")
    )
    
    # create name ddbb
    links_descarregar_all[i,4] <- str_sub(links_descarregar_all[i,2], start=posicio_nombbdd+1, end= -4)

  }
  
  # length(unique(links_descarregar_all[,4]))
  
  links_descarregar<-as_tibble(links_descarregar_all)
  names(links_descarregar) <- c("Titol_cat", "Eliminar", "Enllac", "Nom_BBDD")
  links_descarregar <- links_descarregar[,c("Titol_cat", "Enllac", "Nom_BBDD")]
  
  return(links_descarregar)
}

# a IBestat.txt hi ha els links on hi ha mes links per descarregar les bbdd.
links_ibestat_df <- read_csv("Data/IBestat_mini.txt")
links_ibestat_df <- links_ibestat_df[str_detect(links_ibestat_df$"Links:", "https"), ]
links_ibestat_df <- as.data.frame(links_ibestat_df)

# funció de lectura dels links, hi hem d'introduir unicament els links en format df

links_descarregar <-lectura.links.ibestat(links_ibestat_df)

# ens quedem amb els unics, en principi ho son!

links_descarregar <- unique(links_descarregar)

directori <- getwd()

write.csv(
  links_descarregar,
  #"links_descarregar.csv",
  paste0(directori, "/Data/links_descarregar.csv"),
  fileEncoding = "UTF-8"
)



## rendiment:

library(microbenchmark)

res <- microbenchmark(
  funcio_original(links_descarregar_all),
  funcio_chat(links_descarregar_all),
  funcio_chat_2(links_descarregar_all),
  times = 100L
)

print(res)


lectura.links.ibestat.chatGTP <- function(links_ibestat_df){
  
  links_descarregar_all <- data.frame()
  
  links_tots <- data.frame()
  links_brut <- html_nodes(read_html(links_ibestat_df), "a")
  links_tots[,1] <- html_attr(links_brut, "title")
  links_tots[,2] <- html_attr(links_brut, "href")
  
  links_descarregar_all <- links_tots[str_ends(links_tots[,2], ".px"),]
  links_descarregar_all <- links_descarregar_all[!is.na(links_descarregar_all[, 2]), ]
  
  links_descarregar_all[,3] <- str_c(
    "https://ibestat.caib.es/ibestat/service/ibestat/pxcontent",
    str_replace(str_sub(links_descarregar_all[,2], str_locate_all(links_descarregar_all[,2], "/")[[1]][[nrow(str_locate_all(links_descarregar_all[,2], "/")[[1]])-2,1]]), "/es/", "/")
  )
  
  links_descarregar_all[,4] <- str_sub(links_descarregar_all[,2], 
                                       str_locate_all(links_descarregar_all[,2], "/")[[1]][[nrow(str_locate_all(links_descarregar_all[,2], "/")[[1]]),1]] + 1, 
                                       -4)
  
  links_descarregar <- as_tibble(links_descarregar_all)
  colnames(links_descarregar) <- c("Titol_cat", "Eliminar", "Enllac", "Nom_BBDD")
  links_descarregar <- links_descarregar[,c("Titol_cat", "Enllac", "Nom_BBDD")]
  
  return(links_descarregar)
}

links_descarregar <-lectura.links.ibestat.(links_ibestat_df)
