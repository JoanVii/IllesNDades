read.ibestat.links <- function(links_ibestat){
  
  links_to_download <- tibble()
  
  # clean HTML code from multiple urls:
  
  for (i in 1:nrow(links_ibestat)) {
    links_messy <- read_html(links_ibestat[i,]) %>% html_nodes("a")
    
    links_all <- as.data.frame(links_messy %>% html_attr("title"))
    links_all[,2] <- as.data.frame(links_messy %>% html_attr("href"))
    
    links_to_download<-links_all[str_ends(links_all[,2], ".px"),]
  }
  
  links_to_download <- links_to_download[!is.na(links_to_download[, 2]), ]
  
  # getting the urls whit possible data:
  
  for (i in 1:nrow(links_to_download)) {
    position <- str_locate_all(links_to_download[i,2], "/")
    position_name_ddbb <- position[[1]][[nrow(position[[1]]), 1]]
    position_link <- position[[1]][[nrow(position[[1]])-2, 1]]
    
    # create link
    links_to_download[i,3] <- str_c(
      "https://ibestat.caib.es/ibestat/service/ibestat/pxcontent",
      str_replace(str_sub(links_to_download[i,2], start=position_link), "/es/", "/")
    )
    
    # create name of ddbb
    links_to_download[i,4] <- str_sub(links_to_download[i,2], start=position_name_ddbb+1, end= -4)
    
  }
  
  links_to_download<-as_tibble(links_to_download)
  names(links_to_download) <- c("Titol_cat", "Eliminar", "Enllac", "Nom_BBDD")
  links_to_download <- links_to_download[,c("Titol_cat", "Enllac", "Nom_BBDD")]
  
  return(links_to_download)
}