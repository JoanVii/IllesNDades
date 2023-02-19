grafic_main<-function(doc_df = NULL, eix_x = NULL, eix_y = "value", eix_color = NULL, eix_shape = NULL, eix_linetype = NULL, eix_size = NULL, titol=NULL){
  grafic<- doc_df %>%
    ggplot(aes(
      #x = if(!is.null(eix_x)) {if(length(unique(get(eix_x))) > 1) {get(eix_x)}} else(eix_x),
      x = get(eix_x),
      #y = if(!is.null(eix_y)) {if(length(unique(get(eix_y))) > 1) {get(eix_y)}} else(eix_y),
      y = get(eix_y),
      color = if(!is.null(eix_color)) {if(length(unique(get(eix_color))) > 1) {get(eix_color)}} else(eix_color),
      shape = if(!is.null(eix_shape)) {if(length(unique(get(eix_shape))) > 1) {get(eix_shape)}} else(eix_shape),
      linetype = if(!is.null(eix_linetype)) {if(length(unique(get(eix_linetype))) > 1) {get(eix_linetype)}} else(eix_linetype),
      size = if(!is.null(eix_size)) {if(length(unique(get(eix_size))) > 1) {get(eix_size)}} else(eix_size),
      group = interaction(
        if(!is.null(eix_color)) {get(eix_color)} else(""), 
        if(!is.null(eix_shape)) {get(eix_shape)} else(""),
        if(!is.null(eix_linetype)) {get(eix_linetype)} else(""),
        if(!is.null(eix_size)) {get(eix_size)} else(""), 
        drop = TRUE
      )
    ))
  
  grafic <- grafic +
    geom_point() +
    geom_line()
  
  grafic <- grafic +  
    labs(
      # tag = NULL,
      title = titol, 
      # subtitle = NULL,
      x = eix_x, y = eix_y,
      color = eix_color,
      shape = eix_shape,
      linetype = eix_linetype,
      size = eix_size,
      caption ="JV Cladera",
    ) +
    theme(axis.text.x = element_text(angle =90, hjust = 1)) +
    theme_minimal()
  
  return(grafic)
}



grafic_labs <-function(grafic_main = NULL,  eix_x = NULL, eix_y = NULL, eix_color = NULL, eix_shape = NULL, eix_linetype = NULL, eix_size = NULL, titol=NULL, eix_tag = NULL, eix_subtitle = NULL){
  grafic <- grafic_main +  
    labs(
      tag = eix_tag,
      title = titol, 
      subtitle = eix_subtitle,
      x = eix_x, y = eix_y,
      color = eix_color,
      shape = eix_shape,
      linetype = eix_linetype,
      size = eix_size,
      caption ="JV Cladera",
    ) +
    theme(axis.text.x = element_text(angle =90, hjust = 1)) +
    theme_minimal()
  
  return(grafic)
}







dades <- as_tibble(read.px("https://ibestat.caib.es/ibestat/service/ibestat/pxcontent/74ccdb33-2bef-408b-bde9-00c4a58123ae/vre_40200.px"))

names(dades)

dades_2 <- dades %>% filter(
  Zona.de.procedencia != "Total procedencia",
  Comarca.de.destino..NUTS.4. != "ILLES BALEARS", 
  Sexo != "Ambos sexos",
  Año == 2020
)



grafic_0<- grafic_main(
  doc_df = dades_2,
  eix_x = "Comarca.de.destino..NUTS.4.",
  eix_y = "value",
  eix_color = "Sexo",
  eix_shape = "Zona.de.procedencia",
  eix_linetype = NULL,
  eix_size = NULL,
  titol= "Mal dia"
)

graf <- grafic_labs(
  grafic_main = grafic_0,
  titol = "Bon dia",
  eix_x = "Comarca de destí NUTS 4",
  eix_y = "Persones",
  eix_color = "Sexe",
  eix_shape = "Zona de procedència",
  eix_linetype = NULL,
  eix_size = NULL,
  eix_tag = NULL,
  eix_subtitle = NULL
)

dades_2 %>% ggplot(
    aes(x=Comarca.de.destino..NUTS.4.,
        y=value)
) + geom_bar()







