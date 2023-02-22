funcio.grafics<-function(doc_df = NULL, eix_x = NULL, eix_y = NULL, eix_color = NULL, eix_shape = NULL, eix_linetype = NULL, eix_size = NULL, titol=NULL){
  grafic<- doc_df %>%
    ggplot(aes(
      x = get(eix_x),
      y = get(eix_y),
      #color = if(!is.null(eix_color)) {get(eix_color)} else(eix_color),
      color = if(!is.null(eix_color)) {if(length(unique(get(eix_color))) > 1) {get(eix_color)}} else(eix_color),
      #shape = if(!is.null(eix_shape)) {get(eix_shape)} else(eix_shape),
      shape = if(!is.null(eix_shape)) {if(length(unique(get(eix_shape))) > 1) {get(eix_shape)}} else(eix_shape),
      #linetype = if(!is.null(eix_linetype)) {get(eix_linetype)} else(eix_linetype),
      linetype = if(!is.null(eix_linetype)) {if(length(unique(get(eix_linetype))) > 1) {get(eix_linetype)}} else(eix_linetype),
      #size = if(!is.null(eix_size)) {get(eix_size)} else(eix_size),
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
      # tag = NULL",
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