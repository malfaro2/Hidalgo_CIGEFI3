get_maps <- function(datos,i){
  
  #xlabs = seq(-102,-77, 3)
  xlabs = seq(-102,-77, 5)
  ylabs = seq(7, 23, 3)
  
  map1 <- rnaturalearth::ne_countries(
    country = c("guatemala", "honduras",
                "el salvador", "panama",
                "nicaragua", "costa rica",
                "belize", "mexico",
                "colombia","cuba","jamaica"), returnclass = "sf")

  a<- datos %>%
    mutate(filtro = ifelse(data.table::between(S,P_025,P_975), 'TRUE', 'FALSE'))
    
    b <- data.frame(a %>% mutate(Significance = case_when(
      filtro == FALSE & signo == "NEGATIVE"  ~ "red",
      filtro == FALSE & signo == "POSITIVE"~ "black",
      filtro == TRUE & signo == "NEGATIVE" ~"NA",
      filtro == TRUE & signo == "POSITIVE" ~"NA")
    ))%>%
  
    ggplot(aes(-lon, lat)) +
      geom_sf(data = map1, inherit.aes = FALSE, color = "black", fill = "white") +
      coord_sf( xlim = c(-102, -77), ylim = c(7,23)) +
     
      geom_point(aes(fill = Significance , size = abs(S),
                     shape=Significance)) + 
      
   
      ####
      #Solo positivos, caso i=4,6,9
      scale_shape_manual(values = c(24, 5),
                         labels=c("POSITIVE", "NA")) +
      
      scale_fill_manual(values=c("black",NA),
                        labels=c("Positive", "non-significant")) +
      
      guides(fill = guide_legend(override.aes = list(shape = c(24,5))))+
      guides(shape = FALSE)+
     #######  
      
    theme_ipsum() + theme(
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 12)) +
    scale_size_area(max_size = 6, guide = "none") +
    
    
    theme_linedraw() +
    labs(x = "Longitude", y = "Latitude", title = paste("Central America:",all[i], "Trend"))+
    scale_x_continuous(breaks = xlabs, labels = paste0(xlabs,'°W')) +
    scale_y_continuous(breaks = ylabs, labels = paste0(ylabs,'°N')) 
  
  
  return(b)
}



