get_plots<-function(i,m){
  a<-trends %>% dplyr::filter(variable==all[i], month==m) %>% 
    full_join(estaciones, .id = "station") %>% 
    ggplot(aes(lon, lat)) +
    geom_sf(data = map1, inherit.aes = FALSE) +
    coord_sf(ylim = c(0,25), xlim = c(-110, -70)) +
    geom_point(aes(fill = pMK>0.05, size = (abs(tauMK)+0.01)*100), 
               shape = 21) +
    scale_fill_manual(values=c("red","blue"),
                      limits=c("FALSE","TRUE")) +
    theme_ipsum() + theme(
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 12)) +
    scale_size_area(max_size = 6, guide = "none") +
    labs(title = paste("Central America: tau and p",all[i]), 
         x = "Lon", y = "Lat") 
  
  b<-trends %>% dplyr::filter(variable==all[i], month==m) %>%  
    full_join(estaciones, .id = "station") %>% 
    ggplot(aes(lon, lat)) +
    geom_sf(data = map1, inherit.aes = FALSE) +
    coord_sf(ylim = c(0,25), xlim = c(-110, -70)) +
    geom_point(aes(fill = pZ>0.05, size = abs(Z)+0.01), 
               shape = 21) +
    scale_fill_manual(values=c("red","blue"),
                      limits=c("FALSE","TRUE")) +
    theme_ipsum() + theme(
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 12)) +
    scale_size_area(max_size = 6, guide = "none") +
    labs(title = paste("Central America: Z from KM",all[i]), 
         x = "Lon", y = "Lat") 
  
  return(a+b)
}
