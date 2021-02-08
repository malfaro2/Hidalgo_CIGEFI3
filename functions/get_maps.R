get_maps <- function(datos,i){

map1 <- rnaturalearth::ne_states(
  country = c("guatemala", "honduras", 
              "el salvador", "panama", 
              "nicaragua", "costa rica", 
              "belize", "mexico", 
              "colombia"), returnclass = "sf")

 a<- datos %>% 
    mutate(filtro = between(S,P_025,P_975)) %>% 
    filter(filtro == FALSE) %>% 
  ggplot(aes(-lon, lat)) +
  geom_sf(data = map1, inherit.aes = FALSE) +
  coord_sf(ylim = c(0,25), xlim = c(-110, -70)) +
  geom_point(aes(fill = S>0, size = S),
                 shape=21)+
  scale_fill_manual(values=c("red","blue"),
                    limits=c("FALSE","TRUE"),
                    labels=c("NEGATIVE","POSITIVE")) +
  theme_ipsum() + theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12)) +
  scale_size_area(max_size = 6, guide = "none") +
  labs(title = paste("Central America:",all[i], "Trend"), 
       x = "Lon", y = "Lat") 
  return(a)
}

