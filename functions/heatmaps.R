heatmap_function <- function(.x_var){
  x_var <- sym(.x_var)
  col1 = "#fee8c8" 
  col2 = "#e34a33"
  a <- dat %>%
    ggplot(aes(x=time, y = station, fill=!! x_var)) +
    geom_tile(aes(fill = !! x_var), na.rm = TRUE) +
    scale_fill_gradient(low = col1, high = col2) +  
    guides(fill=guide_legend(title=paste(x_var))) +
    theme_bw() + theme_minimal() + 
    labs(title = paste("Heatmap for variable",x_var),
       x = "Time", y = "Station") +
    theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
return(a)
}
