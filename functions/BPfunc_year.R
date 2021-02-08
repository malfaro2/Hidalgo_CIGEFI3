BPfunc_year <- function(.x_var, units){
  x_var <- sym(.x_var)
  a <- dat %>%
    ggplot(aes(x=as.factor(year), y=!! x_var)) +
    geom_boxplot() +
    theme_ipsum() +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)) +
    xlab("Year") +
    ylab(x_var)  +
    labs(caption=paste("units: ",units))
  return(a)
}