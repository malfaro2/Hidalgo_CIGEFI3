BPfunc_month <- function(.x_var, units){
  x_var <- sym(.x_var)
  a <- dat %>%
    ggplot(aes(x=time, y=!! x_var, group=time)) +
    geom_boxplot() +
    theme_ipsum() +
    theme(
      legend.position="none",
      panel.spacing = unit(1, "lines"),
      strip.text.x = element_text(size = 4)) +
    xlab("Time") +
    ylab(x_var)  #+
    #labs(caption=paste("units: ",units))
  return(a)
}
