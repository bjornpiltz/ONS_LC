library(tidyverse)
library(hrbrthemes)
library(ggtext)

hrbrthemes::update_geom_font_defaults()

theme_set(theme_ipsum_rc(plot_margin = margin(l=10, t=10, b=10, r=10)) +
         theme(plot.title=element_markdown(),
               plot.caption=element_markdown(color = "gray40"),
               plot.subtitle=element_markdown(color = "gray40")))

pal <-c('#EFF3FF', '#C6DBEF', '#9ECAE1', '#6BAED6', '#4292C6', '#3181BD', '#2171B5', '#084594')

savePlot_ <- function(plot, name) {
  w <- getOption("repr.plot.width")/s
  h <- getOption("repr.plot.height")/s
  dpi <- getOption("repr.plot.res")
    
  ggsave(paste0("img/", name, ".png"), plot = plot,
         width = w, height = h, bg = "white", dpi = dpi, scale = s)
  ggsave(paste0("img/", name, "@2x", ".png"), plot = plot,
         width = w, height = h, bg = "white", dpi = dpi*2, scale = s)
  return(plot)
}

savePlot <- function(name) {
  structure(
    "A save function maskerading as a layer.", 
    class = "save_plot",
    fn = "savePlot_",
    name = name
  )
}
ggplot_add.save_plot <- function(object, plot, object_name) {
  # a method for the `+` operator for save_plot objects.
  # - "object to add" (arguments to the RHS of the `+`)
  # - plot is the existing plot (on the LHS of the `+`)
  # - object_name is the unevaluated call on the RHS of the `+`
  
  # extract the `fn` attribute from `save_plot` output
  fn <- attr(object, "fn")
  
  # extract arguments `arg1` and `arg2` from `save_plot` output
  save_plot_args <- attributes(object)[!names(attributes(object)) %in% 
                                   c("class", "fn")]
  
  # call `fn` with the arguments `plot`, `arg1`, and `arg2`
  new_plot <- do.call(
    fn,
    c(list(plot), save_plot_args)
  )
  
  # return the new plot
  new_plot
}
