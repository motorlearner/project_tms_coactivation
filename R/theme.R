# SETUP ---------------------------------------------------------------------------------

# load packages
require(ggplot2)

# THEME ---------------------------------------------------------------------------------

theme_master <- function(textsz = 9){
  
  theme_void() + theme(
    
    # plot margin
    plot.margin = margin(10,10,20,10),
    # axes
    panel.border = element_rect(size = .5, color = "black", fill = NA),
    axis.ticks.x = element_line(size = .5, color = "black"),
    axis.ticks.y = element_line(size = .5, color = "black"),
    axis.ticks.length = unit(0.5, "lines"),
    axis.text.x = element_text(size = textsz, color = "black"),
    axis.text.y = element_text(size = textsz, color = "black", hjust=1),
    axis.title.x = element_text(size = textsz, color = "black", angle = 0,
                                margin = margin(t = 10)),
    axis.title.y = element_text(size = textsz, color = "black", angle = 90,
                                margin = margin(r = 10)),
    # title
    axis.title = element_text(size = textsz, color = "black", hjust = .5),
    # facet labels
    strip.text.x = element_text(size = textsz, color = "black",
                                margin = margin(b = 5)),
    strip.text.y = element_text(size = textsz, color = "black",
                                margin = margin(l = 5)),
    panel.spacing = unit(10, "pt"),
    # legend
    legend.position = "none",
    legend.title = element_text(size = textsz, color = "black", hjust = 0),
    legend.text = element_text(size = textsz, color = "black", hjust = 0),
    legend.margin = margin(l = 3)

  )
  
}