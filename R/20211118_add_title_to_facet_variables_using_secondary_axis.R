#the secondary axis is now an option: https://ggplot2.tidyverse.org/reference/sec_axis.html

# Basic faceted plot
p <- ggplot(mtcars, aes(cyl, mpg)) +
  geom_point() +
  facet_grid(vs ~ am)
    
    # Create a simple secondary axis for the facets (use the appropriate scale_x function)
p + 
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "SECOND Y AXIS", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "SECOND X AXIS", breaks = NULL, labels = NULL))
