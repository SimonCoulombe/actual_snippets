v <- ggplot(faithfuld, aes(waiting, eruptions, z = density))
v + geom_contour(aes(colour = after_stat(level)))

ggplot() + 
  geom_contour(data = dem_spdf, 
               aes(x=lon, y=lat, z = elev, colour = after_stat(level))
               )  +
  scale_color_viridis_c()
