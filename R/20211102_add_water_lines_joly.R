#https://www.katiejolly.io/blog/2020-03-21/water-lines

library(sf) # spatial data
library(tidyverse) # wrangling/cleaning
library(colorspace) #  for lightern()
library(showtext) # google fonts
library(osmdata) # to get shapes
library(polylabelr) # to get pole of inacessibility (point furthest from any edge, to give us an idea of the distance between water lines)
library(purrr) # to add a bunch of buffer lines automatically
font_add_google("Assistant", regular.wt = 300)
font_add_google("Arapey")
showtext_auto()

water_fill <- "#FAFDFF"
water_outline <- "#89A4B2"



# bounding box du québec pour nos recherches openstreet map 
bb <- getbb("Quebec, Canada", format_out = "sf_polygon") 
bb <-  bb[1,] # garder le polygone de la province de québec


# download lac st jean polygon
lac_st_jean_results <-  bb %>% 
  opq() %>%
  add_osm_feature("name" , "Lac Saint-Jean") %>%
  osmdata_sf()  %>%
  .$osm_multipolygons %>%
  filter(osm_id == 58214)

# which crs?


#' get_utm_crs returns the crs for a UTM in the northern hemisphere.  Example:    Montreal is in UTM zone 18, thus crs 32618
#'
#' @param longitude
#'
#' @return
#' @export
#'
#' @examples
get_utm_crs <- function(longitude){ 32600 + ceiling((longitude - -180)/ 6)} #  pour calculer des buffers ronds en mètres comme dans le day 5 de https://gitlab.com/dickoa/30daymapchallenge/-/blob/master/day5/day5-blue.R
#  la UTM zone (genre 19N pour québec à longitude -71   se trouve en comptant combien de zones de 6 degrés de longitude tu es à partir de -180 degrés... le CRS c'est 32600 + numéro de zone)

my_crs <-  get_utm_crs(st_centroid(lac_st_jean_results)  %>% st_coordinates() %>% .[1])
# answer: UTM 18N 

#  reproject to the best UTM (18N)
lac_st_jean.sf <- lac_st_jean_results %>% 
  st_transform(crs = my_crs)


# here is  my add_water_line() function and add_water_lines() wrapper
add_water_line <- function(data, buffer, lighten=0.1){
  geom_sf(data = data %>% st_buffer(buffer), fill = NA, 
          color = lighten(water_outline, lighten), lwd = 0.35
  ) 
}

add_water_lines <- function(data, buffers = c(-300), lightens = c(0.1)){
  z <- tibble(data = list(data), buffers, lightens)
  purrr::pmap(list(data = z$data, buffer = z$buffers, lighten = z$lightens), add_water_line)
}

# ok let's plot with a bunch of water lines 
ggplot() + 
  geom_sf(data = lac_st_jean.sf,fill = water_fill)+ 
# the pole of inaccessiblity could be a good candidate to display the text labels.. but not this time.
  #  geom_sf(data =  poi(lac_st_jean.sf)[[1]] %>% as_tibble() %>% st_as_sf(coords= c(lon = "x", lat = "y"),crs = my_crs)) +
  geom_sf_text(data = st_centroid(lac_st_jean.sf), aes(label = "Lac Saint-Jean"), color = water_outline, family = "Assistant", size = 7) +
  add_water_lines(lac_st_jean.sf, 
                  buffers = c(-300,
                              -600,
                              -900,
                              -1200,
                              -1500
                              ), 
                  lightens = c(0.1, 0.3, 0.5, 0.7, 0.9))+ 
  theme_void() + 
  labs(title = "Manually add buffers like an animal")

## let's try to add the buffers automatically  
# to do this, i figure out where the pole of inaccessibility is (point furthest away from the polygon)
# then we look at how far the point is from the polygon (here: 8000 meters)
# and add buffer at  1/30, 2/30, 3/30, 4/50 and 5/30 of that distance

add_default_water_lines <- function(data){
  distance_between_pole_of_inacessibility_and_polygon <- polylabelr::poi(data)[[1]]$dist
  
  z <- tibble(data = list(data), 
              buffers = distance_between_pole_of_inacessibility_and_polygon / 30  * seq(from=-1, to = -5),
              lightens = c(0.1, 0.3, 0.5, 0.7, 0.9)
              )
  purrr::pmap(list(data = z$data, buffer = z$buffers, lighten = z$lightens), add_water_line)
}

ggplot() + 
  geom_sf(data = lac_st_jean.sf,fill = water_fill)+ 
  geom_sf_text(data = st_centroid(lac_st_jean.sf), aes(label = "Lac Saint-Jean"), color = water_outline, family = "Assistant", size = 7) +
  add_default_water_lines(lac_st_jean.sf)+ 
  theme_void() + 
  labs(title = "Automatically pick buffer distance")
