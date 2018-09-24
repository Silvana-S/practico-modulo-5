library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tigris)
library(sf)
library(stringr)
library(viridis)

options(tigris_class = "sf")
####
#opened the shape file
geom_neighborhoods<- "barrios_badata.shp"
geom_neighborhoods<- st_read(geom_neighborhoods)
geom_neighborhoods$BARRIO <- as.character(geom_neighborhoods$BARRIO)
#checked the cheatsheet of readxl to add range argument and drop col1
pop_neighborhoods <- read_excel("data/PB_barrio_ARIP_CNP2010.xls", skip= 1, range = cell_cols(2:65))
pop_neighborhoodnew <- select(pop_neighborhoods, -"Total var?n", -"Total mujer", -starts_with("X__"))
pop_neighborhoodnew2 <- filter(pop_neighborhoodnew,  row_number() != 1L)
pop_neighborhoodnew3 <- filter(pop_neighborhoodnew2, Barrio !="Total")
#checked exploratory.io on how to add select and starts_with()
#when I built the code I did without the pipes but when later I added
#them, the code did not work! A mistery...
#Here aldo dropped 2 cols and several rows
 pop_neighborhoodnew3[41,1] = "Nu?ez"
 pop_neighborhoodnew3[31,1] = "Villa Gral. Mitre"
 pop_neighborhoodnew3[11,1] = "Boca"
 #matched the neighborhood names. There is a problem with Spanish characters
 # that I was not able to solve. I tried to reopen with encoding and other
 #suggestions that I've found online but they did not work:
 # Each time the file is oppened I have to change all the characters again
pop_neighborhoodnew3$Barrio <-str_to_upper(pop_neighborhoodnew3$Barrio)
popgeomneighborhood <- left_join(geom_neighborhoods, pop_neighborhoodnew3, by=c("BARRIO" = "Barrio"))

popgeomneighborhood %>%
  ggplot(aes(fill = Total, color = Total)) +
  geom_sf() +
  coord_sf() + 
  scale_fill_viridis(direction=-1, option = "plasma") +
  scale_color_viridis(direction=-1, option = "plasma") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  labs(title="Population in Buenos Aires Neighborhoods", caption="Source: Indec, Datos Argentina (datos.gob.ar)")










