library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tigris)
library(leaflet)
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
pop_neighborhoods <- read_excel("PB_barrio_ARIP_CNP2010.xls", skip= 1, range = cell_cols(2:65))

#checked exploratory.io on how to add select and starts_with()
#when I built the code I did without the pipes but when later I added
#them, the code did not work! A mistery...
#Here aldo dropped 2 cols and several rows
pop_neighborhoods2 <- select(pop_neighborhoods, -"Total var?n", -"Total mujer", -starts_with("X__"))
pop_neighborhoods3 <- filter(pop_neighborhoods2, row_number() != 1L) 
pop_neighborhoods4 <- filter(pop_neighborhoods3,Barrio !="Total")

#matched the neighborhood names. There is a problem with Spanish characters
# that I was not able to solve. I tried to reopen with encoding and other
#suggestions that I've found online but they did not work:
# Each time the file is oppened I have to change all the characters again
pop_neighborhoods4[41,1] = "Nuñez"
pop_neighborhoods4[31,1] = "Villa Gral. Mitre"
pop_neighborhoods4[11,1] = "Boca"


### I could not find another way to change the type of 
## multiple columns but this one. Embarrasing...
pop_neighborhoods4$`0-4` <- as.numeric(pop_neighborhoods4$`0-4`)
pop_neighborhoods4$`5-9` <- as.numeric(pop_neighborhoods4$`5-9`)
pop_neighborhoods4$`10-14` <- as.numeric(pop_neighborhoods4$`10-14`)
pop_neighborhoods4$`15-19` <- as.numeric(pop_neighborhoods4$`15-19`)
pop_neighborhoods4$`20-24` <- as.numeric(pop_neighborhoods4$`20-24`)
pop_neighborhoods4$`25-29` <- as.numeric(pop_neighborhoods4$`25-29`)
pop_neighborhoods4$`30-34` <- as.numeric(pop_neighborhoods4$`30-34`)
pop_neighborhoods4$`35-39` <- as.numeric(pop_neighborhoods4$`35-39`)
pop_neighborhoods4$`40-44` <- as.numeric(pop_neighborhoods4$`40-44`)
pop_neighborhoods4$`45-49` <- as.numeric(pop_neighborhoods4$`45-49`)
pop_neighborhoods4$`50-54` <- as.numeric(pop_neighborhoods4$`50-54`)
pop_neighborhoods4$`55-59` <- as.numeric(pop_neighborhoods4$`55-59`)
pop_neighborhoods4$`60-64` <- as.numeric(pop_neighborhoods4$`60-64`)
pop_neighborhoods4$`65-69` <- as.numeric(pop_neighborhoods4$`65-69`)
pop_neighborhoods4$`70-74` <- as.numeric(pop_neighborhoods4$`70-74`)
pop_neighborhoods4$`75-79` <- as.numeric(pop_neighborhoods4$`75-79`)
pop_neighborhoods4$`80-84` <- as.numeric(pop_neighborhoods4$`80-84`)
pop_neighborhoods4$`85-89` <- as.numeric(pop_neighborhoods4$`85-89`)
pop_neighborhoods4$`90-94` <- as.numeric(pop_neighborhoods4$`90-94`)
pop_neighborhoods4$`95 y más` <- as.numeric(pop_neighborhoods4$`95 y más`)

### Created six age groups
pop_neighborhoods5 <- mutate(pop_neighborhoods4, `0-14` =(`0-4`+`5-9`+`10-14`)*100/`Total`,
                               `15-29` =(`15-19`+`20-24`+`25-29`)*100/`Total`,
                               `30-44` = (`30-34`+`35-39`+`40-44`)*100/`Total`,
                              `45-59` = (`45-49`+ `50-54`+ `55-59`)*100/`Total`,
                               `60-74` = (`60-64`+ `65-69`+`70-74`)*100/`Total`,
                               `75-+95` = (`75-79`+`80-84` +`85-89`+`90-94`+ `95 y más`)*100/`Total`)

###Matched the name of neighborhoods to upper
pop_neighborhoods5$Barrio <-str_to_upper(pop_neighborhoods5$Barrio)

#Created the variables for aes
pop_neighborhoods6 <- pop_neighborhoods5 %>% gather("ages",  "age group pct", 23:28) 

##Leftjoined both data sets
popgeomneighborhood <- left_join(geom_neighborhoods, pop_neighborhoods6, by=c("BARRIO" = "Barrio"))

### Created the map with a different palette 
## and positioned the ages label at the bottom of the map
popgeomneighborhood %>%
ggplot(aes(fill = `age group pct`, color = `age group pct`)) +
  facet_wrap(~ages, strip.position = "bottom") +
  geom_sf() +
  coord_sf() + 
  scale_fill_viridis(direction=-1, option = "plasma") +
  scale_color_viridis(direction=-1, option = "plasma") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  labs(title="Age groups in Buenos Aires Neighborhoods", caption="Source: Indec, Datos Argentina (datos.gob.ar)")









