##==============================================================================================================
##	Journal of Applied Ecology 'Mediterranean marine protected areas have higher biodiversity via increased evenness, not abundance'
##  code: Shane Blowes
##==============================================================================================================
# code to produce map for main text with two protection classes 
source('~/Dropbox/1current/mediterranean_mpa/MediterraneanMPA/01a_data_join_and_clean.R')

 
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

ggplot() +
  geom_sf(data = world, fill = 'antiquewhite') +
  geom_point(data=unique_coords2, aes(x=lon, y=lat, colour=alt_enforcement, shape=alt_enforcement),
             alpha=0.7, size = 2) +
  # geom_text(data= world_points,
  #           aes(x=X, y=Y, label=name),
  #           fontface = "bold", check_overlap = FALSE,
  #           hjust = 1) +
  annotate(geom = 'text', x = 0, y = 41.5,
           label = 'Spain',
           # fontface = 'bold',
           hjust = 0) +
  annotate(geom = 'text', x = 3.5, y = 44.75,
           label = 'France',
           # fontface = 'bold'
           ) +
  annotate(geom = 'text', x = 9, y = 44.75,
           label = 'Italy',
           # fontface = 'bold'
           ) +
  scale_colour_manual(name='Protection', values=c('#e41a1c', '#006600')) +
  # scale_size_area(name='Number of\ntransects', breaks=c(25, 50)) +
  scale_shape_manual(name='Protection', values=c(1, 5)) +
  scale_x_continuous(breaks = seq(0,10,by=2)) +
  scale_y_continuous(breaks = seq(38, 45 ,by=2)) +
  coord_sf(xlim = c(0,10), ylim = c(38, 45)) +
  # ggspatial::annotation_scale(location = "bl")
  xlab('Longitude') +
  ylab('Latitude') +
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.key = element_blank(),
        panel.background = element_rect(fill = 'aliceblue'),
        plot.margin = margin())

# ggsave('~/Dropbox/1current/mediterranean_mpa/submitted/JApplEcol/revision2/figs/Fig1_new.png', width = 100, height = 100, units = 'mm')
