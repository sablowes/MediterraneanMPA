# code to produce a map 
source('~/Dropbox/1current/mediterranean_mpa/MediterraneanMPA/')


# get the map
# map <- ggmap::get_stamenmap(bbox=c(1, 38, 10, 45), zoom=7, maptype = 'terrain-background')
# # world map for country borders
# world <- map_data('world')
# # filter to countries of interest
# countries <- world %>% 
#   filter(region=='France' | region=='Spain' | region=='Italy')
# 
# ggmap::ggmap(map, darken=0.2) + #med_map_hybrid
#   borders(regions = c('France', 'Spain', 'Italy'), colour = 'black') +
#   geom_point(data=unique_coords2, aes(x=lon, y=lat, colour=alt_enforcement, shape=alt_enforcement),
#              alpha=0.7, size = 2) +
#   #	scale_colour_brewer(name='Protection', type='qual', palette='Set1') +
#   scale_colour_manual(name='Protection', values=c('#e41a1c', '#006600')) +
#   # scale_size_area(name='Number of\ntransects', breaks=c(25, 50)) +
#   scale_shape_manual(name='Protection', values=c(1, 5)) +
#   # scale_x_continuous(breaks = seq(0,10,by=2)) +
#   # coord_fixed(xlim = c(0,10), ylim = c(37, 45)) +
#   xlab('Longitude') +
#   ylab('Latitude') +
#   theme(legend.position = 'top',
#         legend.direction = 'horizontal')
# # 
# # ggsave('/Users/sb25gaqy/Dropbox/1current/mediterranean_mpa/submitted/JApplEcol/revision1/figs/Fig1.png', width = 100, height = 100, units = 'mm')
# 
# library(rnaturalearth)
# library(rnaturalearthdata)
# library(sf)
# 
# world <- ne_countries(scale = "medium", returnclass = "sf")
# class(world)
# 
# world_points<- st_centroid(world)
# world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))
# 
# ggplot() +
#   geom_sf(data = world, fill = 'antiquewhite') +
#   geom_point(data=unique_coords2, aes(x=lon, y=lat, colour=alt_enforcement, shape=alt_enforcement),
#              alpha=0.7, size = 2) +
#   # geom_text(data= world_points,
#   #           aes(x=X, y=Y, label=name),
#   #           fontface = "bold", check_overlap = FALSE,
#   #           hjust = 1) +
#   annotate(geom = 'text', x = 0, y = 41.5,
#            label = 'Spain', 
#            # fontface = 'bold', 
#            hjust = 0) +
#   annotate(geom = 'text', x = 3.5, y = 44.75,
#            label = 'France', 
#            # fontface = 'bold'
#            ) +
#   annotate(geom = 'text', x = 9, y = 44.75,
#            label = 'Italy', 
#            # fontface = 'bold'
#            ) +
#   scale_colour_manual(name='Protection', values=c('#e41a1c', '#006600')) +
#   # scale_size_area(name='Number of\ntransects', breaks=c(25, 50)) +
#   scale_shape_manual(name='Protection', values=c(1, 5)) +
#   scale_x_continuous(breaks = seq(0,10,by=2)) +
#   scale_y_continuous(breaks = seq(38, 45 ,by=2)) +
#   coord_sf(xlim = c(0,10), ylim = c(38, 45)) +
#   # ggspatial::annotation_scale(location = "bl")
#   xlab('Longitude') +
#   ylab('Latitude') +
#   theme(legend.position = 'top',
#         legend.direction = 'horizontal',
#         legend.key = element_blank(),
#         panel.background = element_rect(fill = 'aliceblue'),
#         plot.margin = margin())
# 
# ggsave('~/Dropbox/1current/mediterranean_mpa/submitted/JApplEcol/revision2/figs/Fig1_new.png', width = 100, height = 100, units = 'mm')
