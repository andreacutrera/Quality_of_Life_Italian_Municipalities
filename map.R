library(maptools)
library(ggplot2)
library(ggalt)
library(ggthemes)
library(tibble)
library(viridis)

# get italy region map
italy_map <- map_data("italy")

# your data will need to have these region names
print(unique(italy_map$region))

# we'll simulate some data for this
set.seed(1492)
choro_dat <- data_frame(region=unique(italy_map$region),
                        value=sample(100, length(region)))

# we'll use this in a bit
italy_proj <- "+proj=aea +lat_1=38.15040684902542
+lat_2=44.925490198742295 +lon_0=12.7880859375"

gg <- ggplot()

# lay down the base layer
gg <- gg + geom_map(data=italy_map, map=italy_map,
                    aes(long, lat, map_id=region),
                    color="#b2b2b2", size=0.1, fill=NA)

# fill in the regions with the data
gg <- gg + geom_map(data=choro_dat, map=italy_map,
                    aes(fill=value, map_id=region),
                    color="#b2b2b2", size=0.1)

# great color palette (use a better legend title)
gg <- gg + scale_fill_viridis(name="Scale title")

# decent map projection for italy choropleth
gg <- gg + coord_proj(italy_proj)

# good base theme for most maps
gg <- gg + theme_map()

# move the legend
gg <- gg + theme(legend.position=c(0.95, 0.3))

gg