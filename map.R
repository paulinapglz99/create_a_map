#map.R
#This is a template to create a map
#Install packages
install.packages("ggspatial")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")

#Libraries --- ---
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggplot2)
library(ggspatial)
library(cowplot)
library(gridExtra)

# Download global country data and state boundaries
world <- ne_countries(scale = "medium", returnclass = "sf")
states <- ne_states(country = "Mexico", returnclass = "sf")

# Filter to get only Mexico
mexico <- world[world$name == "Mexico", ]

# Creating points of interest with geographic coordinates
isla_tiburon <- st_point(c(-112.36, 28.96))  # Aproximadamente 28.966666666667, -112.36666666667
punta_chueca <- st_point(c(-112.16, 29.015))  # Aproximadamente 29.015, -112.16166667
desemboque_seri <- st_point(c(-112.39, 29.5))  # Aproximadamente 29.50500, -112.39556
geometries <- st_sfc(isla_tiburon, punta_chueca, desemboque_seri, crs = 4326)

# Create a data frame sf for ggplot2
puntos <- st_sf(
  Location = c("Isla Tiburón", "Punta Chueca", "Desemboque de los Seri"),
  color = c("red", "blue", "darkseagreen"),
  geometry = geometries
)

# Create the base map with zoom
mapa_zoom <- ggplot() +
  geom_sf(data = mexico, fill = "gray80", color = "black") +  # Mapa de México
  geom_sf(data = states, fill = NA, color = "white", size = 0.3) +  # Límites de estados
  geom_sf(data = puntos, aes(geometry = geometry, color = Location), size = 3) +
  geom_sf_label(data = puntos, aes(label = Location, geometry = geometry), size = 3, nudge_y = c(0.15, -0.15, 0.15), nudge_x = c(-0.3, 0.2, 0.3)) +  # Isla Tiburon, Punta Chueca, Desemboque
  scale_color_manual(values = c("Isla Tiburón" = "red", "Punta Chueca" = "blue", "Desemboque de los Seri" = "green")) +
  coord_sf(xlim = c(-115, -110), ylim = c(26, 32), expand = FALSE) +  # Ajustar límites de coordenadas para el zoom
  theme_minimal() +
  annotation_scale(location = "bl", width_hint = 0.5) +  # Agregar escala
  annotation_north_arrow(location = "br", which_north = "true", style = north_arrow_fancy_orienteering) +  # Flecha del norte
  labs(title = "",
       caption = "")

mapa_zoom

ggsave("map_comcaac.png", plot = mapa_zoom, dpi = 300)

# Create the base map without zoom (reference)
mapa_referencia <- ggplot() +
  geom_sf(data = mexico, fill = "gray80", color = "black") +  # Mapa de México
  geom_sf(data = states, fill = NA, color = "white", size = 0.3) +  # Límites de estados
  geom_rect(aes(xmin = -115, xmax = -110, ymin = 26, ymax = 32), color = "red", fill = NA, size = 1) +  # Cuadro de zoom
  theme_minimal() +
  coord_sf(xlim = c(-118, -86), ylim = c(14, 33), expand = FALSE) +  # Ajustar límites de coordenadas para la referencia
  theme_void()  # Sin ejes ni etiquetas

mapa_referencia

# Combine the maps with gridExtra and make the second map smaller.
mapa_final <- grid.arrange(
  mapa_referencia,
  mapa_zoom, 
  ncol = 2, 
  widths = c(1.2, 2)  # Ajustar ancho relativo de las columnas
)

#Save plots

ggsave("map_comcaac_final.png",
       plot = mapa_final, 
       device = "png",
       width = 15, 
       height = 10,
       units = "in",
       dpi = 300)
