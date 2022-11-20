# Generates Ctenomys Distributions without water bodies

# 0. Set-up ---------------------------------------------------------------

library(sf)
library(sp)
library(tidyverse)

crs <- 'WGS84'

# 1. Ctenomys Distribution Areas ---------------------------------------------

ctenomys <- sf::read_sf("data/ctenomys_distribution_areas.shp")

# **Subtracting water -----------------------------------------------------

water_bodies <- sf::st_read('data/water_bodies.shp') %>%
  sf::st_zm(drop=T, what='ZM') %>%
  sf::st_transform(crs) %>%
  mutate(valido = sf::st_is_valid(.))

## Water Moll Transformation
water_bodies_moll <- water_bodies %>% 
  sf::st_transform(crs="+proj=moll")
 
## free memory
gc()

 
## Ctenomys Moll Transformation

ctenomys_moll <- ctenomys %>%
  sf::st_transform(crs="+proj=moll") %>%
  mutate(raw_area = round(units::set_units(sf::st_area(.),km^2)) %>% as.numeric())

## Iterative water bodies subtraction

ctenomys_without_water <- sf::st_sf(sf::st_sfc(),crs="+proj=moll")

for(i in unique(ctenomys_moll$species)){

  # Me quedo con una especie
  tuco <- ctenomys_moll %>%
    filter(species == i)

  # Le resto el agua
  int_tuco_water <- sf::st_intersection(tuco,water_bodies_moll) %>%
    sf::st_union()

  if(is_empty(int_tuco_water)) {
    # Si la intersecci?n tuco-agua da vac?a, la distribuci?n es la misma
    tuco_without_water <- tuco
  } else {
    # Si no, se la resto a la distribuci?n original
    tuco_without_water <- sf::st_difference(tuco,int_tuco_water)
  }

  ctenomys_without_water <- ctenomys_without_water %>%
    rbind(tuco_without_water)

  print(i)

}

## free memory
gc()
rm(water_bodies)
rm(water_bodies_moll)

## For some reason the water subtraction makes C. magellanicus GEOMETRYCOLLECTION
## and can't save the shapefile 

geometry_check <- ctenomys_without_water %>%
  mutate(geo_type=st_geometry_type(.)) %>%
  sf::st_drop_geometry()

## Necesito volverlo POLYGON o MULTIPOLYGON para poder guardar el shape file.

magellanicus_geo <- ctenomys_without_water %>%
  filter(species =='Ctenomys magellanicus') %>%
  st_collection_extract(., "POLYGON") %>%
  st_union(.,by_feature = FALSE) %>%
  st_as_sf() %>%
  rename(geometry=x) %>%
  mutate(species='Ctenomys magellanicus') %>% 
  left_join(sf::st_drop_geometry(ctenomys_without_water))

 
# #** Calculo areas de tucos sin agua en proyeccion Moll----

ctenomys_without_water_areas <- ctenomys_without_water %>%
  filter(species!='Ctenomys magellanicus') %>%
  bind_rows(magellanicus_geo) %>%
  mutate(area_without_water = round(units::set_units(sf::st_area(.),km^2)) %>% as.numeric(),
         # dif=raw_area-area_sin_agua,
         tipo_geo=st_geometry_type(.)) %>%
  arrange(species)


#** Guardo el shape de los tucos sin agua en Mercator para figuras (con area en Moll)----
sf::st_write(sf::st_transform(ctenomys_without_water_areas, crs = 4326 ),
             "data/ctenomys_distribution_areas_water_subtracted.shp")

rm(magellanicus_geo)
rm(int_tuco_water)
rm(tuco_without_water)

gc()

# 2. Intersecciones con las areas protegidas ---------------------------------

# Cargo areas protegidas generadas con el script: generate_protected_areas.R
protected_areas <- sf::st_read("data/raw_interest_protected_areas.shp")

protected_areas_moll <- protected_areas %>% 
  sf::st_transform(crs="+proj=moll") 

areas_united <- protected_areas_moll %>%
  sf::st_union()

# Calculo intersecciones

# areas_ctenomys_without_water <-  sf::read_sf("data/ctenomys_distribution_areas_water_subtracted.shp") %>%
areas_ctenomys_without_water <-  ctenomys_without_water_areas %>%
  mutate(valido=st_is_valid(.)) %>% 
  sf::st_transform(crs="+proj=moll") %>% 
  mutate(valido_moll=st_is_valid(.))

# Intersecciones con las areas sueltas para registrar nombres y cantidad de areas
int_global_names <- sf::st_intersection(areas_ctenomys_without_water, protected_areas_moll) %>%
  mutate(int_name = paste0(species," ",name," ",wdpaid),
         lugar = paste0(orig_name," ","(",iso3,")"))

int_global_names_sp <- int_global_names %>%
  as_tibble() %>%
  group_by(species,lugar) %>% 
  group_by(species) %>% 
  # mutate(n_areas = n_distinct(lugar)) %>% 
  summarize(areas = paste(sort(unique(lugar)),collapse=", "),
            n_areas = n_distinct(lugar),
            paises = paste(sort(unique(iso3)),collapse=", ")) %>% 
  select(species,n_areas,areas,paises)

# Tucos & Areas unidas (no deber?a haber overlapping)
int_global <- sf::st_intersection(areas_ctenomys_without_water, areas_united) %>% 
  mutate(protected_area = round(units::set_units(sf::st_area(.),km^2)) %>% as.numeric())

# Guardo shape de intersecciones en Mercator para figura (con areas en Moll)
sf::st_write(sf::st_transform(int_global, crs = 4326 ),
             "data/intersections_ctenomys_protected_areas.shp")

gc()


## 3. Resultados----
##**Tabla de nombre de especie, areas de tucos y areas de intersecciones----

ctenomys_areas_table <- ctenomys_moll %>%
  sf::st_drop_geometry()

ctenomys_without_water_areas_table <- ctenomys_without_water_areas %>% 
  sf::st_drop_geometry() %>% 
  select(-tipo_geo)

intersections_areas_table <- int_global %>% 
  as.data.frame() %>% 
  select(-c(valido,valido_moll,geometry)) 

intersections_name_table <- int_global_names_sp 

summary_table <- ctenomys_areas_table %>%
  left_join(ctenomys_without_water_areas_table) %>% 
  left_join(intersections_areas_table) %>% 
  mutate(prop = round(protected_area/area_without_water*100,2)) %>% 
  left_join(intersections_areas_table) %>% 
  replace_na(list(protected_area = 0,
                  prop = 0,
                  n_areas = 0,
                  areas = '-',
                  paises = '-'))

xlsx::write.xlsx(summary_table, "summary_table.xlsx")


# All layers --------------------------------------------------------
## Generates an only .shp with all layers for make the interactive map

ctenomys <- ctenomys_without_water_areas %>% 
  sf::st_transform(crs = 4326 ) %>% 
  mutate(capa = 'DA',
         orig_name = NA,
         ar_prtg = NA)

areas <- protected_areas %>% 
  select(orig_name) %>% 
  mutate(capa = 'PA',
         BINOMIA = NA,
         ar_sn_g = NA,
         ar_prtg = NA) 

intersections <- int_global %>%
  sf::st_transform(crs = 4326 ) %>% 
  mutate(capa = 'I',
         ar_sn_g = NA,
         orig_name = NA)

complete_shape <- ctenomys %>% 
  bind_rows(areas) %>% 
  bind_rows(intersections)

## Saving new shape----

st_write(complete_shape, "data/all_layers_interactive_map.shp")
