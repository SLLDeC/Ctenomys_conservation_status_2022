## Generates the figures 1 and 2 

library(tidyverse)
library(ggspatial)
library(rnaturalearth)

# ***Species CS*** --------------------------------------------------------

cat_peligro <- c('VU','EN','CR')
cat_no_peligro <- c('LC','NT')

species_list <- read_csv("data/full_species_list.csv",
                         col_types = list( "distribution_area_km2" = col_number())) %>%
  janitor::clean_names() %>% # formatea nombres de variables
  filter(!is.na(species)) %>%  # elimina las filas sin nombre de especie (por ej. las que tiene totales)
  filter(!str_detect(species,'candidate')) # elmina la fila de 'unnamed candidate specie'

species_list_norm <- species_list %>% 
  mutate(categoria = case_when(
    str_detect(conservation_status_cs,'Higher category than LC') ~ "VU" ,
    str_detect(conservation_status_cs,'VU*') ~ "VU",
    str_detect(conservation_status_cs,'-') ~ "-",
    str_detect(conservation_status_cs,regex('[aA-zZ]')) ~ conservation_status_cs,
  ),
  categoria_bi = case_when(
    categoria %in% cat_peligro ~ 'peligro',
    categoria %in% cat_no_peligro ~ 'no peligro',
    T ~ 'no evaluada')) %>% 
  select(species, conservation_status_cs, categoria, categoria_bi, 
         no_localities, distribution_area_km2)

species_graph <- species_list_norm %>% 
  # acorto y formateo nombres de especies
  mutate(species_red = str_replace(species,'Ctenomys','C.'),
         species_label = paste("italic('",species_red,"')")) %>% 
  # traduzco categorias
  # mutate(categoria_bi_en =case_when(categoria_bi == 'peligro' ~ 'Threatened',
  mutate(categoria_bi_en =case_when(categoria_bi == 'peligro' ~ 'VU, EN, CR',
                                    # categoria_bi == 'no peligro' ~ 'Not Threatened',
                                    categoria_bi == 'no peligro' ~ 'LC, NT',
                                    categoria_bi == 'no evaluada' ~ 'Not Evaluated')) %>% 
  mutate(nud_x = case_when(species_red %in% c('C. sericeus','C. fulvus','C. lami') ~ -0.5,
                           species_red %in% c('C. talarum') ~ -1,
                           species_red %in% c('C. boliviensis','C. argentinus','C. maulinus') ~ 1,
                           T ~ 0),
         nud_y = case_when(species_red %in% c('C. talarum','C. lami') ~ 12000,
                           distribution_area_km2 <= 5000 & no_localities >= 19 ~ 15000,
                           species_red %in% c('C. boliviensis') ~ 20000,
                           T ~ 0))

species_graph[68,8] <- 'Iberá'

# ordeno los niveles
species_graph$categoria_bi_en <- factor(
  species_graph$categoria_bi_en,
  # levels = c("Threatened", "Not Threatened", "Not Evaluated"))
  levels = c("LC, NT", "VU, EN, CR", "Not Evaluated"))


# All species -------------------------------------------------------------

species_graph_greater <- species_graph %>% filter(no_localities>10 | distribution_area_km2>20000) %>% 
  filter(!str_detect(species,'rioneg'))

categories_all <- ggplot(species_graph, aes(x=no_localities, y=distribution_area_km2, color=categoria_bi_en, label=species_label)) +
  geom_rect(mapping=aes(xmin=0, xmax=10, ymin=0, ymax=20000, fill=T),
            color="red", alpha=0.01, show.legend = FALSE)+
  geom_vline(aes(xintercept=10), colour='darkblue', linetype = "dashed") +
  geom_hline(aes(yintercept=20000), colour='darkblue', linetype = "dashed") +
  geom_hline(aes(yintercept=5000), colour='darkblue', linetype = "dashed") +
  geom_hline(aes(yintercept=100), colour='darkblue', linetype = "dashed") +
  geom_point(size=1) +
  ggrepel::geom_text_repel(data = species_graph_greater,
                           seed=1,
                           size=3.5,parse=T, show.legend = FALSE,
                           segment.size = 0.25,
                           # box.padding=0.5,
                           nudge_x = species_graph_greater$nud_x,
                           nudge_y = species_graph_greater$nud_y) +
  ggrepel::geom_text_repel(data = species_graph %>% filter(str_detect(species,'rioneg')),
                           segment.size = 0.25,
                           box.padding = 2,
                           size=3.5,parse=T, show.legend = FALSE) +
  scale_color_manual(values=c('green4', 'red', 'grey27')) +
  scale_x_continuous(breaks=seq(0,max(species_graph$no_localities,na.rm=T)+2,2), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x='Number of localities',
       y= expression(Distribution~area~(Km^2)),
       color='Threat category') 



# Zoom in -----------------------------------------------------------------

species_graph_zoom <- species_graph %>% 
  filter(no_localities<=10 & distribution_area_km2<=20000) %>% 
  mutate(nud_y = case_when(species_red %in% c('C. sociabilis','C. bergi') ~ -600,
                           species_red %in% c('C. brasiliensis') ~ 300,
                           # species_red %in% c('C. brasiliensis') ~ 1,
                           T ~ nud_y))

categories_zoom <- ggplot(species_graph_zoom,
                          aes(x=no_localities, y=distribution_area_km2, color=categoria_bi_en, 
                              label=species_label)) +
  geom_vline(aes(xintercept=10), colour='darkblue', linetype = "dashed") +
  geom_hline(aes(yintercept=20000), colour='darkblue', linetype = "dashed") +
  geom_hline(aes(yintercept=5000), colour='darkblue', linetype = "dashed") +
  geom_hline(aes(yintercept=100), colour='darkblue', linetype = "dashed") +
  geom_point(size=1, show.legend = FALSE) +
  coord_cartesian(xlim=c(0,10),ylim=c(-100,20000), clip='off') +
  ggrepel::geom_text_repel(seed=1,
                           segment.size=0.25,
                           max.overlaps=Inf,
                           # Repel away from the left edge, not from the right.
                           xlim = c(-Inf, Inf),
                           # # Do not repel from top or bottom edges.
                           ylim = c(-Inf, Inf),
                           size=3.5,parse=T,
                           nudge_y = species_graph_zoom$nud_y,
                           show.legend = FALSE) +
  scale_color_manual(values=c('green4', 'red', 'grey27')) +
  scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
  labs(x='Number of localities',
       y= expression(Distribution~area~(Km^2)))


# Save Plot ---------------------------------------------------------------

ggsave("figure_1.pdf",
       gridExtra::arrangeGrob(categories_all, categories_zoom, nrow=2),
       units = 'cm',
       width = 19,
       height = 29,
       dpi = "print")

# Figure 2: Maps --------------------------------------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf")

# colores http://sape.inf.usi.ch/quick-reference/ggplot2/colour

#* Ctenomys distribution ---------------------------------------------------

ctenomys_with_water <- sf::read_sf("data/ctenomys_distribution_areas.shp")

ctenomys_dist <- ggplot(data = world, fill= "gray98") +
  geom_sf() +
  # geom_sf(data = world, fill= "gray98") +
  geom_sf(data = ctenomys_with_water %>% add_row(),aes(fill = species),lwd=0.1, show.legend = FALSE) +
  coord_sf(xlim = c(-75.00, -50.00), ylim = c(-55.00, -10.00), expand = T) +
  ylab('Latitude') +
  xlab('Longitude') +
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = 'dashed', size = 0.2), 
        panel.background = element_rect(fill = 'aliceblue'))

ggsave("figure_2_a.png",
       ctenomys_dist,
       width = 3,
       height = 6,
       dpi = "print")

#* Ctenomys intersections and PA ---------------------------------------------------

intersections <- sf::read_sf("data/intersections_ctenomys_protected_areas.shp")
protected_areas <- sf::read_sf("data/raw_interest_protected_areas.shp")

intersections_and_pa <- ggplot(data = world, fill= "gray98") +
  geom_sf() +
  geom_sf(data = protected_areas,col='gold2', fill = 'gold2',show.legend = FALSE) +
  geom_sf(data = intersections, col = 'purple4', fill = 'purple4',show.legend = FALSE) +
  annotation_scale(location = 'br',
                   width_hint = 0.5) +
  annotation_north_arrow(location = 'br',
                         which_north = 'true',
                         # pad_x = unit(0.75, 'in'),
                         pad_y = unit(0.2, 'in'),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-75.00, -50.00), ylim = c(-55.00, -10.00), expand = T) +
  ylab('') +
  xlab('Longitude') +
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = 'dashed', size = 0.2), 
        panel.background = element_rect(fill = 'aliceblue'))

#Edited with photoshop buecause elimiating ticks expands figure
# axis.title.y=element_blank(),
# axis.text.y.left = element_blank(),
# axis.ticks.y.left = element_blank())


ggsave("figure_2_b.png",
       intersections_and_pa,
       width = 3,
       height = 6,
       dpi = "print")

#* Save figure ----------------------------------------------------------------

ggsave("figure_2.png",
       gridExtra::arrangeGrob(ctenomys_dist, intersections_and_pa, ncol=2),
       width = 6,
       height = 6,
       dpi = "print")

