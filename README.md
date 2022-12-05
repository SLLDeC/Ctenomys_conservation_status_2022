# Filling the gap in distribution ranges and conservation status in _Ctenomys_ (Rodentia, Ctenomyidae).
Caraballo, D. A., López, S. L., Botero-Cañola, S., y Gardner, S. L. Journal of Mammalogy, in _press_

This paper proposes new conservation status based on rewieved _Ctenomys_ distribution areas while checking for they overlap with protected areas and the conservation status of the species from this genus and includes an interactive map. This is made using Rstudio, tidyverse, sf, and leaflet packages.

## Data

Due to size restriction, the data can be downloaded from [here](https://drive.google.com/drive/folders/1a2Rsq3zyFP0Rr3twxRhqQF6Lt1axf1F1?usp=sharing)

### Initial Data
**ctenomys_distribution_areas**

**water_bodies**

**raw_interest_protected_areas**


### Generated Data
-  **ctenomys_distribution_areas_water_subtracted**: _ctenomys_distribution_areas_ with water_bodies subtracted.
-  **intersections_ctenomys_protected_areas**: computed intersections between _ctenomys_distribution_areas_water_subtracted_ and _raw_interest_protected_areas_.

-  **all_layers_interactive_map**: one shape with the layers _ctenomys_distribution_areas_water_subtracted_, -  _intersections_ctenomys_protected_areas_ and _raw_interest_protected_areas_ for teh interactive map.
-  **all_layers_simplified_interactive_map**: all_layers_interactive_map simplified at [mapshaper](https://mapshaper.org
) to 10% for interactive map.



## Files

-  **data_preparation.R**: manipulates distributions for area calculations and data visualization

-  **figures_1_and_2.R**

-  **figure_3.html**: html sankey code.

- **Ctenomys_Land_Cover_Change.r**: Estimate large scale land cover changes within the geographic range of each Ctenomys species.

Files with _SD_ prefixes generates the Supplementary Data:
-  **Supplementary Data SD1**. Interactive map showing _Ctenomys_ geographic distribution areas (DAs), protected areas (PAs), and their intersection.

-  **Supplementary Data SD4**. List of protected areas (PAs) that overlap _Ctenomys_ geographic distributions.

-  **Supplementary Data SD5**. Interactive table showing distribution areas, conservation status, overlap with protected areas, and habitat transformation in the past 20 years for 68 extant _Ctenomys_ species.
