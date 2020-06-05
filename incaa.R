##----------------------------------------------------------------
## INCAA peliculas estrenadas en Argentina en 2018
## Autor: German Gonzalez
## Fecha 5/6/20
##----------------------------------------------------------------

# install.packages("extrafont")
# install.packages("waffle", repos = "https://cinc.rud.is")

library(tidyverse)
library(readxl)
library(stringr)
library(waffle)
library(hrbrthemes)
library(RColorBrewer)

options(bitmapType ="cairo")

##---------------------- Cargamos los datos -----------------------------


nacionales <- read_excel("peliculas_2018_incaa.xlsx", sheet="nacionales")
internacionales <- read_excel("peliculas_2018_incaa.xlsx", sheet="internacionales")

glimpse(nacionales)
glimpse(internacionales)

##---------------------- Creamos tabla resumen -----------------------------

## Tablas
nac_tab <- nacionales %>%
  mutate(GENERO = str_split_fixed(gsub(" ", "",GENERO), "/", n=2)[,1] ) %>%
  mutate(GENERO = fct_lump_min(GENERO, min=5, other_level = "OTRO")) %>%
  group_by(GENERO) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  mutate(ORIGEN="Nacionales")
  
int_tab <- internacionales %>%
  mutate(GENERO = str_split_fixed(gsub(" ", "",GENERO), "/", n=2)[,1] ) %>%
  mutate(GENERO = fct_lump_min(GENERO, min=10, other_level = "OTRO")) %>%
  group_by(GENERO) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  mutate(ORIGEN="Internacionales")
 
incaa <- rbind(nac_tab, int_tab)
 
##--------------------------- ploteamos ----------------------------------
 
ggplot(incaa, aes(fill = GENERO, values = n)) +
  geom_waffle(n_rows = 20, size = 0.33, colour = "white", flip = TRUE) +
  coord_equal() +
  facet_wrap(~ORIGEN, ncol=2) +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  #scale_fill_brewer(palette="Set1")+
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Set1"))(nrow(incaa))) +
  theme(legend.position="bottom") +
  guides(fill=guide_legend(nrow=2)) +
  theme(legend.position="bottom") + 
  labs(title = "Películas estrenadas en Argentina en 2018",
              subtitle = "Por Germán González (@germangeler)",
              caption = "Fuente: Anuario INCAA de la Industria Cinematográfica y Audiovisual Argentina 2018")
ggsave("waffle_inca.png", dpi=300, height=9, width=9) 

  
 
