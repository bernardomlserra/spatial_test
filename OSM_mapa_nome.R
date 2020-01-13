library(sf)
library(dplyr)
library(mapview)
library(beepr)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
#library(ggtext)
library(sysfonts)
library(showtext)
library(extrafont)
font_import("Roboto")
fonts()

setwd('/Users/mackook/Desktop/R/')

i= 'rio de janeiro'


# Open OSM street network
malha_viaria <- st_read(paste0('./dados/infra_transporte/vias/pbf/', i ,'_malha viaria.pbf'),  'lines')%>%
  select(osm_id, name, highway, z_order,other_tags, geometry) %>%
  filter(highway %in% c('motorway', 'trunk', 'primary', 'secondary', 'tertiary', 'unclassified', 'residential',
                        'motorway_link', 'trunk_link', 'primary_link', 'secondary_link', 'tertiary_link',
                        'living_street', 'service', 'track', 'bus_guideway', 'escape','raceway', 'road'))

# Open city limits 
muni <- st_read('./dados/IBGE/br_municipios/BRMUE250GC_SIR.shp')%>%
  mutate(CD_GEOCMU = as.numeric(as.character(CD_GEOCMU))) %>%
  filter(CD_GEOCMU == subset(munis_df, name_muni==i)$code_muni)
muni <- st_transform(muni, 4326)

# Use only street network of the city
malha_viaria_muni <- st_intersection(malha_viaria, muni)
malha_viaria_muni$nome <- tolower(malha_viaria_muni$name)
beep()

# Identify main name to consider
malha_viaria_muni$tipo <- tolower(substr(malha_viaria_muni$name,0, 5))
tipo_ordenados <- sort(malha_viaria_muni$tipo)
unique(tipo_ordenados)
table (tipo_ordenados)

a <- malha_viaria_muni %>% filter(tipo == 'largo')
sort(unique(a$name))
mapview(a)
head(a)
str(a)

## Tipos com valores importantes:
## acess, aenid, alame, autoe, av, av., aveni, bec , beco, camin, condo, estra, lad , 
## ladei, largo, linha, mergu, ponte, praca, praça, ##praia, ##qd , ##qudr, r. , rau, retor, 
## ria , riua, rodov, rua, ruas, rue, ruia, rus, rya, trans, trave, treav, trevo, trev, 
## trv, túnel , via, viadu, vila, vila, vila, villa

# str_detect(name, "alameda | rua | rus  | r. | ruas | rue | ruia | ria | rau | rya | traves | treavessa | lad d | ladeira | tv. | tv | trv | beco | bec") ~ "Rua",
# str_detect(name, "aenida | av  | av.enida | av.| avenida | boulevard") ~ "Avenida",
# str_detect(name, "brt t| trans") ~ "BRT",
# str_detect(name, "praca| praça| largo") ~ "Praça",
# str_detect(name, "ponte") ~ "Ponte",
# str_detect(name, "acesso | anel viário | aterro | autoestrada| auto estrada| marginal | auto estrada | curva chico | elevado | estrada | linha | mergulhão | rodovia | rodovia | retorno | trevo | trev | túnel, viaduto") ~ "Vias rápidas",
# str_detect(name, "bairro gratidão | condomínio | edifício | entrada  | iraja | morada do sol | parque dona laura | parque carolina | residencial | vila | vila, | villa") ~ "Condominio",
# str_detect(name, "caminho | celestino | cemiterio | cetep | comunidade | arco do teles | galeria | orla prefeito luiz paulo conde | gran meliá nacional | lote | loteamento | outra adutora de cedae | passarela | pista cláudio coutinho") ~ "Caminho"

font_add_google("Oswald")

df_classificado <- 
  malha_viaria_muni %>% 
  filter(!is.na(nome)) %>% 
  mutate(type = case_when(
      str_detect(nome, "alameda|rua|traves|treavessa|lad d|ladeira|tv.|trv|beco|bec") ~ "Rua",
      str_detect(nome, "avenida|aenida|av|boulevard") ~ "Avenida",
      str_detect(nome, "praça|largo|praca ") ~ "Praça",
      str_detect(nome, "caminho|arco do teles|galeria|orla prefeito luiz paulo conde|gran meliá nacional|lote|loteamento|passarela|pista cláudio coutinho") ~ "Caminho",
      str_detect(nome, "brt t|trans") ~ "BRT",
      str_detect(nome, "acesso|anel viário|aterro|autoestrada|auto estrada|marginal|auto estrada|elevado|estrada|linha|mergulhão|rodovia|retorno|trevo|trev|túnel|viaduto") ~ "Estrada/Via rápida",
      str_detect(nome, "condomínio|vila|vila,|villa") ~ "Condomínio/Vila",
      TRUE ~ "Outros"), 
      type = factor(type, levels = c("Rua", "Avenida", "Praça", "Caminho", "BRT", "Estrada/Via rápida", "Condomínio/Vila", "Outros"))
      )

table(df_classificado$type)

# Rua ~ 24292/49795= 49%
# Avenida ~ 3413/49795 = 7%
# Condomínio/Vila ~ 2203/49795 = 4%
# Sem nome ~ 17861/49795= 36%


df_classificado %>% 
  ggplot() + 
  # ## water
  # geom_sf(data = sf_bln_water,
  #         fill = "#cedded",  
  #         color = "#cedded") +
  ## roads by type
  geom_sf(aes(color = type), 
          size = 0.2, 
          show.legend = "point") + 
  scale_color_manual(
    values = c("#f6cf71", ## Rua 
               "#019868", ## Praça 
               "#9dd292", ## Caminho
               "#ec0b88", ## Avenida
               "#651eac", ## BRT
               "#2b7de5", ## Estrada/Via rápida
               "#f21811", ## Condomínio/Vila  
               "#c6c6c6"), ## Outros
    name = NULL,
    labels = c("Rua", "Praça/Largo", "Caminho", "Avenida", "BRT", "Estrada/Via rápida", "Condomínio/Vila", "Outros")
  ) +
  geom_sf(data=muni, fill=NA, colour = 'black', size = 0.1)+
  guides(color = guide_legend(title.position = "top", 
                              title.hjust = 0.5, nrow = 1,
                              label.position = "right",
                              override.aes = list(size = 5))) +
  theme(line = element_blank(),                          # remove axis lines ..
        axis.text=element_blank(),                       # .. tickmarks..
        axis.title=element_blank(),
        legend.position = "top",
        plot.title = element_text(hjust = 0.6, size=24, face = "bold"), 
        text=element_text(size=12,  family= "Tahoma"),
        panel.background = element_blank())+
  # annotate("text", x = 13.45, y = 52.344,
  #          hjust = 0.6, vjust = 1,
  #          label = "Dados OpenStreetMap",
  #          size = 4.5,
  #          color = "grey95") +
  labs(title = "NOME DAS VIAS NO RIO DE JANEIRO")

ggsave(paste0('./resultados/mapas/teste_ruas_', i, '.png'), dpi = 500, width = 12, height = 11.63)

