##### Importation des library #####
library('sf')
library('dplyr')
library("ggplot2")
library("ggsn")
library('viridis')
library('RColorBrewer')
library('leaflet')
library('osmdata')
library('spdep')


##### Traitement des données ######

#Importation des données
setwd()#put your working directory
df <- read.csv2("infra_pena_2021.csv")

#importation des shapefiles
districts_ch <- st_read('./shapefiles/districts/swiss_districts.shp')
lacs_ch <- st_read('./shapefiles/lacs/lacs.shp')
cantons_ch <- st_read('./shapefiles/cantons/swiss_cantons.shp')

#uniformisation du système de projection
st_crs(districts_ch)
st_crs(lacs_ch)
lacs_ch <- st_transform(lacs_ch, crs = 4326)
districts_ch<- st_transform(districts_ch, crs = 4326)
cantons_ch<- st_transform(cantons_ch, crs = 4326)

#shapefile avec les cantons-districts
cantons_dist <- subset(cantons_ch, NAME %in% c('Appenzell Innerrhoden',
                      'Basel-Stadt','Genève', 'Glarus', 'Neuchâtel','Nidwalden',
                      'Obwalden','Uri', 'Zug'))

#Agrège les cantons en un seul polygon
cantons_dist <- cantons_dist %>%
  group_by(NAME)%>%
  summarise()

#Ajoute le n° de districts aux cantons-districts
cantons_dist$BEZIRKSNUM <- c(1600,1200,2500,800,2400,700,600,400,900)

#Garde seuleument le nom et numéro de district des shapefiles
districts_ch <- subset(districts_ch, select=c(BEZIRKSNUM,NAME,geometry))

#Agrège les districts en un seul polygon
districts_ch <- districts_ch %>%
  group_by(BEZIRKSNUM,NAME)%>%
  summarize()

#Ajoute les cantons-ditricts aux districts
districts_ch<- rbind(districts_ch, cantons_dist)

#Ajoute les données aux districts
df <- rename(df, BEZIRKSNUM=district_num)
districts_ch <- merge(districts_ch, df, by='BEZIRKSNUM')
row.names(districts_ch)<- districts_ch$NAME

##### Visualisations des données #####

#carte des distrcits en fonction des infractions
ggplot() +
  geom_sf(data=districts_ch, aes(fill=infra_cp_1000), size=0, alpha=1) +
  geom_sf(data=lacs_ch, fill='#c2c2c2', size=0, alpha=1) +
  theme_void() +
  scale_fill_viridis(
    option="rocket",
    direction = -1,
    breaks = c(20,50,80, 110, 140),
    name='infractions/1000 hab.',
    guide = guide_legend( keyheight = unit(4, units = "mm"), 
                          keywidth=unit(10, units = "mm"), 
                          label.position = "right", title.position = 'top', 
                          nrow=5, reverse=TRUE) ) +
  labs(
    title = "Infraction Pénale pour 1000 habitants",
    subtitle = "Par districts en 2021",
    caption = "Données : CH, OFS, 2021 | Auteur : Paul Zignani"
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size=16, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size=14, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text(size=9, color = "#4e4d47", margin = margin(b = 0.1, unit = "cm") ),
    
    legend.position = c(0.9, 0.8)
  )+
  scalebar(districts_ch, dist = 30, dist_unit = "km", st.size=3,
           transform = TRUE, model = "WGS84", border.size = 0.5, height = 0.01, 
           box.fill = c("#4e4d47", "#f5f5f2"), st.color = "#4e4d47", 
           box.color = "#4e4d47")

#label des districts
labels<- paste0(districts_ch$NAME,": ",round(districts_ch$infra_cp_1000), collapse = NULL)
#palette de couleurs
pal <- colorNumeric("magma", reverse=TRUE, NULL)

#carte "intéractive"
map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView( lat=46.63, lng=8.22 , zoom=7.5) %>%
  addPolygons(
    data = districts_ch,
    fillColor = ~pal(infra_cp_1000),
    weight = 1,
    opacity = 1,
    color = "white",
    fillOpacity =1,
    highlight = highlightOptions(
      weight = 1,
      color = "#f2f2f2",
      fillOpacity = 0.7,
      bringToFront = F),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) %>%
  addLegend(pal = pal, values = districts_ch$infra_cp_1000, opacity = 1, title = NULL,
            position = "bottomright")%>%
  addPolygons(
    data=lacs_ch,
    fillColor = ' #468499',
    weight = 1,
    opacity = 1,
    color='white',
    fillOpacity = 1,
  )
map

##### Autocorrélation spatiale #####

#liste des adjacence de chaque district
# w <- poly2nb(districts_ch, row.names=districts_ch$NAME)
# summary(w)

#liste des poid pour être utilisé avec spdep (matrice de transition)
#le style="W" divise chaque ligne par son total
Wlistw<- nb2listw(poly2nb(districts_ch, row.names=districts_ch$NAME), style='W')

#I de Moran
IM<- moran(infra_cp_1000, Wlistw, n=length(Wlistw$neighbours), S0=Szero(Wlistw))

#### adjeacences affichées sur une carte

#palette de couleurs
my_colors <- brewer.pal(12, "Paired")
my_colors <- colorRampPalette(my_colors)(143)

#attribue une couleur pour chaque districts
class_of_dist <- cut(as.numeric(districts_ch$BEZIRKSNUM), 143)
my_colors <- my_colors[as.numeric(class_of_dist)]
#mélange la liste pour éviter d'avoir des couleurs similaires côte à côte
set.seed(7)
my_colors <- sample(my_colors)


#point sur chaque district
dist_point <- st_point_on_surface(districts_ch$geometry)

#Affiche la carte
plot(districts_ch$geometry, col=my_colors, bg='#f2f2f2', border='#000000', lwd=1.8)
plot(lacs_ch$geometry, col="#F0FFFF", bg="#f2f2f2", lwd=1.8, border='#000000', add=T)
plot(Wlistw,dist_point, col='#ffffff', lwd=1.5, add=T)

#sortie
infra_cp_1000 <- districts_ch$infra_cp_1000

# test unilatéral du Moran 
moran_test_normal=moran.test(infra_cp_1000, listw=Wlistw, randomisation=FALSE,
                             alternative="greater")
moran_test_normal

# test unilatéral du Moran pour nsim permutations
moran_test_permutation=moran.mc(infra_cp_1000, listw=Wlistw, nsim=999,
                                alternative="greater")
moran_test_permutation

hist(moran_test_permutation$res,breaks=100,xlab="valeurs permutées",ylab="effectif",main="Histogramme des valeurs permutées du I de Moran (B=999)")
abline(v=moran_test_permutation$statistic)

#Moran Scatterplot
moran.plot(infra_cp_1000, listw=Wlistw, labels="NULL",return_df=TRUE)

# #valeurs "LISA : local indicators of spatial autocorrelation" Ii,
# #avec moyennes et variances attendues sous H0, valeurs standardisées et valeurs p
# moranLocal=localmoran(infra_cp_1000, listw=Wlistw)
# 
# Ii=moranLocal[,1]
# mean(Ii) # doit coincider avec le I de Moran
