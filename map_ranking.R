library('rgdal')


shp <- readOGR("brazil_cities\\.", "BR_Municipios_2020", stringsAsFactors=FALSE, 
               encoding="UTF-8")

class(shp)

best_schools_region <- df_list$approv$cit$all[c('cod_city', 'ranking')]
map_best_region <- merge(shp, best_schools_region, by.x = 'CD_MUN',
                         by.y = 'cod_city')

proj4string(map_best_region) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

install.packages('RColorBrewer')
library('RColorBrewer')

pal <- colorBin("Blues",domain = NULL,n=5) #cores do mapa

state_popup <- paste0("<strong>Cidade: </strong>", 
                      map_best_region$NM_MUN, 
                      "<br><strong>Pontuação: </strong>", 
                      map_best_region$ranking)
leaflet(data = map_best_region) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(map_best_region$ranking), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = state_popup) %>%
  addLegend("bottomright", pal = pal, values = ~map_best_region$ranking,
            title = "Pontuação no ranking",
            opacity = 1)



#Carregando o mapa
load(file = "T_LM_MUNICIPIOS_2010Polygon.shp")

#Visualizando o mapa
tm_shape(mapa_sp) + 
  tm_borders()

#Acrescentando informações ao mapa
mapa_sp@data$COD_DIST <- as.numeric(mapa_sp@data$COD_DIST)

distritos_dados <- merge(mapa_sp,
                         atlasambiental,
                         by.x = "COD_DIST",
                         by.y = "cod_ibge")

#Plotando os rankings
tmap_mode("view") #modo interativo - para acionar o modo offline, basta 
#argumentar "plot"

tm_shape(distritos_dados) +
  tm_fill("pontuacao", midpoint = 0, palette = "RdBu", 
          style = "quantile", n = 10, legend.show = T) +
  tm_borders(alpha = 0.8) +
  tm_text("distritos") 
