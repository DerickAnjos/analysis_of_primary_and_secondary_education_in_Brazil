# Installing and loading packages -------------------------------------------- 

pacotes <- c('tidyverse', 'knitr', 'plotly', 'kableExtra', 'ggrepel', 'reshape2',
             'sjPlot', 'FactoMineR', 'cabootcrs', 'gifski', 'gganimate', 'car',
             'rgl', 'gridExtra', 'PerformanceAnalytics','rayshader','psych',
             'pracma', 'polynom', 'rqPen','sp', 'tmap', 'magick', 'readr', 
             'missMDA', 'factoextra')

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  
  install.packages(instalador, dependencies = T)
  
  sapply(pacotes, FUN = require, character = T)
} else {
  sapply(pacotes, FUN = require, character = T)
}

