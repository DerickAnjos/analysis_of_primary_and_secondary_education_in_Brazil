# Loading the Dataset-----------------------------------------------------------

school_perform <- as_tibble(read.csv2('TX RENDIMENTO MUNICIPIOS 2010_2.csv',
                                     dec = '.',sep=';', stringsAsFactors = F))

# Preparing each DF -----------------------------------------------------------

# Variable names from original dataset
name_variables <- c('year','region', 'state', 'cod_city', 'name_city',
  'localization', 'sector', 'ap_1y', 'ap_2y', 'ap_3y', 'ap_4y', 'ap_5y', 
  'ap_6y', 'ap_7y', 'ap_8y', 'ap_9y', 'ap_1_to_5', 'ap_6_to_9', 
  'ap_element_middle_school', 'ap_1y_high', 'ap_2y_high', 'ap_3y_high',
  'ap_4y_high', 'ap_high_notseries', 'ap_high', 'rp_1y', 'rp_2y', 'rp_3y', 
  'rp_4y', 'rp_5y', 'rp_6y','rp_7y', 'rp_8y', 'rp_9y', 'rp_1_to_5', 'rp_6_to_9',
  'rp_element_middle_school', 'rp_1y_high', 'rp_2y_high', 'rp_3y_high', 
  'rp_4y_high', 'rp_high_notseries', 'rp_high', 'ev_1y', 'ev_2y', 'ev_3y',
  'ev_4y', 'ev_5y', 'ev_6y', 'ev_7y', 'ev_8y', 'ev_9y', 'ev_1_to_5',
  'ev_6_to_9', 'ev_element_middle_school', 'ev_1y_high', 'ev_2y_high',
  'ev_3y_high', 'ev_4y_high', 'ev_high_notseries', 'ev_high')

names(school_perform) <- name_variables
school_perform[school_perform == '--'] <- NA # Missing values

# Changing class of variables
school_perform %>% 
  mutate_each_(list(as.numeric), 
               vars = c(8:ncol(school_perform))) -> school_perform

# Selecting observations for analysis

# Cities
school_perform_cit <- school_perform[school_perform$localization == "Total" & 
                                       school_perform$sector == "Total",]

# Urban x rural
school_perform_local <- school_perform[(school_perform$localization == 'Rural' |
                                    school_perform$localization == 'Urbana') &
                                    school_perform$sector == 'Total',]

# Private x Public
school_perform_sector <- school_perform[(school_perform$localization == 'Total')
                                        & (school_perform$sector == 'Publico' | 
                                        school_perform$sector == 'Particular'),]

# Dropping observations --------------------------------------------------------
# removing all observations with more than k/2 missing values (k = number of
# variables) and separating each DF

# Cities - approved

approv_elemen_cit <- school_perform_cit[rowSums(!is.na(school_perform_cit[,8:16]
                  )) >= ceiling(ncol(school_perform_cit[8:16])/2), c(1:7,8:16)]

approv_high_cit <- school_perform_cit[rowSums(!is.na(school_perform_cit[,20:23])
                )>= ceiling(ncol(school_perform_cit[20:23])/2), c(1:7,20:23)]

approv_cit <- school_perform_cit[rowSums(!is.na(school_perform_cit[,c(8:16,
           20:23)])) >= ceiling(ncol(school_perform_cit[c(8:16,20:23)])/2),
           c(1:7,c(8:16,20:23))]

# Concentrating datasets
approv_cit_list <- list('elemen' = approv_elemen_cit, 'high' = 
                         approv_high_cit, 'all' = approv_cit)


# Cities - evasion

evasi_elemen_cit <- school_perform_cit[rowSums(!is.na(school_perform_cit[,44:52]
                 )) >= ceiling(ncol(school_perform_cit[44:52])/2), c(1:7,44:52)]

evasi_high_cit <- school_perform_cit[rowSums(!is.na(school_perform_cit[,56:59]))
               >= ceiling(ncol(school_perform_cit[56:59])/2), c(1:7,56:59)]

evasi_cit <- school_perform_cit[rowSums(!is.na(school_perform_cit[,c(44:52,
          56:59)])) >= ceiling(ncol(school_perform_cit[c(44:52,56:59)])/2)
          ,c(1:7,c(44:52,56:59))]

# Concentrating datasets
evasi_cit_list <- list('elemen' = evasi_elemen_cit, 'high' = 
                         evasi_high_cit, 'all' = evasi_cit)


# Localization - approved

approv_elemen_local <- school_perform_local[rowSums(!is.na(school_perform_local
                    [,8:16])) >= ceiling(ncol(school_perform_local[8:16])/2)
                    ,c(1:7,8:16)]

approv_high_local <- school_perform_local[rowSums(!is.na(school_perform_local
                  [,20:23])) >= ceiling(ncol(school_perform_local[20:23])/2)
                  ,c(1:7,20:23)]

approv_local <- school_perform_local[rowSums(!is.na(school_perform_local
             [,c(8:16,20:23)])) >= ceiling(ncol(school_perform_local[c(8:16,
             20:23)])/2), c(1:7,c(8:16,20:23))]

# Concentrating datasets
approv_local_list <- list('elemen' = approv_elemen_local, 'high'
                         = approv_high_local, 'all' = approv_local)


# Localization - evasion

evasi_elemen_local <- school_perform_local[rowSums(!is.na(school_perform_local
                   [,44:52])) >= ceiling(ncol(school_perform_local[44:52])/2)
                   ,c(1:7,44:52)]

evasi_high_local <- school_perform_local[rowSums(!is.na(school_perform_local
                 [,56:59]))>= ceiling(ncol(school_perform_local[56:59])/2)
                 ,c(1:7,56:59)]

evasi_local <- school_perform_local[rowSums(!is.na(school_perform_local
            [,c(44:52,56:59)])) >= ceiling(ncol(school_perform_local[c(44:52,
            56:59)])/2), c(1:7,c(44:52,56:59))]

# Concentrating datasets
evasi_local_list <- list('elemen' = evasi_elemen_local, 'high'
                         = evasi_high_local, 'all' = evasi_local)


# Sector - approved

approv_elemen_sector <- school_perform_sector[rowSums(!is.na(
                     school_perform_sector[,8:16])) >= ceiling(ncol(
                     school_perform_sector[8:16])/2), c(1:7,8:16)]

approv_high_sector <- school_perform_sector[rowSums(!is.na(school_perform_sector
                   [,20:23])) >= ceiling(ncol(school_perform_sector[20:23])/2)
                   ,c(1:7,20:23)]

approv_sector <- school_perform_sector[rowSums(!is.na(school_perform_sector[,
              c(8:16,20:23)])) >= ceiling(ncol(school_perform_sector[c(8:16,
              20:23)])/2), c(1:7,c(8:16,20:23))]

# Concentrating datasets
approv_sector_list <- list('elemen' = approv_elemen_sector, 'high'
                         = approv_high_sector, 'all' = approv_sector)


# Sector - evasion

evasi_elemen_sector <- school_perform_sector[rowSums(!is.na(
                    school_perform_sector[,44:52])) >= ceiling(ncol(
                    school_perform_sector[44:52])/2), c(1:7,44:52)]

evasi_high_sector <- school_perform_sector[rowSums(!is.na(school_perform_sector
                  [,56:59])) >= ceiling(ncol(school_perform_sector[56:59])/2)
                  ,c(1:7,56:59)]

evasi_sector <- school_perform_sector[rowSums(!is.na(school_perform_sector[,
             c(44:52,56:59)])) >= ceiling(ncol(school_perform_sector[c(44:52,
             56:59)])/2), c(1:7,c(44:52,56:59))]

# Concentrating datasets
evasi_sector_list <- list('elemen' = evasi_elemen_sector, 'high'
                         = evasi_high_sector, 'all' = evasi_sector)

# Making a object prototype, with all necessary cases
level_school <- list('elemen' = NA, 'high' = NA, 'all' = NA)
object_prototype <- list('approv' = list('cit' = level_school, 
                                         'local' = level_school, 
                                         'sector' = level_school), 
                         'evasi' = list('cit' = level_school,
                                        'local' = level_school, 
                                        'sector' = level_school))

df_list <- object_prototype

# Making a list with all datasets
df_list$approv$cit <- approv_cit_list
df_list$approv$local <- approv_local_list
df_list$approv$sector <- approv_sector_list
df_list$evasi$cit <- evasi_cit_list
df_list$evasi$local <- evasi_local_list
df_list$evasi$sector <- evasi_sector_list

# Removing unused objects
rm(approv_cit, approv_elemen_cit, approv_elemen_local, approv_elemen_sector,
   approv_high_cit, approv_high_local, approv_high_sector, approv_local, 
   approv_sector, evasi_cit, evasi_elemen_cit, evasi_elemen_local, 
   evasi_elemen_sector, evasi_high_cit, evasi_high_local, evasi_high_sector,
   evasi_local, evasi_sector, approv_cit_list, approv_local_list,
   approv_sector_list, evasi_cit_list, evasi_local_list, evasi_sector_list,
   school_perform_cit, school_perform_local, school_perform_sector, 
   level_school)


# Dropping variables -----------------------------------------------------------
# removing all variables with more than n/2 missing values (n = number of
# observations) 
                                                 
# Making a list for metadata
df_list_meta <- object_prototype
  
# Dropping variables 

for(k in 1:length(df_list)){
  
  for(j in 1:length(df_list$approv)){
  
    for(i in 1:length(df_list$approv$cit)){
  
      df_list[[k]][[j]][[i]] <- df_list[[k]][[j]][[i]][,colSums(!is.na(
                             df_list[[k]][[j]][[i]])) >= ceiling(nrow(
                             df_list[[k]][[j]][[i]])/2)]
      
      df_list_meta[[k]][[j]][[i]] <- df_list[[k]][[j]][[i]][,c(1:7)]
      df_list[[k]][[j]][[i]] <- df_list[[k]][[j]][[i]][,-c(1:7)]
    
    }
  }
}


# Treating missing values -----------------------------------------------------

# Imputing missing values with the PCA - multiple imputation

for(k in 1:length(df_list)){
  
  for(j in 1:length(df_list$approv)){
    
    for(i in 1:length(df_list$approv$cit)){

      nb <- estim_ncpPCA(df_list[[k]][[j]][[i]], scale = TRUE)
      df_list[[k]][[j]][[i]] <- imputePCA(df_list[[k]][[j]][[i]],ncp = nb$ncp,
                                scale = TRUE)[[1]]
      
      # Adjusting the imputing missing value to real case
      df_list[[k]][[j]][[i]][df_list[[k]][[j]][[i]]>100] <- 100
      df_list[[k]][[j]][[i]][df_list[[k]][[j]][[i]]<0] <- 0
      
    }
  }
}

# Removing unused objects
rm(nb)


# Analyzing data --------------------------------------------------------------

# Chart correlation (example)
chart.Correlation(df_list$evasi$cit$all, histogram = T)

# Calculating correlation matrix
rho <- object_prototype

for(k in 1:length(df_list)){
  
  for(j in 1:length(df_list$approv)){
      
    rho[[k]][[j]] <- lapply(df_list[[k]][[j]], FUN = cor)
      
  }
}

# Heatmap - correlation (example)
rho$approv$cit$all %>% 
  melt() %>% 
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  geom_text(aes(x = Var1, y = Var2, label = round(x = value, digits = 3)),
            size = 4) +
  labs(x = NULL,
       y = NULL,
       fill = "Correlações") +
  scale_fill_gradient2(low = "dodgerblue4", 
                       mid = "white", 
                       high = "brown4",
                       midpoint = 0) +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0))


# Bartlett test of sphericity - homogeneity of variances ----------------------

bartlett <- object_prototype

# Creating a logical object to verify the condition: 
# p-value < significance (0.05)

bartlett_results <- c(rep(FALSE,length(df_list)*length(df_list$approv)))
significance <- 0.05

for(k in 1:length(df_list)){
  
  for(j in 1:length(df_list$approv)){
    
    bartlett[[k]][[j]] <- lapply(rho[[k]][[j]], FUN = cortest.bartlett, 
                            n = min(nrow(df_list[[k]][[i]]$elemen),
                                    nrow(df_list[[k]][[i]]$high),
                                    nrow(df_list[[k]][[i]]$all)))
    
    bartlett_results[(k-1)*3 + j] <- all(lapply(bartlett[[k]][[j]], 
                                     FUN = function(x) x[[2]] < significance))
    
  }
}

# Test results
all(bartlett_results) # Returns TRUE

# Running the PCA - prcomp() from psych ---------------------------------------

pca_list <- object_prototype

for(k in 1:length(df_list)){
  
  for(j in 1:length(df_list$approv)){
    
    pca_list[[k]][[j]] <- lapply(df_list[[k]][[j]], FUN = prcomp, scale. = TRUE)
    
  }
  
}

# Reporting results -----------------------------------------------------------

report <- object_prototype

for(k in 1:length(df_list)){
  
  for(j in 1:length(df_list$approv)){
    
    for(i in 1:length(df_list$approv$cit)){
      
      report[[k]][[j]][[i]] <- data.frame(
        eigenvalue = pca_list[[k]][[j]][[i]]$sdev^2,
        var_shared = summary(pca_list[[k]][[j]][[i]])$importance[2,],
        var_cumulative = summary(pca_list[[k]][[j]][[i]])$importance[3,])
      
    }
  }
}

# Variables weights - plot example
ggplotly(
  data.frame(pca_list$approv$cit$all$rotation) %>% 
    mutate(var = names(data.frame(df_list$approv$cit$all))) %>% 
    melt(id.vars = 'var') %>% 
    mutate(var = factor(var)) %>% 
    ggplot(aes(x = var, y = value, fill = var))+
    geom_bar(stat = 'identity', color = 'black')+
    facet_wrap(~variable)+
    labs(x=NULL, y=NULL, fill = 'Legend:',)+
    scale_fill_viridis_d()+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90))
)


# Choosing the number of Principal Components ---------------------------------

# Kaiser rule - eigenvalues > 1

# Scree Plot
ggplotly(
  fviz_eig(X = pca_list$approv$cit$all,
           ggtheme = theme_bw(), 
           barcolor = "black", 
           barfill = "dodgerblue4",
           linecolor = "darkgoldenrod4")
)


# Factor loading --------------------------------------------------------------

factor_loading <- object_prototype

for(k in 1:length(df_list)){
  
  for(j in 1:length(df_list$approv)){
    
    for(i in 1:length(df_list$approv$cit)){
      
      factor_loading[[k]][[j]][[i]] <- pca_list[[k]][[j]][[i]]$rotation %*%
        diag(pca_list[[k]][[j]][[i]]$sdev[])
      
    }
  }
}

# Report plot - example
data.frame(factor_loading$approv$cit$all) %>%
  rename(F1 = X1, F2 = X2, F3 = X3, F4 = X4, F5 = X5, F6 = X6, F7 = X7,
         F8 = X8, F9 = X9, F10 = X10, F11 = X11, F12 = X12) %>%
  mutate(Comunalidades = rowSums(factor_loading$approv$cit$all^2)) %>% 
  kable() %>%
  kable_styling(bootstrap_options = 'striped',
                full_width = T,
                font_size = 12)

# Ploting factor loadings - example
data.frame(factor_loading$approv$cit$all) %>% 
  rename(F1 = X1, F2 = X2) %>% 
  ggplot(aes(x = F1, y = F2))+
  geom_point(color = 'dodgerblue4') +
  geom_hline(yintercept = 0, color = 'darkorchid')+
  geom_vline(xintercept = 0,color = 'darkorchid')+
  geom_text_repel(label = row.names(factor_loading$approv$cit$all))+
  labs(x='F1', y= 'F2')+
  theme_bw()

# Factor scores ---------------------------------------------------------------

factor_scores <- object_prototype

for(k in 1:length(df_list)){
  
  for(j in 1:length(df_list$approv)){
    
    for(i in 1:length(df_list$approv$cit)){
      
      factor_scores[[k]][[j]][[i]] <- t(pca_list[[k]][[j]][[i]]$rotation)/
        pca_list[[k]][[j]][[i]]$sdev
      
    }
  }
}


# Including PCs on the DF -----------------------------------------------------

# Separating just the PCs used for this analysis

approv_cit_fact = approv_local_fact = approv_sector_fact = evasi_cit_fact =
  evasi_local_fact = evasi_sector_fact <- 
  list('elemen' = list(F1 = NA, F2 = NA, F3 = NA, F4 = NA, F5 = NA,
                                 F6 = NA, F7 = NA, F8 = NA, F9 = NA),
       'high' = list(F1 = NA, F2 = NA, F3 = NA),
       'all' = list(F1 = NA, F2 = NA, F3 = NA, F4 = NA, F5 = NA,
                          F6 = NA, F7 = NA, F8 = NA, F9 = NA, F10 = NA,
                          F11 = NA, F12 = NA))

const <- c(1:6)

if(i == 1 & all(factor_scores$fs_approv_cit[[j]][i,]<0)){
  const <- -1
} else{
  const <- 1
}
factor_scores$approv$cit$elemen

for(j in 1:length(approv_cit_fact)) {
  
  for(i in 1:length(approv_cit_fact[[j]])) {
    
  # Approved
  approv_cit_fact[[j]][[i]] <- (rowSums(t(apply(scale(approv_cit_list[[j]]),
                                MARGIN = 1, function(x) 
                                x*t(factor_scores$fs_approv_cit[[j]][i,])))))
  
  approv_local_fact[[j]][[i]] <- rowSums(t(apply(scale(approv_local_list[[j]]),
                                  MARGIN = 1, function(x) 
                                  x*t(factor_scores$fs_approv_local[[j]][i,]))))
  
  approv_sector_fact[[j]][[i]] <- rowSums(t(apply(scale(approv_sector_list[[j]]),
                                  MARGIN = 1, function(x) 
                                  x*t(factor_scores$fs_approv_sector[[j]][i,]))))
  
  # Evasion
  evasi_cit_fact[[j]][[i]] <- rowSums(t(apply(scale(evasi_cit_list[[j]]),
                                  MARGIN = 1, function(x) 
                                  x*t(factor_scores$fs_evasi_cit[[j]][i,]))))
  
  evasi_local_fact[[j]][[i]] <- rowSums(t(apply(scale(evasi_local_list[[j]]),
                                  MARGIN = 1, function(x) 
                                  x*t(factor_scores$fs_evasi_local[[j]][i,]))))
  
  
  evasi_sector_fact[[j]][[i]] <- rowSums(t(apply(scale(evasi_sector_list[[j]]),
                                  MARGIN = 1, function(x) 
                                  x*t(factor_scores$fs_evasi_sector[[j]][i,]))))
  
  }
  
}

# Building a ranking ----------------------------------------------------------

# Multiplying F1 for -1 (big numbers are associated with the minus signal)
F1 <- data.frame(F1) %>% 
  mutate(factor1 = rowSums(.)*-1)

F2 <- data.frame(F2) %>% 
  mutate(factor2 = rowSums(.)*1)

aprovacao_fundamental['factor1'] <- F1$factor1
aprovacao_fundamental['factor2'] <- F2$factor2

# Sum of weighted PCs 
aprovacao_fundamental %>% 
  mutate(pontuation = factor1 * report$var_shared[1] + 
           factor2 * report$var_shared[2]) -> aprovacao_fundamental

aprovacao_fundamental %>% 
  arrange(desc(pontuation)) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = 'striped',
              full_width = T, font_size = 12)

install.packages('rgdal')
library('rgdal')

??'readOGR'
??'readOGR'
mapa <- readOGR(dsn = '.', layer = 'T_LM_MUNICIPIOS_2010Polygon')
#Carregando o mapa
load(file = "mapa_sp.RData")

tm_shape(mapa) +
  tm_borders()
plot(mapa)
