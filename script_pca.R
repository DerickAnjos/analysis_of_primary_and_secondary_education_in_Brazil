# Loading DB ------------------------------------------------------------------

school_perform <- as_tibble(read.csv2('TX RENDIMENTO MUNICIPIOS 2010_2.csv',
                                     dec = '.',sep=';', stringsAsFactors = F))

# Preparing each DF -----------------------------------------------------------

name_variables <- c('year','region', 'state', 'cod_city', 'name_city',
  'localization', 'sector', 'ap_1y', 'ap_2y', 'ap_3y', 'ap_4y', 'ap_5y', 'ap_6y',
  'ap_7y', 'ap_8y', 'ap_9y', 'ap_1_to_5', 'ap_6_to_9', 'ap_element_middle_school', 'ap_1y_high',
  'ap_2y_high', 'ap_3y_high', 'ap_4y_high', 'ap_high_notseries', 'ap_high',
  'rp_1y', 'rp_2y', 'rp_3y', 'rp_4y', 'rp_5y', 'rp_6y','rp_7y', 'rp_8y', 'rp_9y',
  'rp_1_to_5', 'rp_6_to_9', 'rp_element_middle_school', 'rp_1y_high', 'rp_2y_high', 
  'rp_3y_high', 'rp_4y_high', 'rp_high_notseries', 'rp_high', 'ev_1y', 'ev_2y', 
  'ev_3y', 'ev_4y', 'ev_5y', 'ev_6y', 'ev_7y', 'ev_8y', 'ev_9y', 'ev_1_to_5',
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

# Separating each DF ----------------------------------------------------------
# and removing all observations with more than k/2 missing values (k = number of
# variables) and variables with more than n/2 missing values (n = number of
# observations) 

# DF of qualitative informations
info_school_perform <- school_perform[,1:7]

# Dropping observations --------------------------------------------------------

# Cities - approved

approv_elemen_cit <- school_perform_cit[rowSums(!is.na(school_perform_cit[,8:16]))
                  >= ceiling(ncol(school_perform_cit[8:16])/2), c(1:7,8:16)]

approv_high_cit <- school_perform_cit[rowSums(!is.na(school_perform_cit[,20:23])
                   )>= ceiling(ncol(school_perform_cit[20:23])/2), c(1:7,20:23)]

approv_cit <- school_perform_cit[rowSums(!is.na(school_perform_cit[,c(8:16,20:23)
                   ]))>= ceiling(ncol(school_perform_cit[c(8:16,20:23)])/2)
                   ,c(1:7,c(8:16,20:23))]

# Concentrating all DFs in one List
approv_cit_list <- list('elemen' = approv_elemen_cit, 'high' = 
                         approv_high_cit, 'all' = approv_cit)


# Cities - evasion

evasi_elemen_cit <- school_perform_cit[rowSums(!is.na(school_perform_cit[,44:52]))
                  >= ceiling(ncol(school_perform_cit[44:52])/2), c(1:7,44:52)]

evasi_high_cit <- school_perform_cit[rowSums(!is.na(school_perform_cit[,56:59]))>=
                                      ceiling(ncol(school_perform_cit[56:59])/2)
                                    ,c(1:7,56:59)]

evasi_cit <- school_perform_cit[rowSums(!is.na(school_perform_cit[,c(44:52,56:59)]))>=
                                 ceiling(ncol(school_perform_cit[c(44:52,56:59)])/2)
                               ,c(1:7,c(44:52,56:59))]

# Concentrating all DFs in one List
evasi_cit_list <- list('elemen' = evasi_elemen_cit, 'high' = 
                         evasi_high_cit, 'all' = evasi_cit)


# Localization - approved

approv_elemen_local <- school_perform_local[rowSums(!is.na(school_perform_local[,8:16]))>=
                                      ceiling(ncol(school_perform_local[8:16])/2)
                                    ,c(1:7,8:16)]

approv_high_local <- school_perform_local[rowSums(!is.na(school_perform_local[,20:23]))>=
                                       ceiling(ncol(school_perform_local[20:23])/2)
                                     ,c(1:7,20:23)]

approv_local <- school_perform_local[rowSums(!is.na(school_perform_local[,c(8:16,20:23)]))>=
                                   ceiling(ncol(school_perform_local[c(8:16,20:23)])/2)
                                 ,c(1:7,c(8:16,20:23))]

# Concentrating all DFs in one List
approv_local_list <- list('elemen' = approv_elemen_local, 'high'
                         = approv_high_local, 'all' = approv_local)


# Localization - evasion

evasi_elemen_local <- school_perform_local[rowSums(!is.na(school_perform_local[,44:52]))>=
                                        ceiling(ncol(school_perform_local[44:52])/2)
                                      ,c(1:7,44:52)]

evasi_high_local <- school_perform_local[rowSums(!is.na(school_perform_local[,56:59]))>=
                                         ceiling(ncol(school_perform_local[56:59])/2)
                                       ,c(1:7,56:59)]

evasi_local <- school_perform_local[rowSums(!is.na(school_perform_local[,c(44:52,56:59)]))>=
                                   ceiling(ncol(school_perform_local[c(44:52,56:59)])/2)
                                 ,c(1:7,c(44:52,56:59))]

# Concentrating all DFs in one List
evasi_local_list <- list('elemen' = evasi_elemen_local, 'high'
                         = evasi_high_local, 'all' = evasi_local)


# Sector - approved

approv_elemen_sector <- school_perform_sector[rowSums(!is.na(school_perform_sector[,8:16]))>=
                                          ceiling(ncol(school_perform_sector[8:16])/2)
                                        ,c(1:7,8:16)]

approv_high_sector <- school_perform_sector[rowSums(!is.na(school_perform_sector[,20:23]))>=
                                           ceiling(ncol(school_perform_sector[20:23])/2)
                                         ,c(1:7,20:23)]

approv_sector <- school_perform_sector[rowSums(!is.na(school_perform_sector[,c(8:16,20:23)]))>=
                                     ceiling(ncol(school_perform_sector[c(8:16,20:23)])/2)
                                   ,c(1:7,c(8:16,20:23))]

# Concentrating all DFs in one List
approv_sector_list <- list('elemen' = approv_elemen_sector, 'high'
                         = approv_high_sector, 'all' = approv_sector)


# Sector - evasion

evasi_elemen_sector <- school_perform_sector[rowSums(!is.na(school_perform_sector[,44:52]))>=
                                          ceiling(ncol(school_perform_sector[44:52])/2)
                                        ,c(1:7,44:52)]

evasi_high_sector <- school_perform_sector[rowSums(!is.na(school_perform_sector[,56:59]))>=
                                           ceiling(ncol(school_perform_sector[56:59])/2)
                                         ,c(1:7,56:59)]

evasi_sector <- school_perform_sector[rowSums(!is.na(school_perform_sector[,c(44:52,56:59)]))>=
                                     ceiling(ncol(school_perform_sector[c(44:52,56:59)])/2)
                                   ,c(1:7,c(44:52,56:59))]

# Concentrating all DFs in one List
evasi_sector_list <- list('elemen' = evasi_elemen_sector, 'high'
                         = evasi_high_sector, 'all' = evasi_sector)

# Removing old objects
rm(approv_cit, approv_elemen_cit, approv_elemen_local, approv_elemen_sector,
   approv_high_cit, approv_high_local, approv_high_sector, approv_local, 
   approv_sector, evasi_cit, evasi_elemen_cit, evasi_elemen_local, 
   evasi_elemen_sector, evasi_high_cit, evasi_high_local, evasi_high_sector,
   evasi_local, evasi_sector)


# Dropping variables -----------------------------------------------------------

# Cities

for(i in 1:length(approv_cit_list)){
  
  approv_cit_list[[i]] <- approv_cit_list[[i]][,colSums(!is.na(approv_cit_list[[i]]
                                      ))>= ceiling(nrow(approv_cit_list[[i]])/2)]

  approv_local_list[[i]] <- approv_local_list[[i]][,colSums(!is.na(approv_local_list[[i]]
                                      ))>= ceiling(nrow(approv_local_list[[i]])/2)]
  
  approv_sector_list[[i]] <- approv_sector_list[[i]][,colSums(!is.na(approv_sector_list[[i]]
                                      ))>= ceiling(nrow(approv_sector_list[[i]])/2)]
  
  evasi_cit_list[[i]] <- evasi_cit_list[[i]][,colSums(!is.na(evasi_cit_list[[i]]
                                      ))>= ceiling(nrow(evasi_cit_list[[i]])/2)]
  
  evasi_local_list[[i]] <- evasi_local_list[[i]][,colSums(!is.na(evasi_local_list[[i]]
                                      ))>= ceiling(nrow(evasi_local_list[[i]])/2)]
  
  evasi_sector_list[[i]] <- evasi_sector_list[[i]][,colSums(!is.na(evasi_sector_list[[i]]
                                      ))>= ceiling(nrow(evasi_sector_list[[i]])/2)]
  
}


# Treating missing values -----------------------------------------------------

# Imputing missing values with the PCA - multiple imputation
for(i in 1:length(approv_cit_list)){

# Approved
nb <- estim_ncpPCA(approv_cit_list[[i]][-c(1:7)], scale = T)
approv_cit_list[[i]] <- imputePCA(approv_cit_list[[i]][-c(1:7)],ncp = nb$ncp,
                              scale = TRUE)[[1]]

nb <- estim_ncpPCA(approv_local_list[[i]][-c(1:7)], scale = T)
approv_local_list[[i]] <- imputePCA(approv_local_list[[i]][-c(1:7)],ncp = nb$ncp,
                                  scale = TRUE)[[1]]

nb <- estim_ncpPCA(approv_sector_list[[i]][-c(1:7)], scale = T)
approv_sector_list[[i]] <- imputePCA(approv_sector_list[[i]][-c(1:7)],ncp = nb$ncp,
                                  scale = TRUE)[[1]]

# Evasion
nb <- estim_ncpPCA(evasi_cit_list[[i]][-c(1:7)], scale = T)
evasi_cit_list[[i]] <- imputePCA(evasi_cit_list[[i]][-c(1:7)],ncp = nb$ncp,
                                  scale = TRUE)[[1]]

nb <- estim_ncpPCA(evasi_local_list[[i]][-c(1:7)], scale = T)
evasi_local_list[[i]] <- imputePCA(evasi_local_list[[i]][-c(1:7)],ncp = nb$ncp,
                                    scale = TRUE)[[1]]

nb <- estim_ncpPCA(evasi_sector_list[[i]][-c(1:7)], scale = T)
evasi_sector_list[[i]] <- imputePCA(evasi_sector_list[[i]][-c(1:7)],ncp = nb$ncp,
                                     scale = TRUE)[[1]]

}

# Analyzing data --------------------------------------------------------------

# Chart correlation (example)
chart.Correlation(evasi_cit_list$evasi_all, histogram = T)

# Calculating correlation matrix

# Approved
rho_approv_cit <- lapply(approv_cit_list, FUN = cor)
rho_approv_local <- lapply(approv_local_list, FUN = cor)
rho_approv_sector <- lapply(approv_sector_list, FUN = cor)

# Evasion
rho_evasi_cit <- lapply(evasi_cit_list, FUN = cor)
rho_evasi_local <- lapply(evasi_local_list, FUN = cor)
rho_evasi_sector <- lapply(evasi_sector_list, FUN = cor)

# Heatmap - correlation (example)
rho_approv_cit$approv_cit %>% 
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

# Approved
approv_cit_bartlett <- lapply(rho_approv_cit, FUN = cortest.bartlett, 
                              n = min(nrow(approv_cit_list$elemen),
                                      nrow(approv_cit_list$high),
                                      nrow(approv_cit_list$all)))

approv_local_bartlett <- lapply(rho_approv_local, FUN = cortest.bartlett, 
                              n = min(nrow(approv_local_list$elemen),
                                      nrow(approv_local_list$high),
                                      nrow(approv_local_list$all)))

approv_sector_bartlett <- lapply(rho_approv_sector, FUN = cortest.bartlett, 
                                n = min(nrow(approv_sector_list$elemen),
                                        nrow(approv_sector_list$high),
                                        nrow(approv_sector_list$all)))

# Evasion
evasi_cit_bartlett <- lapply(rho_evasi_cit, FUN = cortest.bartlett, 
                              n = min(nrow(evasi_cit_list$elemen),
                                      nrow(evasi_cit_list$high),
                                      nrow(evasi_cit_list$all)))

evasi_local_bartlett <- lapply(rho_evasi_local, FUN = cortest.bartlett, 
                                n = min(nrow(evasi_local_list$elemen),
                                        nrow(evasi_local_list$high),
                                        nrow(evasi_local_list$all)))

evasi_sector_bartlett <- lapply(rho_evasi_sector, FUN = cortest.bartlett, 
                                 n = min(nrow(evasi_sector_list$elemen),
                                         nrow(evasi_sector_list$high),
                                         nrow(evasi_sector_list$all)))

# Verifying the bartlett condition - p-value < 0.05 (adopted significance level)

bartlett_results <- data.frame()

for(i in 1:length(approv_cit_bartlett)){
  
  bartlett_results[i,1] <- approv_cit_bartlett[[c(i,2)]] < 0.05
  bartlett_results[i,2] <- approv_local_bartlett[[c(i,2)]] < 0.05
  bartlett_results[i,3] <- approv_sector_bartlett[[c(i,2)]] < 0.05
  
  bartlett_results[i,4] <- evasi_cit_bartlett[[c(i,2)]] < 0.05
  bartlett_results[i,5] <- evasi_local_bartlett[[c(i,2)]] < 0.05
  bartlett_results[i,6] <- evasi_sector_bartlett[[c(i,2)]] < 0.05
  
}

# Test results
all(bartlett_results)

# Running the PCA - prcomp() from psych ---------------------------------------

# Approved
pca_approv_cit <- lapply(approv_cit_list, FUN = prcomp, scale. = TRUE)
pca_approv_local <- lapply(approv_local_list, FUN = prcomp, scale. = TRUE)
pca_approv_sector <- lapply(approv_sector_list, FUN = prcomp, scale. = TRUE)

# Evasion
pca_evasi_cit <- lapply(evasi_cit_list, FUN = prcomp, scale. = TRUE)
pca_evasi_local <- lapply(evasi_local_list, FUN = prcomp, scale. = TRUE)
pca_evasi_sector <- lapply(evasi_sector_list, FUN = prcomp, scale. = TRUE)

# Reporting results -----------------------------------------------------------

r_approv_cit  = r_approv_local = r_approv_sector = r_evasi_cit = r_evasi_local =
  r_evasi_sector <- list('element' = NA, 'high' = NA, 'all' = NA)

for(i in 1:length(pca_approv_cit)){

# Approved
r_approv_cit[[i]] <- data.frame(eigenvalue = pca_approv_cit[[i]]$sdev^2,
           var_shared = summary(pca_approv_cit[[i]])$importance[2,],
           var_cumulative = summary(pca_approv_cit[[i]])$importance[3,]) 

r_approv_local[[i]] <- data.frame(eigenvalue = pca_approv_local[[i]]$sdev^2,
           var_shared = summary(pca_approv_local[[i]])$importance[2,],
           var_cumulative = summary(pca_approv_local[[i]])$importance[3,]) 

r_approv_sector[[i]] <- data.frame(eigenvalue = pca_approv_sector[[i]]$sdev^2,
           var_shared = summary(pca_approv_sector[[i]])$importance[2,],
           var_cumulative = summary(pca_approv_sector[[i]])$importance[3,])

# Evasion
r_evasi_cit[[i]] <- data.frame(eigenvalue = pca_evasi_cit[[i]]$sdev^2,
            var_shared = summary(pca_evasi_cit[[i]])$importance[2,],
            var_cumulative = summary(pca_evasi_cit[[i]])$importance[3,]) 

r_evasi_local[[i]] <- data.frame(eigenvalue = pca_evasi_local[[i]]$sdev^2,
            var_shared = summary(pca_evasi_local[[i]])$importance[2,],
            var_cumulative = summary(pca_evasi_local[[i]])$importance[3,]) 

r_evasi_sector[[i]] <- data.frame(eigenvalue = pca_evasi_sector[[i]]$sdev^2,
            var_shared = summary(pca_evasi_sector[[i]])$importance[2,],
            var_cumulative = summary(pca_evasi_sector[[i]])$importance[3,])
}

reports <- list('r_approv_cit' = r_approv_cit, 'r_approv_local' = r_approv_local,
                'r_approv_sector' = r_approv_sector, 'r_evasi_cit' = r_evasi_cit,
             'r_evasi_local' = r_evasi_local, 'r_evasi_sector' = r_evasi_sector)

# Removing old objects
rm(r_approv_cit, r_approv_local, r_approv_sector, r_evasi_cit, r_evasi_local,
   r_evasi_sector)

# Variables weights - plot example
ggplotly(
  data.frame(pca_approv_cit$high$rotation) %>% 
    mutate(var = names(data.frame(approv_cit_list$high))) %>% 
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
k = sum(afpc_aprovacao_fundamental$sdev^2>1)

# Scree Plot
ggplotly(
  fviz_eig(X = afpc_aprovacao_fundamental,
           ggtheme = theme_bw(), 
           barcolor = "black", 
           barfill = "dodgerblue4",
           linecolor = "darkgoldenrod4")
)

# Both methods indicates number of PCs = 2

# Extracting factor loading and ----------------------------------------------

fl_approv_cit  = fl_approv_local = fl_approv_sector = fl_evasi_cit =
  fl_evasi_local = fl_evasi_sector <- list('element' = NA, 'high' = NA, 
                                           'all' = NA)

for(i in 1:length(pca_approv_cit)){
  
# Approved
fl_approv_cit[[i]] <- pca_approv_cit[[i]]$rotation[] %*%
    diag(pca_approv_cit[[i]]$sdev[])

fl_approv_local[[i]] <- pca_approv_local[[i]]$rotation[] %*%
  diag(pca_approv_local[[i]]$sdev[])

fl_approv_sector[[i]] <- pca_approv_sector[[i]]$rotation[] %*%
  diag(pca_approv_sector[[i]]$sdev[])

# Evasion
fl_evasi_cit[[i]] <- pca_evasi_cit[[i]]$rotation[] %*%
  diag(pca_evasi_cit[[i]]$sdev[])

fl_evasi_local[[i]] <- pca_evasi_local[[i]]$rotation[] %*%
  diag(pca_evasi_local[[i]]$sdev[])

fl_evasi_sector[[i]] <- pca_evasi_sector[[i]]$rotation[] %*%
  diag(pca_evasi_sector[[i]]$sdev[])
}

reports_fl <- list('fl_approv_cit' = fl_approv_cit, 'fl_approv_local' = 
                fl_approv_local, 'fl_approv_sector' = fl_approv_sector,
                'fl_evasi_cit' = fl_evasi_cit, 'fl_evasi_local' = fl_evasi_local,
                'fl_evasi_sector' = fl_evasi_sector)

# Removing old objects
rm(fl_approv_cit, fl_approv_local, fl_approv_sector, fl_evasi_cit, fl_evasi_local,
   fl_evasi_sector)

# Report plot - example
data.frame(reports_fl$fl_approv_cit$all) %>%
  rename(F1 = X1, F2 = X2, F3 = X3, F4 = X4, F5 = X5, F6 = X6, F7 = X7,
         F8 = X8, F9 = X9, F10 = X10, F11 = X11, F12 = X12) %>%
  mutate(Comunalidades = rowSums(reports_fl$fl_approv_cit$all^2)) %>% 
  kable() %>%
  kable_styling(bootstrap_options = 'striped',
                full_width = T,
                font_size = 12)

# Ploting factor loadings - example
data.frame(reports_fl$fl_approv_cit$all) %>% 
  rename(F1 = X1, F2 = X2) %>% 
  ggplot(aes(x = F1, y = F2))+
  geom_point(color = 'dodgerblue4') +
  geom_hline(yintercept = 0, color = 'darkorchid')+
  geom_vline(xintercept = 0,color = 'darkorchid')+
  geom_text_repel(label = row.names(reports_fl$fl_approv_cit$all))+
  labs(x='F1', y= 'F2')+
  theme_bw()

# Factor scores ---------------------------------------------------------------

fs_approv_cit  = fs_approv_local = fs_approv_sector = fs_evasi_cit =
  fs_evasi_local = fs_evasi_sector <- list('elemen' = NA, 'high' = NA, 
                                           'all' = NA)

for(i in 1:length(pca_approv_cit)){
  
# Approved
fs_approv_cit[[i]] <- t(pca_approv_cit[[i]]$rotation)/
  pca_approv_cit[[i]]$sdev

fs_approv_local[[i]] <- t(pca_approv_local[[i]]$rotation)/
  pca_approv_local[[i]]$sdev

fs_approv_sector[[i]] <- t(pca_approv_sector[[i]]$rotation)/
  pca_approv_sector[[i]]$sdev

# Evasion
fs_evasi_cit[[i]] <- t(pca_evasi_cit[[i]]$rotation)/
  pca_evasi_cit[[i]]$sdev

fs_evasi_local[[i]] <- t(pca_evasi_local[[i]]$rotation)/
  pca_evasi_local[[i]]$sdev

fs_evasi_sector[[i]] <- t(pca_evasi_sector[[i]]$rotation)/
  pca_evasi_sector[[i]]$sdev
}

factor_scores <- list('fs_approv_cit' = fs_approv_cit, 'fs_approv_local' = 
                     fs_approv_local, 'fs_approv_sector' = fs_approv_sector,
                   'fs_evasi_cit' = fs_evasi_cit, 'fs_evasi_local' = fs_evasi_local,
                   'fs_evasi_sector' = fs_evasi_sector)

# Removing old objects
rm(fs_approv_cit, fs_approv_local, fs_approv_sector, fs_evasi_cit, fs_evasi_local,
   fs_evasi_sector)

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
