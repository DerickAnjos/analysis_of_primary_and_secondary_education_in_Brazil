# Loading the results of Ranking ----------------------------------------------

load('ranking_results.RData')


# Lift of 1st and 4th quartile in ranking -------------------------------------

# Making a object prototype, with all necessary cases
level_school <- list('elemen' = tibble(), 'high' = tibble(), 'all' = tibble())
object_prototype <- list('approv' = list('cit' = level_school, 
                                         'local' = level_school, 
                                         'sector' = level_school), 
                         'evasi' = list('cit' = level_school,
                                        'local' = level_school, 
                                        'sector' = level_school))

quartile = pop_rate = lift <- object_prototype
analysis <- c('region', 'localization', 'sector')

for(k in 1:length(df_list)){
  
  for(j in 1:length(df_list$approv)){
    
    for(i in 1:length(df_list$approv$cit)){
      
      quartile[[k]][[j]][[i]] <-
        list('first_quartile' = arrange(df_list[[k]][[j]][[i]][
        (round(nrow(df_list[[k]][[j]][[i]])*.75, digits = 0)) :
          nrow(df_list[[k]][[j]][[i]]),], desc(-ranking)), 
        'fourth_quartile' = df_list[[k]][[j]][[i]][1:round(nrow(
          df_list[[k]][[j]][[i]])*.25, digits = 0),])
      
      pop_rate[[k]][[j]][[i]] <-
        list('sector' = unname(summary(df_list[[k]][[j]][[i]]$sector) /
          length(df_list[[k]][[j]][[i]]$sector)),
        'localization' = unname(summary(df_list[[k]][[j]][[i]]$localization) /
          length(df_list[[k]][[j]][[i]]$localization)),
        'region' = unname(summary(df_list[[k]][[j]][[i]]$region) /
          length(df_list[[k]][[j]][[i]]$region)))
        
      lift[[k]][[j]][[i]] <- 
        list('best_schools' = 
          (summary(quartile[[k]][[j]][[i]][['first_quartile']][[analysis[j]]]) / 
          length(quartile[[k]][[j]][[i]][['first_quartile']][[analysis[j]]])) /
            pop_rate[[k]][[j]][[i]][[analysis[j]]],
        'worst_schools' = 
          (summary(quartile[[k]][[j]][[i]][['fourth_quartile']][[analysis[j]]]) / 
          length(quartile[[k]][[j]][[i]][['fourth_quartile']][[analysis[j]]])) /
            pop_rate[[k]][[j]][[i]][[analysis[j]]])
        
    }
  }
}
