#!/usr/bin/env Rscript

#--------------------------------------------------------------------------------
# aggregate presence per activity and mode, presence density, fluctuation, and attractiveness
#--------------------------------------------------------------------------------

#----------------------------------------------------------
# Main function to generate indicators of presence, fluctuation, and attractiveness
#----------------------------------------------------------
# depla_df: expanded deplacement table
# traj_df: expanded trajet table
# pop_df: population data for spatial locations
# sf: shapefile for spatial partition (optional)
# partition: territorial partition (default: 'none')
# aggregate_time: TRUE for 24-hour aggregation, FALSE for hourly
# aggregate_space: TRUE for aggregated space, FALSE for disaggregated
# per_class: TRUE to calculate indicators per class
getIndicators <- function(depla_df, traj_df, pop_df, sf = NULL, partition = 'none', aggregate_time = FALSE, aggregate_space = FALSE, per_class = FALSE){

  temporal <- c('start', 'end')
  by_temporal <- c('start'='start', 'end'='end')
  
  # Helper function to define variables for distinct rows
  distinct_vars <- function(status = FALSE){
    vars <- if (status)
        if (per_class) c('pcode', 'status', 'class') else c('pcode', 'status')
      else
        if (per_class) c('pcode', 'class') else c('pcode')
    
    vars <- if (aggregate_time) vars else c(vars, 'start', 'end')
    return(vars)
  }
  
  # Helper function to define grouping variables
  group_vars <- function(status = FALSE, pcode = FALSE){
    vars <- if (per_class) 
      if (status) c('status', 'class') else if (pcode) c('pcode', 'class') else c('class')
    else 
      if (status) c('status') else if (pcode) c('pcode') else c()


    vars <- if (aggregate_space) vars else c(vars, 'code')
    vars <- if (aggregate_time) vars else c(vars, 'start', 'end')

    return(vars)
  }
  
  # Helper function to define variables for selection
  select_vars <- function(){
    vars <- c('status', 'total')
    vars <- if(aggregate_space) vars else c('code', vars)
    vars <- if(aggregate_time) vars else c(vars, 'start', 'end')
    if (per_class) c(vars, 'class') else vars
  }
  
  # Helper function to define variables for comparison in joins
  compare_vars <- function(status = TRUE){
    vars <- if (aggregate_space) c() else c('code')
    vars <- if (aggregate_time) vars else c(vars, 'start', 'end')
    vars <- if (per_class) c(vars, 'class') else vars
    if (status) c(vars, 'status') else vars
  }
  
  # Calculate presence indicators
  getPresence <- function(df){
    presence <- df %>% distinct_at(distinct_vars(), .keep_all = TRUE) %>%
      group_by_at(group_vars()) %>%
      summarise(moving = sum(coem)) %>%
      mutate(indicator = 'presence')
    
    if (aggregate_space) 
      presence %>%
        rename(total = moving) %>%
        mutate(value = total/sum(pop_df$pop))
    else{
      if (!aggregate_time){
        start <- 4:28
        temp <- tibble(code = unique(presence$code))
        temp <- expand(temp, nesting(code), start)
        
        presence <- temp %>%
          left_join(presence, by = c('code'='code', 'start'='start')) %>%
          mutate(end = start + 1) %>%
          replace(is.na(.), 0) %>%
          distinct(code, start, end, .keep_all = TRUE)
      }
      
      presence %>% left_join(pop_df, by = c('code'='code')) %>%
        mutate(total = moving + not_mov) %>% # consider the moving people and the ones who reported staying at home the period of the survey
        mutate(value = total/sum(pop_df$pop)) %>%
        left_join(sf, by = c("code" = "code")) %>% # recover the shapefile and calculate the density over 24 hours for 97 sectors
        mutate(density = total/sqkm) %>%
        select(-pop, -mov, -not_mov)
    }
  }
  
  # Calculate presence indicators per status (activity or mode)
  getPresencePerStatus <- function(df){
    temp <- df %>%
      distinct_at(distinct_vars(status = TRUE), .keep_all = TRUE) %>%
      group_by_at(group_vars(pcode = TRUE)) %>%
      mutate(times = 1/n()) %>%
      mutate(total_multi = ifelse(times < 1, times*coem, 0)) %>%
      group_by_at(group_vars(status = TRUE)) %>%
      summarise(total = sum(times*coem), total_multi = sum(total_multi)) # sum up the number of people doing each activity
    
    keys <- unique(temp$status)
    activity <- temp %>%
      select(select_vars()) %>%
      spread(status, total) %>%
      gather(status, total, keys) %>%
      left_join(temp %>% select(-total), by = compare_vars())
    
    if (aggregate_time & aggregate_space & !per_class)
      activity %>% mutate(value = total/presence$total[1]) %>% select(status, total, value)
    else 
      activity %>% left_join(presence %>% select(-status, -value, -indicator) %>% rename(pres = total), by = compare_vars(status = FALSE)) %>%
        mutate(value = total/pres, value_multi = total_multi/total)
  }
  
  # Compute presence indicators for modes of transport
  presence <- getPresence(df = traj_df) %>% mutate(status = 'modes')
  print(paste('Computing indicator of presence PER MODE OF TRANSPORT Space:', ifelse(aggregate_space, 'aggregate.', 'disaggregate.'), 'Time:', ifelse(aggregate_time, 'aggregate.', 'disaggregate.'), ifelse(per_class, 'Per Class.', '')))
  modes <- getPresencePerStatus(df = traj_df) %>% mutate(indicator = 'modes') %>% arrange(match(status, c("car", "walk", "pts", 'bike', 'other')))
  bind_df <- bind_rows(presence, modes)
  
  # Compute presence indicators for activities
  presence = getPresence(df = depla_df) %>% mutate(status = 'activity')
  print(paste('Computing indicator of presence PER ACTIVITY. Space:', ifelse(aggregate_space, 'aggregate.', 'disaggregate.'), 'Time:', ifelse(aggregate_time, 'aggregate.', 'disaggregate.'), ifelse(per_class, 'Per Class.', '')))
  activity <- getPresencePerStatus(df = depla_df) %>% mutate(indicator = 'activity')
  bind_df <- bind_rows(bind_df, presence, activity)
  
  # Compute fluctuation and attractiveness indicators if applicable
  fluctuation <- tibble()
  attractivity <- tibble()
  
  if (!per_class & !aggregate_space){
    print(paste('Computing FLUCTUATION indicator. Space: disaggregate. Time:', ifelse(aggregate_time, 'aggregate.', 'disaggregate.')))
    
    fluctuation <- presence %>% 
      mutate(pres = total) %>%
      left_join(pop_df, by = c("code"="code")) %>%
      mutate(total = pres - pop) %>%
      mutate(value = total / pop) %>% # fluctuation of population (the proportion between the number of people present on the location and its population)
      select(if(aggregate_time) c('code', 'total', 'value') else c('code', 'total', 'value', temporal)) %>%
      mutate(indicator = 'fluctuation')
    
    if (aggregate_time){
      print('Computing ATTRACTIVENESS indicator. Space: disaggregate. Time: aggregate.')
      total_res <- sum(pop_df$pop) # the territory's population
      total_mov <- sum(presence$moving) # the number of people in activity within the territory (do not take in account people who stays at home the whole time)
      attractivity <- presence %>%
        left_join(pop_df, by = c('code'='code')) %>%
        group_by(code) %>%
        summarise(value = (moving/pop)*(total_res/total_mov)) %>%
        select(code, value) %>%
        mutate(indicator = 'attractiveness') 
    }
  }
  
  # Combine all indicators into a single table
  print('Binding indicators into a unique table...')
  
  if (aggregate_space) 
    bind_df %>%
      mutate(partition = 'none', space = 'aggregate', time = ifelse(aggregate_time, 'aggregate', 'individual'))
  else
    bind_rows(bind_df %>% select(-moving), fluctuation, attractivity) %>%
      mutate(name = mapvalues(code, from = sf$code, to = sf$name, warn_missing = FALSE),
             partition = partition, 
             time = ifelse(aggregate_time, 'aggregate', 'individual'),
             space = 'individual')
}

#-----------------------------------------------------------
# Main function to generate all presence indicators
#-----------------------------------------------------------
generateIndicators <- function(){
  # Prepare deplacement data
  depla_df <- deplaexpanded_df %>% rename(code = D7, status = D5) %>%
    filter(code %in% space_ref$DTIR) %>%
    mutate(status = mapvalues(status, as.numeric(activity_ref$code), activity_ref$desc_en, warn_missing = FALSE)) 
  
  if (args$class)
    depla_df <- depla_df %>% left_join(class_ref, by = c('pcode'='pcode')) # to calculate the presence per activity and class (state distribution plot)
  
  # Prepare trajet data
  traj_df <- deplatraj_df %>% rename(code = D7, status = T3) %>%
    filter(code %in% space_ref$DTIR) %>%
    mutate(status = mapvalues(status, as.numeric(mode_ref$code), mode_ref$desc_en, warn_missing = FALSE)) %>%
    mutate(status = replace_na(status, 'walk')) 
  
  if (args$class)
    traj_df <- traj_df %>% left_join(class_ref, by = c('pcode'='pcode')) # to calculate the presence per activity and class (state distribution plot)
  
  # Expand deplacement and trajet tables by time intervals
  print('Expanding deplacement table according to one-hour time intervals...')
  deplaexp <- expandTime(depla_df) # deplacement table hourly expanded per spatial location
  
  print('Expanding trips table according to one-hour time intervals...')
  trajexp <- expandTrajet(traj_df) # trajet table expanded per hour and per spatial location
  
  # Compute aggregated and disaggregated indicators
  aggreg <- getIndicators(depla_df = deplaexp, traj_df = trajexp, pop_df = population_df, aggregate_space = TRUE, aggregate_time = TRUE) # indicators of presence general and per activity over 24 hours
  disaggreg <- getIndicators(depla_df = deplaexp, traj_df = trajexp, pop_df = population_df, aggregate_space = TRUE) # indicators of presence general and per activity per time interval
  
  # Combine results into a single table
  presence_df <- bind_rows(aggreg, disaggreg)
  
  # Compute indicators per class if applicable
  if (args$class){
    aggreg_class <- getIndicators(depla_df = deplaexp, traj_df = trajexp, pop_df = population_df, aggregate_space = TRUE, aggregate_time = TRUE, per_class = TRUE)
    disaggreg_class <- getIndicators(depla_df = deplaexp, traj_df = trajexp, pop_df = population_df, aggregate_space = TRUE, per_class = TRUE)
    
    presence_df <- bind_rows(presence_df, aggreg_class, disaggreg_class)
  }

  # Compute indicators for each territorial partition
  for (p in args$partitions){
    print(paste('Computing indicators for territorial partition', p, '...'))

    pop_bis <- population_df %>% ungroup() %>%
      mutate(TIRA = mapvalues(TIRA, from = space_ref$DTIR, to = space_ref[[p]], warn_missing = FALSE)) %>%
      group_by(TIRA) %>% summarise(mov = sum(mov), not_mov = sum(not_mov), pop = mov + not_mov) %>%
      rename(code = TIRA)

    deplaexp_bis <- deplaexp %>% mutate(code = mapvalues(code, from = space_ref$DTIR, to = space_ref[[p]], warn_missing = FALSE))
    trajexp_bis <- trajexp %>% mutate(code = mapvalues(code, from = space_ref$DTIR, to = space_ref[[p]], warn_missing = FALSE))
    
    sf <- readRDS(getFilePath(args$rds, paste0(p, '_surface.rds')))

    aggreg <- getIndicators(depla_df = deplaexp_bis, traj_df = trajexp_bis, pop_df = pop_bis, sf = sf, partition = p, aggregate_time = TRUE)
    disaggreg <- getIndicators(depla_df = deplaexp_bis, traj_df = trajexp_bis, pop_df = pop_bis, sf = sf, partition = p)
    
    presence_df <- bind_rows(presence_df, aggreg, disaggreg)
    
    if (args$class){
      aggreg_class <- getIndicators(depla_df = deplaexp_bis, traj_df = trajexp_bis, pop_df = pop_bis, sf = sf, partition = p, aggregate_time = TRUE, per_class = TRUE)
      disaggreg_class <- getIndicators(depla_df = deplaexp_bis, traj_df = trajexp_bis, pop_df = pop_bis, sf = sf, partition = p, per_class = TRUE)
    
      presence_df <- bind_rows(presence_df, aggreg_class, disaggreg_class)
    }
    
  }
  
  # Prepare final output
  select_final_vars <- function(){
    vars <- c('time', 'space', 'code', 'name', 'start', 'end', 'indicator', 'status', 'total', 'value', 'density', 'total_multi', 'value_multi', 'partition')
    if(args$class) c(vars, 'class') else vars
  }
  
  print('Preparing final file...')
  presence_df <- presence_df %>% ungroup() %>%
    select(select_final_vars()) %>%
    mutate(status = replace_na(status, 'none'), name = replace_na(name, args$area)) %>%
    replace(is.na(.), 0) %>%
    filter(start <= 29 & end <= 30)
    
  file_name <- getFilePath(args$csv, 'presence.csv')
  write_csv(presence_df, file_name)
  print(paste('Saved as', file_name))
}

# Start generating indicators
print(paste('Generating presence indicators for', args$area, 'area'))
generateIndicators()

