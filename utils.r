#!/usr/bin/env Rscript

## -------------------------------------------------------------------------
# load packages

loadPackages <- function(packages){
    for (p in packages){
        if(!(p %in% rownames(installed.packages()))){
          print(paste("You are missing the package", p, ",we will now try to install it..."))
          install.packages(p, repos='http://cran.us.r-project.org')
        }    
        library(p, character.only=TRUE)
    }
}

getArgs <- function(){
    parser$parse_args()
}

setParser <- function(title){
    loadPackages(c('argparse'))    

    parser <- ArgumentParser(description = title)
    
    parser$add_argument('-area', type='character', default = 'grenoble', help = "HTS' area name")
    parser$add_argument('-spss', type="character", default = 'data/SPSS/', help='path to the original HTS files')
    parser$add_argument('-csv', type = 'character', default = 'data/csv/', help = 'path to csv files')
    parser$add_argument('-shape', type = 'character', default = 'map/', help = 'path to the shape files')
    parser$add_argument('-rds', type = 'character', default = 'data/rds/', help = 'path to the previously treated data (saved as rds files)')
    parser$add_argument('-traminer', type = 'character', default = 'traminer/data/', help = 'path to files generated through traminer')

    return(parser)
}

factorToNumeric <- function(x) (as.numeric(as.character(x)))

getPCode <- function(val1, val2, val3, val4){
  val <- unique(val2)
  if(length(val) == 1 & is.na(val)) paste0(val1, str_pad(val3, 4, pad = '0'), str_pad(val4, 2, pad = '0')) else paste0(val1, str_pad(val2, 3, pad = '0'), str_pad(val3, 4, pad = '0'), str_pad(val4, 2, pad = '0'))
}

getHCode <- function(val1, val2, val3){
  val <- unique(val2)
  if(length(val) == 1 & is.na(val)) paste0(val1, str_pad(val3, 4, pad = '0')) else paste0(val1, str_pad(val2, 3, pad = '0'), str_pad(val3, 4, pad = '0'))
}

group_vars <- function(trajet = FALSE, flows = FALSE) {
  vars <- c('pcode')
  vars <- if (flows) c(vars, 'origin', 'destin') else c(vars, 'code')
  vars <- c(vars, 'start', 'end', 'dura')
  vars <- if (trajet) c(vars, 'D1') else vars
}

select_vars <- function(trajet = FALSE, flows = FALSE){
  vars <- c('pcode')
  vars <- if (flows) c(vars, 'origin', 'destin', 'D1') else c(vars, 'code')
  vars <- c(vars, 'start', 'end', 'coem')
  vars <- if (args$class) c(vars, 'class') else vars
  if (trajet) c(vars, 'T3', 'D5') else c(vars, 'status')
}

distinct_vars <- function(flows = FALSE){
  vars <- c('pcode')
  vars <- if (flows) c(vars, 'origin', 'destin') else c(vars, 'code')
  c(vars, 'D1', 'T3', 'D5', 'start', 'end', 'dura')
}

# expands the table so it has one row for each one-hour displacement parts
expandTrajet <- function(df, flows = FALSE){
  
  temp_df <- df %>%
    group_by(pcode) %>%
    mutate(start = D4A, end = D8A,
           dura = end - start, 
           times = ifelse(dura == 0, 1, trunc(dura)))
  
  # replicate the rows according to each displacement duration
  temp_df <- temp_df[rep(seq_len(nrow(temp_df)), temp_df$times),]
  
  temp_df <- temp_df %>%
    # distinct_at(distinct_vars(flows = flows), .keep_all = TRUE) %>%
    group_by_at(group_vars(trajet = TRUE, flows = flows)) %>%
    mutate(index = row_number()) %>%
    ungroup() %>%
    mutate(start = ifelse(dura > 1, start + index - 1, start), 
           end = start + 1) %>%
    select(select_vars(trajet = TRUE, flows = flows))
}

# -------------------------------------------------
# expand the data to add a row for each hour of multi hours activities
# aggregate -> spatial aggregation
# data -> deplacement table expanded

expandTime <- function(df){

  temp_df <- df %>%
    group_by(pcode) %>%
    mutate(start = D8A, 
           end = ifelse(is.na(lead(D4A)), 29, lead(D4A)),
           dura = end - start, 
           times = ifelse(dura == 0, 1, dura))
  
  # replicate each row according to how many hours the stay lasted
  temp_df <- temp_df[rep(seq_len(nrow(temp_df)), temp_df$times),]
  
  temp_df %>%
    group_by_at(group_vars()) %>%
    mutate(index = row_number()) %>%
    ungroup() %>%
    mutate(start = ifelse(dura > 1, start + (index-1), start), 
           end = ifelse(dura > 1, start + 1, ifelse(dura == 0, end+1, end))) %>%
    select(select_vars())
}

## -----------------------------------------------------------------------------------------------------
# once the trajectories are classified, the population per class can be calculated through this function
# df : table personne
# class_ref : the classes of people according to activities or modes of transport
computePopulationPerClass <- function(df, class_ref, file_name){
  population_df <- df %>%
    mutate(hcode = getHCode(TIRA, PP2, ECH), pcode = getPCode(TIRA, PP2, ECH, P0)) %>%
    left_join(class_ref, by = c('pcode')) %>%
    left_join(coem_ref, by = c('hcode')) %>%
    select(TIRA, P25, coem, class) %>%
    mutate(P25 = replace_na(P25, 2)) %>% # if the person did not report what they were doing in the eve we assume they did not move
    filter(P25 <= 2) %>% # we remove the codes for non-considered displacements, i.e. 3 and 4
    group_by(TIRA, P25, class) %>%
    summarise(total = sum(coem)) %>%
    spread(P25, total) %>%
    rename(pop = '1', not_mov = '2') %>%
    mutate(pop = ifelse(is.na(pop), not_mov, pop), class = replace_na(class, 'not_mov')) %>%
    select(TIRA, class, pop) %>%
    rename(code = TIRA)
  return(population_df)
}
  #saveRDS(population_df, file_name)
  #print(paste('Saved as', file_name))

getFilePath <- function(file_path, file_name){
  paste0(file_path, args$area, '/', file_name)
}
