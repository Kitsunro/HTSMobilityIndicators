#!/usr/bin/env Rscript

getLabels <- function(){
    labels <- list(c('home', 'work', 'leisure', 'shopping', 'studying', 'other', 'moving'), c('car', 'pts', 'walk', 'bike', 'other', 'stay'))
    names(labels) <- c('act', 'mode')
    labels
}

getColors <- function(){
    colors <- list(c('#1b9e77', '#e6ab02', '#d85d00', '#7570b3', '#e7298a', '#e5c494', '#808080'), c('#1f78b4', '#a6cee3', '#33a02c', '#b2df8a', '#e5c494', '#808080'))
    names(colors) <- c('act', 'mode')
    colors
}

#code personne
getPCode <- function(val1, val2, val3, val4){
  val <- unique(val2)
  if(length(val) == 1 & is.na(val)) paste0(val1, str_pad(val3, 4, pad = '0'), str_pad(val4, 2, pad = '0')) else paste0(val1, str_pad(val2, 3, pad = '0'), str_pad(val3, 4, pad = '0'), str_pad(val4, 2, pad = '0'))
}

#code menage
getHCode <- function(val1, val2, val3){
  val <- unique(val2)
  if(length(val) == 1 & is.na(val)) paste0(val1, str_pad(val3, 4, pad = '0')) else paste0(val1, str_pad(val2, 3, pad = '0'), str_pad(val3, 4, pad = '0'))
}

createDirectory <- function(folder_name){
    if (!dir.exists(file.path(folder_name))) {
        dir.create(file.path(folder_name))
        print(paste('Directory', folder_name, 'is created.'))
    }else print(paste('Directory', folder_name, 'already exists.'))
}

getFilePath <- function(file_path, file_name){
  paste0(file_path, args$area, '/', file_name)
}

setPlotsFolder <- function(){
  plots_folder <- getFilePath(args$plots, args$sample)
  createDirectory(plots_folder)
}

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

setParser <- function(title){
    loadPackages(c('argparse'))    

    parser <- ArgumentParser(description = title)
    
    parser$add_argument('-area', type = 'character', default = 'grenoble', help = "the HTS' area")
    parser$add_argument('-csv', type="character", default = '../data/csv/', help='path to the folder containing the trajectories files (also serve as the output folder)')
    parser$add_argument('-rds', type = "character", default='../data/rds/', help='path to the folder containing common rds files')
    parser$add_argument('-map', type = 'character', default = '../map/', help = 'path to the maps folder')
    parser$add_argument('-device', type = 'character', default = 'ps', help = 'device to generate charts', choices = c('ps', 'png'))
    parser$add_argument('-sample', type = 'character', default = '', help = 'specifies how to sample the data', choices = c('', '_car_users', '_mode_2345', '_act_14'))
    parser$add_argument('-event', type = 'character', default = 'act', help = 'the event used to classify the trajectories', choices = c('act', 'mode'))
    parser$add_argument('-class', nargs = '*', default = c(6), help = 'the number of classes to split the data (multiple values generate multiple classifications)')
    parser$add_argument('-plots', type = 'character', default = 'plots/', help = 'the path to the plots folder')
    
    return(parser)
}


