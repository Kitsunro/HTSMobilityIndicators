#!/usr/bin/env Rscript

source('utils.r')

parser <- setParser('Script to Calculate Mobility indicators')

parser$add_argument('-indicator', type = 'character', nargs = '*', default = 'presence', help = 'specifies the view to which compute the indicators', choices = c('presence', 'mobility', 'flows'))
parser$add_argument('-partitions', type = "character", nargs = '*', default = c('DTIR', 'D30', 'D10'), help='available territory partitions')
parser$add_argument('-class', action='store_true', help = 'whether to calculate the indicators per class or not')

args <- parser$parse_args()

# temporary
args$area = 'lyon'
args$indicator = 'presence'

loadPackages(c('plyr', 'tidyverse', 'flows', 'jsonlite'))

loadFile <- function(file_path){
  print(paste('Loading data from', file_path))
  readRDS(file_path)
}

getPullingZone <- function(x) (as.numeric(substr(x, 1, 3)))

population_df <- loadFile(getFilePath(args$rds, 'population.rds')) 
depla_df <- loadFile(getFilePath(args$rds, 'deplacement.rds')) %>%
  mutate(D3 = getPullingZone(D3), D7 = getPullingZone(D7))
deplaexpanded_df <- loadFile(getFilePath(args$rds, 'deplaexpanded.rds')) %>%
  mutate(D7 = getPullingZone(D7))
deplatraj_df <- loadFile(getFilePath(args$rds, 'deplatraj.rds')) %>%
  mutate(D3 = getPullingZone(D3), D7 = getPullingZone(D7))

space_ref <- loadFile(getFilePath(args$rds, 'space_ref.rds')) %>%
  mutate_all(function(x) (as.numeric(x)))
activity_ref <- loadFile(getFilePath(args$rds, 'activity_ref.rds')) %>%
  mutate(code = as.numeric(code))
mode_ref <- loadFile(getFilePath(args$rds, 'modes_ref.rds')) %>%
  mutate(code = as.numeric(code))

if (args$class){
  class_ref <- loadFile(getFilePath(args$rds, 'act_class_ref.rds'))
  pop_class <- loadFile(getFilePath(args$rds, 'pop_class.rds'))
}

source(paste0(args$indicator, '.r'))
