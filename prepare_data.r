source('utils.r')

parser <- setParser('Prepare files for indicators')
parser$add_argument('-partitions', type="character", nargs = '*', default = c('OD', 't90', 't30', 't10'), help='available territory partitions')

args <- parser$parse_args()

# temporary
args$area = 'lyon'
args$partitions = c('OD', 'DTIR', 'D30', 'D10')

loadPackages(c('plyr', 'tidyverse', 'sf', 'foreign', 'rlang'))

#--------------------------------------------
# load the shapefiles for each territorial partition

sf_use_s2(FALSE)  # Disable the use of S2 geometry library

getSamplePoints <- function(points, sf){ # generate random points within each spatial location to represent activities or modes of transport (used on the STC)
  
  sample <- apply(sf, 1, function(x){
    points <- st_sample(x$geometry, size = points, exact = TRUE)
    tbl <- tibble(name = as.character(), code = as.numeric(), index = as.numeric(), lat = numeric(), long = numeric())
    for (p in points){
      coord <- st_coordinates(p)
      tbl <- add_row(tbl, name = x$name, code = x$code, index = nrow(tbl), lat = coord[2], long = coord[1])
    }
    tbl
  })
  
  points <- tibble(name = as.character(), code = as.numeric(), index = as.numeric(), lat = numeric(), long = numeric())
  for (l in sample){
    points <- bind_rows(points, as.data.frame(l))
  }
  return(points)
}

for (p in args$partitions){
  file_path <- getFilePath(args$shape, paste0(p, '_sectors.shp'))
  print(paste('Reading shapefile from ', file_path))                      
  
  sf <- st_read(file_path, stringsAsFactors = FALSE) %>%
    st_transform("+proj=longlat +ellps=GRS80") 
  
  st <- st_make_valid(sf)  # Ensure geometries are valid
  st <- st_buffer(st, 0)   # Apply a small buffer to fix any remaining issues

  st <- sf %>% select(code, name)
    
  print('Computing centroids for each shape...')
  # calculate the centroid of each shape
  centroids <- st_centroid(st, byid = TRUE)  # Use the valid geometries
  coords <- as_tibble(st_coordinates(centroids$geometry))
  centroids$lat <- coords$Y
  centroids$long <- coords$X
  st_geometry(centroids) <- NULL
  
  file_path <- getFilePath(args$rds, paste0(p, '_centroids.rds'))
  saveRDS(centroids, file_path)
  print(paste('File saved as', file_path))
  
  if (p == 'OD'){ # since the STC receives the shapefile for OD sectors, only compute the points for this partition
    print('Generating points per activity inside each shape...')
    # generate random points inside each shape to represent activities and modes in the STC (6 modes of transport, 7 activities)
    points <- getSamplePoints(6, sf) # generate only 6 points representing each activity  
    file_path <- getFilePath(args$rds, paste0(p, '_points.rds'))
    saveRDS(points, file_path)
    print(paste('File saved as', file_path))
  }
  
  print('Computing area of each shape...')
  # calculate the area of each shape
  sf <- sf %>% mutate(sqm = st_area(sf), sqkm = sqm / 1000000) %>%
    select(code, name, sqm, sqkm)
  st_geometry(sf) <- NULL
  
  file_path <- getFilePath(args$rds, paste0(p, '_surface.rds'))
  saveRDS(sf, file_path)
  print(paste('File saved as', file_path))
}

#--------------------------------------------
# load the raw HTS data

# mutate all factors to numeric
# rename the key variables to standardize them (use grenoble's HTS example)

vars <- readRDS(getFilePath(args$rds, 'variables_ref.rds'))
vars <- vars %>% spread(ref, variable)

vars <- vars %>% replace(is.na(.), '*') # to use inside syms() every value must be a string

getTIRA <- function(x) (as.numeric(substr(x, 1, 3)))
getP2 <- function(x) (substr(x, 4, 6))

depla <- read.spss(getFilePath(args$spss, 'deplacement.sav'), to.data.frame=TRUE) %>%
  mutate_all(factorToNumeric) %>%
  rename(TIRA = vars$pulling_depla, ECH = vars$sample, P0 = vars$person, D1 = vars$disp_nb,
         D2 = vars$depart_motif,
         D3 = vars$origin,
         D7 = vars$destination,
         D5 = vars$arrival_motif) %>%
  mutate_at(if(vars$depart_min == '*') vars$depart_hour else integer(0), function(x) (str_pad(x, 4, pad = 0))) %>% # if hours and minutes are concatenated, add a 0 to the left to guarantee 4 digits
  mutate_at(if(vars$arrival_min == '*') vars$arrival_hour else integer(0), function(x) (str_pad(x, 4, pad = 0))) %>%
  mutate(D4A = as.numeric(if(vars$depart_min == '*') substr(!!!syms(vars$depart_hour), 1,2) else !!!syms(vars$depart_hour))) %>%
  mutate(D4B = as.numeric(if(vars$depart_min == '*') substr(!!!syms(vars$depart_hour), 3,4) else !!!syms(vars$depart_min))) %>%
  mutate(D8A = as.numeric(if(vars$arrival_min == '*') substr(!!!syms(vars$arrival_hour), 1,2) else !!!syms(vars$arrival_hour))) %>%
  mutate(D8B = as.numeric(if(vars$arrival_min == '*') substr(!!!syms(vars$arrival_hour), 3,4) else !!!syms(vars$arrival_min))) %>%
  mutate(DP2 = if(vars$depla_id == '*') getP2(TIRA) else !!!syms(vars$depla_id)) %>%
  mutate(TIRA = if(vars$depla_id == '*') getTIRA(TIRA) else !!!syms(vars$pulling_depla))

menage <- read.spss(getFilePath(args$spss, 'menage.sav'), to.data.frame=TRUE) %>%
  mutate_all(factorToNumeric) %>%
  rename(TIRA = vars$pulling_menage, ECH = vars$sample, coem = vars$coem) %>%
  mutate(MP3 = if(vars$menage_id == '*') getP2(TIRA) else !!!syms(vars$menage_id)) %>%
  mutate(TIRA = if(vars$menage_id == '*') getTIRA(TIRA) else !!!syms(vars$pulling_depla))

personne <- read.spss(getFilePath(args$spss, 'personne.sav'), to.data.frame=TRUE) %>%
  mutate_all(factorToNumeric) %>%
  rename(TIRA = vars$pulling_personne, ECH = vars$sample, P0 = vars$person, P25 = vars$moving) %>%
  mutate(PP2 = if(vars$personne_id == '*') getP2(TIRA) else !!!syms(vars$personne_id)) %>%
  mutate(TIRA = if(vars$personne_id == '*') getTIRA(TIRA) else !!!syms(vars$pulling_depla))

trajet <- read.spss(getFilePath(args$spss, 'trajet.sav'), to.data.frame=TRUE) %>%
  mutate_all(factorToNumeric) %>%
  rename(TIRA = vars$pulling_trajet, ECH = vars$sample, P0 = vars$person, D1 = vars$disp_nb, T1 = vars$trip_nb, T3 = vars$mode) %>%
  mutate(DP2 = if(vars$depla_id == '*') getP2(TIRA) else !!!syms(vars$depla_id)) %>%
  mutate(TIRA = if(vars$depla_id == '*') getTIRA(TIRA) else !!!syms(vars$pulling_depla))

saveRDS(depla, getFilePath(args$rds, 'deplacement.rds'))
saveRDS(personne, getFilePath(args$rds, 'personne.rds'))
saveRDS(menage, getFilePath(args$rds, 'menage.rds'))
saveRDS(trajet, getFilePath(args$rds, 'trajet.rds'))

# csv for external inspection
write_csv(depla, getFilePath(args$csv, 'deplacement.csv'))
write_csv(personne, getFilePath(args$csv, 'personne.csv'))
write_csv(menage, getFilePath(args$csv, 'menage.csv'))
write_csv(trajet, getFilePath(args$csv, 'trajet.csv'))

# extract the coem variable
coem_ref <- menage %>% 
  mutate(hcode = getHCode(TIRA, MP3, ECH)) %>%
  select(hcode, coem)

saveRDS(coem_ref, getFilePath(args$rds, 'coem_ref.rds'))

# estimates the population of each spatial location (secteur de tirage)
population_df <- personne %>%
  mutate(hcode = getHCode(TIRA, PP2, ECH), pcode = getPCode(TIRA, PP2, ECH, P0)) %>%
  left_join(coem_ref, by=c('hcode'='hcode')) %>%
  select(hcode, pcode, TIRA, P25, coem) %>%
  mutate(P25 = ifelse(is.na(P25), 2, P25)) %>% # if the person did not report what they were doing in the eve we assume they did not move
  filter(P25 <= 2) %>% # we remove the codes for non-considered displacements, i.e. 3 and 4
  group_by(TIRA, P25) %>%
  summarise(total = sum(coem)) %>%
  spread(P25, total) %>%
  rename(mov = '1', not_mov = '2') %>%
  replace(is.na(.), 0) %>%
  mutate(pop = mov + not_mov)

saveRDS(population_df, getFilePath(args$rds, 'population.rds'))

# we are only using activity classes, even though classes according to modes were computed
class_path <- getFilePath(args$rds, 'act_class_ref.rds')
if (file.exists(class_path)){
  class_ref <- readRDS(class_path)
  computePopulationPerClass(df = personne, class_ref = class_ref, file_name = getFilePath(args$rds, 'pop_class.rds'))
}

# extract the useful variables  
depla_df <- depla %>%
  mutate(hcode = getHCode(TIRA, DP2, ECH), pcode = getPCode(TIRA, DP2, ECH, P0)) %>%
  select(hcode, pcode, D1, D2, D3, D4A, D4B, D5, D7, D8A, D8B) %>%
  left_join(coem_ref, by = c('hcode'='hcode')) %>%
  arrange(hcode, pcode, D1) %>%
  filter(D4A <= 28 & D8A <= 28)

saveRDS(depla_df, getFilePath(args$rds, 'deplacement.rds'))

# extends the deplacement table so that we can estimate the number of people moving/stopped at each hour
deplaexpanded <- depla_df %>%
  mutate(D4B = as.integer(ifelse(D4B %% 5 < 3, D4B - (D4B %% 5), D4B + (5 - (D4B %% 5)))),
         D8B = as.integer(ifelse(D8B %% 5 < 3, D8B - (D8B %% 5), D8B + (5 - (D8B %% 5))))) %>% # round the minutes to multiples of 5
  group_by(pcode) %>%
  do(add_row(., .before = 0)) %>% # add an empty row before each row 
  ungroup() %>%
  mutate(hcode = ifelse(is.na(hcode), lead(hcode), hcode), 
         pcode = ifelse(is.na(pcode), lead(pcode), pcode),
         D1 = ifelse(is.na(D1), lead(D1), D1),
         D5 = ifelse(is.na(D5), lead(D2), D5), 
         D7 = ifelse(is.na(D7), lead(D3), D7), 
         D8A = replace_na(D8A, 4), 
         D8B = replace_na(D8B, 0),
         coem = ifelse(is.na(coem), lead(coem), coem)) # if the value is NA, replace it by the value on the next row

saveRDS(deplaexpanded, getFilePath(args$rds, 'deplaexpanded.rds'))

# append the mode of transport of the trajet table to deplacement
deplatraj <- depla %>%
  filter(D4A <= 28 & D8A <= 28) %>%
  mutate(D4B = as.integer(ifelse(D4B %% 5 < 3, D4B - (D4B %% 5), D4B + (5 - (D4B %% 5)))), # round minutes into multiples of 5
         D8B = as.integer(ifelse(D8B %% 5 < 3, D8B - (D8B %% 5), D8B + (5 - (D8B %% 5))))) %>%
  left_join(trajet, by=c('TIRA'='TIRA', 'DP2'='DP2', 'ECH'='ECH', 'P0'='P0', 'D1'='D1')) %>%
  mutate(hcode = getHCode(TIRA, DP2, ECH), pcode = getPCode(TIRA, DP2, ECH, P0)) %>%
  select(hcode, pcode, D1, T1, T3, D3, D4A, D4B, D7, D8A, D8B, D5) %>%
  left_join(coem_ref, by=c('hcode'='hcode'))

saveRDS(deplatraj, getFilePath(args$rds, 'deplatraj.rds'))
