if(!require(remotes)){
  print("You are missing the package 'remotes', we will now try to install it...")
  install.packages("remotes")
  library(remotes)
}

if(!require(sf)){
  print("You are missing the package 'sf', we will now try to install it...")
  install.packages("sf")
  library(sf)
}

if(!require(PBSmapping))
{
  print("You are missing the package 'PBSmapping', we will now try to install it...")
  install.packages("PBSmapping")
  library(PBSmapping)
}

if(!require(gridExtra))
{
  print("You are missing the package 'gridExtra', we will now try to install it...")
  install.packages("gridExtra")
  library(gridExtra)
}

if(!require(reshape2))
{
  print("You are missing the package 'reshape2', we will now try to install it...")
  install.packages("reshape2")
  library(reshape2)
}

if(!require(dplyr))
{
  print("You are missing the package 'dplyr', we will now try to install it...")
  install.packages("dplyr")
  library(dplyr)
}

if(!require(plyr))
{
  print("You are missing the package 'plyr', we will now try to install it...")
  install.packages("plyr")
  library(plyr)
}

if(!require(readxl))
{
  print("You are missing the package 'readxl', we will now try to install it...")
  install.packages("readxl")
  library(readxl)
}

if(!require(readr))
{
  print("You are missing the package 'readr', we will now try to install it...")
  install.packages("readr")
  library(readr)
}


if(!require(cartography))
{
  print("You are missing the package 'cartography', we will now try to install it...")
  install.packages("cartography")
  library(cartography)
}

#never called in the code
if(!require(rgeos))
{
  print("You are missing the package 'rgeos', we will now try to install it...")
  install.packages("rgeos", type = "source")
  library(rgeos)
}

#never called in the code
#if(!require(rgdal))
#{
#  print("You are missing the package 'rgdal', we will now try to install it...")
#  install.packages("rgdal", type="source")
#  library(rgdal)
#}

if(!require(ggplot2))
{
  print("You are missing the package 'ggplot2', we will now try to install it...")
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(tigris))
{
  print("You are missing the package 'tigris', we will now try to install it...")
  install.packages("tigris")
  library(tigris)
}

if(!require(sp))
{
  print("You are missing the package 'sp', we will now try to install it...")
  install.packages("sp")
  library(sp)
}

if(!require(raster))
{
  print("You are missing the package 'raster', we will now try to install it...")
  install.packages("raster")
  library(raster)
}

#never called in the code
#if(!require(maptools))
#{
#  print("You are missing the package 'maptools', we will now try to install it...")
#  install.packages("maptools")
#  library(maptools)
#}

if(!require(chron))
{
  print("You are missing the package 'chron', we will now try to install it...")
  install.packages("chron")
  library(chron)
}

if(!require(foreign))
{
  print("You are missing the package 'foreign', we will now try to install it...")
  install.packages("foreign")
  library(foreign)
}

if(!require(scales))
{
  print("You are missing the package 'scales', we will now try to install it...")
  install.packages("scales")
  library(scales)
}

if(!require(plotly))
{
  print("You are missing the package 'plotly', we will now try to install it...")
  install.packages("plotly")
  library(plotly)
}

if(!require(jsonlite))
{
  print("You are missing the package 'jsonlite', we will now try to install it...")
  install.packages("jsonlite")
  library(jsonlite)
}

if(!require(data.table))
{
  print("You are missing the package 'data.table', we will now try to install it...")
  install.packages("data.table")
  library(data.table)
}

#never called in the code
#if(!require(gdata))
#{
#  print("You are missing the package 'gdata', we will now try to install it...")
#  install.packages("gdata")
#  library(gdata)
#}


if(!require(tcltk))
{
  print("You are missing the package 'tcltk', we will now try to install it...")
  install.packages("tcltk")
  library(tcltk)
}


Sys.setenv("plotly_username"="alinemenin")
Sys.setenv("plotly_api_key"="5DFW03MSOy39mqAR5w1L")

if(!require(tidyverse))
{
  print("You are missing the package 'tidyverse', we will now try to install it...")
  install.packages("tidyverse")
  library(tidyverse)
}


if(!require(leaflet))
{
  print("You are missing the package 'leaflet', we will now try to install it...")
  install.packages("leaflet")
  library(leaflet)
}

if(!require(classInt))
{
  print("You are missing the package 'classInt', we will now try to install it...")
  install.packages("classInt")
  library(classInt)
}

if(!require(igraph))
{
  print("You are missing the package 'igraph', we will now try to install it...")
  install.packages("igraph")
  library(igraph)
}

if(!require(flows))
{
  print("You are missing the package 'flows', we will now try to install it...")
  install.packages("https://cran.r-project.org/src/contrib/Archive/flows/flows_1.1.1.tar.gz", repos = NULL, type = 'source',dependencies = TRUE)
  library(flows)
}

if(!require(TraMineR))
{
  print("You are missing the package 'TraMineR', we will now try to install it...")
  install.packages("TraMineR")
  library(TraMineR)
}

if(!require(TraMineRextras))
{
  print("You are missing the package 'TraMineRextras', we will now try to install it...")
  install.packages("TraMineRextras")
  library(TraMineRextras)
}



if(!require(cluster))
{
  print("You are missing the package 'cluster', we will now try to install it...")
  install.packages("cluster")
  library(cluster)
}
