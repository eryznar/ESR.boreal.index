# LOAD PACKAGES ------
library(tidyverse)
library(ncdf4)
library(zoo)
library(maps)
library(mapdata)
library(chron)
library(fields)
library(oce)
library(mice)
library(Hmisc)
library(ggmap)
library(mgcv)
library(MARSS)
library(corrplot)
library(crabpack)

# SET THEME ------
theme_set(theme_bw())


cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# LOAD UNIVERSAL PARAMS ------
current.year <- 2024
