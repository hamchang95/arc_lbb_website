########## SET UP ##########
#-Clear enviornment
rm(list = ls())

#-Install / load packages
pacman::p_load(here, tidyverse, data.table, rio)

#-Path setting
path_home <- here()
path_data <- paste0(path_home, "/1_data")
path_output <- paste0(path_home, "/3_output")

#--Import ons postcode directory
onspd <- import(paste0(path_data, "/9_geo", "/ONSPD_FEB_2024_UK.csv"))
names(onspd)

######### FILTER ########
#--Filter Barnet
onspd_bnt <- onspd |> 
    filter(oslaua == "E09000003")

head(onspd_bnt)
tail(onspd_bnt)

######### EXPORT #########
export(onspd_bnt, paste0(path_data, "/9_geo", "/onspd_bnt_0224.csv"))
