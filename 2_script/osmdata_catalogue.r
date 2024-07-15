########## SET UP ##########
#-Clear enviornment
rm(list = ls())

#-Install / load packages
pacman::p_load(sf, httr, jsonlite, here, tmap, osmdata, tidyverse, data.table, rmapshaper, xml2, rvest)

#-Path setting
path_home <- getwd()
path_data <- paste0(path_home, "/1_data")
path_output <- paste0(path_home, "/3_output")

#--Wiki paths
path_amn <- "https://wiki.openstreetmap.org/wiki/Key:amenity#Values"
path_buil <- "https://wiki.openstreetmap.org/wiki/Buildings#Values"
wiki_list <- list(path_amn, path_buil)

#--Form table
form_table <- function(path, start_word, n_col){

    tbody <- read_html(path) |>
        html_nodes("td") |> 
         html_text() 

    tbody <- str_replace_all(tbody, paste0(".*", start_word), start_word)
    n_row <- length(tbody)


    tbl <- array(tbody, dim = c(n_col, n_row)) |>
        t() |>
         as.data.frame()

    if (n_col == 6){
        names(tbl) <- c("Key", "Value", "Element", "Comment", "Carto rendering", "Photo")
    } else if (n_col == 4){
        names(tbl) <- c("Key", "Value", "Comment", "Photo")
    }
    
    return(tbl)
}

building <- form_table(path_buil, "building", 4)
ammenity <- form_table(path_amn, "amenity", 6)
paste0(".*", "building")
