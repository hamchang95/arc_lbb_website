#--Load all features
source(here("2_script", "osm_features.r"))

########### SET UP ##########
#--Flatten list
place_list <- do.call(c, do.call(c, place_list_fin))
names(place_list) <- str_replace_all(names(place_list), fixed("."), "_")

#--Import park
park <- st_read(here("3_output", "place", "park.shp"))
place_list[["park"]] <- park

#--Remove empty list
place_list <- Filter(function(x) nrow(x) > 0, place_list)

#--Import Barnet shapefile
bnt_shp <- sf::st_read(here("1_data", "9_geo", "bnt_lad.json"), crs = 4326) |>
  st_make_valid()

########## PROCESS ########
#--Select only the features that intersect the Barnet's boundary
poi_bnt_check <- map(place_list, ~st_intersects(.x, bnt_shp, sparse = FALSE))
poi_bnt_check <- map(poi_bnt_check, ~which(.x == TRUE))

poi_bnt <- map2(.x = place_list, .y = poi_bnt_check, ~.x[.y,])
poi_bnt <- Filter(function(x) nrow(x) > 0, poi_bnt)

#--Divide features into polygons / points to get centroid for polygons
#---Check if features are polygon / point
poly_check <- map(poi_bnt, ~grepl("POLYGON", st_geometry_type(.x)))
point_check <- map(poi_bnt, ~grepl("POINT", st_geometry_type(.x)))

#---Filter features by geometry type
poi_bnt_poly <- map2(.x = poi_bnt, .y = poly_check, ~.x[.y,])
poi_bnt_pt <-  map2(.x = poi_bnt, .y = point_check, ~.x[.y,])

#---Remove empty list
poi_bnt_poly <- Filter(function(x) nrow(x) > 0, poi_bnt_poly)
poi_bnt_pt <- Filter(function(x) nrow(x) > 0, poi_bnt_pt)

poi_bnt_poly <- map(poi_bnt_poly, ~st_centroid(.x))
poi_bnt_fin <- c(poi_bnt_poly, poi_bnt_pt)

####### SELECT NECESSARY COLUMNS #####
poi_bnt_fin <- map(poi_bnt_fin, ~.x |> 
                                    select(osm_id, geometry) |>
                                    mutate(row_id = 1:nrow(.x), .before = everything()))

###### COMBINE SIMILAR POIS #######
map(poi_bnt_fin, ~nrow(.x))

#--Places of worship
id_pow <- list()

id_pow[[1]] <- poi_bnt_fin$building_chapel$geometry %in% poi_bnt_fin$amenity_place_of_worship$geometry
id_pow[[2]] <- poi_bnt_fin$building_church$geometry %in% poi_bnt_fin$amenity_place_of_worship$geometry
id_pow[[3]] <- poi_bnt_fin$building_synagogue$geometry %in% poi_bnt_fin$amenity_place_of_worship$geometry
id_pow[[4]] <- poi_bnt_fin$building_temple$geometry %in% poi_bnt_fin$amenity_place_of_worship$geometry

poi_bnt_fin[["building_chapel"]] <- poi_bnt_fin[["building_chapel"]][!id_pow[[1]],]
poi_bnt_fin[["building_church"]] <- poi_bnt_fin[["building_church"]][!id_pow[[2]],]
poi_bnt_fin[["building_synagogue"]] <- poi_bnt_fin[["building_synagogue"]][!id_pow[[3]],]
poi_bnt_fin[["building_temple"]] <- poi_bnt_fin[["building_temple"]][!id_pow[[4]],]

poi_bnt_fin[["amenity_place_of_worship"]] <- rbind(rbind(rbind(rbind(poi_bnt_fin[["building_chapel"]], poi_bnt_fin[["amenity_place_of_worship"]]), poi_bnt_fin[["building_church"]]), poi_bnt_fin[["building_synagogue"]]), poi_bnt_fin[["building_temple"]])


#--University & College
poi_bnt_fin$post_secondary <- rbind(poi_bnt_fin$amenity_collge, poi_bnt_fin$amenity_university)

#--Social facility
poi_bnt_fin$social_facility <- rbind(poi_bnt_fin$amenity_social_facility, poi_bnt_fin$amenity_nursing_home)

#--Money exchange/transfer
poi_bnt_fin$money_exchange_transfer <- rbind(poi_bnt_fin$amenity_bureau_de_change, poi_bnt_fin$amenity_money_transfer)

#### REMOVE ######
#--Remove places that are fewer than 5 
poi_bnt_fin <- Filter(function(x) nrow(x)>=5, poi_bnt_fin)
#poi_bnt_fin <- Filter(function(x) !names(x) %in% c("amenity_university", "amenity_social_facility"), poi_bnt_fin)

#--Remove overlapping places
poi_null_nm <- c("amenity_social_facility", "amenity_university")

poi_bnt_fin <- poi_bnt_fin[!names(poi_bnt_fin) %in% poi_null_nm]

#--Tidy up names
pattern_null <- c("amenity_|building_|shop_|office_")
names(poi_bnt_fin) <- str_remove(names(poi_bnt_fin), pattern_null)

####### CLEAN UP #####
rm(list = c('poly_check', 'point_check', 'poi_bnt_poly', 'poi_bnt_pt', 'poi_bnt_check', 'i', 'j', 'place_list_fin', 'place_list', 'park'))