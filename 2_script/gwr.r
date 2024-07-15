pacman::p_load(spgwr, here, tidyverse, maptools, caret, leaflet)
source(here("2_script", "pre_reg.r"))

################# PCA ################
#--Run PCA
pca <- caret::preProcess(grid_by_n_num[names(grid_by_n_num)!= "n"], method = 'pca', pcaComp = 4)
loadings <- pca$rotation
loadings
#--Check loadings
for (i in 1:ncol(loadings)) {
  cat("Principal Component", i, ":\n")
  print(sort(abs(loadings[, i]), decreasing = TRUE))
  cat("\n")
}
    #PC1: accountant, restaurant, bicycle_parking, atm, car_repair
    #   : high streets
    #PC2: bridge, alcohol, computer, lawyer, electronics
    #PC3: clinic, doctors, bar, houseware, lawyer
    #   : residential
    #PC4: grave_yard, garage, bar, post_depot, warehouse
    #   : outskirts

#--Make prediction
pca_data <- predict(pca, grid_by_n_num)

#--Combine data
grid_by_n_pca <- cbind(grid_by_n, pca_data[, -1])

ggplot(data = grid_by_n_pca) +
  geom_sf(aes(fill = PC1)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  ggtitle("Spatial Distribution of PC1")


# Creating a color palette for the PC1 scores
pal <- colorQuantile("viridis", grid_by_n_pca$PC1, n = 5)
pal2 <- colorQuantile("magma", grid_by_n_pca$PC2, n = 5)
pal3 <- colorQuantile("YlGnBu", grid_by_n_pca$PC3, n = 5)
pal4 <- colorQuantile("YlGnBu", grid_by_n_pca$PC4, n = 5)

# Creating the leaflet map for PC1
leaflet(grid_by_n_pca) |> 
  addTiles() |>
  addPolygons(fillColor = ~pal4(PC4),
              weight = 1,
              opacity = 1,
              color = 'white',
              fillOpacity = 0.5,
              label = ~paste("PC4 score:", round(PC4, 2))) |>
  addLegend(pal = pal4, values = ~PC4, opacity = 0.5, title = "PC4",
            position = "bottomright")

#--Reproject data to OSGB
grid_by_n_pca_osgb <- st_transform(grid_by_n_pca, 27700) 

grid_by_n_pca_osgb <- grid_by_n_pca_osgb |>
    mutate(long = st_coordinates(st_centroid(grid_by_n_pca_osgb))[, 1],
           lat = st_coordinates(st_centroid(grid_by_n_pca_osgb))[, 2]) 
           
grid_pca_df <- grid_by_n_pca_osgb |>
    st_drop_geometry()

lm_pca <- lm(n ~ PC1 + PC2 + PC3 + PC4 + (PC1 + PC2 + PC3 + PC4)^2, data = grid_pca_df)
summary(lm_pca)
plot(lm_pca, which = 3)

lm_pca_outliers <- abs(rstudent(lm_pca)) > 3
lm_pca_no_outliers <- lm(n ~ PC1 + PC2 + PC3 + PC4 + (PC1 + PC2 + PC3 + PC4)^2, data = grid_pca_df[!lm_pca_outliers, ])
summary(lm_pca_no_outliers)

lm_pca_no_outliers2 <- lm(n ~ PC1 + PC2 + PC3 + PC4 + PC1:PC4 + PC3:PC4, data = grid_pca_df[!lm_pca_outliers, ])
summary(lm_pca_no_outliers2)

resids <- residuals(lm_pca_no_outliers2)
colours <- c("dark blue", "blue", "red", "dark red") 

x <- grid_pca_df[!lm_pca_outliers, "long"]
y <- grid_pca_df[!lm_pca_outliers, "lat"]

map.resids <- SpatialPointsDataFrame(data=data.frame(resids), coords=cbind(x, y)) 
spplot(map.resids, cuts=quantile(resids), col.regions=colours, cex=0.3) 

##### GWR ######
#--Calculate kernel bandwidth
gwr_bandwidth <- gwr.sel(
    formula = n ~ PC1 + PC2 + PC3 + PC4 + PC1:PC4 + PC3:PC4,
    data = grid_pca_df[!lm_pca_outliers, ],
    coords = as.matrix(grid_pca_df[!lm_pca_outliers, c("long", "lat")]),
    adapt = T)

#run the gwr model
gwr_model = gwr(
    n ~ PC1 + PC2 + PC3 + PC4 + PC1:PC4 + PC3:PC4,
    data = grid_pca_df[!lm_pca_outliers, ],
    coords = as.matrix(grid_pca_df[!lm_pca_outliers, c("long", "lat")]),
    adapt = gwr_bandwidth, hatmatrix=TRUE,
    se.fit = TRUE) 
    
#print the results of the model
gwr_model

results<-as.data.frame(gwr_model$SDF)
head(results)

#--Add coefficients
grid_pca_gwr <- grid_pca_df[!lm_pca_outliers, ]

grid_pca_gwr$coef_pc1 <- results$PC1
grid_pca_gwr$coef_pc2 <- results$PC2
grid_pca_gwr$coef_pc3 <- results$PC3
grid_pca_gwr$coef_pc4 <- results$PC4
grid_pca_gwr$coef_pc1_4 <- results$PC1.PC4
grid_pca_gwr$coef_pc3_4 <- results$PC3.PC4

##### MAP #####
bnt_osgb <- st_transform(bnt_shp, 27700)

grid_pca_gwr <- st_as_sf(grid_pca_gwr, crs = 27700, coords = c("long", "lat"), remove = FALSE) 

#--Define a plot function
plot_gwr <- function(coef){
    coefficient <- parse(text = coef)

    ggplot()+
        geom_point(data = grid_pca_gwr, aes(x = long, y= lat, colour=eval(coefficient)))+
        labs(colour = coef) +
        scale_colour_gradient2(low = "red", mid = "white", high = "blue")+
        geom_sf(data = bnt_osgb, fill = NA)+
        coord_sf()+
        theme_minimal()
}

#--Loop it
gwr_list <- names(grid_pca_gwr)[9:14]
p_gwr_list <- vector("list", length = length(gwr_list))

for (i in seq_along(gwr_list)){
    p_gwr_list[[i]] <- plot_gwr(gwr_list[[i]])
}

gridExtra::grid.arrange(grobs = p_gwr_list, ncol = 2)

########### EVALUATE ##########
#--Define a significance test function
test_sig <- function(var){
    var_se <- paste0(var, "_se")
    var_stat <- paste0(var, "_stat")

    stat <- abs(gwr_model[["SDF"]][[var]]) - 2 * gwr_model[["SDF"]][[var_se]]
    grid_pca_gwr[[var_stat]] <- round(stat, 3)

    return(grid_pca_gwr)
}

var_list <- names(gwr_model$SDF)[3:8]

for (i in seq_along(gwr_list)){
    grid_pca_gwr <- test_sig(var_list[[i]])
}

str(grid_pca_gwr)
grid_pca_gwr
