######### SETTING ##########
#--Install / load packages
pacman::p_load(sp, data.table, rio, sf, here, leaflet, gstat, tidyverse, Metrics, scales, corrr, ggcorrplot, FactoMineR, factoextra, corrplot)

#--Import street-level asb data
asb <- import(here("3_output", "asb_with_nearest_distances.csv"))

#--Get only numerical data
asb_fin <- asb |> select(starts_with('d_'))

#--Inspect data
round(colMeans(asb_fin), 2)
round(apply(asb_fin, 2, sd), 2)
    #means & standard deviations vary hence the data needs to be centered and scaled 

#--Normalise / standardise (Xi-mean(X)/sd(X))
asb_norm <- scale(asb_fin)
mean(asb_norm); sd(asb_norm)
    #~0; ~1

####### USING PCA() #######
#--PCA
pca <- PCA(asb_norm, graph = FALSE, scale.unit = FALSE)

#--Eigenvalues
eig <- get_eigenvalue(pca)
eig

#--Scree plot
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 25)) 
    #first 7 principal components altogether account for 71.4% of variances contained in the data

var <- get_pca_var(pca)
    #coord
    #cos2: square cosine / coordinates; quality on the factor map
    #contrib: contirubtions to the principal components

#--Correlation circle
fviz_pca_var(pca, col.var = "black")
    #Correlation between a variable and a PC is used as the coordinates of the variable on the PC 

#--Quality of representation (how well the variable is represnted by PC)
#---View cos2 value in each dimension
var$cos2[,1] |> sort(TRUE) |> head(10)  
var$cos2[,2] |> sort(TRUE) |> head(10) 
var$cos2[,3] |> sort(TRUE) |> head(10)
var$cos2[,4] |> sort(TRUE) |> head(10)
var$cos2[,5] |> sort(TRUE) |> head(10)
    #PC1: Best represents distances to commercial and service points (electronics shop, car repair shop or verterinary)
    #PC2: Best represents distances to everyday services and retail locations (bakery, clothes, bank)
    #PC3: Best represents distances to bars, beauty services, and healthcare facilities
    #PC4: Best represents distances to utility and car-related services (post depot, car dealer, post office)
    #PC5: Best represents distances to car washes, graveyards, and hospitals

#---Correlation plot
corrplot(var$cos2, is.corr=FALSE)

#---Bar chart
fviz_cos2(pca, choice = "var", axes = 1)
fviz_cos2(pca, choice = "var", axes = 2)
fviz_cos2(pca, choice = "var", axes = 3)
fviz_cos2(pca, choice = "var", axes = 4)
fviz_cos2(pca, choice = "var", axes = 5)

fviz_cos2(pca, choice = "var", axes = 1:2)
fviz_cos2(pca, choice = "var", axes = 1:3)
fviz_cos2(pca, choice = "var", axes = 1:4)
fviz_cos2(pca, choice = "var", axes = 1:5)

#---Correlation circle coloured by cos2 values
fviz_pca_var(pca, col.var = "cos2", axes = c(1,2),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE 
             )
fviz_pca_var(pca, col.var = "cos2", axes = c(1,3),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE 
             )
fviz_pca_var(pca, col.var = "cos2", axes = c(2,3),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE 
             )

#--Contribution (how much each variable contributes to the construction of a principal component)
#---View contribution values
var$contrib[,1] |> sort(TRUE) |> head(10) 
var$contrib[,2] |> sort(TRUE) |> head(10) 
var$contrib[,3] |> sort(TRUE) |> head(10)
var$contrib[,4] |> sort(TRUE) |> head(10)
var$contrib[,5] |> sort(TRUE) |> head(10)

#---Corelation plot
corrplot(var$contrib, is.corr=FALSE)    

#----Bar chart
fviz_contrib(pca, choice = "var", axes = 1, top = 10)
fviz_contrib(pca, choice = "var", axes = 2, top = 10)
fviz_contrib(pca, choice = "var", axes = 3, top = 10)
fviz_contrib(pca, choice = "var", axes = 1:3, top = 10)
fviz_contrib(pca, choice = "var", axes = 1:5, top = 10)

#---Correlation circle coloured by contribution
fviz_pca_var(pca, col.var = "contrib", axes = c(1,2),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
             )
fviz_pca_var(pca, col.var = "contrib", axes = c(1,3),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
             )
fviz_pca_var(pca, col.var = "contrib", axes = c(2,3),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
             )

#--Colour by groups
#---Create a grouping variable using kmeans
#----Create 3 groups of variables (centers = 3)
set.seed(123)
km3 <- kmeans(var$coord, centers = 3, nstart = 25)
grp3 <- as.factor(km$cluster)


#---Corrleation circle coloured by groups
fviz_pca_var(pca, col.var = grp, axes = c(1,2),
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")

#--Dimension description: most significantly associated variables with a given PC
desc <- dimdesc(pca, axes = c(1,2), proba = 0.05)
desc$Dim.1
desc$Dim.2


####### USING PRINCOMP() ######
#--Correlation matrix
corr_matrix <- cor(asb_norm)
ggcorrplot(corr_matrix)

high_corr <- which(corr_matrix>0.7 & corr_matrix != 1, arr.ind = TRUE) |> 
    as.data.frame() |> 
    arrange(row) |> 
    mutate(poi = colnames(corr_matrix)[high_corr$col])

#--Apply PCA
pca2 <- princomp(corr_matrix)
summary(pca2)
pca2$loadings[,1:3] |> as.data.frame() |> arrange(desc(Comp.3))
    #alcohol, fuel, electronics, car repair, laundry (comp1); doctors, layer, houseware, clothes, post-secondary institutions (comp2); beauty, hairdresser, pharmacy, social facility, dentist (comp3)

#--Variance & cumulative variance
var <- pca2$sdev^2
(pve <- var / sum(var))
cumsum(pve)


#--Scree plot: Eigenvalue
#---Visualise the importance of each principal component & determine the number of principal components to retain
fviz_eig(pca2, addlabels = TRUE)
    # first three components can be considered most significant
    # they contain 77.1% of total information of the data 

#--Biplot
#---Visualise the similarities & dissimilarities between the samples & show the impact of each attribute on each principal component
fviz_pca_var(pca2, axes = c(1,2), col.var = "black")
fviz_pca_var(pca2, axes = c(1,3), col.var = "black")
fviz_pca_var(pca2, axes = c(2,3), col.var = "black")

#--Contribution of each variable
#---In dimension 1-3
fviz_cos2(pca2, choice = "var", axes = 1:3)
    #alcohol, fuel, bridge, electronics, car repair
    #areas closer to certain commercial and automotive places can be potentially hotspots for anti-social behaviour like public intoxication and vandalism 

#---In dimension 1-2
fviz_cos2(pca2, choice = "var", axes = 1:2)
    #alchol, fuel, car repair, bridge, verterinary 

#--Biplot combined with cos2
fviz_pca_var(pca2, col.var = "cos2", axes=1:2,
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
            repel = TRUE)
            # fuel, electronics, car repair, vertinary - high contribution & high positive correlation 
fviz_pca_var(pca2, col.var = "cos2", axes=c(1,3),
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
            repel = TRUE)
fviz_pca_var(pca2, col.var = "cos2", axes=2:3,
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
            repel = TRUE)

fviz_pca_biplot(pca2, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
                )

#### GEOGRAPHICALLY WEIGHTED PCA ####
pacman::p_load(GWmodel, sp, spdep, gstat, RColorBrewer, classInt, raster, gridExtra, tidyverse)

#--PCA
pca3 <- princomp(asb_norm, cor = FALSE)
(pca3$sdev^2 / sum(pca3$sdev^2)) * 100
pca3$loadings

#--Geographically weighted PCA
#---Create a dataframe with scaled data 
coords <- as.matrix(cbind(asb$location.latitude, asb$location.longitude))
scaled.spdf <- SpatialPointsDataFrame(coords, as.data.frame(asb_norm))

bw.gw.pca <- bw.gwpca(scaled.spdf, 
                      vars = colnames(scaled.spdf@data),
                      k = 3,
                      robust = FALSE, 
                      adaptive = TRUE)

bw.gw.pca

gw.pca<- gwpca(scaled.spdf, 
               vars = colnames(scaled.spdf@data), 
               bw=bw.gw.pca,
               k = 3, 
               robust = FALSE, 
               adaptive = TRUE)