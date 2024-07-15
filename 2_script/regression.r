names(poi_bnt_fin)
####### REGRESSION ###
#--Single linear regression
mod_parking <- map(ct_all, ~lm(n ~ amenity_parking, data = .x) |> summary()) 
#significant, predictive power increases with larger grid cells
cor(test$amenity_parking, test$n)

mod_garage <- map(ct_all, ~lm(n ~ building_garage, data = .x) |> summary()) #not significant

mod_bicycle <- map(ct_all, ~lm(n ~ amenity_bicycle_parking, data = .x) |> summary())
#significant, predictive power increases with larger grid cells

leaflet(poi_bnt[["amenity_bicycle_parking"]]) |>
leaflet::addTiles() |>
leaflet::addCircleMarkers(radius = 0.3)
#bicycle parking places tend to be on high streets

cor(test$amenity_bicycle_parking, test$amenity_parking)
#0.3 = weak to moderate level of correlation

mod_school <- map(ct_all, ~lm(n ~ amenity_school, data = .x) |> summary())
#significant, predictive power increases with larger grid cells

leaflet(poi_bnt[["amenity_school"]]) |>
leaflet::addTiles() |>
leaflet::addPolygons()



mod4 <- lm(n ~ amenity_parking + building_garage+amenity_bicycle_parking+park+building_garages+amenity_school+amenity_place_of_worship+amenity_restaurant+amenity_cafe+shop_convenience+shop_hairdresser+shop_clothes+shop_beauty+office_estate_agent+amenity_atm+amenity_pharmacy+amenity_toilets+amenity_pub, test)

step(mod4, direction = "backward")

mod5 <- lm(formula = n ~ amenity_parking + amenity_bicycle_parking +
    amenity_school + amenity_restaurant + shop_convenience +
    shop_hairdresser + shop_clothes + shop_beauty + office_estate_agent +
    amenity_atm + amenity_toilets + amenity_pub, data = test)

summary(mod5)

# make prediction
test$cr_pred <- predict(mod5, test)

# calculating RMSE
sqrt(mean((test$cr_pred - test$n)^2))

range(test$n)