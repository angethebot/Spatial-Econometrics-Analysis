
###### Final Project: How does immigration impact Canada's housing prices - evidence from a spatial panel perspective
###### Student name: Yuxin Gong
###### Student ID: 21088401


# source("D:/Angie/Grad in UWaterloo/Term 2 W24/ECON623/Final Project/Code/Rcode.r",echo = T, max.deparse.length=1000)
# Just copy and paste this source line to console and  it'll run the whole code. Do not uncomment the comment line.
# The source doesn't work. Please ignore. Will fix later.

######################
## Set your wd here ##
######################
path_global="D:/Angie/Grad in UWaterloo/Term 2 W24/ECON623/Final Project/Code"
setwd(path_global)
sink(file=paste0(path_global,"/Yuxin Gong - Log.txt"), split=T)
# sink file actually saves itself in the working directory, but just to be safe, I specified the file 
# After opening the sink file, all output will go into the text file instead of showing up in console


########Load all the libraries
library(ggplot2)
library(tidyverse)
library(sf)
#library(raster)
library(readxl)
library(skimr)

library(tseries)

library(plm)
library(sf)
library(spdep)
library(RColorBrewer)
library(SDPDmod)
library(splm)

library(car)


######## 1. Clean the data #########
# Load the panel data and the shapefile#
province = read_xlsx("provincial.xlsx")
province = province %>% filter(time > as.Date("2008-03-01"))
anyNA(province)
province$province_factor = as.factor(province$province)
province$time = as.Date(province$time)
head(province)
skim_without_charts(province)
is.pbalanced(province)
# It is a Balanced Panel Data

#Ignore the following
#province_sf = st_as_sf(province, coords = c("lon","lat"))
#plot(province_sf)
#summary(province_sf)

# Read shapefile (map data)
province_shp=st_read("D:/Angie/Grad in UWaterloo/Term 2 W24/ECON623/Final Project/Code/shapefile_province/lpr_000b21a_e.shp")
table(province_shp$PRNAME)
province_shp$PRNAME[province_shp$PRNAME == "Newfoundland and Labrador / Terre-Neuve-et-Labrador"] = "Newfoundland and Labrador"
province_shp$PRNAME[province_shp$PRNAME == "Prince Edward Island / Île-du-Prince-Édouard"] = "Prince Edward Island"
province_shp$PRNAME[province_shp$PRNAME == "Nova Scotia / Nouvelle-Écosse"] = "Nova Scotia"
province_shp$PRNAME[province_shp$PRNAME == "New Brunswick / Nouveau-Brunswick"] = "New Brunswick"
province_shp$PRNAME[province_shp$PRNAME == "Quebec / Québec"] = "Quebec"
province_shp$PRNAME[province_shp$PRNAME == "British Columbia / Colombie-Britannique"] = "British Columbia"

#Merge map data to the panel data to plot
province_panel_test2 = full_join(province_shp, province, by=c("PRNAME" = "province"))
province_panel_shp = province_panel_test2 %>% filter(PRNAME != "Yukon") %>% filter(PRNAME != "Northwest Territories / Territoires du Nord-Ouest") %>% filter(PRNAME != "Nunavut")
class(province_panel_shp)
anyNA(province_panel_shp)
summary(province_panel_shp)
#spplot(province_panel_shp, "housing_land_index")


############################
####### Map Plots ##########
############################

#plot individual month's map
# This takes a bit time to run.
# Remove the comments if you want to run this

# format(Sys.time(), "%H:%M:%S")
# province_2023_Jan = filter(province, time == "2023-01-01")
# province_2023_Jan
# # Merge map data with data to plot
# map_data_2023_Jan = full_join(province_shp, province_2023_Jan, by = c("PRNAME"="province"))
# map_data_2023_Jan = map_data_2023_Jan %>% filter(PRNAME != "Yukon") %>% filter(PRNAME != "Northwest Territories / Territoires du Nord-Ouest") %>% filter(PRNAME != "Nunavut")
# ggplot() +
#   geom_sf(data=map_data_2023_Jan, aes(fill=housing_land_index))
# ggplot() +
#   geom_sf(data = map_data_2023_Jan, aes(fill = housing_land_index)) +
#   scale_fill_gradient(low = "orange", high = "darkred", na.value = "grey",
#                       guide = guide_colorbar(title = "Housing Land Index", 
#                                              title.position = "top",
#                                              title.theme = element_text(size = 10))) +
#   theme_minimal()
# 
# province_2023_Feb = filter(province, time == "2023-02-01")
# province_2023_Feb
# map_data_2023_Feb = full_join(province_shp, province_2023_Feb, by = c("PRNAME"="province"))
# map_data_2023_Feb = map_data_2023_Feb %>% filter(PRNAME != "Yukon") %>% filter(PRNAME != "Northwest Territories / Territoires du Nord-Ouest") %>% filter(PRNAME != "Nunavut")
# ggplot() +
#   geom_sf(data = map_data_2023_Feb, aes(fill = housing_land_index)) +
#   scale_fill_gradient(low = "orange", high = "darkred", na.value = "grey",
#                       guide = guide_colorbar(title = "Housing Land Index 2023 Feb", 
#                                              title.position = "top",
#                                              title.theme = element_text(size = 10))) +
#   theme_minimal()
# 
# 
# province_2023_Mar = filter(province, time == "2023-03-01")
# province_2023_Mar
# map_data_2023_Mar = full_join(province_shp, province_2023_Mar, by = c("PRNAME"="province"))
# map_data_2023_Mar = map_data_2023_Mar %>% filter(PRNAME != "Yukon") %>% filter(PRNAME != "Northwest Territories / Territoires du Nord-Ouest") %>% filter(PRNAME != "Nunavut")
# ggplot() +
#   geom_sf(data = map_data_2023_Mar, aes(fill = housing_land_index)) +
#   scale_fill_gradient(low = "orange", high = "darkred", na.value = "grey",
#                       guide = guide_colorbar(title = "Housing Land Index 2023 Mar", 
#                                              title.position = "top",
#                                              title.theme = element_text(size = 10))) +
#   theme_minimal()
# 
# 
# province_2023_Apr = filter(province, time == "2023-04-01")
# province_2023_Apr
# map_data_2023_Apr = full_join(province_shp, province_2023_Apr, by = c("PRNAME"="province"))
# map_data_2023_Apr = map_data_2023_Apr %>% filter(PRNAME != "Yukon") %>% filter(PRNAME != "Northwest Territories / Territoires du Nord-Ouest") %>% filter(PRNAME != "Nunavut")
# ggplot() +
#   geom_sf(data = map_data_2023_Apr, aes(fill = housing_land_index)) +
#   scale_fill_gradient(low = "orange", high = "darkred", na.value = "grey",
#                       guide = guide_colorbar(title = "Housing Land Index 2023 Apr", 
#                                              title.position = "top",
#                                              title.theme = element_text(size = 10))) +
#   theme_minimal()
# 
# 
# province_2023_May = filter(province, time == "2023-04-01")
# province_2023_May
# map_data_2023_May = full_join(province_shp, province_2023_May, by = c("PRNAME"="province"))
# map_data_2023_May = map_data_2023_May %>% filter(PRNAME != "Yukon") %>% filter(PRNAME != "Northwest Territories / Territoires du Nord-Ouest") %>% filter(PRNAME != "Nunavut")
# ggplot() +
#   geom_sf(data = map_data_2023_May, aes(fill = housing_land_index)) +
#   scale_fill_gradient(low = "orange", high = "darkred", na.value = "grey",
#                       guide = guide_colorbar(title = "Housing Land Index 2023 May", 
#                                              title.position = "top",
#                                              title.theme = element_text(size = 10))) +
#   theme_minimal()
# 
# province_2023_June = filter(province, time == "2023-04-01")
# province_2023_June
# map_data_2023_June = full_join(province_shp, province_2023_June, by = c("PRNAME"="province"))
# map_data_2023_Jan = map_data_2023_Jan %>% filter(PRNAME != "Yukon") %>% filter(PRNAME != "Northwest Territories / Territoires du Nord-Ouest") %>% filter(PRNAME != "Nunavut")
# ggplot() +
#   geom_sf(data = map_data_2023_June, aes(fill = housing_land_index)) +
#   scale_fill_gradient(low = "orange", high = "darkred", na.value = "grey",
#                       guide = guide_colorbar(title = "Housing Land Index 2023 June", 
#                                              title.position = "top",
#                                              title.theme = element_text(size = 10))) +
#   theme_minimal()
# format(Sys.time(), "%H:%M:%S")

#province_panel_test = left_join(province_shp, province, by=c("PRNAME" = "province"))
#province_panel = left_join(province, province_shp, by=c("province" = "PRNAME"))
#summary(province_panel_test)
#class(province_panel_test)
#anyNA(province_panel_test)
#na.omit(province_panel_test)
#head(province_panel_test)
#ggplot() +
#    geom_sf(data=province_panel, aes(fill=housing_land_index, geometry = geometry))
############################



##########################################################
##### 2. Data Descriptive Statistics & Visualization #####
##########################################################

#Descriptive Analysis
summary(province)

#Visualization
library(ggExtra)
Q2_scatter = ggplot(data = province, aes(x=immigrant_ratio_employmnt,y=housing_land_index, color=province)) + 
  geom_point() +
  theme(legend.position="none")+
  xlab("immigrant_ratio_employmnt")+
  ylab("Housing Land Index")+
  labs(color = "Province")+  
  theme(legend.position = "right")
ggMarginal(Q2_scatter, type="histogram")
# From the scatter plot, we can see that the housing index is increasing as immigration increases.

library(foreign)
coplot(housing_land_index~time|province,type="l",data=province)

library(foreign)
coplot(immigrant_ratio_employmnt~time|province,type="l",data=province)

#the bar is just ordered in alphabetic order
# This shows the similar conclusion as the scatter plot.



##################################
##### 3. Pre-regression test #####
##################################

####################
### Stationarity ###
####################

# To test for stationarity in panel data, you can use various statistical tests. One common test is the Augmented Dickey-Fuller (ADF) test, which is commonly used to test for the presence of a unit root in a time series. 
#install.packages("tseries")

adf.test(province$housing_land_index)
ggplot(province, aes(x = time, y = housing_land_index, group = province, color = province)) +
  geom_line() +
  labs(x = "Year", y = "Housing Land Index", color = "Province") +
  theme_minimal()


#adf.test(province$housing_index_log)
#ggplot(province, aes(x = time, y = housing_index_log, group = province, color = province)) +
#  geom_line() +
#  labs(x = "Year", y = "Log Housing Land Index", color = "Province") +
#  theme_minimal()
# The housing index is stationary, it is appropriate to add a temporal lag term in my econometric model

# adf.test(province$housing_index_growth)
# housing index growth is stationary

#ggplot(province, aes(x = time, y = immigrant_ratio_employmnt, group = province, color = province)) +
#  geom_line() +
#  labs(x = "Year", y = "Immigration", color = "Province") +
#  theme_minimal()
# However, when dealing with panel data, you need to consider panel-specific tests that account for both individual heterogeneity and time-series properties.
#One such test is  The Levin–Lin–Chu test, which extends the ADF test for panel data. The IPS test considers both the cross-sectional and time-series dimensions of the panel data.
library(plm)
head(province)

purtest(housing_index_log ~ trend, data = province, lags="AIC", pmax =2, index = c("province", "time"), test="levinlin")
#purtest(housing_land_index ~ trend, data = province, lags="AIC", pmax =2, index = c("province", "time"), test="levinlin")
#purtest(housing_index_growth ~ trend, data = province, lags="AIC", pmax =2, index = c("province", "time"), test="levinlin")
# stationary

#################################
###### Spatial Effects ##########
#################################
head(province)

########### IGNORE #########
#Ignore the following
#province$resid = residuals(panel_pooled)
#table(province$province)
#resid_alberta = province[province$province == "Alberta",]$resid
#resid_bc = province[province$province == "British Columbia",]$resid
#resid_manitoba = province[province$province == "Manitoba",]$resid
#resid_nb = province[province$province == "New Brunswick",]$resid
#resid_nl = province[province$province == "Newfoundland and Labrador",]$resid
#resid_ns = province[province$province == "Nova Scotia",]$resid
# resid_ontario = province[province$province == "Ontario",]$resid
# resid_pei = province[province$province == "Prince Edward Island",]$resid
# resid_quebec = province[province$province == "Quebec",]$resid
# resid_sk = province[province$province == "Saskatchewan",]$resid
# length(resid_nb)
# 
# residuals_df = data.frame(
#   Alberta = resid_alberta,
#   BC = resid_bc,
#   Manitoba = resid_manitoba,
#   NewBrunswick = resid_nb,
#   NewfoundlandLabrador = resid_nl,
#   NovaScotia = resid_ns,
#   Ontario = resid_ontario,
#   PEI = resid_pei,
#   Quebec = resid_quebec,
#   Saskatchewan = resid_sk
# )
# summary(residuals_df)
# correlation_matrix = cor(residuals_df)
# library(reshape2) # For melt function
# melted_corr_matrix <- melt(correlation_matrix)
# ggplot(melted_corr_matrix, aes(Var1, Var2, fill = value)) +
#   geom_tile() +
#   scale_fill_gradient(low = "blue", high = "red") +
#   labs(title = "Correlation Matrix of Residuals by Province") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
########################


################ IGNORE ##################
#map_2023_Jan, map_2023_Feb, map_2023_Mar, map_2023_Apr, map_2023_May, map_2023_June
#Use map_2023_Jan first
#(1) define the neighbors
#neighbor = poly2nb(map_data_2023_Jan, queen = TRUE)
#The first step in a Moran’s I analysis requires that we define “neighboring” polygons. This could refer to contiguous polygons, polygons within a certain distance, or it could be non-spatial in nature and defined by social, political or cultural “neighbors”.
#Here, we’ll adopt a contiguous neighbor definition. We’ll accept any contiguous polygons that share at least one vertex; this is the “queen” case (if one chooses to adopt the chess analogy) and it’s parameterized as queen = TRUE in the call to poly2nb. If we required that just edges be shared between polygons then we would set queen = FALSE (the rook case).
# The above function takes too long, I'll try knearest neighbors

# Try 2023 Jan data
#coords_Jan = st_centroid(st_geometry(map_data_2023_Jan), of_largest_polygon=TRUE)
#col.knn_Jan = knearneigh(coords_Jan,k=2)
#neighbor_Jan = knn2nb(col.knn_Jan)
# (2) Get weights
#weights_Jan = nb2listw(neighbor_Jan, style="B")
# (3) Conduct Moran's I test
#ols_Jan = lm(housing_land_index ~ immigrant_ratio_employmnt, data=province_2023_Jan)
#lm.morantest(ols_Jan,weights_Jan)
# p-value = 0.6554 Do no reject null: there is no spatial autocorrelation in the data. 

# Try with 2023 Feb data
#coords_Feb = st_centroid(st_geometry(map_data_2023_Feb), of_largest_polygon=TRUE)
#col.knn_Feb = knearneigh(coords_Feb,k=2)
#neighbor_Feb = knn2nb(col.knn_Feb)
# (2) Get weights
#weights_Feb = nb2listw(neighbor_Feb, style="B")
# (3) Conduct Moran's I test
#ols_Feb = lm(housing_land_index ~ immigrant_ratio_employmnt, data=province_2023_Feb)
#lm.morantest(ols_Feb,weights_Feb)
# p-value = 0.6621 Do no reject null:  there is no spatial autocorrelation in the data. 
##############################


# Try with the whole panel data
# Short of memory
# 
# # Try 2023-Jan Data
# province_2023_Jan = filter(province_panel_shp, time == "2023-01-01")
# coords_2023_Jan= st_centroid(st_geometry(province_2023_Jan))
# col.knn_2023_Jan = knearneigh(coords_2023_Jan,k=2)
# neighbor_2023_Jan = knn2nb(col.knn_2023_Jan)
# weights_2023_Jan = nb2listw(neighbor_2023_Jan, style="B")
# weights_2023_Jan
# res3=blmpSDPD(housing_land_index ~ immigrant_ratio_employmnt, 
#               data   = province_2023_Jan, 
#               W      = W,
#               index  = c("PRNAME","time"),
#               model  = list("sem"), 
#               effect = "twoways",
#               prior  = "beta")
# 
# panel_ols_2023_Jan = lm(housing_land_index ~ immigrant_ratio_employmnt, data=province_2023_Jan)
# lm.morantest(panel_ols_2023_Jan,weights_2023_Jan)

# #  Try 2023-Jan & Feb Data
# province_2023_Jan_Feb = filter(province_panel_shp, time == "2023-01-01" | time == "2023-02-01")
# coords_2023_Jan_Feb = st_centroid(st_geometry(province_2023_Jan_Feb))
# col.knn_2023_Jan_Feb = knearneigh(coords_2023_Jan_Feb,k=2)
# neighbor_2023_Jan_Feb = knn2nb(col.knn_2023_Jan_Feb)
# weights_2023_Jan_Feb = nb2listw(neighbor_2023_Jan_Feb, style="B")
# weights_2023_Jan_Feb
# panel_ols_2023_Jan_Feb = lm(housing_land_index ~ immigrant_ratio_employmnt, data=province_2023_Jan_Feb)
# lm.morantest(panel_ols_2023_Jan_Feb,weights_2023_Jan_Feb)

#  Try 2023-Jan & Feb & Mar Data
province_2023_Jan_Feb_Mar = filter(province_panel_shp, time == "2023-01-01" | time == "2023-02-01" | time == "2023-03-01")
coords_2023_Jan_Feb_Mar = st_centroid(st_geometry(province_2023_Jan_Feb_Mar))
col.knn_2023_Jan_Feb_Mar = knearneigh(coords_2023_Jan_Feb_Mar,k=2)
neighbor_2023_Jan_Feb_Mar = knn2nb(col.knn_2023_Jan_Feb_Mar)
weights_2023_Jan_Feb_Mar = nb2listw(neighbor_2023_Jan_Feb_Mar, style="B")
weights_2023_Jan_Feb_Mar
panel_ols_2023_Jan_Feb_Mar = lm(housing_land_index ~ immigrant_ratio_employmnt, data=province_2023_Jan_Feb_Mar)
lm.morantest(panel_ols_2023_Jan_Feb_Mar,weights_2023_Jan_Feb_Mar)
# This already pass the Moran I test, it is safe to conclude that the prices are auto-correlated spatially

#Try 2023 yearly data
province_panel_2023 = province_panel_shp %>% filter (time > as.Date("2022-12-01"))
province_panel_2023_data = province %>% filter (time > as.Date("2022-12-01"))
# (1) Get coordinates 
coords_2023= st_centroid(st_geometry(province_panel_2023))
col.knn_2023 = knearneigh(coords_2023,k=2)
neighbor_2023 = knn2nb(col.knn_2023)
# (2) Get weights
weights_2023 = nb2listw(neighbor_2023, style="B")
weights_2023
length(weights_2023)
# (3) Conduct Moran's I test
panel_ols_2023 = lm(housing_land_index ~ immigrant_ratio_employmnt, data=province_panel_2023_data)
lm.morantest(panel_ols_2023,weights_2023)
#p-value < 2.2e-16, with a slight larger dataset, we get to reject the null and accept the alternative: there is spatial autocorrelation in the data
#Hence, it is fair to include a spatial elements in the regression 


######################################
#### Pooled OLS or Random Effects ####
######################################
panel_pooled_test = plm(housing_land_index ~ lag(housing_land_index) + immigrant_ratio_employmnt + population_density + unemployment + wage + CPI + COVID, index=c("province","time"), data = province, model = "pooling")
summary(panel_pooled_test)
# There is a possibility that there is still unit root in houosing_land_index, so I decide not to use time lag for now
panel_pooled = plm(housing_land_index ~ immigrant_ratio_employmnt + population_density + unemployment + wage + CPI + COVID, index=c("province","time"), data = province, model = "pooling")
summary(panel_pooled)
vif(panel_pooled)
LM_test = plmtest(panel_pooled,type=c("bp"))
print(LM_test)

#The LM test helps you decide between a random effects regression and a simple OLS regression. The null hypothesis in the LM test is that variances across entities is zero. This is, no  significant difference across units (i.e. no panel effect)
# p-value < 2.2e-16 suggests: reject the null, we need to use panel data models


##### Fixed or random effects #### Breusch-Pagan Lagrange multiplier (LM) test
panel_fixed = plm(housing_land_index ~ immigrant_ratio_employmnt + population_density + unemployment + wage + CPI + COVID, index=c("province","time"), data = province, model = "within")
summary(panel_fixed)

panel_random = plm(housing_land_index ~ immigrant_ratio_employmnt + population_density + unemployment + wage + CPI + COVID,index=c("province","time"), data = province, model = "random")
summary(panel_random)

hausman_test = phtest(panel_fixed,panel_random)
print(hausman_test)
#In the context of the Hausman test, the null hypothesis typically states that the coefficients estimated under the fixed effects model are consistent and efficient, meaning that the fixed effects model is the preferred model for your panel data.
#The alternative hypothesis is that the coefficients estimated under the random effects model are consistent and efficient, indicating that the random effects model is the preferred model for your data.
#In summary, the null hypothesis for the Hausman test is that there is no difference between the coefficients estimated under the fixed effects model and the random effects model. The alternative hypothesis is that there is a difference between the coefficients estimated under the two models.
# p-value = 7.053e-11. The Hausman test suggests that the random effects model should be adopted.



##############################
###### Collinearity ##########
##############################
vif(panel_random)
# I Want to remove CPI first because it is too high in VIF

panel_random2 = plm(housing_land_index ~ immigrant_ratio_employmnt + population_density + unemployment + wage + COVID,index=c("province","time"), data = province, model = "random")
summary(panel_random2)
vif(panel_random2)
# Now everything looks nice and safe


#################################
######### REGRESSION ############
#################################

############################
###### Panel Regression ####
############################
summary(panel_random2)


################################
######  Spatial Regression #####
################################

########### Ignore #############
# ### Method 1 of creating weight matrix ###
# coords_2023= st_centroid(st_geometry(province_panel_2023))
# col.knn_2023 = knearneigh(coords_2023,k=2)
# neighbor_2023 = knn2nb(col.knn_2023)
# # (2) Get weights
# weights_2023 = nb2listw(neighbor_2023, style="B")
# lm.morantest(panel_ols_2023,weights_2023)
# 
# ### Method 2 of creating weight matrix ###
# shapefile_centroid = st_centroid(st_geometry(province_panel_2023)) 
# nb.d125 = dnearneigh(shapefile_centroid,0,125000,row.names=shapefile_centroid$province)
# mat.d125 = nb2mat(nb.d125,glist=NULL,style="W",zero.policy=TRUE)
# listd125 = mat2listw(mat.d125, style="W")
# listd125
# lm.morantest(panel_ols_2023,listd125)
###################################

### Method 3 of creating weight matrix (manually) ###
# See spatial weight matrix details:
# example: https://cran.r-project.org/web/packages/SDPDmod/vignettes/spatial_model.html
# spatial matrix 1: https://cran.r-project.org/web/packages/SDPDmod/vignettes/spatial_matrices.html
# spatial mareix 2: https://cran.r-project.org/web/packages/spNetwork/vignettes/SpatialWeightMatrices.html
province_list = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan")
spatial_weight = matrix(
  c(0,1,0,0,0,0,0,0,0,1,
    1,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,1,0,0,1,
    0,0,0,0,0,1,0,0,1,0,
    0,0,0,0,0,1,0,0,1,0,
    0,0,0,1,0,0,0,1,0,0,
    0,0,1,0,0,0,0,0,1,0,
    0,0,0,1,0,1,0,0,0,0,
    0,0,0,1,1,0,1,0,0,0,
    1,1,0,0,0,0,0,0,0,0),
  nrow=10,
  ncol=10,
  byrow = TRUE
)
rownames(spatial_weight) = province_list
colnames(spatial_weight) = province_list
str(spatial_weight)
W = rownor(spatial_weight)



formula1= housing_land_index ~ immigrant_ratio_employmnt + population_density + unemployment + wage + COVID
formula2= housing_land_index ~ immigrant_ratio_employmnt + population_density + unemployment + wage

######### IGNORE ##############
# ## Example 1 ####
# data("Cigar",package = "plm")
# data("usa46",package="SDPDmod")
# summary(Cigar)
# data1 = Cigar
# data1$logc<-log(data1$sales)
# data1$logp<-log(data1$price/data1$cpi)
# data1$logy<-log(data1$ndi/data1$cpi)
# data1$lpm<-log(data1$pimin/data1$cpi)
# W1= rownor(usa46)
# mod1= SDPDm(formula = logc ~ logp+logy, data = data1, W = W1,
#             index = c("state","year"),
#             model = "sar", 
#             effect = "individual")
# summary(mod1)
# summary(data1)
# str(data1)
# 
# table(data1$state)
# table(data1$year)
# rownames(W1)
# 
# ## Example 2 ###
# data(Produc, package = "plm") 
# data(usaww, package = "splm") 
# head(Produc)
# table(Produc$table)
# table(Produc$year)
# view(Produc)
# row(usaww)
# class(usaww)
# class(Produc)
# str(Produc)
# form1 = log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp
# mod1 = SDPDm(formula = form1, 
#              data = Produc, 
#              W = usaww, 
#              index = c("state","year"), 
#              model = "sar",
#              effect = "individual", 
#              LYtrans = TRUE)
# mod1
# summary(mod1)
# impactsSDPDm(mod1)
###########################



# 2023 Yearly data
## random effects panel with spatial lag, included spatial lag (autoregressive) as well as spatial error (SDEM)
# ML panel with spatial lag, random effects, spatial error correlation 
# Estimation: Maximum likelihood https://cran.r-project.org/web/packages/splm/splm.pdf
spatial_random_SDEM_2023 = spml(formula2, data = province_panel_2023_data, listw = spdep::mat2listw(W,style="W"), model="random", spatial.error="b", lag=TRUE)
# Still unable tp include COVID data
summary(spatial_random_SDEM_2023) 
## calculate impact measures 
impact_random_SDEM_2023 = impacts(spatial_random_SDEM_2023, listw = spdep::mat2listw(W, style = "W"), time = 17) 
summary(impact_random_SDEM_2023, zstats=TRUE, short=TRUE)
# The p-value of impact from immigrant_ratio_employmnt are all pretty small and significant


# Whole data
## random effects panel with spatial lag 
spatial_random_SDEM = spml(formula2, data = province, listw = spdep::mat2listw(W,style="W"), model="random", spatial.error="b", lag=TRUE)
summary(spatial_random_SDEM) 
## calculate impact measures 
impact_random_SDEM = impacts(spatial_random_SDEM, listw = spdep::mat2listw(W, style = "W"), time = 17) 
summary(impact_random_SDEM , zstats=TRUE, short=TRUE)
# The p-value of impact from immigrant_ratio_employmnt are all pretty small and significant
# random effects panel without spatial lag (just SEM)
spatial_random_SEM = spml(formula2, data = province, listw = spdep::mat2listw(W,style="W"), model="random", spatial.error="b", lag=F)
summary(spatial_random_SEM) 
# Impacts Estimates are not available for Error Model, as there will not be spillover effects given that there is only spatial error


###############################
#### Policy Impact Effect #####
###############################
# Here we only use the data for Ontario, so it will be a time series data, which we treat as a pooled dataset

# Ontario Subgroup regressions before and after 2017 (before including 2017, after starting from 2018)
ontario = province %>% filter(province=="Ontario")
ontario_before2017 = ontario %>% filter(time < as.Date("2018-01-01"))
ontario_after2017 = ontario %>% filter(time > as.Date("2017-12-01"))

# Before_OLS
ols_ontario_before = lm(formula1, data=ontario_before2017)
summary(ols_ontario_before)
# Coef for immigration is sig

# After_OLS
ols_ontario_after = lm(formula1, data=ontario_after2017)
summary(ols_ontario_after)
# Coef for immigration is no longer sig

# Now include a dummy for tax
formula3 = housing_land_index ~ immigrant_ratio_employmnt + population_density + unemployment + wage + COVID + after_tax
formula4 = housing_land_index ~ immigrant_ratio_employmnt + population_density + unemployment + wage + COVID + after_tax + immigrant_ratio_employmnt*after_tax
ols_ontario_dummy = lm(formula3, data=ontario)
ols_ontario_interaction = lm(formula4, data=ontario)

summary(ols_ontario_dummy)
summary(ols_ontario_interaction)
# As it's shown, the impact of immigration is positive, tax impact is negative. And indeed there is an interaction between immigration and tax. With tax, the impact from immigration is lowered.


############
### EOF ####
############
sink()
