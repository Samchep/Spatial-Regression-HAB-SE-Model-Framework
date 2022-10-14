library(spatialreg)
setwd("~/Documents/Spatial Ecology Files")
setwd("~/Documents/Spatial Ecology Files/Spatial_Project")
OysterSpatial <- readOGR(dsn = ".", layer = "Oyster")
names(OysterSpatial)
print(spplot(OysterSpatial, "CPUE_numeric"))
print(spplot(OysterSpatial, "VALUE_12"))
print(spplot(OysterSpatial, "Area_numeric"))

print(spplot(OysterSpatial, "Value"))
#Creating the weights matrix, if 1 cell is a neighbor of another and proximity to others
Queen_Oyster = poly2nb(OysterSpatial)
Queen_Oyster_List = nb2listw(OysterSpatial)

Queen_Oyster_List = nb2listw(Queen_Oyster)
List_1 = Queen_Oyster_List

#Non-spatial regression 
Reg_Oyster_1 = VALUE_12 ~ CPUE__Kg_h + Area + SGArea
Reg_Parameters_Oyster_1 = lm(Reg_Oyster_1, data=OysterSpatial)
summary(Reg_Parameters_Oyster_1)


OysterSpatial$CPUE_numeric = as.numeric(OysterSpatial$CPUE__Kg_h)
OysterSpatial$Area_numeric = as.numeric(OysterSpatial$Area)
OysterSpatial$SGArea_numeric = as.numeric(OysterSpatial$SGArea)
OysterSpatial$VALUE_12_numeric = as.numeric(OysterSpatial$VALUE_12)

Reg_Oyster_V2 = VALUE_12_numeric ~ CPUE_numeric + Area_numeric + SGArea_numeric
Reg_Parameters_Oyster_V2 = lm(Reg_Oyster_V2, data=OysterSpatial)
summary(Reg_Parameters_Oyster_V2)

Reg_Oyster_Noclosure = VALUE_12_numeric ~ CPUE_numeric + Area_numeric
Reg_Parameters_Oyster_NoClosure = lm(Reg_Oyster_Noclosure, data=OysterSpatial)
summary(Reg_Parameters_Oyster_NoClosure)
#Moran's I Test
lm.morantest(Reg_Parameters_Oyster_NoClosure,List_1)
#Manski Model
Manski_Test_Oyster = sacsarlm(Reg_Parameters_Oyster_NoClosure, data = OysterSpatial, List_1, type = "sacmixed")
summary(Manski_Test_Oyster)
#Spatial Durbin Error Model
SDEM_Oyster = errorsarlm(Reg_Parameters_Oyster_NoClosure, data = OysterSpatial, List_1, na.action = na.exclude, etype = "emixed")
summary(SDEM_Oyster)

#Spatial Durbin Model
SDM_Oyster = lagsarlm(Reg_Parameters_Oyster_NoClosure, data = OysterSpatial, List_1, type = "mixed")
summary(SDM_Oyster)
#SAR Model
SAR_Oyster = lagsarlm(Reg_Parameters_Oyster_NoClosure, data = OysterSpatial, List_1)
summary(SAR_Oyster)

#plot of covariates
print(spplot(CPUE__Kg_h))

morans




