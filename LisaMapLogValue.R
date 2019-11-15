#useful links that helped along the way
#http://rstudio-pubs-static.s3.amazonaws.com/4938_b5fc230d586c48b291627ff6ea484d2e.html
#https://rstudio-pubs-static.s3.amazonaws.com/176549_91ca98ae396e416e998542559536da33.html

#packages used during this process at one point)

library(maptools)
library(rgdal)
library(spdep)
library(gstat)
library(classInt)
library(raster)
library(rio)
library(dplyr)
library(stringr)
library(sp)
library(sas7bdat)
library(prettymapr)

#Load in from .shp from census#
MAP <- readOGR("file/location")

#Need to remove middle rows before adding AND add dashes('-') #
d <- read.csv("file/location", stringsAsFactors = FALSE)
d <- subset(d, d$City == "SF")

#Dont use this merge
MAP <- merge(MAP, d, by.x = "AFFGEOID", by.y = "AFFGEOID")

#use THIS merge
MAP@data = data.frame(MAP@data,d[match(MAP@data$AFFGEOID, d$AFFGEOID),])

#Found this code online that easily omits NA's from a spatial file#
require(sp)


sp.na.omit <- function(x, margin=1) {
  if (!inherits(x, "SpatialPointsDataFrame") & !inherits(x, "SpatialPolygonsDataFrame")) 
    stop("MUST BE sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame CLASS OBJECT") 
  na.index <- unique(as.data.frame(which(is.na(x@data),arr.ind=TRUE))[,margin])
  if(margin == 1) {  
    cat("DELETING ROWS: ", na.index, "\n") 
    return( x[-na.index,]  ) 
  }
  if(margin == 2) {  
    cat("DELETING COLUMNS: ", na.index, "\n") 
    return( x[,-na.index]  ) 
  }
}

#End OmitCode thing
  
MAP2 <- sp.na.omit(MAP)
dim(MAP2)

#2006
MAP2 <- MAP2[(MAP2$B25077e1>0),]
MAP2 <- MAP2[(MAP2$B08303e1>0),]

#2017
MAP2 <- MAP2[(MAP2$B25077e1>0),]
MAP2 <- MAP2[(MAP2$B08134e1>0),]

#Convert from Factor to numeric# 
#2006 B08303e1, 2017 B08134e1 (ALSO NEED TO USE 'DOUBLE' CLASS FOR WLR)#

#2006 Blank values given zeroes
MAP2$CalcWLR <- MAP2$B08303e1 / (MAP2$ALAND10 * 0.000247105)
MAP2$CalcWLR2 <- as.double(log(MAP2$CalcWLR))
MAP2$logValue2 <- as.double(log(MAP2$B25077e1))

#2017 Blank values given zeroes, then removed (WLR and logValue)
MAP2$CalcWLR <- MAP2$B08134e1 / (MAP2$ALAND.1 * 0.000247105)
MAP2$CalcWLR2 <- as.double(log(MAP2$CalcWLR))
MAP2$logValue2 <- as.double(log(MAP2$B25077e1))

#Choose Queen's neighbor's as weights
MAP2_m_nbq <- poly2nb(MAP2, queen=T)
MAP2_m_nbq_w <- nb2listw(MAP2_m_nbq, style="W", zero.policy = TRUE)
locm_MAP <- as.data.frame(localmoran(MAP2$CalcWLR2, MAP2_m_nbq_w, zero.policy = TRUE, na.action = na.omit))
%%
  
#KNearneighbor, insert number of neighbors 'K=8' between last two commas in knn#
MAP2.dists <- as.matrix(dist(cbind(MAP2$X_Centroid, MAP2$Y_Centroid), method = "euclidean"))

MAP2.dists.knn <- knearneigh(cbind(MAP2$X_Centroid, MAP2$Y_Centroid), k=8)
MAP2.dists.nb <- knn2nb(MAP2.dists.knn)

MAP2.dists.listw <- nb2listw(MAP2.dists.nb, zero.policy = TRUE)

locm_MAP <- as.data.frame(localmoran(MAP2$logValue, MAP2.dists.listw, zero.policy = TRUE))
##

#DNearest, choose neighbors within distance threshold
MAP2.dists.dnn <- dnearneigh((cbind(MAP2$X_Centroid, MAP2$Y_Centroid)), d1 = 0, d2 = 1.5, longlat = TRUE)  
MAP2.dists.dnb <- nb2listw(MAP2.dists.dnn, style="W", zero.policy = TRUE)
locm_MAP <- as.data.frame(localmoran(MAP2$CalcWLR2, MAP2.dists.dnb, zero.policy = TRUE))

#Gives you the Local Moran's stat for an area
moransEmployDense <- moran.test(MAP2$CalcWLR2, MAP2.dists.dnb, zero.policy = TRUE)
moransEmployDense

#Use MonteCarlo Sim too (not necessary)
MonteCarlo <- moran.mc(MAP2$CalcWLR2, MAP2.dists.dnb, nsim = 999, zero.policy = TRUE)
MonteCarlo
##
 
#Creating the Local Moran's Map of whatever city you're looking at
  
Whatever_MAP <- SpatialPolygonsDataFrame(MAP2, locm_MAP, match.ID = FALSE)

quadrant <- vector(mode= "numeric", length = nrow(Whatever_MAP))
worklandmean3 <- MAP2$CalcWLR2 - mean(MAP2$CalcWLR2)
c_mi <- Whatever_MAP$Ii - mean(Whatever_MAP$Ii)

signif <- 0.1

quadrant[worklandmean3 < 0 & c_mi < 0] <- 1 
quadrant[worklandmean3 < 0 & c_mi > 0] <- 2
quadrant[worklandmean3 > 0 & c_mi < 0] <- 3
quadrant[worklandmean3 > 0 & c_mi > 0] <- 4 
quadrant[locm_MAP[, 5] > signif] <- 0


setwd("Y:/Dan/HousingPaper/DataSets/OrganizedFindings/MoranMaps")

#Code used to design map, taken in part from http://rstudio-pubs-static.s3.amazonaws.com/4938_b5fc230d586c48b291627ff6ea484d2e.html
brks <- c(0, 1, 2, 3, 4)
colors <- c(rgb(0.7, 0.7, 0.6, alpha = 0.4), "blue", rgb(0, 0, 1, alpha = 0.4), rgb(1, 0, 0, alpha = 0.4), 
            "red")

jpeg(file = "NYC2012_16.jpeg", width = 2096, height = 1600)
plot(MAP2, col = colors[findInterval(quadrant, brks)], lwd = 0.1)
box()
legend("topleft", legend = c("insignificant", "low-low", "low-high", "high-low", 
                             "high-high"), fill = colors, bty = "n", cex = 3.0, y.intersp = 1, x.intersp = 1)

addscalebar(plotepsg = 4326, pos = "bottomright", htin = 0.5, padin = c(0.2,0.2), label.cex = 2.5, tick.cex = 2.5)
title("New York City, 2012-2016", cex.main = 3.5)
dev.off()
