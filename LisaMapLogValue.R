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
MAP <- readOGR("Y:/Dan/HousingPaper/ArcGIS/SFShape.shp")

#Need to remove middle rows before adding AND add dashes('-') #
d <- read.csv("Y:/Dan/HousingPaper/Rstudio/2017_10Cities.csv", stringsAsFactors = FALSE)
d <- subset(d, d$City == "SF")

#Dont use this merge
MAP <- merge(MAP, d, by.x = "AFFGEOID", by.y = "AFFGEOID")

#use THIS merge
MAP@data = data.frame(MAP@data,d[match(MAP@data$AFFGEOID, d$AFFGEOID),])

#Omit Code thing#
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

%End OmitCode thing%
  
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

%QueenNeighbors%
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

#DNearest, choose#
MAP2.dists.dnn <- dnearneigh((cbind(MAP2$X_Centroid, MAP2$Y_Centroid)), d1 = 0, d2 = 1.5, longlat = TRUE)  
MAP2.dists.dnb <- nb2listw(MAP2.dists.dnn, style="W", zero.policy = TRUE)
locm_MAP <- as.data.frame(localmoran(MAP2$CalcWLR2, MAP2.dists.dnb, zero.policy = TRUE))
moransEmployDense <- moran.test(MAP2$CalcWLR2, MAP2.dists.dnb, zero.policy = TRUE)
moransEmployDense

#Use MonteCarlo Sim too
MonteCarlo <- moran.mc(MAP2$CalcWLR2, MAP2.dists.dnb, nsim = 999, zero.policy = TRUE)
MonteCarlo
##

#inverse distance weights#
#use KNN (or whatever segment you like)after nbdists here# 
MAP2.dists.nbw <- nbdists(MAP2.dists.dnn, cbind(MAP2$X_Centroid, MAP2$Y_Centroid), longlat=TRUE)
#Option1# 
idw <- lapply(MAP2.dists.nbw, function(x) {ifelse(is.finite(1/x^2), (1/x^2), (1/0.001^2))})
MAP2.idw <- nb2listw(MAP2.dists.dnn, glist = idw, style = "W", zero.policy = TRUE)
#Option2#
idw <- lapply(MAP2.dists.nbw, function(x) 1/(x/1000))
MAP2.idw <- nb2listw(MAP2.dists.dnn, glist = idw, style = "B", zero.policy = TRUE)
#Option3,inv > (Distance of whole region to be tested)#
MAP2.dists.inv <- 1/(MAP2.dists.nbw)^2
MAP2.dists.inv [MAP2.dists.inv > 20000] <- 0
diag(MAP2.dists.inv)
MAP2.dists.invR <- MAP2.dists.inv/rowSums(MAP2.dists.inv)
MAP2.dists.invR [1:5, 1:5]
MAP2.idw <- mat2listw(MAP2.dists.invR)
#option4#
apple <- idw(MAP2$CalcWLR2, MAP2.dists, method="Shepard", p=2)


locm_MAP <- as.data.frame(localmoran(MAP2$CalcWLR2, MAP2.idw, zero.policy = TRUE, na.action = na.omit)) 
summary(MAP2.idw)  
%%
  
  
moransEmployDense <- moran.test(MAP2$CalcWLR2, MAP2.dists.dnb, zero.policy = TRUE)
moransEmployDense
dog <- geary.test(MAP2$CalcWLR, listw2U(MAP2.dists.dnb))
dog


%Needs to reflect which weights you use (lmi for kneighbors, locm for queens)%
  
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


#check quadrant#' 
quadrant2 <- quadrant[quadrant==2]

setwd("Y:/Dan/HousingPaper/DataSets/OrganizedFindings/MoranMaps")

brks <- c(0, 1, 2, 3, 4)
colors <- c(rgb(0.7, 0.7, 0.6, alpha = 0.4), "blue", rgb(0, 0, 1, alpha = 0.4), rgb(1, 0, 0, alpha = 0.4), 
            "red")

jpeg(file = "NYC2012_16.jpeg", width = 2096, height = 1600)
plot(MAP2, col = colors[findInterval(quadrant, brks)], lwd = 0.1)
box()
legend("topleft", legend = c("insignificant", "low-low", "low-high", "high-low", 
                             "high-high"), fill = colors, bty = "n", cex = 3.0, y.intersp = 1, x.intersp = 1)

addscalebar(plotunit = "km", pos = "bottomright", htin = 0.5, padin = c(0.2,0.2), label.cex = 2.5)
title("New York City, 2012-2016", cex.main = 3.5)
dev.off()

scalebar(d = 1000,  below="Kilometers", xy=c(0,0), divs = 2, lonlat = NULL, type = "bar")
