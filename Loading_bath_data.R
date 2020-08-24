#Find deepest point in atoll
library(ncdf4)
library(raster)

setwd("C:/Users/kanoe/Documents/Research Data")

load("Introsems/Shark_project/other data/bath_obj.Rdata")

f <- nc_open("Introsems/Shark_project/other data/ETOPO1_Ice_g_gmt4.grd")
bath_lon <- ncvar_get(f, "x")
bath_lat <- ncvar_get(f, "y")
bath <- ncvar_get(f, "z")
nc_close(f)

half_dist <- function(pts) {
  x <- pts[4] - pts[2]
  y <- pts[3] - pts[1]
  sqrt(x * x + y * y) / 4
}


#read in overall values table
overall_values <- read.csv("BSURP/Atolls/Overall_values.csv", stringsAsFactors = F)
rownames(overall_values) <- overall_values[,2]
overall_values <- t(overall_values[-1,-(1:2)])
overall_values <- data.frame(overall_values[!is.na(overall_values[,1]),])

overall = overall_values %>% mutate_if(is.factor, as.character)
overall = overall %>% mutate_if(is.character, as.numeric)

atolls <- rownames(overall_values)

max_depths <- data.frame(matrix(nrow = length(atolls), ncol = 2))
colnames(max_depths) <- c("atoll", "max_depth")

narrow <- which(is.infinite(max_depths$max_depth))
max_depths[narrow,]

deep <- which(max_depths$max_depth < -500)
max_depths[deep,]
#1:length(atolls)

for(i in 1:length(atolls)){
  dat <- as.numeric(overall[i,])
  
  lon <- dat[10]
  lat <- dat[9]
  
  #range <- half_dist(dat[5:8])
  #range <- 2 * min(c(half_dist(dat[1:4]), half_dist(dat[5:8])))
  range <- min(c(half_dist(dat[1:4]), half_dist(dat[5:8])))
  #range <- 0.01
  
  lat_ind <- which(bath_lat <= (lat + range) & bath_lat >= (lat - range))
  lon_ind <- which(bath_lon <= (lon + range) & bath_lon >= (lon - range))
  sub_bath_lat <- bath_lat[lat_ind]
  sub_bath_lon <- bath_lon[lon_ind]
  
  sub_bath <- bath[lon_ind,lat_ind]
  
  max_depths[i,1] <- atolls[i]
  which.min(sub_bath)
  max_depths[i,2] <- min(sub_bath)
}


max_depths <- t(max_depths)
write.csv(max_depths, file = "BSURP/Atolls/atoll_baths.csv")
