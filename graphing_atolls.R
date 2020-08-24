library(raster)
library(tidyverse)
library(fishualize)
library(ggplot2)
library(dplyr)

setwd("C:/Users/kanoe/Documents/Research Data/BSURP/Atolls")
values <- read.csv("Overall_values_all.csv", stringsAsFactors = F)

rownames(values) <- values[,2]
values <- values[,-(1:2)]
atolls <- colnames(values)
values <- as.data.frame(t(values[-(2:9),]))

values <- values %>% mutate_if(is.factor, as.character) 
values <- values %>% mutate(region = as.factor(region)) 
values <- values %>% mutate_if(is.character, as.numeric)
values <- values %>% mutate(diff = MMMi - MMMo) %>% group_by(region)

values$atoll <- atolls

values <- values %>% filter(region != "Palau")
cleaned <- values %>% group_by(region) %>% filter(n() > 4)

small <- values[values$area_pts < 20,]
#variables: lat, MMMo, depth, area, enclosure, region

## new graphing
diff_v_MMMo <- values %>% ggplot(aes(MMMo, diff)) + geom_point(aes(color = region)) + geom_smooth(method = lm) 
diff_v_MMMo

diff_v_lat <- values %>% ggplot(aes(abs(ctr_lat), diff)) + geom_point(aes(color = region)) + 
  geom_smooth(method = lm) + ggtitle("Latitude vs MMM difference for all Atolls") + 
  theme(plot.title = element_text(color = "red", hjust = .5)) + 
  labs(x = "Absolute Value of Center Latitude", y = "MMM difference")
diff_v_lat

diff_v_depth <- values %>% filter(depth > -500) %>% ggplot(aes(depth, diff)) + 
  geom_point(aes(color = region)) + geom_smooth(method = lm) + ggtitle("Maximum Depth vs MMM difference for all Atolls") + 
  theme(plot.title = element_text(color = "red", hjust = .5)) + 
  labs(x = "Atoll Depth", y = "MMM difference")
diff_v_depth

diff_v_enc <- cleaned %>% ggplot(aes(enclosure, diff)) + geom_point(aes(color = region)) + 
  geom_smooth(method = lm) + ggtitle("Submersion vs MMM difference for all Atolls") + 
  theme(plot.title = element_text(color = "red", hjust = .5)) + 
  labs(x = "Submersion rating", y = "MMM difference")
diff_v_enc

diff_v_area <- values %>% ggplot(aes(area_pts, diff)) + 
  geom_point(aes(color = region)) + geom_smooth(method = lm) + ggtitle("Area vs MMM difference for all Atolls") + 
  theme(plot.title = element_text(color = "red", hjust = .5)) + 
  labs(x = "Atoll Area", y = "MMM difference")
diff_v_area

diff_v_vol <- values %>% ggplot(aes(area_pts * depth / 1000, diff)) + 
  geom_point(aes(color = region)) + geom_smooth(method = lm) + ggtitle("Volume vs MMM difference for all Atolls") + 
  theme(plot.title = element_text(color = "red", hjust = .5)) + 
  labs(x = "Atoll volume", y = "MMM difference")
diff_v_vol

#comprehensive models
m1 <- cleaned %>% lm(formula = diff ~ enclosure + abs(ctr_lat) + MMMo + area_pts + depth + region)
summary(m1)
summary(m1)$coefficients[,4]

m2 <- values %>% ungroup() %>% lm(formula = diff ~ MMMo + area_pts + depth + enclosure + region)
summary(m2)

#m2 w lat instead of MMM
m3 <- values %>% ungroup() %>% lm(formula = diff ~ abs(ctr_lat) + area_pts + depth + enclosure + region)
summary(m3)

MMM_v_best <- cleaned %>% lm(formula = diff ~ abs(ctr_lat) + enclosure + area_pts * depth + region)
summary(MMM_v_best)

MMM_v_physical <- values %>% lm(formula = diff ~ area_pts * depth + enclosure + MMMo)
summary(MMM_v_physical)

#trait models
MMM_lat <- values %>% ungroup() %>% lm(formula = MMMo ~ ctr_lat)
summary(MMM_lat)
summary(MMM_lat)$coefficients[,4]

MMM_lat <- cleaned %>% ungroup() %>% lm(formula = MMMo ~ ctr_lat + region)
summary(MMM_lat)
summary(MMM_lat)$coefficients[,4]

area_depth <- values %>% ungroup() %>% lm(formula = area_pts ~ depth)
summary(area_depth)
summary(area_depth)$coefficients[,4]

MMM_v_size <- values %>% lm(formula = diff ~ area_pts + depth)
summary(MMM_v_size)

MMM_v_vol <- values %>% lm(formula = diff ~ depth * area_pts)
summary(MMM_v_vol)

MMM_v_enc <- values %>% lm(formula = diff ~ enclosur)
summary(MMM_v_enc)

MMM_v_enc <- cleaned %>% lm(formula = diff ~ enclosure + region)
summary(MMM_v_enc)

#separating regions for analysis: solomon, Micro, Marshall



#graphing relationships between variables
lat_v_temp <- values %>% ggplot(aes(abs(ctr_lat), MMMo)) + geom_point(aes(color = region)) + 
  geom_smooth(method = lm) + labs(x = "Latitude", y = "outside MMM") + ggtitle("Latitude vs Temperature for all Atolls") + 
  theme(plot.title = element_text(color = "red", hjust = .5))
lat_v_temp

area_v_depths <- values %>% ggplot(aes(area_pts, depth)) + geom_point(aes(color = region)) + 
  geom_smooth(method = lm) + labs(x = "Latitude", y = "outside MMM") + ggtitle("Latitude vs Temperature for all Atolls") + 
  theme(plot.title = element_text(color = "red", hjust = .5))
area_v_depths

#regional trait analysis
regional_vals <- values %>% group_by(region) %>% summarise(avg_MMMo = mean(MMMo), avg_MMMi = mean(MMMi),
                                                           avg_enc = mean(enclosure), avg_depth = mean(depth))

region_enc <- cleaned %>% group_by(region) %>% ggplot(aes(region, enclosure), color = region) + geom_boxplot()
region_enc

region_depth <- cleaned %>% group_by(region) %>% ggplot(aes(region, depth), fill = region) + geom_boxplot()
region_depth

region_MMMo <- values %>% group_by(region) %>% ggplot(aes(region, MMMo), fill = region) + geom_boxplot()
region_MMMo




