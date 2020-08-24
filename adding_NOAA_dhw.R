#adding new stats to atolls
#this script works if all your atolls are broken into folders how Kyle organized them
library(varhandle)
library(dplyr)
path_to_wd <<- "C:/Users/kanoe/Documents/Research Data/BSURP/Atolls"
setwd(path_to_wd)


values <- read.csv("other data/Overall_values.csv", stringsAsFactors = F)
atolls <- colnames(values)[-(1:2)]
values <- as.data.frame(t(values[31:32, -(1:2)]))

values <- values %>% mutate_if(is.factor, as.character) 
values <- values %>% mutate_if(is.character, as.numeric)
values$atolls <- atolls

colnames(values) <- c("MMMi", "MMMo", "atoll")

new_stats <- data.frame(days_4I = 1:length(atolls), days_4O = NA, days_4IO = NA, 
                        days_8I = NA, days_8O = NA, days_8IO = NA, 
                        maxI = NA, maxO = NA, maxIO = NA)
rownames(new_stats) <- atolls


#rerun plots for selected atolls
#####
for(num in 1:length(atolls)) {
  dat <- values[num,]
  print(dat)
  atoll <- as.character(dat[1,3])
  MMMi <- as.numeric(dat[1,1]) - 1.5
  MMMo <- as.numeric(dat[1,2])
  
  ellipses <- read.csv(paste0(atoll, "/data/", atoll, "_processed_data.csv"), stringsAsFactors = F)
  print(length(ellipses[,1]))
  
  ellipses = ellipses[-1,]
  
  ellipses$latitude <- as.numeric(ellipses$latitude)
  ellipses$longitude <- as.numeric(ellipses$longitude)
  ellipses$date <- as.Date(ellipses$date)
  
  ellipses <- ellipses[ellipses$date != 2002 & ellipses$date != 2020,]
  
  #Inside stats
  DHW_I = ellipses %>% filter(location == "I") %>% group_by(date) %>% 
    mutate(mean_sst = mean(analysed_sst)) %>% filter(!duplicated(date)) %>%
    mutate(degree_above = 0) %>% mutate(DHW = NA)
  
  for(i in 1:nrow(DHW_I)){
    if(DHW_I$mean_sst[i] > (MMMi + 1)){
      DHW_I$degree_above[i] = DHW_I$mean_sst[i] - (MMMi)
    }
  }
  
  for(u in 84:nrow(DHW_I)){
    DHW_I$DHW[u] = (sum(DHW_I$degree_above[(u-83):u]))/7
  }
  
  
  #outside stats
  DHW_O = ellipses %>% filter(location == "O") %>% group_by(date) %>% 
    mutate(mean_sst = mean(analysed_sst)) %>% filter(!duplicated(date)) %>%
    mutate(degree_above = 0) %>% mutate(DHW = NA)
  
  for(i in 1:nrow(DHW_O)){
    if(DHW_O$mean_sst[i] > (MMMo + 1)){
      DHW_O$degree_above[i] = DHW_O$mean_sst[i] - (MMMo)
    }
  }
  
  for(i in 84:nrow(DHW_O)){
    DHW_O$DHW[i] = (sum(DHW_O$degree_above[(i-83):i]))/7
  }
  
  print("plotting")
  graph <- DHW_I %>% filter(year == 2016) %>% ggplot(aes(date, DHW)) + geom_point()
  print(graph)
  
  #inside w outside MMM
  DHW_IO = ellipses %>% filter(location == "I") %>% group_by(date) %>% 
    mutate(mean_sst = mean(analysed_sst)) %>% filter(!duplicated(date)) %>%
    mutate(degree_above = 0) %>% mutate(DHW = NA)
  
  for(i in 1:nrow(DHW_IO)){
    if(DHW_IO$mean_sst[i] > (MMMo + 1)){
      DHW_IO$degree_above[i] = DHW_IO$mean_sst[i] - (MMMo)
    }
  }
  
  for(i in 84:nrow(DHW_IO)){
    DHW_IO$DHW[i] = (sum(DHW_IO$degree_above[(i-83):i]))/7
  }
  
  Pct_dys_ovr4DHW_I = (nrow(DHW_I%>%filter(DHW>4))/nrow(DHW_I)) * 100
  Pct_dys_ovr4DHW_O = (nrow(DHW_O%>%filter(DHW>4))/nrow(DHW_O)) * 100
  Pct_dys_ovr4DHW_IO = (nrow(DHW_IO%>%filter(DHW>4))/nrow(DHW_IO)) * 100
  print(Pct_dys_ovr4DHW_I)
  
  Pct_dys_ovr8DHW_I = (nrow(DHW_I%>%filter(DHW>8))/nrow(DHW_I)) * 100
  Pct_dys_ovr8DHW_O = (nrow(DHW_O%>%filter(DHW>8))/nrow(DHW_O)) * 100
  Pct_dys_ovr8DHW_IO = (nrow(DHW_IO%>%filter(DHW>8))/nrow(DHW_IO)) * 100
  print(Pct_dys_ovr8DHW_I)
  
  max_DHW_I = max(DHW_I$DHW, na.rm = T)
  max_DHW_O = max(DHW_O$DHW, na.rm = T)
  max_DHW_IO = max(DHW_IO$DHW, na.rm = T)
  
  new_stats[atoll,] <- c(Pct_dys_ovr4DHW_I, Pct_dys_ovr4DHW_O, Pct_dys_ovr4DHW_IO,
                         Pct_dys_ovr8DHW_I, Pct_dys_ovr8DHW_O, Pct_dys_ovr8DHW_IO,
                         max_DHW_I, max_DHW_O, max_DHW_IO)
  remove(ellipses)
}

new_stats <- t(new_stats) 
write_csv(new_stats, "noaa_style_bleaching")

graph <- DHW_I %>% filter(year == 2016) %>% ggplot(aes(date, DHW)) + geom_point()
ggsave(paste0("DHWplots/", atoll))