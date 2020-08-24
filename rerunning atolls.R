#fixing atolls
library(varhandle)
path_to_wd <<- "C:/Users/kanoe/Documents/Research Data/BSURP/Atolls/"
setwd(path_to_wd)


server <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/"
sst_id <- "jplMURSST41.csv?analysed_sst"
start_date <- as.Date("2003-01-01T09:00:00Z")                                       
end_date <- as.Date("2019-12-31T09:00:00Z")


atoll_list <- read.csv("other data/atolls_sheet.csv", stringsAsFactors = F)
fix <- atoll_list[which(!is.na(atoll_list$Atoll)),]
full_run <- atoll_list[which(atoll_list$redo == "do"),]
replot <- atoll_list[which(atoll_list$redo == "plot"),]

#rerun plots for selected atolls
#####
for(num in 1:length(replot$Atoll)) {
  current_dat <- replot[num,-1]
  
  #degrees above max monthly mean that is counted
  bleaching_threshold_C = 0.5
  degree_day_threshold = 0
  
  lat  <- list(min = as.numeric(current_dat[2]), max = as.numeric(current_dat[3]))
  long <- list(min = as.numeric(current_dat[4]), max = as.numeric(current_dat[5]))
  
  atoll <- (current_dat[1])
  
  major1 <- as.numeric(c(current_dat[6], current_dat[7]))
  major2 <- as.numeric(c(current_dat[8], current_dat[9]))
  minor1 <- as.numeric(c(current_dat[10], current_dat[11]))
  minor2 <- as.numeric(c(current_dat[12], current_dat[13]))
  
  shift <- as.numeric(c(current_dat[15],current_dat[14]))
  
  path_to_atoll <<- paste0(path_to_wd, atoll)
  path_to_plots <<- paste0(path_to_wd, atoll , "/Plots")
  path_to_data <<- paste0(path_to_wd, atoll, "/Data")
  
  ellipses <- read.csv(paste0(atoll, "/Data/", atoll, "_processed_data.csv"), stringsAsFactors = F)
  
  ellipses = ellipses[-1,]
  ellipses$latitude <- as.numeric(ellipses$latitude)
  ellipses$longitude <- as.numeric(ellipses$longitude)
  
  ellipses$date <- as.Date(ellipses$date)
  
  ellipses <- ellipses[ellipses$date != 2002 & ellipses$date != 2020,]
  
  create_url_and_files(server, sst_id, start_date, end_date, lat, long, atoll)
  plots()
  rm.all.but(keep = c("server", "sst_id", "start_date", "path_to_wd", "end_date", "replot"), keep_functions = T)
}

#full run for selected atolls

#####
for(num in 1:length(full_run$Atoll)) {
  current_dat <- full_run[num,-1]
  
  #degrees above max monthly mean that is counted
  bleaching_threshold_C = 0.5
  degree_day_threshold = 0
  
  lat  <- list(min = as.numeric(current_dat[2]), max = as.numeric(current_dat[3]))
  long <- list(min = as.numeric(current_dat[4]), max = as.numeric(current_dat[5]))
  
  atoll <- (current_dat[1])
  
  major1 <- as.numeric(c(current_dat[6], current_dat[7]))
  major2 <- as.numeric(c(current_dat[8], current_dat[9]))
  minor1 <- as.numeric(c(current_dat[10], current_dat[11]))
  minor2 <- as.numeric(c(current_dat[12], current_dat[13]))
  
  shift <- as.numeric(c(current_dat[15],current_dat[14]))
  
  path_to_atoll <<- paste0(path_to_wd, atoll)
  path_to_plots <<- paste0(path_to_wd, atoll , "/Plots")
  path_to_data <<- paste0(path_to_wd, atoll, "/Data")
  
  big_ol_run()
  rm.all.but(keep = c("server", "sst_id", "start_date", "path_to_wd", "end_date", "full_run"), keep_functions = T)
}




#recalculating table values

fix_fn <- function(ellipses) {
  #percent days bleached stats
  inside <- filter(ellipses, location == "I")
  outside <- filter(ellipses, location == "O")
  
  #####
  #mean of hottest month per year
  inside_monthly = inside %>%
    group_by(year, month) %>%  
    summarise(monthly_avg = mean(analysed_sst)) %>%
    filter(!duplicated(month)) %>%
    ungroup() %>%
    group_by(year) %>%
    filter(
      monthly_avg == max(monthly_avg)) %>%
    ungroup() %>%
    filter(year != 2020) #2020 isnt over :( 
  
  outside_monthly = outside %>%
    group_by(year, month) %>%
    summarise(monthly_avg = mean(analysed_sst)) %>%
    filter(!duplicated(month)) %>%
    ungroup() %>%
    group_by(year) %>%
    filter(monthly_avg == max(monthly_avg)) %>% ungroup() %>% 
    filter(year != 2020)
  
  inside_max_mean_summer_monthly <- mean(inside_monthly$monthly_avg)
  outside_max_mean_summer_monthly <- mean(outside_monthly$monthly_avg)
  rm(inside_monthly)
  rm(outside_monthly)
  
  #####
  inside_day <- inside %>% group_by(date) %>%
    mutate(
      bleached = mean(analysed_sst) > (inside_max_mean_summer_monthly + bleaching_threshold_C), 
      mean_sst = mean(analysed_sst)
    ) %>% filter(!duplicated(date))
  
  outside_day <- outside %>% group_by(date) %>%
    mutate(
      bleached = mean(analysed_sst) > (outside_max_mean_summer_monthly +bleaching_threshold_C), 
      mean_sst = mean(analysed_sst)
    ) %>%filter(!duplicated(date)) 
  
  
  pct_dys_ovr_29_5in <- (nrow(inside_day[inside_day$mean_sst > 29.5,1])*100)/nrow(inside_day)
  pct_dys_ovr_29_5out <- (nrow(outside_day[outside_day$mean_sst > 29.5,])*100)/nrow(outside_day)
  pct_dys_ovr_30in <- (nrow(inside_day[inside_day$mean_sst>30,])*100)/nrow(inside_day)
  pct_dys_ovr_30out <- (nrow(outside_day[outside_day$mean_sst>30,])*100)/nrow(outside_day)
  pct_dys_ovr_30_5in <- (nrow(inside_day[inside_day$mean_sst>30.5,])*100)/nrow(inside_day)
  pct_dys_ovr_30_5out <- (nrow(outside_day[outside_day$mean_sst>30.5,])*100)/nrow(outside_day)
  pct_dys_ovr_31in <- (nrow(inside_day[inside_day$mean_sst>31,])*100)/nrow(inside_day)
  pct_dys_ovr_31out <- (nrow(outside_day[outside_day$mean_sst>31,])*100)/nrow(outside_day)
  
  
  atoll <- as.character(atoll[1,1])
  overall_values <- read.csv(paste0(path_to_wd, "/Overall_values.csv"))
  change_these <- c("%Days>29.5 Inside",
                    "%Days>29.5 Outside",
                    "%Days>30 Inside",
                    "%Days>30 Outside",
                    "%Days>30.5 Inside",
                    "%Days>30.5 Outside",
                    "%Days>31 Inside",
                    "%Days>31 Outside")
  new_vals <- c(pct_dys_ovr_29_5in, 
                pct_dys_ovr_29_5out, 
                pct_dys_ovr_30in, 
                pct_dys_ovr_30out, 
                pct_dys_ovr_30_5in, 
                pct_dys_ovr_30_5out, 
                pct_dys_ovr_31in, 
                pct_dys_ovr_31out)
  
  atoll_dat <- data.frame(overall_values[,atoll])
  rownames(atoll_dat) <- overall_values[,1]
  atoll_dat[change_these,] <- new_vals
  
  overall_values[,atoll] <- atoll_dat
  write_csv(overall_values, paste0(path_to_wd, "/Overall_values.csv"), append=FALSE)
}

ellipse_area <- function(major1, major2, minor1, minor2){
  major_dist <- 0.5*sqrt((major2[1] - major1[1])^2 + (major2[2] - major1[2])^2)
  minor_dist <- 0.5*sqrt((minor2[1] - minor1[1])^2 + (minor2[2] - minor1[2])^2)
  area <- 12321*pi*major_dist*minor_dist
  return(area)
}


server <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/" 
sst_id <- "jplMURSST41.csv?analysed_sst"
lat <- list (min = -16.16, max = -16.02)
long <- list(min = -179.4, max = -179.15)
start_date <- as.Date("2003-01-01T09:00:00Z")
end_date <- as.Date("2019-12-31T09:00:00Z")
atoll <- "Qelevu"

#degrees above max monthly mean that is counted
bleaching_threshold_C = 0.5
degree_day_threshold = 0

major1 <- c(-16.0814, -179.3747)
major2 <- c(-16.10713, -179.1996)
minor1 <- c(-16.12626, -179.29505)
minor2 <- c(-16.05764, -179.28544)

shift<-c(0,0.25)


big_ol_run()
rm(ellipses)
rm(list = setdiff(ls(), lsf.str()))

