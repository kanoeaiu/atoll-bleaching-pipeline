library(rgdal)
library(ncdf4)
library(raster)
library(tidyverse)
library(fishualize)
library(gganimate)
library(ggplot2)
library(ggpubr)
library(crayon)
source("C:/Users/kanoe/Documents/Research Data/BSURP/Atolls/ellipse_generation.R")

##You need to change the directory above and the path directories in the first function, that's it
path_to_wd <<- "C:/Users/kanoe/Documents/Research Data/BSURP/Atolls/"
setwd(path_to_wd)

##Creates path names, directories, urls, file names, and downloads unprocessed data files (if they don't exist)
create_url_and_files <- function(server, sst_id, start_date, end_date, lat, long, atoll) {
  path_to_atoll <<- paste0(path_to_wd, atoll)
  path_to_plots <<- paste0(path_to_wd, atoll , "/Plots")
  path_to_data <<- paste0(path_to_wd, atoll, "/Data")
  if (!dir.exists(path_to_atoll) ) {dir.create(path_to_atoll)}
  if (!dir.exists(path_to_plots) ) {dir.create(path_to_plots)}
  if (!dir.exists(path_to_data) ) {dir.create(path_to_data)}
  cat(green("DIRECTORIES SUCCESSFULLY CREATED") %+% "\n")
  
  inside_url <<- paste0(server, sst_id, "[", "(", start_date, ")", ":1:", "(", end_date, ")", "]", "[", "(", lat$min, ")", ":1:", "(", lat$max, ")", "]", "[", "(", long$min, ")", ":1:", "(", long$max, ")","]" )
  outside_url <<- paste0(server, sst_id, "[", "(", start_date, ")", ":1:", "(", end_date, ")", "]", "[", "(", lat$min + shift[1], ")", ":1:", "(", lat$max + shift[1], ")", "]", "[", "(", as.character(long$min+shift[2]), ")", ":1:", "(", as.character(long$max+shift[2]), ")","]" )
  inside_file <<- paste0(atoll, "/Data/", atoll, "_inside_sst.csv")
  outside_file <<- paste0(atoll, "/Data/", atoll, "_outside_sst.csv")
  
  if(!file.exists(paste0(path_to_wd, "/Overall_values.csv"))){
    overall_values <- data.frame(Variables = c("Major 1, Lat", "Major 1, Long", "Major 2, Lat", "Major 2, Long",
                                               "Minor 1, Lat", "Minor 1, Long", "Minor 2, Lat", "Minor 2, Long",
                                               "Center, Lat", "Center, Long",
                                               "Longitude Shift", "Bleaching Threshold (Celsius)", "Degree Day Threshold",
                                               "#of Days Inside", "#of Days Outside",
                                               "%Days Bleached Inside", "%Days Bleached Outside","%Days>29.5 Inside", "%Days>29.5 Outside", "%Days>30 Inside",
                                               "%Days>30 Outside","%Days>30.5 Inside", "%Days>30.5 Outside", "%Days>31 Inside",
                                               "%Days>31 Outside",
                                               "%days bleach inside using outside MMM",
                                               "Degree Days Inside", "Degree Days Outside","Degree Days Inside using outside MMM",
                                               "Max Mean Summer Monthly Inside (Celsius)", "Max Mean Summer Monthly Outside (Celsius)", 
                                               "Average Run Length Inside (Days)","Average Run Length Outside (Days)", 
                                               "Average Run Length Inside using outside MMM (Days)",
                                               "Average Degree Days/ Run Inside", "Average Degree Days/ Run Outside",
                                               "Average Degree Days/ Run Inside using outside MMM",
                                               "Multi Day Runs Inside", "Multi Day Runs Outside","Multi Day Runs Inside using outside MMM",
                                               "Ellipse Area (Functional, sq.km)", "Ellipse Area (Points, sq.km)"))
    
    
    write_csv(overall_values, paste0(path_to_wd, "/Overall_values.csv"))
  }
}

download <- function(url, file) {
  cat(blue("FILE DOWNLOAD COMMENCING") %+% "\n")
  if(!file.exists(file)) curl::curl_download(url, file, quiet=F)
  cat(green("INSIDE ATOLL DATA SUCCESSFULLY DOWNLOADED") %+% "\n")
}

##"Fixes" data frames (column class types, etc), combines them, and runs the ellipse_generation function
fix_combine_data_frames <- function() {
  cat(blue("FIXING AND COMBINING DATA FRAMES"))
  inside <- read.csv(inside_file, stringsAsFactors = FALSE)
  outside <- read.csv(outside_file, stringsAsFactors = FALSE)
  
  inside = inside[-1,]
  inside$latitude <- as.numeric(inside$latitude)
  inside$longitude <- as.numeric(inside$longitude)
  cat(red("BEEP "))
  inside$analysed_sst <- as.numeric(as.character(inside$analysed_sst))
  
  
  outside = outside[-1,]
  outside$latitude <- as.numeric(outside$latitude)
  outside$longitude <- as.numeric(outside$longitude)
  cat(silver("BOP BOOP")%+% "\n")
  outside$analysed_sst <- as.numeric(outside$analysed_sst)
  
  print(length(inside$latitude))
  print(length(outside$latitude))
  
  cat(blue("CREATING ELLIPTICAL MASKS NOW") %+% "\n")
  inside_ellipse <- ellipse_generation(inside, major1, major2, minor1, minor2)
  outside_ellipse <- ellipse_generation(outside, major1+shift, major2+shift, minor1+shift, minor2+shift)
  
  print(length(inside_ellipse[,1]))
  print(length(outside_ellipse[,1]))
  
  cat(red("BOOOOOOOOP") %+% "\n")
  
  inside_ellipse$time <- as.character(inside_ellipse$time)
  inside_ellipse$date <- as.Date(sapply(inside_ellipse$time, function(x) strsplit(x, "T")[[1]][1]))
  inside_ellipse$month <- lubridate::month(inside_ellipse$date)
  inside_ellipse$year <- lubridate::year(inside_ellipse$date)
  lapply(inside_ellipse, class)
  
  
  cat(black("Fax machine crunching noise") %+% "\n")
  outside_ellipse$time <- as.character(outside_ellipse$time)
  outside_ellipse$date <- as.Date(sapply(outside_ellipse$time, function(x) strsplit(x, "T")[[1]][1]))
  outside_ellipse$month <- lubridate::month(outside_ellipse$date)
  outside_ellipse$year <- lubridate::year(outside_ellipse$date)
  cat(silver("Elephant roar") %+% "\n")
  lapply(outside_ellipse, class)
  
  outside_ellipse  = outside_ellipse %>% mutate(
    location  = as.factor("O")
  )
  inside_ellipse = inside_ellipse %>% mutate(
    location = as.factor("I")
  )
  cat(black("Do elephants roar??") %+% "\n")
  #####
  
  ellipses <- rbind(inside_ellipse, outside_ellipse)[,-1]
  ellipses <<- ellipses[ellipses$year != 2002 & ellipses$year != 2020,]
  
  
  write.csv(ellipses, paste0(atoll, "/Data/", atoll, "_processed_data.csv"))
  cat(green("ELLIPTICAL DATA COMBINED AND SAVED AS ") %+% black(atoll) %+% black(" _processed_data.csv") %+% "\n")
  print("Ellipses (after ellipse generation):")
  print(head(ellipses))
  
  cat(green("DATA PROCESSING COMPLETED") %+% "\n")
}

##Runs all of Sriram's data analysis, creates the plots, and spits them out into the plots folder as .png's
plots <- function(){
  #average temperature by plot over time
  #######
  cat(blue("GENERATING PLOTS") %+% "\n")
  
  temp_over_time1 <- ellipses %>%
    group_by(location, year, date) %>%
    summarise(Mean = mean(analysed_sst)) %>%
    ggplot(aes(date, Mean, color = location, group = location)) +
    geom_point() + geom_smooth(se = F) +
    ggtitle(paste(atoll, "Temperature Over Years")) + 
    theme(plot.title = element_text(color = "red", hjust = 0.5)) +
    xlab("Date") + ylab("Location Average (Celsius)") 
  
  ggsave(file = paste0(path_to_plots, "/TempOverTime1.png"), temp_over_time1)
  
  temp_over_time2 <- ellipses %>%
    group_by(location, year, date) %>%
    summarise(Mean = mean(analysed_sst)) %>%
    ggplot(aes(date, Mean, color = location, group = location)) +
    geom_point(alpha = 0.05) + geom_smooth(se = F) +
    ggtitle(paste(atoll, "Temperature Over Years")) + 
    theme(plot.title = element_text(color = "red", hjust = 0.5)) +
    xlab("Date") + ylab("Location Average (Celsius)") 
  
  ggsave(file = paste0(path_to_plots, "/TempOverTime2.png"), temp_over_time2)
  
  #days above average monthly maximum
  
  #______________________________________________________
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
  
  inside_max_mean_summer_monthly <<- mean(inside_monthly$monthly_avg)
  outside_max_mean_summer_monthly <<- mean(outside_monthly$monthly_avg)
  rm(inside_monthly)
  rm(outside_monthly)
  
  
  #####
  #how many days above mean monthly max as a whole area
  #degree days average
  inside_day <<- inside %>% group_by(date) %>%
    mutate(
      bleached = mean(analysed_sst) > (inside_max_mean_summer_monthly + bleaching_threshold_C), 
      mean_sst = mean(analysed_sst)
    ) %>% filter(!duplicated(date))
  
  outside_day <<- outside %>% group_by(date) %>%
    mutate(
      bleached = mean(analysed_sst) > (outside_max_mean_summer_monthly +bleaching_threshold_C), 
      mean_sst = mean(analysed_sst)
    ) %>%filter(!duplicated(date)) 
  
  inside_day_outsideMMM <<- inside %>% group_by(date) %>%
    mutate(
      bleached = mean(analysed_sst) >(outside_max_mean_summer_monthly + bleaching_threshold_C), 
      mean_sst = mean(analysed_sst)
    ) %>% filter(!duplicated(date))
  
  pct_dys_ovr_29_5in <<- (nrow(inside_day[inside_day$mean_sst > 29.5,])*100)/nrow(inside_day)
  pct_dys_ovr_29_5out <<- (nrow(outside_day[outside_day$mean_sst > 29.5,])*100)/nrow(outside_day)
  pct_dys_ovr_30in <<- (nrow(inside_day[inside_day$mean_sst>30,])*100)/nrow(inside_day)
  pct_dys_ovr_30out <<- (nrow(outside_day[outside_day$mean_sst>30,])*100)/nrow(outside_day)
  pct_dys_ovr_30_5in <<- (nrow(inside_day[inside_day$mean_sst>30.5,])*100)/nrow(inside_day)
  pct_dys_ovr_30_5out <<- (nrow(outside_day[outside_day$mean_sst>30.5,])*100)/nrow(outside_day)
  pct_dys_ovr_31in <<- (nrow(inside_day[inside_day$mean_sst>31,])*100)/nrow(inside_day)
  pct_dys_ovr_31out <<- (nrow(outside_day[outside_day$mean_sst>31,])*100)/nrow(outside_day)
  percent_days_bleach_inside <<- (sum(inside_day$bleached)*100)/nrow(inside_day)
  percent_days_bleach_outside <<- (sum(outside_day$bleached)*100)/nrow(outside_day)
  percent_days_bleach_inside_outsideMMM <<- (sum(inside_day_outsideMMM$bleached)*100)/nrow(inside_day)
  
  days_in_dataset_outside <<- nrow(outside_day)
  days_in_dataset_inside <<- nrow(inside_day)
  
  #_______________________________________________________________________
  #Inside bleaching compared to inside MMM
  inside_day = inside_day %>%
    filter(
      mean_sst > (inside_max_mean_summer_monthly + degree_day_threshold) #THIS MAKES INSIDE_DAY based on outside MMM
    ) %>%
    group_by(date) %>%
    mutate(
      degreeabove = mean_sst - inside_max_mean_summer_monthly
    ) %>% ungroup() %>%
    mutate(
      consec = cumsum(!c(TRUE, diff(date) == 1)),
      dummy = T
    ) %>% group_by(consec) %>%
    mutate(
      degree_days = sum(degreeabove),
      run_length = sum(dummy)) %>%
    filter(!duplicated(consec))
  
  degree_days_inside <<- sum(inside_day$degree_days)
  avg_run_inside <<-mean(inside_day$run_length)
  avg_degree_days_inside <<- mean(inside_day$degree_days)
  
  inside_day = inside_day%>% group_by(year) %>%
    mutate(
      degreedays_year = sum(degree_days),
      run_length_year = mean(run_length),
      avg_run_degree_days = mean(degree_days),
      dummy = run_length >1, 
      n_runs_over_1 = sum(dummy)
    ) %>% ungroup() %>% group_by(year, month) %>%
    mutate(
      degreedays_month = sum(degree_days),
      run_length_month = mean(run_length),
      avg_run_degree_days_m = mean(degree_days),
      dummy = run_length >1, 
      n_runs_over_1_m = sum(dummy)) 
  
  n_multi_day_runs_inside<<- sum(inside_day$dummy)
  
  inside_day = inside_day[c("date","year", "month", "degreedays_year","run_length_year", "avg_run_degree_days", "n_runs_over_1" , 
                            "degreedays_month", "run_length_month", "avg_run_degree_days_m","n_runs_over_1_m","location")]
  
  inside_yearly <<- inside_day %>%
    group_by(year, month) %>%
    filter(!duplicated(month))
  
  #THIS MAKES INSIDE_DAY based on outside MMM
  inside_day_outsideMMM  = inside_day_outsideMMM  %>%
    filter(
      mean_sst > (outside_max_mean_summer_monthly + degree_day_threshold) #
    ) %>%
    group_by(date) %>%
    mutate(
      degreeabove = mean_sst - outside_max_mean_summer_monthly
    ) %>% ungroup() %>%
    mutate(
      consec = cumsum(!c(TRUE, diff(date) == 1)),
      dummy = T
    ) %>% group_by(consec) %>%
    mutate(
      degree_days = sum(degreeabove),
      run_length = sum(dummy)) %>%
    filter(!duplicated(consec))
  
  degree_days_inside_outsideMMM <<- sum(inside_day_outsideMMM $degree_days)
  avg_run_inside_outsideMMM <<-mean(inside_day_outsideMMM $run_length)
  avg_degree_days_inside_outsideMMM <<- mean(inside_day_outsideMMM $degree_days)
  
  inside_day_outsideMMM  = inside_day_outsideMMM %>% group_by(year) %>%
    mutate(
      degreedays_year = sum(degree_days),
      run_length_year = mean(run_length),
      avg_run_degree_days = mean(degree_days),
      dummy = run_length >1, 
      n_runs_over_1 = sum(dummy)
    ) %>% ungroup() %>% group_by(year, month) %>%
    mutate(
      degreedays_month = sum(degree_days),
      run_length_month = mean(run_length),
      avg_run_degree_days_m = mean(degree_days),
      dummy = run_length >1, 
      n_runs_over_1_m = sum(dummy)) 
  
  n_multi_day_runs_inside_outsideMMM<<- sum(inside_day_outsideMMM$dummy)
  
  inside_day_outsideMMM  = inside_day_outsideMMM [c("date","year", "month", "degreedays_year","run_length_year", "avg_run_degree_days", "n_runs_over_1" , 
                                                    "degreedays_month", "run_length_month", "avg_run_degree_days_m","n_runs_over_1_m","location")]
  
  inside_yearly_outsideMMM <<- inside_day_outsideMMM  %>%
    group_by(year, month) %>%
    filter(!duplicated(month))
  
  #outside stats
  
  outside_day = outside_day %>%
    filter(
      mean_sst > (outside_max_mean_summer_monthly + degree_day_threshold)
    ) %>% group_by(date) %>%
    mutate(
      degreeabove = mean_sst - outside_max_mean_summer_monthly
    ) %>% ungroup() %>%
    mutate(
      consec = cumsum(!c(TRUE, diff(date) == 1)),
      dummy = T
    ) %>% group_by(consec) %>%
    mutate(
      degree_days = sum(degreeabove),
      run_length = sum(dummy)) %>%
    filter(!duplicated(consec))
  
  degree_days_outside <<- sum(outside_day$degree_days)
  avg_run_outside <<-mean(outside_day$run_length)
  avg_degree_days_outside <<- mean(outside_day$degree_days)
  
  outside_day = outside_day%>% group_by(year) %>%
    mutate(
      degreedays_year = sum(degree_days),
      run_length_year = mean(run_length),
      avg_run_degree_days = mean(degree_days),
      dummy = run_length >1, 
      n_runs_over_1 = sum(dummy)) %>% ungroup() %>% group_by(year, month) %>%
    mutate(
      degreedays_month = sum(degree_days),
      run_length_month = mean(run_length),
      avg_run_degree_days_m = mean(degree_days),
      dummy = run_length >1, 
      n_runs_over_1_m = sum(dummy)) 
  
  n_multi_day_runs_outside<<- sum(outside_day$dummy)
  
  outside_day = outside_day[c("date","year", "month", "degreedays_year","run_length_year", "avg_run_degree_days", "n_runs_over_1" , 
                              "degreedays_month", "run_length_month", "avg_run_degree_days_m","n_runs_over_1_m","location")]
  
  
  
  outside_yearly <<- outside_day %>%
    group_by(year, month) %>%
    filter(!duplicated(month))
  
  
  
  
  
  
  rm(inside_day)
  rm(outside_day)
  
  
  
  ##### 
  #point data
  #percent bleached over time
  
  ######
  #points bleached and percentage of total area
  inside = inside %>%
    group_by(latitude, longitude, year, month) %>%
    mutate(mean_monthly_max = mean(analysed_sst)) %>%
    ungroup() %>% group_by(latitude, longitude,year) %>%
    mutate(mean_monthly_max = max(mean_monthly_max))   %>%
    ungroup() %>% group_by(latitude, longitude) %>%
    mutate(mean_monthly_max = mean(mean_monthly_max))
  
  outside = outside %>%
    group_by(latitude, longitude, year, month) %>%
    mutate(mean_monthly_max = mean(analysed_sst)) %>%
    ungroup() %>% group_by(latitude, longitude,year) %>%
    mutate(mean_monthly_max = max(mean_monthly_max))   %>%
    ungroup() %>% group_by(latitude, longitude) %>%
    mutate(mean_monthly_max = mean(mean_monthly_max))
  
  outside_mmm <- outside %>%
    ggplot(aes(longitude, latitude)) +
    geom_raster(aes(fill = mean_monthly_max)) +
    #scale_fill_gradientn(colours=viridis::plasma(5), limits=c(0.09,0.12)) +
    ylab("Latitude") + xlab("Longitude") +
    labs(fill = "Mean Monthly Max (Celsius)") +
    theme(legend.title = element_text(color = "red", hjust = 0.5))
  
  
  inside_mmm <- inside %>%
    ggplot(aes(longitude, latitude)) +
    geom_raster(aes(fill = mean_monthly_max)) +
    #scale_fill_gradientn(colours=viridis::plasma(5), limits=c(0.09,0.12)) +
    ylab("Latitude") + xlab("Longitude") +
    labs(fill = "Mean Monthly Max (Celsius") +
    theme(legend.title = element_text(color = "red", hjust = 0.5))
  
  combined_mmm <- ggarrange(inside_mmm, outside_mmm, labels=c("Inside", "Outside"), ncol=1, nrow=2)
  combined_mmm_plot <- annotate_figure(combined_mmm, top = "Mean Monthly Maximum")
  
  ggsave(file = paste0(path_to_plots, "/mean_monthly_maximum.png"), combined_mmm_plot)
  
  
  inside = inside %>%
    mutate(
      point_bleach = analysed_sst > (mean_monthly_max + bleaching_threshold_C)
    )
  
  
  n_points = inside %>%
    group_by(date) %>%
    tally()
  n_points = n_points$n[2]
  
  
  inside = inside %>%
    group_by(year, month, date) %>%
    mutate(percent_bleach = sum(point_bleach)/n_points)
  
  rm(n_points)
  
  outside = outside %>%
    mutate(
      point_bleach = analysed_sst> (mean_monthly_max +bleaching_threshold_C))
  
  n_points = outside %>%
    group_by(year, month, date) %>%
    tally()
  n_points = n_points$n[2]
  
  outside = outside %>%
    group_by(year, month, date) %>%
    mutate(percent_bleach = sum(point_bleach)/n_points)
  
  inside_day = inside %>%
    filter(!duplicated(date))
  
  outside_day = outside %>%
    filter(!duplicated(date))
  
  rm(inside_day)
  rm(outside_day)
  
  
  #_________________________________________________________
  #degree days
  inside_degreeday <<- inside %>% 
    mutate(degreeabove = analysed_sst - mean_monthly_max) %>%
    filter(degreeabove > degree_day_threshold)
  
  inside_degreeday = inside_degreeday %>%  group_by(latitude, longitude) %>%
    mutate(
      consec = cumsum(!c(TRUE, diff(date) == 1)),
      dummy = T
    ) %>%
    ungroup() %>%group_by(latitude, longitude, consec) %>%
    mutate(
      degree_days = sum(degreeabove),
      run_length = sum(dummy),
      dummy = run_length >1
    ) %>%
    ungroup() %>%
    group_by(latitude, longitude) %>%
    filter(!duplicated(consec))%>% ungroup() %>%
    group_by(latitude, longitude, year) %>%
    mutate(
      degree_days_year = sum(degree_days),
      run_length_year = mean(run_length))
  
  inside_degreeday  = inside_degreeday %>%
    group_by(latitude, longitude, year) %>%
    mutate(n_multiday_runs = sum(dummy))
  
  inside_degreeday =  inside_degreeday%>%
    group_by(latitude, longitude) %>%
    filter(!duplicated(year) )
  
  inside_degreeday = inside_degreeday[c("latitude","longitude","year","degree_days_year", "run_length_year", "n_multiday_runs")]
  
  outside_degreeday <<- outside %>% 
    mutate(degreeabove = analysed_sst - mean_monthly_max) %>%
    filter(degreeabove > degree_day_threshold)
  
  outside_degreeday = outside_degreeday %>%  group_by(latitude, longitude) %>%
    mutate(
      consec = cumsum(!c(TRUE, diff(date) == 1)),
      dummy = T
    ) %>%
    ungroup() %>%
    group_by(
      latitude, longitude, consec
    ) %>%
    mutate(
      degree_days = sum(degreeabove),
      run_length = sum(dummy),
      dummy = run_length >1
    ) %>%
    ungroup() %>%
    group_by(latitude, longitude) %>%
    filter(!duplicated(consec))%>% ungroup() %>%
    group_by(latitude, longitude, year) %>%
    mutate(
      degree_days_year = sum(degree_days),
      run_length_year = mean(run_length))
  
  outside_degreeday  = outside_degreeday %>%
    group_by(latitude, longitude, year) %>%
    mutate(n_multiday_runs = sum(dummy))
  
  outside_degreeday = outside_degreeday%>%
    group_by(latitude, longitude) %>%
    filter(!duplicated(year))
  
  
  outside_degreeday = outside_degreeday[c("latitude","longitude","year","degree_days_year", "run_length_year", "n_multiday_runs")]
  # test = outside_degreeday %>% filter(
  #   latitude ==16.77, longitude == -169.3, consec == 81
  # )
  
  
  ########
  #percent days bleached raster
  
  degree_days_outside_2016 <- outside_degreeday %>% 
    group_by(latitude, longitude) %>% 
    filter(year == 2016) %>%
    filter(!duplicated(year)) %>%
    ggplot(aes(longitude, latitude)) +
    geom_raster(aes(fill = degree_days_year)) +
    #scale_fill_gradientn(colours=viridis::plasma(5)) +
    ylab("Latitude") + xlab("Longitude") +
    labs(fill = "Degree Days") +
    theme(legend.title = element_text(color = "red", hjust = 0.5))
  
  degree_days_inside_2016 <- inside_degreeday %>% 
    group_by(latitude, longitude) %>% 
    filter(year == 2016) %>%
    filter(!duplicated(year)) %>%
    ggplot(aes(longitude, latitude)) +
    geom_raster(aes(fill = degree_days_year)) +
    #scale_fill_gradientn(colours=viridis::plasma(5)) +
    ylab("Latitude") + xlab("Longitude") +
    labs(fill = "Degree Days") +
    theme(legend.title = element_text(color = "red", hjust = 0.5))
  
  degree_days_plot <- ggarrange(degree_days_inside_2016, degree_days_outside_2016, 
                                labels=c("Inside", "Outside"), ncol=1, nrow=2)
  degree_days_plotty <- annotate_figure(degree_days_plot, top = "Degree Days 2016")
  
  ggsave(file = paste0(path_to_plots, "/degree_days_2016.png"), degree_days_plotty)
  
  
  # tiff(filename=paste0(path_to_plots, "/bleachOutside.tiff"),
  #      res=100, width=6, height=4, units="in", compression="lzw")
  bleach_outside_graph <- outside %>%
    group_by(latitude, longitude) %>%
    summarise(time_bleached  = sum(point_bleach)*100/days_in_dataset_outside) %>%
    ggplot(aes(longitude, latitude)) +
    geom_raster(aes(fill = time_bleached)) +
    #scale_fill_gradientn(colours=viridis::plasma(5), limits=c(0.09,0.12)) +
    
    ylab("Latitude") + xlab("Longitude") +
    labs(fill = "Percent Days \nBleached") +
    theme(legend.title = element_text(color = "red", hjust = 0.5))
  #dev.off()
  
  # tiff(filename=paste0(path_to_plots, "/bleachInside.tiff"),
  #      res=100, width=6, height=4, units="in", compression="lzw")
  bleach_inside_graph <- inside %>%
    group_by(latitude, longitude) %>%
    summarise(time_bleached  = sum(point_bleach)*100/days_in_dataset_inside) %>%
    ggplot(aes(longitude, latitude)) +
    geom_raster(aes(fill = time_bleached)) +
    #scale_fill_gradientn(colours=viridis::plasma(5), limits=c(0.09,0.12)) +
    
    ylab("Latitude") + xlab("Longitude") +
    labs(fill = "Percent Days \nBleached") +
    theme(legend.title = element_text(color = "red", hjust = 0.5))
  #dev.off()
  
  percent_days_bleach <- ggarrange(bleach_inside_graph, bleach_outside_graph, 
                                   labels=c("Inside", "Outside"), ncol=1, nrow=2)
  percent_days_bleach_plot <- annotate_figure(percent_days_bleach, top = "Percent Days Bleached")
  
  ggsave(file = paste0(path_to_plots, "/percent_days_bleached.png"), percent_days_bleach_plot)
  
  
  #####
  #max sst
  #warning: this is not the hottest single day
  max_sst_out <- outside %>% 
    group_by(latitude, longitude) %>%
    filter(year == 2016) %>%
    summarise(sst = max(analysed_sst)) %>% 
    ggplot(aes(longitude, latitude)) +
    geom_raster(aes(fill = sst)) + 
    labs(fill = "Max Sea Surface \nTemperature (Celsius)") +
    theme(legend.title = element_text(size = 8, colour = "red", hjust = 0.5))+
    ylab("Latitude") + xlab("Longitude")
  
  max_sst_in <- ellipses %>% 
    group_by(latitude, longitude) %>%
    filter(location == "I", year == 2016) %>%
    summarise(sst = max(analysed_sst)) %>% 
    ggplot(aes(longitude, latitude)) +
    geom_raster(aes(fill = sst)) + 
    labs(fill = "Max Sea Surface \nTemperature (Celsius)") +
    theme(legend.title = element_text(size = 8, colour = "red", hjust = 0.5))+
    ylab("Latitude") + xlab("Longitude")
  
  max_sst_2016 <- ggarrange(max_sst_in, max_sst_out, labels=c("Inside", "Outside"), ncol=1, nrow=2)
  max_sst_2016_plot <- annotate_figure(max_sst_2016, 
                                       top = "Maximum Sea Surface Temperature \n 2016")
  
  ggsave(file = paste0(path_to_plots, "/max_sst_2016.png"), max_sst_2016_plot)
  
  
  heat_histogram_total = ellipses %>%
    group_by(location, date) %>%
    summarise(Mean = mean(analysed_sst)) %>%
    filter(Mean > outside_max_mean_summer_monthly) %>%
    ggplot(aes(Mean)) +
    geom_histogram(aes(x = Mean,group = location, color = location, fill= location),position = 'dodge') +
    xlab("Mean Celsius") +
    ylab("Frequency") +
    ggtitle(paste(atoll, "Mean Celsius by Day")) + 
    theme(plot.title = element_text(color = "red", hjust = 0.5)) 
  ggsave(file = paste0(path_to_plots, "/histogram_total.png"), heat_histogram_total)
  
  
  yearly_total <<- rbind(inside_yearly, outside_yearly)
  yearly_degree_days = yearly_total %>%
    ggplot() +
    geom_point(aes(x=year, y= degreedays_year, group = location, color = location)) +
    geom_smooth(aes(x=year, y= degreedays_year, group = location, color = location), se = F) +
    ggtitle(paste(atoll, "Yearly Degree Days")) + ylab("Total Degree Days")+ 
    theme(plot.title = element_text(color = "red", hjust = 0.5))  
  ggsave(file = paste0(path_to_plots, "/yearly_degree_days.png"), yearly_degree_days)
  
  yearly_run_l = yearly_total %>%
    ggplot() +
    geom_point(aes(x=year, y= run_length_year, group = location, color = location)) +
    geom_smooth(aes(x=year, y= run_length_year, group = location, color = location), se = F) +
    ggtitle(paste(atoll, "Yearly Mean Run Length")) + ylab("Mean Run Length") + 
    theme(plot.title = element_text(color = "red", hjust = 0.5)) 
  ggsave(file = paste0(path_to_plots, "/yearly_run_length.png"), yearly_run_l)
  
  yearly_runs = yearly_total %>%
    ggplot() +
    geom_point(aes(x=year, y= n_runs_over_1, group = location, color = location)) +
    geom_smooth(aes(x=year, y= n_runs_over_1, group = location, color = location), se = F) +
    ggtitle(paste(atoll, "Yearly Number of Runs")) + ylab("Number of Multiple Day Runs")+ 
    theme(plot.title = element_text(color = "red", hjust = 0.5)) 
  ggsave(file = paste0(path_to_plots, "/yearly_runs.png"),  yearly_runs)
  
  monthly_degree_days = yearly_total %>%
    ggplot() +
    geom_point(aes(x=date, y= degreedays_month, group = location, color = location)) +
    geom_smooth(aes(x=date, y= degreedays_month, group = location, color = location), se = F) +
    ggtitle(paste(atoll, "Monthly Degree Days")) + ylab("Total Monthly Degree Days")+ 
    theme(plot.title = element_text(color = "red", hjust = 0.5)) 
  ggsave(file = paste0(path_to_plots, "/monthly_degree_day.png"), monthly_degree_days)
  
  monthly_mean_run_l = yearly_total %>%
    ggplot() +
    geom_point(aes(x=date, y= run_length_month, group = location, color = location)) +
    geom_smooth(aes(x=date, y= run_length_month, group = location, color = location), se = F) +
    ggtitle(paste(atoll, "Monthly Mean Run Length")) + ylab("Mean Run Length")+ 
    theme(plot.title = element_text(color = "red", hjust = 0.5)) 
  ggsave(file = paste0(path_to_plots, "/monthly_run_length.png"), monthly_mean_run_l)
  
  monthly_runs = yearly_total %>%
    ggplot() +
    geom_point(aes(x=date, y= n_runs_over_1_m, group = location, color = location)) +
    geom_smooth(aes(x=date, y= n_runs_over_1_m, group = location, color = location), se = F) +
    ylab("# of Multi Day Runs")+
    ggtitle(paste(atoll, "Monthly Number of Runs"))+ 
    theme(plot.title = element_text(color = "red", hjust = 0.5)) 
  ggsave(file = paste0(path_to_plots, "/monthly_runs.png"), monthly_runs)
  
  
  ########
  ellipse_area_function <<- ellipse_area(major1, major2, minor1, minor2)
  ellipse_area_points <<- 1.2321 * n_points
  
  midpt1 <- c((major1[1] + major2[1]) / 2, (major1[2] + major2[2]) / 2)
  midpt2 <- c((minor1[1] + minor2[1]) / 2, (minor1[2] + minor2[2]) / 2)
  center <<- (midpt1 + midpt2) / 2
  
  atoll_name <- as.character(atoll)
  overall_values <<- read.csv(paste0(path_to_wd, "/Overall_values.csv"))
  overall_values[,atoll_name] <<- as.character(c("temp_loc", major1[1], major1[2], major2[1], major2[2],
                                                 minor1[1], minor1[2], minor2[1], minor2[2],
                                                 center[1], center[2],
                                                 shift[2], bleaching_threshold_C, degree_day_threshold,
                                                 days_in_dataset_inside, days_in_dataset_outside,
                                                 percent_days_bleach_inside, percent_days_bleach_outside,
                                                 pct_dys_ovr_29_5in, pct_dys_ovr_29_5out, pct_dys_ovr_30in, 
                                                 pct_dys_ovr_30out, pct_dys_ovr_30_5in, pct_dys_ovr_30_5out, 
                                                 pct_dys_ovr_31in, pct_dys_ovr_31out,
                                                 percent_days_bleach_inside_outsideMMM,
                                                 degree_days_inside, degree_days_outside,degree_days_inside_outsideMMM,
                                                 inside_max_mean_summer_monthly, outside_max_mean_summer_monthly, 
                                                 avg_run_inside, avg_run_outside,avg_run_inside_outsideMMM, 
                                                 avg_degree_days_inside, avg_degree_days_outside, avg_degree_days_inside_outsideMMM, 
                                                 n_multi_day_runs_inside, n_multi_day_runs_outside,n_multi_day_runs_inside_outsideMMM, 
                                                 ellipse_area_function, ellipse_area_points, NA))
                                               
                                               
  write_csv(overall_values, paste0(path_to_wd, "/Overall_values.csv"), append=FALSE)
  cat(green("PLOTS SUCCESSFULLY DOWNLOADED"))
}

ellipse_area <- function(major1, major2, minor1, minor2){
  major_dist <- 0.5*sqrt((major2[1] - major1[1])^2 + (major2[2] - major1[2])^2)
  minor_dist <- 0.5*sqrt((minor2[1] - minor1[1])^2 + (minor2[2] - minor1[2])^2)
  area <- 12321*pi*major_dist*minor_dist
  return(area)
}

big_ol_run <- function() {
  start <- Sys.time()
  create_url_and_files(server, sst_id, start_date, end_date, lat, long, atoll)
  download(inside_url, inside_file)
  download(outside_url, outside_file)
  fix_combine_data_frames()
  plots()
  end <- Sys.time()
  print(end - start)
}

atoll_list <- read.csv("atolls_sheet.csv", stringsAsFactors = F)
current_dat <- atoll_list[25,]

##INPUT THIS INFORMATION THEN RUN SCRIPTS; Remember to change out the info for each atoll, but this is all you need to change
server <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/"
sst_id <- "jplMURSST41.csv?analysed_sst"
start_date <- as.Date("2003-01-01T09:00:00Z")                                       
end_date <- as.Date("2019-12-31T09:00:00Z")

