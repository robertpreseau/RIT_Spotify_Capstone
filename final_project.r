#install.packages("arrow", repos = "https://cran.rstudio.com")
#install.packages('dplyr')
#install.packages('ggplot2')
#install.packages('patchwork')

library(arrow)
library(dplyr)
library(ggplot2)
library(patchwork)
library(data.table)
library(sqldf) 


rm(list=ls())

##CODE TO LOAD CSV FILES AND GENERATE PARQUET EQUIVALENTS
##TRAINING DATA
#setwd('C:\\spotify_data\\training_set')
#filenames <- list.files('.', pattern="*.csv", full.names=TRUE)
#
#for (file in filenames) {
#  df <- read.csv(file)
#  out_file <- sub('.csv', '.parquet', file)
#  #print(out_file)
#  
#  write_parquet(df, out_file, compression = "gzip", compression_level = 5)
#
#}

##TRACK FEATURES
#setwd('C:\\spotify_data\\track_features')
#filenames <- list.files('.', pattern="*.csv", full.names=TRUE)
#
#for (file in filenames) {
#  df <- read.csv(file)
#  out_file <- sub('.csv', '.parquet', file)
#  #print(out_file)
#  
#  write_parquet(df, out_file, compression = "gzip", compression_level = 5)
#}

##TRACK ID & SKIP STATUS
setwd('C:\\spotify_data\\training_set')
filenames <- list.files('.', pattern="*.csv", full.names=TRUE)

for (file in filenames) {
  f <- file(file) 
  out_file <- sub('.csv', '.parquet', file)

  bigdf <- sqldf("select track_id_clean, not_skipped, count(not_skipped) from f group by track_id_clean, not_skipped", dbname = tempfile(), file.format = list(header = T, row.names = F))
  write_parquet(df, out_file, compression = "gzip", compression_level = 5)
}


ds <- open_dataset("C:\\spotify_data\\training_set_parquet")

ds_features <- open_dataset("C:\\spotify_data\\track_features_parquet")

gc()

#HOUR OF DAY
sum_hour_of_day <- ds %>% select(hour_of_day) %>% group_by(hour_of_day) %>% collect() %>% summarize(n=n())
names(sum_hour_of_day) <- c('Hour', 'Streams')

gc()

#SESSION POSITION
sum_session_position <- ds %>% select(session_position) %>% group_by(session_position) %>% collect() %>% summarize(n=n())
names(sum_session_position) <- c('Session_Position', 'Count')

gc()

#NOT SKIPPED
sum_not_skipped <- ds %>% select(not_skipped) %>% group_by(not_skipped) %>% collect() %>% summarize(n=n())
names(sum_not_skipped) <- c('Not_Skipped', 'Count')

gc()

#SESSION LENGTH
sum_session_length <- ds %>% select(session_length) %>% group_by(session_length) %>% collect() %>% summarize(n=n())
names(sum_session_length) <- c('Session_Length', 'Count')

gc()

#PREMIUM
sum_premium <- ds %>% select(premium) %>% group_by(premium) %>% collect() %>% summarize(n=n())
names(sum_premium) <- c('Premium_User', 'Count')

gc()

#HIST_USER_BEHAVIOR_REASON_START
sum_hubrs <- ds %>% select(hist_user_behavior_reason_start) %>% group_by(hist_user_behavior_reason_start) %>% collect() %>% summarize(n=n())
names(sum_hubrs) <- c('Reason_For_Start', 'Count')

gc()

#HIST_USER_BEHAVIOR_REASON_END
sum_hubre <- ds %>% select(hist_user_behavior_reason_end) %>% group_by(hist_user_behavior_reason_end) %>% collect() %>% summarize(n=n())
names(sum_hubre) <- c('Reason_For_End', 'Count')

gc()

#HIST_USER_BEHAVIOR_N_SEEKFWD
sum_seek_fwd <- ds %>% select(hist_user_behavior_n_seekfwd) %>% group_by(hist_user_behavior_n_seekfwd) %>% collect() %>% summarize(n=n())
names(sum_seek_fwd) <- c('Seek_Forward', 'Count')

gc()

#HIST_USER_BEHAVIOR_N_SEEKBACK
sum_seek_back <- ds %>% select(hist_user_behavior_n_seekback) %>% group_by(hist_user_behavior_n_seekback) %>% collect() %>% summarize(n=n())
names(sum_seek_back) <- c('Seek_Backward', 'Count')

gc()




#skip_1: dictionary<values=string, indices=int32>
#skip_2: dictionary<values=string, indices=int32>
#skip_3: dictionary<values=string, indices=int32>
#context_switch: int32
#no_pause_before_play: int32
#short_pause_before_play: int32
#long_pause_before_play: int32
#hist_user_behavior_is_shuffle: dictionary<values=string, indices=int32>
#date: dictionary<values=string, indices=int32>
#context_type: dictionary<values=string, indices=int32>


#VISUALIZE
plot_sum_hour <- ggplot(data=sum_hour_of_day, aes(x=Hour, y=Streams, group=1)) + geom_line(color="red") +
  geom_point() + ggtitle("Plot of Streams by Hour") + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

plot_sum_session_pos <- ggplot(data=sum_session_position, aes(x=Session_Position, y=Count, group=1)) +
  geom_col() + ggtitle("Plot of Session Position Counts") + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

plot_sum_not_skipped <- ggplot(data=sum_not_skipped, aes(x=Not_Skipped, y=Count, group=1)) +
  geom_col() + ggtitle("Plot of Dependent Variable 'Not_Skipped'") + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

plot_sum_session_length <- ggplot(data=sum_session_length, aes(x=Session_Length, y=Count, group=1)) +
  geom_col() + ggtitle("Plot of Session Length Counts") + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

plot_sum_premium <- ggplot(data=sum_premium, aes(x=Premium_User, y=Count, group=1)) +
  geom_col() + ggtitle("Plot of Premium User Status") + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

plot_sum_hubrs <- ggplot(data=sum_hubrs, aes(x=Reason_For_Start, y=Count, group=1)) +
  geom_col() + ggtitle("Plot of Start Reason") + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

plot_sum_hubre <- ggplot(data=sum_hubre, aes(x=Reason_For_End, y=Count, group=1)) +
  geom_col() + ggtitle("Plot of End Reason") + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

plot_sum_seek_fwd <- ggplot(data=sum_seek_fwd, aes(x=Seek_Forward, y=Count, group=1)) +
  geom_point() + ggtitle("Plot of Seek Forward") + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

plot_sum_seek_back <- ggplot(data=sum_seek_back, aes(x=Seek_Backward, y=Count, group=1)) +
  geom_point() + ggtitle("Plot of Seek Backward") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

gc()


