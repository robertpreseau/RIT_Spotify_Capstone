#LOAD PACKAGES
library(purrr)
library(data.table)
library(ggplot2)



#HOUSEKEEPING
rm(list=ls())



#LOAD DATA
setDTthreads(14)
setwd('C:\\spotify_data\\training_set')
df_main <- 
  list.files(pattern = "*.csv") %>% 
  map_df(~fread(.))

setwd('C:\\spotify_data\\track_features')
df_features <- 
  list.files(pattern = "*.csv") %>% 
  map_df(~fread(.))

gc()

df <- merge(df_main, df_features, by.x = "track_id_clean", by.y='track_id')

rm(df_main)
rm(df_features)

gc()




#PREP DATA FOR VISUALIZATIONS
sum_hour_of_day <- as.data.frame(table(df$hour_of_day))
names(sum_hour_of_day) <- c('Hour', 'Count')

sum_session_position <- as.data.frame(table(df$session_position))
names(sum_session_position) <- c('Session_Position', 'Count')




#VISUALIZE
plot_sum_hour <- ggplot(data=sum_hour_of_day, aes(x=Hour, y=Count, group=1)) + geom_line(color="red") +
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



table(df$tempo)
