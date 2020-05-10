##LOAD PACKAGES
#install.packages('dlookr')
#install.packages('installr')
#install.packages('dplyr')

# with remotes
#installr::installr()
#remotes::install_github("dreamRs/prefixer")

list.of.packages <- c('dlookr', 'data.table', 'purrr', 'dplyr', 'ggplot2', 'ggcorrplot', 'tidyr', 'Hmisc')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library('dlookr') #EDA package
library('data.table')
library('purrr') #allows for %>% syntax
library('dplyr')
library('ggplot2')
library('ggcorrplot')
library('tidyr')
library('Hmisc')

##HOUSEKEEPING
rm(list=ls())
set.seed(1738)

##LOAD DATA
setDTthreads(14)
setwd('C:\\Users\\rober\\OneDrive\\School\\DSCI799\\spotify_data\\training_set')
df_main <- list.files(pattern = "*.csv") %>% map_df(~fread(.))

#Remove any sessions that aren't 20 songs in length
df_main <- df_main %>% select(everything()) %>% filter(session_length==20)





setwd('C:\\Users\\rober\\OneDrive\\School\\DSCI799\\spotify_data\\track_features')
df_features <- list.files(pattern = "*.csv") %>% map_df(~fread(.))

gc()

df <- merge(df_main, df_features, by.x = "track_id_clean", by.y='track_id')


rm(df_main)
rm(df_features)

gc()

df$acoustic_vector_0 <- NULL
df$acoustic_vector_1 <- NULL
df$acoustic_vector_2 <- NULL
df$acoustic_vector_3 <- NULL
df$acoustic_vector_4 <- NULL
df$acoustic_vector_5 <- NULL
df$acoustic_vector_6 <- NULL
df$acoustic_vector_7 <- NULL
df$organism <- NULL
df$mechanism <- NULL
df$flatness <- NULL
df$dyn_range_mean <- NULL
df$beat_strength <- NULL
df$bounciness <- NULL
df$date <- NULL
df$hist_user_behavior_is_shuffle <- as.integer(df$hist_user_behavior_is_shuffle)
df$premium <- as.integer(df$premium)
df$not_skipped <- as.integer(df$not_skipped)
df$tempo <- as.integer(df$tempo)


#WORK ON THE SOURCE PORTION (THE FIRST 10 TRACKS)
#create a temp dataframe with the first 10 rows of data for each session
df_wide <- df %>% select(everything()) %>% filter(session_position < 11)

#roll the 10 rows up into 10 sets of columns
df_wider <- reshape(df_wide, idvar=c("session_id"), timevar = "session_position", direction="wide")



#create a temp dataframe with the last 10 rows of data for each session
df_over10 <- df %>% select(everything()) %>% filter(session_position > 10)

df_over10$skip_1 <- NULL
df_over10$skip_2 <- NULL
df_over10$skip_3 <- NULL
df_over10$context_switch <-NULL
df_over10$no_pause_before_play <- NULL
df_over10$short_pause_before_play <- NULL
df_over10$long_pause_before_play <-NULL
df_over10$hist_user_behavior_n_seekfwd <- NULL
df_over10$hist_user_behavior_n_seekback <- NULL
df_over10$hist_user_behavior_is_shuffle <-NULL
df_over10$hour_of_day <- NULL
df_over10$premium <- NULL
df_over10$context_type <- NULL
df_over10$hist_user_behavior_reason_start <- NULL
df_over10$hist_user_behavior_reason_end <- NULL





#join the 10 row monster to one row at a time for each of the remaining session rows (aka 1-10,11....1-10,12...1-10,20)
df_widest <- df_wider %>% select(everything()) %>% inner_join(df_over10, by = "session_id")

rm(df)
rm(df_over10)
rm(df_wider)
rm(df_wide)
gc()


#output the file
setwd('c:\\rxp\\train\\')

df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 0) %>% fwrite(.,'reshaped_full0.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 1) %>% fwrite(.,'reshaped_full1.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 2) %>% fwrite(.,'reshaped_full2.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 3) %>% fwrite(.,'reshaped_full3.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 4) %>% fwrite(.,'reshaped_full4.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 5) %>% fwrite(.,'reshaped_full5.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 6) %>% fwrite(.,'reshaped_full6.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 7) %>% fwrite(.,'reshaped_full7.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 8) %>% fwrite(.,'reshaped_full8.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 9) %>% fwrite(.,'reshaped_full9.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 10) %>% fwrite(.,'reshaped_full10.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 11) %>% fwrite(.,'reshaped_full11.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 12) %>% fwrite(.,'reshaped_full12.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 13) %>% fwrite(.,'reshaped_full13.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 14) %>% fwrite(.,'reshaped_full14.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 15) %>% fwrite(.,'reshaped_full15.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 16) %>% fwrite(.,'reshaped_full16.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 17) %>% fwrite(.,'reshaped_full17.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 18) %>% fwrite(.,'reshaped_full18.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 19) %>% fwrite(.,'reshaped_full19.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 20) %>% fwrite(.,'reshaped_full20.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 21) %>% fwrite(.,'reshaped_full21.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 22) %>% fwrite(.,'reshaped_full22.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 23) %>% fwrite(.,'reshaped_full23.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 24) %>% fwrite(.,'reshaped_full24.csv')


