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
setwd('C:\\Users\\rober\\OneDrive\\School\\DSCI799\\spotify_data\\test_set')
df_main <- read.csv('log_prehistory_20180801_000000000000.csv')
df_test <- read.csv('log_input_20180801_000000000000.csv')


setwd('C:\\Users\\rober\\OneDrive\\School\\DSCI799\\spotify_data\\track_features')
df_features <- list.files(pattern = "*.csv") %>% map_df(~fread(.))

gc()

df_main <- merge(df_main, df_features, by.x = "track_id_clean", by.y='track_id')
df_test <- merge(df_test, df_features, by.x = "track_id_clean", by.y='track_id')


rm(df_features)
gc()


df_main$acoustic_vector_0 <- NULL
df_main$acoustic_vector_1 <- NULL
df_main$acoustic_vector_2 <- NULL
df_main$acoustic_vector_3 <- NULL
df_main$acoustic_vector_4 <- NULL
df_main$acoustic_vector_5 <- NULL
df_main$acoustic_vector_6 <- NULL
df_main$acoustic_vector_7 <- NULL
df_main$organism <- NULL
df_main$mechanism <- NULL
df_main$flatness <- NULL
df_main$dyn_range_mean <- NULL
df_main$beat_strength <- NULL
df_main$bounciness <- NULL
df_main$date <- NULL
df_main$context_type <- as.factor(df_main$context_type)
df_main$hour_of_day <- as.factor(df_main$hour_of_day)
df_main$hist_user_behavior_reason_start <- as.factor(df_main$hist_user_behavior_reason_start)
df_main$hist_user_behavior_reason_end <- as.factor(df_main$hist_user_behavior_reason_end)
df_main$mode <- as.factor(df_main$mode)
df_main$hist_user_behavior_is_shuffle <- as.integer(df_main$hist_user_behavior_is_shuffle)
df_main$premium <- as.integer(df_main$premium)
df_main$not_skipped <- as.integer(df_main$not_skipped)
df_main$tempo <- as.integer(df_main$tempo)

df_test$acoustic_vector_0 <- NULL
df_test$acoustic_vector_1 <- NULL
df_test$acoustic_vector_2 <- NULL
df_test$acoustic_vector_3 <- NULL
df_test$acoustic_vector_4 <- NULL
df_test$acoustic_vector_5 <- NULL
df_test$acoustic_vector_6 <- NULL
df_test$acoustic_vector_7 <- NULL
df_test$organism <- NULL
df_test$mechanism <- NULL
df_test$flatness <- NULL
df_test$dyn_range_mean <- NULL
df_test$beat_strength <- NULL
df_test$bounciness <- NULL
df_test$date <- NULL
df_test$mode <- as.factor(df_test$mode)
df_test$tempo <- as.integer(df_test$tempo)






#WORK ON THE SOURCE PORTION (THE FIRST 10 TRACKS)
#create a temp dataframe with the first 10 rows of data for each session

#roll the 10 rows up into 10 sets of columns
df_wider <- reshape(df_main, idvar=c("session_id"), timevar = "session_position", direction="wide")




#join the 10 row monster to one row at a time for each of the remaining session rows (aka 1-10,11....1-10,12...1-10,20)
df_widest <- df_wider %>% select(everything()) %>% inner_join(df_test, by = "session_id")

rm(df)
rm(df_features)
rm(df_main)
rm(df_test)
rm(df_over10)
rm(df_wider)
rm(df_wide)
gc()


#output the file
setwd('c:\\rxp\\')

df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 0) %>% fwrite(.,'reshaped_test0.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 1) %>% fwrite(.,'reshaped_test1.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 2) %>% fwrite(.,'reshaped_test2.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 3) %>% fwrite(.,'reshaped_test3.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 4) %>% fwrite(.,'reshaped_test4.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 5) %>% fwrite(.,'reshaped_test5.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 6) %>% fwrite(.,'reshaped_test6.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 7) %>% fwrite(.,'reshaped_test7.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 8) %>% fwrite(.,'reshaped_test8.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 9) %>% fwrite(.,'reshaped_test9.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 10) %>% fwrite(.,'reshaped_test10.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 11) %>% fwrite(.,'reshaped_test11.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 12) %>% fwrite(.,'reshaped_test12.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 13) %>% fwrite(.,'reshaped_test13.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 14) %>% fwrite(.,'reshaped_test14.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 15) %>% fwrite(.,'reshaped_test15.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 16) %>% fwrite(.,'reshaped_test16.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 17) %>% fwrite(.,'reshaped_test17.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 18) %>% fwrite(.,'reshaped_test18.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 19) %>% fwrite(.,'reshaped_test19.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 20) %>% fwrite(.,'reshaped_test20.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 21) %>% fwrite(.,'reshaped_test21.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 22) %>% fwrite(.,'reshaped_test22.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 23) %>% fwrite(.,'reshaped_test23.csv')
df_widest %>% select(everything()) %>% filter(hour_of_day.10 == 24) %>% fwrite(.,'reshaped_test24.csv')

