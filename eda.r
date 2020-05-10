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
setwd('C:\\Users\\Robert Preseau\\OneDrive\\School\\DSCI799\\spotify_data\\training_set')
df_main <- list.files(pattern = "*.csv") %>% map_df(~fread(.))

setwd('C:\\Users\\Robert Preseau\\OneDrive\\School\\DSCI799\\spotify_data\\track_features')
df_features <- list.files(pattern = "*.csv") %>% map_df(~fread(.))

gc()

df <- merge(df_main, df_features, by.x = "track_id_clean", by.y='track_id')

rm(df_main)
rm(df_features)

gc()


##DATA CLEANUP
df$context_type <- as.factor(df$context_type)
df$hour_of_day <- as.factor(df$hour_of_day)
df$hist_user_behavior_reason_start <- as.factor(df$hist_user_behavior_reason_start)
df$hist_user_behavior_reason_end <- as.factor(df$hist_user_behavior_reason_end)
df$mode <- as.factor(df$mode)
df$hist_user_behavior_is_shuffle <- as.integer(df$hist_user_behavior_is_shuffle)
df$premium <- as.integer(df$premium)
df$not_skipped <- as.integer(df$not_skipped)
df$tempo <- as.integer(df$tempo)

#move our dependent variable to the end of the list
df <- within(df, rm(skip_1, skip_2, skip_3))
df <- df %>% select(-not_skipped,not_skipped)
gc()



df %>% select(session_position, session_length, hour_of_day, context_type) %>% hist.data.frame()
df %>% select(hist_user_behavior_reason_start, hist_user_behavior_reason_end, duration, release_year) %>% hist.data.frame()
df %>% select(us_popularity_estimate, acousticness, beat_strength, bounciness) %>% hist.data.frame()

df %>% select(danceability, dyn_range_mean, energy, flatness) %>% hist.data.frame()

df %>% select(instrumentalness, key, liveness, loudness) %>% hist.data.frame()
df %>% select(mechanism, organism, speechiness, tempo) %>% hist.data.frame()
df %>% select(time_signature, valence, acoustic_vector_0, acoustic_vector_1) %>% hist.data.frame()
df %>% select(acoustic_vector_2, acoustic_vector_3, acoustic_vector_4, acoustic_vector_5) %>% hist.data.frame()
df %>% select(acoustic_vector_6, acoustic_vector_7) %>% hist.data.frame()

df %>% select(not_skipped) %>% hist.data.frame()


plot(table(df$hour_of_day), type='p')




ggplot(data=as.data.frame(table(df$hour_of_day)), aes(x=Var1, y=Freq, group=1)) +
  geom_line(color="red") +
  ggtitle("Counts by Hour Of Day") +
  xlab('Hour') +
  ylab('Total Streams') +
  geom_point()





##GENERATE EDA REPORT
options(tinytex.verbose = TRUE)


cor(df_num_data, use = "complete.obs", method = "pearson")

df %>% eda_report(target = not_skipped, output_dir = "C:\\spotify_data\\training_set_EDA3", output_format = c("pdf"), output_file = "EDA.pdf", browse = FALSE)








#REBUILD CORRELATION MATRIX
# Compute a correlation matrix
corr <- round(cor(Filter(is.numeric, df)), 1)

# Compute a matrix of correlation p-values
p.mat <- cor_pmat(Filter(is.numeric, df))

#graph
ggplot(reshape2::melt(corr), aes(Var1, Var2, fill=value)) +
  geom_tile(height=0.8, width=0.8) +
  scale_fill_gradient2(low="blue", mid="white", high="red") +
  theme_minimal() +
  coord_equal() +
  labs(x="",y="",fill="Corr") +
  theme(axis.text.x=element_text(size=10, angle=90, vjust=0, hjust=0, 
                                 margin=margin(-3,0,0,0)),
        axis.text.y=element_text(size=10, margin=margin(0,-3,0,0)),
        panel.grid.major=element_blank())



##LOOK AT ONE SESSION
df_session <- select(filter(df, session_id == '17_51d2a810-9a51-4989-8315-4b306438bb26'), everything())

#Session Position & Skip
plot(df_session$session_position, df_session$not_skipped)

plot(df_session$tempo, df_session$not_skipped)


session_position_skipped <- df %>% 
  select(not_skipped, session_position) %>% 
  group_by(session_position, not_skipped) %>% 
  summarize(Percentage=n()) %>%
  group_by(session_position) %>% 
  mutate(Percentage=Percentage/sum(Percentage)*100) %>%
  filter(not_skipped == 0)

tempo_skipped <- df %>% 
  select(not_skipped, tempo) %>% 
  group_by(tempo, not_skipped) %>% 
  summarize(Percentage=n()) %>%
  group_by(tempo) %>% 
  mutate(Percentage=Percentage/sum(Percentage)*100) %>%
  filter(not_skipped == 0)

key_skipped <- df %>% 
  select(not_skipped, key) %>% 
  group_by(key, not_skipped) %>% 
  summarize(Percentage=n()) %>%
  group_by(key) %>% 
  mutate(Percentage=Percentage/sum(Percentage)*100) %>%
  filter(not_skipped == 0)



##skipped percentages
ggplot(data=not_skipped, aes(x=position_skipped, y=Percentage, group=1)) +
  geom_line(color="red") +
  geom_point() +
  expand_limits(x = 0, y = 0)

##skipped percentages - Key
ggplot(data=key_skipped, aes(x=key, y=Percentage, group=1)) +
  geom_line(color="red") +
  geom_point() +
  expand_limits(x = 0, y = 0)


##skipped percentages
ggplot(data=tempo_skipped, aes(x=tempo, y=Percentage, group=1)) +
  geom_line(color="red") +
  geom_point() +
  expand_limits(x = 0, y = 0)




df %>% 
  summarise_each(funs(100*mean(is.na(.))))


