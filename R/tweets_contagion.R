library(dplyr)      # For %>%, filter, select
library(tidyr)      # For separate()
library(lubridate)  # For date functions
library(ggplot2)    # For plots
library(deSolve)    # For ods()
library(readr)      # Fore read_csv()

df <- read_csv("thinspo_tweets.csv")
twitter_bass <- read_csv("twitter_bass.csv")

twitter_bass <- twitter_bass[1:60,]
twitter_bass

# add day to year-month string so it can be a valid date object 
#df$month <- as.Date(paste(df$month,"-01",sep=""))
df$month <- paste(df$month, "-01", sep = "")

# convert month column to date 
df$month <- as.Date(df$month, "%Y-%m-%d")

# select data starting from 2017
df <- df[df$month >= "2017-01-01",]
df

theme_contagion <- function() {
  theme_bw()+
    theme(
      plot.title         = element_text(
        hjust = 0.5, size = 20, colour="black", face = "bold"),
      plot.subtitle      = element_text(
        hjust = 0.5, size = 16, colour="black", face = "bold"),
      legend.title       = element_text(
        hjust = 0.5, size = 14, colour="black", face = "bold"),
      plot.caption       = element_text(size = 10, colour="black"),
      axis.title         = element_text(size = 14, colour="black"),
      axis.text.x        = element_text(
        size = 12, colour="black"),
      axis.text.y        = element_text(size = 12, colour="black"),
      legend.position    = 'bottom',
      legend.direction   = "horizontal",
      legend.text        = element_text(size = 12, colour="black")
    )
}


# SCATTERPLOT: # of tweets 
ggplot()+
  geom_point(df, mapping = aes(x = as.numeric(rownames(df)), y = tweet_count), alpha= 0.5)+
  geom_vline(xintercept = 39, colour = "blue")+
  geom_line(data = twitter_bass, mapping = aes(x = 1:60, y = `Total Infected`))+
  geom_text(
    aes(
      x=39,
      label="pandemic start\n"
      , y = 7500),
    colour="blue"
    , angle=90)+
  theme_contagion()+
  xlab("time series")+
  ylab("# of tweets")+
  labs(
    title = "Tweets with 'thinspo'",
    caption = "Data source: Tweet API
    P_contact: 0.31, P_spread: 0.2")


