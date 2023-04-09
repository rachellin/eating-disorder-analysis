library(dplyr)      # For %>%, filter, select
library(tidyr)      # For separate()
library(lubridate)  # For date functions
library(ggplot2)    # For plots
library(deSolve)    # For ods()
library(readr)      # Fore read_csv()
library(readxl)

twitter <- read_csv("thinspo_tweets.csv")
tiktok <- read_csv("proed_tiktok.csv")
df <- read_csv("policing_vs_interactions.csv")
df_avg <- read_csv("policing_vs_interactions_avg.csv")
df_months <- read_csv("spatial-police.csv")
df_months
df_avg

# convert to date 
df_months$month <- paste(df_months$month, "-01", sep = "")
df_months$month <- as.Date(df_months$month, "%Y-%m-%d")
df_months

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


ggplot(df, aes(x = ranking, y = interactions, color = platform))+
  geom_point(alpha= 0.5)+
  
  #scale_y_continuous(breaks = seq(0, 100, 20))+
  theme_contagion()+
  labs(
    title = "Tweets with 'thinspo'",
    caption = "Data source: Tweet API
    Author: Rachel Lin")

# w/ log transformation
ggplot(df, aes(x = ranking, y = log(interactions), color = platform))+
  geom_point(alpha= 0.5, size=4)+
  scale_color_manual(values=c("#ff4500", "#ee1d52", "#e1306c","#a044cd", "#1da1f2"))+
  #scale_y_continuous(breaks = seq(0, 100, 20))+
  theme_contagion()+
  labs(
    title = "Platform Policing vs. # of Interactions")
    

# with months + log transformation
ggplot(df_months, aes(x = ranking, y = log(interactions), color = platform))+
  geom_point(size=2.5, alpha= 0.5, position = position_jitter(0.3))+
  #scale_color_manual(values=c("#1da1f2", "#35465d", "#69c9d0", "#ff4500"))+
  scale_color_manual(values=c("#ff4500", "#ee1d52", "#a044cd", "#1da1f2"))+
  #scale_y_continuous(breaks = seq(0, 500000, 100000))+
  #scale_y_continuous(limits=seq(0, 500000, 10000))+
  theme_contagion()+
  labs(
    title = "Platform Policing vs. # of Interactions")

# w/o tiktok
df
nott_df <- df[-4,]
nott_df 
ggplot(nott_df, aes(x = ranking, y = interactions, color = platform))+
  geom_point(alpha= 0.5)+
  
  #scale_y_continuous(breaks = seq(0, 100, 20))+
  theme_contagion()+
  labs(
    title = "Tweets with 'thinspo'",
    caption = "Data source: Tweet API
    Author: Rachel Lin")

# avg
ggplot(df_avg, aes(x = ranking, y = interactions, color = platform))+
  geom_point(alpha= 0.5)+
  
  #scale_y_continuous(breaks = seq(0, 100, 20))+
  theme_contagion()+
  labs(
    title = "Tweets with 'thinspo'",
    caption = "Data source: Tweet API
    Author: Rachel Lin")


# with months
ggplot(df_months, aes(x = ranking, y = interactions, color = platform))+
  geom_point(alpha= 0.5)+
  
  #scale_y_continuous(breaks = seq(0, 500000, 100000))+
  #scale_y_continuous(limits=seq(0, 500000, 10000))+
  theme_contagion()+
  labs(
    title = "Platform Policing vs. # of Interactions",
    caption = "Author: Rachel Lin")

# with months + log transformation
ggplot(df_months, aes(x = ranking, y = log(interactions), color = platform))+
  geom_point(size=2.5, alpha= 0.5, position = position_jitter(0.3))+
  #scale_color_manual(values=c("#1da1f2", "#35465d", "#69c9d0", "#ff4500"))+
  scale_color_manual(values=c("#ff4500", "#ee1d52", "#a044cd", "#1da1f2"))+
  #scale_y_continuous(breaks = seq(0, 500000, 100000))+
  #scale_y_continuous(limits=seq(0, 500000, 10000))+
  theme_contagion()+
  labs(
    title = "Platform Policing vs. # of Interactions")

# with months + log transformation + gradient (by month)
# convert date to integer 
df_months$time_int <- as.integer(df_months$month)
df_months
ggplot(df_months, aes(x = ranking, y = log(interactions), color = platform))+
  geom_point(size=5,alpha= 0.5, position = position_jitter(0.5), aes(color = time_int))+
  scale_colour_identity()
  #scale_y_continuous(breaks = seq(0, 500000, 100000))+
  #scale_y_continuous(limits=seq(0, 500000, 10000))+
  theme_contagion()+
  labs(
    title = "Platform Policing vs. # of Interactions",
    caption = "Author: Rachel Lin")

# facet wrap
ggplot(df_months, aes(x = 0, y = interactions, color = platform))+
  geom_point(alpha= 0.5, position = position_jitter(10))+
  
  #scale_y_continuous(breaks = seq(0, 500000, 100000))+
  #scale_y_continuous(limits=seq(0, 500000, 10000))+
  facet_wrap(vars(platform), scales = "free_y")+
  theme_contagion()+
  labs(
    title = "Platform Policing vs. # of Interactions",
    caption = "Author: Rachel Lin")

