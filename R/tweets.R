library(dplyr)      # For %>%, filter, select
library(tidyr)      # For separate()
library(lubridate)  # For date functions
library(ggplot2)    # For plots
library(deSolve)    # For ods()
library(readr)      # Fore read_csv()

df <- read_csv("thinspo_tweets.csv")
mdau_df <- read_csv("clean data/twitter_mdau.csv")

tiktok <- read_csv("proed_tiktok.csv")

tiktok


# add day to year-month string so it can be a valid date object 
#df$month <- as.Date(paste(df$month,"-01",sep=""))
df$month <- paste(df$month, "-01", sep = "")
tiktok$month <- paste(tiktok$month, "-01", sep = "")

# convert month column to date 
df$month <- as.Date(df$month, "%Y-%m-%d")
tiktok$month <- as.Date(tiktok$month, "%Y-%m-%d")
mdau_df$quarter <- as.Date(mdau_df$quarter, "%Y-%m-%d")
mdau_df
tiktok

# select data starting from 2017
df <- df[df$month >= "2017-01-01",]

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
ggplot(df, aes(x = month, y = tweet_count))+
  geom_point(alpha= 0.5)+
  geom_vline(xintercept = ymd("2020-03-11"))+
  geom_text(
    aes(
      x=ymd("2020-03-11"),
      label="pandemic start\n"
      , y = 7500),
    colour="blue"
    , angle=90)+
  scale_x_date(date_breaks = "1 year")+
  #scale_y_continuous(breaks = seq(0, 100, 20))+
  theme_contagion()+
  labs(
    title = "Tweets with 'thinspo'",
    caption = "Data source: Tweet API
    Author: Rachel Lin")

# SCATTERPLOT: tiktok
# remove outlier
tiktok <- tiktok[-59,]

ggplot(tiktok, aes(x = month, y = play_count))+
  geom_point(alpha= 0.5)+
  geom_vline(xintercept = ymd("2020-03-11"))+
  geom_text(
    aes(
      x=ymd("2020-03-11"),
      label="pandemic start\n"
      , y = 750000),
    colour="blue"
    , angle=90)+
  scale_x_date(date_breaks = "1 year")+
  #scale_y_continuous(breaks = seq(0, 100, 20))+
  theme_contagion()+
  labs(
    title = "tiktok",
    caption = "Data source: todo
    Author: Rachel Lin")

mdau_df
# SCATTERPLOT: mdau 
ggplot(mdau_df, aes(x = quarter, y = mdau))+
  geom_point(alpha= 0.5)+
  geom_line()+
  geom_vline(xintercept = ymd("2020-03-11"))+
  geom_text(
    aes(
      x=ymd("2020-03-11"),
      label="pandemic start\n"
      , y = 162.5),
    colour="blue"
    , angle=90)+
  scale_x_date(date_breaks = "1 year")+
  #scale_y_continuous(breaks = seq(0, 100, 20))+
  theme_contagion()+
  labs(
    title = "Twitter mDAU Worldwide",
    caption = "Data source: Statista
    Author: Rachel Lin
    mDAU = monetizable daily active users")

# STANDARDIZE VALUES 
mdau_mean <- mean(mdau_df$mdau, na.rm = TRUE)
mdau_sd <- sd(mdau_df$mdau, na.rm = TRUE)
mdau_std <- (mdau_df$mdau - mdau_mean)/mdau_sd # standardized mean value
mdau_std

tweet_count_mean <- mean(df$tweet_count)
tweet_count_sd <- sd(df$tweet_count)
tweet_count_std <- (df$tweet_count - tweet_count_mean)/tweet_count_sd
tweet_count_std

mdau_std_df <- mdau_df
mdau_std_df["mdau"] <- mdau_std
mdau_std_df

tweet_std_df <- df
tweet_std_df["tweet_count"] <- tweet_count_std
tweet_std_df

# SCATTERPLOT: # of tweets + mdau (w/ standardized values)
ggplot()+
  geom_point(data = tweet_std_df, aes(x = month, y = tweet_count, color = "blue"))+
  geom_point(data = mdau_std_df, aes(x = quarter, y = mdau, color = "red"))+
  geom_line(data = tweet_std_df, aes(x = month, y = tweet_count, color = "blue"))+
  geom_line(data = mdau_std_df, aes(x = quarter, y = mdau, color = "red"))+
  scale_color_manual(labels = c("tweet count", "mDAU"), values = c("blue", "red")) +
  geom_vline(xintercept = ymd("2020-03-11"))+
  geom_text(
    aes(
      x=ymd("2020-03-11"),
      label="pandemic start\n"
      , y = 0.75),
    colour="blue"
    , angle=90)+
  scale_x_date(date_breaks = "1 year")+
  theme_contagion()+
  ylab("standardized values")+
  labs(
    title = "Tweets w/ 'thinspo' & Twitter mDAU Worldwide",
    caption = "Data source: Statista, Twitter API
    Author: Rachel Lin")

get_sir <- function(beta, gamma, S0, I0, R0, times) {
  # Equation for Calculating SIR 
  sir_equations <- function(time, variables, parameters) {
    with(as.list(c(variables, parameters)), {
      N = S+I+R
      lambda = beta*(I/N) 
      dS = -lambda*S
      dI = lambda*S-gamma*I
      dR = gamma*I
      return(list(c(dS, dI, dR)))
    })
  }
  
  # Parameter Values
  parameters_values <- c(beta = beta, gamma = gamma)
  
  # Initial Values
  initial_values <- c(S = S0, I = I0, R = R0)
  
  # Generate the model
  out <- ode(initial_values, times, 
             sir_equations, parameters_values)
  
  # Return
  as.data.frame(out)
}

get_bass <- function(P_broad, S, P_diffuse, I, S, N_pop) {
  
}

# get SIR values 
keto_SIR <- get_sir( 
  beta = 0.5, gamma = 0, 
  S0 = 2000, I0 = 10, R0 = 6.67, 
  times = seq(0, 169, 1)) %>% 
  reshape2::melt(id = "time")

# plot SIR 
ggplot(
  keto_SIR, 
  aes(x = time, y = value, 
      group = variable, color = variable))+
  geom_line(size = 2)+
  theme_contagion()

I_keto <- keto_SIR %>% 
  filter(variable == "I")
I_keto
keto_SIR
keto

# bass model
# P_broad * S + P_diffuse * (I_t * S)/N_pop 
# P_diffuse * (I_t * (N_pop - I_t))/N_pop
I <- 0.054 * (1000 * (34000 - 1000))/34000 

# plot data points with SIR infection curve 
ggplot(df, aes(x = as.numeric(rownames(df)), y = tweet_count, group=1))+
  geom_point()+
  geom_line(I_keto, mapping = aes(x = time, y = value, group = 1))+
  labs(
    title = "Searches for 'Keto'",
    caption = "Data source: Google Trends
    beta: 0.2, gamma: 0.03, s0: 100, I0: 10, R0: 6.67"
  )+
  xlab("# of day in series")+
  ylab("# of searches")+
  theme_contagion()
