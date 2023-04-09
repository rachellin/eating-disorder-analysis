library(dplyr)      # For %>%, filter, select
library(tidyr)      # For separate()
library(lubridate)  # For date functions
library(ggplot2)    # For plots
library(deSolve)    # For ods()
library(readr)      # Fore read_csv()

df <- read_csv("thinspo_tweets.csv")

df

# add day to year-month string so it can be a valid date object 
#df$month <- as.Date(paste(df$month,"-01",sep=""))
df$month <- paste(df$month, "-01", sep = "")
typeof(df$month)

# convert month column to date 
df$month <- as.Date(df$month, "%Y-%m-%d") 
typeof(df$month)

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

ggplot(df, aes(x = month, y = tweet_count))+
  geom_point(alpha= 0.5)+
  scale_x_date(date_breaks = "1 year")+
  #scale_y_continuous(breaks = seq(0, 100, 20))+
  theme_contagion()+
  labs(
    title = "Tweets with 'thinspo'",
    caption = "Data source: Tweet API
    Author: Rachel Lin")

# get SIR values 
keto_SIR <- get_sir( 
  beta = 0.2, gamma = 0.03, 
  S0 = 100, I0 = 10, R0 = 6.67, 
  times = seq(0, 70, 1)) %>% 
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
keto_SIR
keto

# plot data points with SIR infection curve 
ggplot(keto, aes(x = as.numeric(rownames(keto)), y = searches, group=1))+
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
