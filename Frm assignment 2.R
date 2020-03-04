set.seed(1337)

library(pacman)
p_load(tidyverse, ggplot2, broom, glue, readxl,
       hexbin, reshape2, ggthemes, moments, quantmod, lubridate)



# Import data
data <- read_excel("BBERG_Oil_Gasoline_Prices_for_Students.xlsx", 
                                                     col_types = c("date", "numeric", "numeric", 
                                                                   "numeric"), skip = 6)
data <- data %>%
  rename(sweet.crude = `USD/BBL...2`,
         sour.crude = `USD/BBL...3`,
         CBOB.gasoline = `USD/BBL...4`)


# Create a df with returns rather than prices

ret <- data %>%
  mutate(sweet.crude.lag = lag(sweet.crude,1),
         sweet.crude.ret = (sweet.crude-sweet.crude.lag)/sweet.crude.lag,
         sour.crude.lag = lag(sour.crude,1),
         sour.crude.ret = (sour.crude-sour.crude.lag)/sour.crude.lag,
         CBOB.gasoline.lag = lag(CBOB.gasoline,1),
         CBOB.gasoline.ret = (CBOB.gasoline-CBOB.gasoline.lag)/CBOB.gasoline.lag,) %>%
  select(-c(sweet.crude, sour.crude, CBOB.gasoline,
            sweet.crude.lag, sour.crude.lag, CBOB.gasoline.lag)) %>%
  rename(sweet.crude = sweet.crude.ret,
         sour.crude = sour.crude.ret,
         CBOB.gasoline = CBOB.gasoline.ret) %>%
  drop_na()

# Think it would be smart to only include prices from 2015 in order to remove the high prices in the sample (might not be relevant)
# Can justify with the general world stance towards oil demand in the future
data2015 <- data %>%
  filter(Dates > "2015-01-01")

ret2015 <- ret %>%
  filter(Dates > "2015-01-01")

# Count number of induvidual months

nr.months <- n_distinct(ceiling_date(ymd(data$Dates), "month") - 1) # number of months since start of sample
nr.months.2015 <- n_distinct(ceiling_date(ymd(data2015$Dates), "month") - 1) #number of months since 2015

#Count sample return lengths
obs <- length(ret$Dates)
obs2015 <- length(ret2015$Dates)

# =================================================================================================
# Task 3 - based on sweet crude returns
# =================================================================================================

# #=======
# # Bootstrapping
# 
# sweet.crude.ret.sorted <- ret %>%           # Sort sweet crude return from lowest to highest
#   transmute(sweet.crude = sweet.crude) %>%
#   arrange(sweet.crude)
# 
# test.days = 30 # Sample length
# ntrials = 10000 # number of Monte Carlo trials
# 
# 
# generate.path <- function() {
#   days <- test.days
#   changes <- rnorm(days,mean=0,sd=1)
#   sample.path <- cumprod(c(0,changes))
#   sim <- sample.path[days+1] #+1 because we add the opening price
#   return(sim)
# }
# sweet.crude.sim <- as.data.frame(replicate(ntrials, generate.path()))


#=======
# Daily rolling 90-day volatility of sweet crude

vol.90 <- ret %>%
  select(Dates,sweet.crude) %>%
  mutate(vol.90d = rollapply(sweet.crude,
                                width = 90,
                                FUN = sd,
                                by.column = T,
                                fill = NA,
                                align = "right")) %>%
  select(-sweet.crude) %>%
  drop_na()

plot.vol.90 <- ggplot(vol.90) + geom_line(aes(x = Dates, y = vol.90d)) +  # plotting prices
  theme_economist() + scale_color_economist() + 
  labs(title = "90-day rolling volatility sweet crude") +
  ylab(expression(bold('90-day Vol'))) +
  xlab('')

#=======
# Prediction of vol

m.day <- 30 # how many days to reserve for testing period
training.start <- "2018-01-01" #Set start date for training sample
# Training data
training.volatility <- vol.90 %>%
  filter(Dates > training.start,
         Dates < max(ymd(Dates)-m.day))

training.return <- vol.90 %>%
  filter(Dates > training.start,
         Dates < max(ymd(Dates)-m.day))



# =================================================================================================
# Plots
# =================================================================================================


# df <- melt(data[, c("Dates", "sweet.crude", "sour.crude", "CBOB.gasoline")], id = "Dates") # make a column with type
# df.ret <- melt(ret[, c("Dates", "sweet.crude", "sour.crude", "CBOB.gasoline")], id = "Dates") # make a column with type
# 
# plot.price <- ggplot(df) + geom_line(aes(x = Dates, y = value, colour = variable)) +  # plotting prices
#   theme_economist() + scale_color_economist() + 
#   labs(title = "Price plot")
# 
# plot.ret <- ggplot(df.ret) + geom_line(aes(x = Dates, y = value, colour = variable)) +  # plotting prices
#   theme_economist() + scale_color_economist() + 
#   labs(title = "Return plot")
# # plot.ret + facet_grid()
# plot.price
# plot.ret
# 
# numb <- 10000 #Number of simulations
# 
# mean.sweet <- mean(data$sweet.crude)
# mean.sour <- mean(data$sour.crude)
# mean.CBOB <- mean(data$CBOB.gasoline)
# sd.sweet <- sd(data$sweet.crude)
# sd.sour <- sd(data$sour.crude)
# sd.CBOB <- sd(data$CBOB.gasoline)
# 
# n.sweet <- rnorm(n = numb, mean = mean.sweet, sd = sd.sweet)
# n.sour <- rnorm(n = numb, mean = mean.sour, sd = sd.sour)
# n.CBOB <- rnorm(n = numb, mean = mean.CBOB, sd = sd.CBOB)
# 
# skewness.sweet <- skewness(n.sweet)
# skewness.sour <- skewness(n.sour)
# skewness.CBOB <- skewness(n.CBOB)
# 
# kurtosis.sweet <- kurtosis(n.sweet)
# kurtosis.sour <- kurtosis(n.sour)
# kurtosis.CBOB <- kurtosis(n.CBOB)
# 
# datasim.sweet <- data.frame(n.sweet)
# ggplot(datasim.sweet, aes(x = n.sweet), 'binwidth' = 2) + 
#   geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.8) + 
#   geom_density(colour = 'blue') + xlab(expression(bold('Simulated Samples Sweet'))) + 
#   ylab(expression(bold('Density')))
# 
# datasim.sour <- data.frame(n.sour)
# ggplot(datasim.sour, aes(x = n.sour), 'binwidth' = 2) + 
#   geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.8) + 
#   geom_density(colour = 'blue') + xlab(expression(bold('Simulated Samples Sour'))) + 
#   ylab(expression(bold('Density')))
# 
# datasim.CBOB <- data.frame(n.CBOB)
# ggplot(datasim.CBOB, aes(x = n.CBOB), 'binwidth' = 2) + 
#   geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.8) + 
#   geom_density(colour = 'blue') + xlab(expression(bold('Simulated Samples CBOB'))) + 
#   ylab(expression(bold('Density')))
