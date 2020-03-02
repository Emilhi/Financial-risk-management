set.seed(1337)

library(pacman)
p_load(tidyverse, ggplot2, broom, glue, readxl, hexbin, reshape2, ggthemes, moments)



# Import data
data <- read_excel("BBERG_Oil_Gasoline_Prices_for_Students.xlsx", 
                                                     col_types = c("date", "numeric", "numeric", 
                                                                   "numeric"), skip = 6)
data <- data %>%
  rename(sweet.crude = `USD/BBL...2`,
         sour.crude = `USD/BBL...3`,
         CBOB.gasoline = `USD/BBL...4`)

# =================================================================================================
# Task 3
# =================================================================================================

df <- melt(data[, c("Dates", "sweet.crude", "sour.crude", "CBOB.gasoline")], id = "Dates") # make a column with type

ggplot(df) + geom_line(aes(x = Dates, y = value, colour = variable)) +  # plotting prices
  theme_economist() + scale_color_economist() + 
  labs(title = "Price plot")


qqline(data$sour.crude)

qplot(data$sweet.crude, geom = "histogram", binwidth = 2)


numb <- 10000

mean.sweet <- mean(data$sweet.crude)
mean.sour <- mean(data$sour.crude)
mean.CBOB <- mean(data$CBOB.gasoline)
sd.sweet <- sd(data$sweet.crude)
sd.sour <- sd(data$sour.crude)
sd.CBOB <- sd(data$CBOB.gasoline)

n.sweet <- rnorm(n = numb, mean = mean.sweet, sd = sd.sweet)
n.sour <- rnorm(n = numb, mean = mean.sour, sd = sd.sour)
n.CBOB <- rnorm(n = numb, mean = mean.CBOB, sd = sd.CBOB)

skewness.sweet <- skewness(n.sweet)
skewness.sour <- skewness(n.sour)
skewness.CBOB <- skewness(n.CBOB)

kurtosis.sweet <- kurtosis(n.sweet)
kurtosis.sour <- kurtosis(n.sour)
kurtosis.CBOB <- kurtosis(n.CBOB)

datasim.sweet <- data.frame(n.sweet)
ggplot(datasim.sweet, aes(x = n.sweet), 'binwidth' = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.8) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Simulated Samples Sweet'))) + 
  ylab(expression(bold('Density')))

datasim.sour <- data.frame(n.sour)
ggplot(datasim.sour, aes(x = n.sour), 'binwidth' = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.8) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Simulated Samples Sour'))) + 
  ylab(expression(bold('Density')))

datasim.CBOB <- data.frame(n.CBOB)
ggplot(datasim.CBOB, aes(x = n.CBOB), 'binwidth' = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.8) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Simulated Samples CBOB'))) + 
  ylab(expression(bold('Density')))
