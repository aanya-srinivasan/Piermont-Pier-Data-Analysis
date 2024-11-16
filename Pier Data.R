getwd()
setwd("C:/Users/srini/Downloads/EDA Labs")
Pier <- read.table ("PiermontPier_data_2017_daily.csv", header = T, sep = ",")
View(Pier)
Pier$Date <- as.Date(Pier$DateTime, "%m/%d/%y")
View(Pier)
str(Pier)
library(tidyverse)
library(ggplot2)
ggplot(Pier, aes(x= Date, y= DissolvedOxygen)) +
  xlab ("Date") + ylab("Dissolved Oxygen (ppm)") +
  ggtitle("Dissolved Oxygen by Date") +
  geom_point (shape = 1) +
  geom_smooth(method = lm,
              se = FALSE)
ggplot(Pier, aes(x= Date, y= Acidity)) +
  xlab ("Date") + ylab("Acidity (pH)") +
  ggtitle("Acidity by Date") +
  geom_point (shape = 1) +
  geom_smooth(method = lm,
              se = FALSE)

str(Pier)
Pier_regression <- Pier [ 2:9]
str(Pier_regression)
pairs(Pier_regression)
cor(Pier_regression)

mod1 <- lm(DissolvedOxygen ~ Acidity + Salinity + Turbidity + WaterTemp + WindSpeed + AirTemp + SpecificConductivity, data = Pier)
summary(mod1)
mod2 <- lm(DissolvedOxygen ~ Acidity + Salinity + Turbidity + WaterTemp + AirTemp + SpecificConductivity, data = Pier)
summary(mod2)
mod3 <- lm(DissolvedOxygen ~ Acidity + Salinity + Turbidity + WaterTemp + SpecificConductivity, data = Pier)
summary(mod3)
mod4 <- lm(DissolvedOxygen ~ Acidity + Salinity + Turbidity + WaterTemp + DewPoint, data = Pier)
summary(mod4)