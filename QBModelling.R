library(plyr)
library(dplyr)
library(ggplot2)
library(MASS)
library(scales)
library(fitdistrplus)

# Read in formatted data
ffdata.raw <- readRDS("~/Documents/R/FantasyFootball/Data/FFtoday_formatted.RDS")
ffdata <- ffdata.raw

# Find all the unique players from 2016
players.2016 <- ffdata %>%
  dplyr::filter(season == 2016) %>%
  dplyr::select(First.Name, Last.Name, position) %>%
  dplyr::distinct() %>%
  dplyr::mutate(Full.Name = paste(First.Name, Last.Name))

# Filter the historic data to entries that only include players who also played in 2016
ffdata.recent <- ffdata %>%
  dplyr::mutate(Full.Name = paste(First.Name, Last.Name)) %>%
  dplyr::filter(Full.Name %in% players.2016$Full.Name) 

# Filter for the QB data, only players with at least 8 games (half a season)
QB.data <- ffdata.recent %>%
  dplyr::filter(position == "QB") %>%
  dplyr::add_count(First.Name, Last.Name) %>%
  dplyr::filter(n >= 8) %>%
  dplyr::select(-n) %>%
  dplyr::filter(Passing.Att >= 1)

# Create a histogram of all the QB weekly scores
QB.FPts.Hist <- ggplot(data = QB.data) +
  geom_histogram(aes(x = Fantasy.FPts))

print(QB.FPts.Hist)

# Walking throught the fitdistrplus package
descdist(QB.data$Fantasy.FPts, boot = 100, discrete = F)

# Reverse the data and try the fit
fit.gamma.flip <- fitdist(100 - QB.data$Fantasy.FPts, "gamma")
par(mar = rep(2,4))
plot(fit.gamma.flip)

# Try the fit on the data without a flip (linearly transform to remove negatives)
fit.gamma <- fitdist(QB.data$Fantasy.FPts + 10, "gamma")
plot(fit.gamma)

# Median Fantasy Points for All QBs
100 - qgamma(0.5, shape = fit.gamma.flip$estimate[1], rate = fit.gamma.flip$estimate[2])

QB.data.fit <- QB.data %>%
  dplyr::group_by(Full.Name) %>%
  dplyr::mutate(shape = fitdist(100 - Fantasy.FPts, "gamma")[1][1][[1]][1]) %>%
  dplyr::mutate(rate = fitdist(100 - Fantasy.FPts, "gamma")[1][1][[1]][2]) %>%
  dplyr::mutate(median = 100 - qgamma(0.5, shape, rate)) %>%
  dplyr::mutate(fifth.percentile = 100 - qgamma(0.05, shape, rate)) %>%
  dplyr::mutate(ninety.fifth.percentile = 100 - qgamma(0.95, shape, rate)) %>%
  dplyr::add_count()

QB.scores <- QB.data.fit %>%
  dplyr::select(Full.Name, median, fifth.percentile, ninety.fifth.percentile, n) %>%
  dplyr::distinct() %>%
  dplyr::arrange(desc(median))

write.csv(QB.scores, "~/Documents/R/FantasyFootball/Data/QB_gamma_scores.csv", row.names = F)

# What's up with Dak!?  
Dak <- QB.data %>%
  dplyr::filter(Full.Name == "Dak Prescott")

Dak.gamma <- fitdist(100 - Dak$Fantasy.FPts, "gamma")
plot(Dak.gamma)

# Andrew Luck  
Luck <- QB.data %>%
  dplyr::filter(Full.Name == "Andrew Luck")

Luck.gamma <- fitdist(100 - Luck$Fantasy.FPts, "gamma")
plot(Luck.gamma)
