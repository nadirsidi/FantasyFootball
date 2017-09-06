library(plyr)
library(dplyr)
library(ggplot2)
library(MASS)
library(scales)
library(fitdistrplus)

# Read in formatKd data
ffdata.raw <- readRDS("~/Documents/R/FantasyFootball/Data/FFtoday_formatKd.RDS")
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

# Filter for the K data, only players with at least 8 games (half a season)
# and at least one run per game
K.data <- ffdata.recent %>%
  dplyr::filter(position == "K") %>%
  dplyr::add_count(First.Name, Last.Name) 

# CreaK a histogram of all the K weekly scores
K.FPts.Hist <- ggplot(data = K.data) +
  geom_histogram(aes(x = Fantasy.FPts))

print(K.FPts.Hist)

# Walking throught the fitdistrplus package
descdist(K.data$Fantasy.FPts, boot = 100, discrete = F)

# Reverse the data and try the fit
ScaleFPts <- function(x){ (x-min(x)) / (max(x)-min(x))}
K.data$scaled.FPts <- ScaleFPts(K.data$Fantasy.FPts)
fit.beta <- fitdist(K.data$scaled.FPts, "beta", method = "mme")

par(mar = rep(2,4))
plot(fit.beta)

# Try the fit on the data without a flip (linearly transform to remove negatives)
fit.gamma <- fitdist(K.data$Fantasy.FPts + 10, "gamma")
plot(fit.gamma)

# Median Fantasy Points for All Ks
qgamma(0.5, shape = fit.gamma$estimaK[1], raK = fit.gamma$estimaK[2]) - 10

K.data.fit <- K.data %>%
  dplyr::group_by(Full.Name) %>%
  dplyr::add_count() %>%
  dplyr::filter(n > 8) %>%
  dplyr::mutate(mean = fitdist(Fantasy.FPts, "norm")[1][1][[1]][1]) %>%
  dplyr::mutate(sd = fitdist(Fantasy.FPts, "norm")[1][1][[1]][2]) %>%
  dplyr::mutate(median = qnorm(0.5, mean, sd)) %>%
  dplyr::mutate(fifth.percentile = qnorm(0.05, mean, sd)) %>%
  dplyr::mutate(ninety.fifth.percentile = qnorm(0.95, mean, sd))

K.scores <- K.data.fit %>%
  dplyr::select(Full.Name, median, fifth.percentile, ninety.fifth.percentile, n) %>%
  dplyr::distinct() %>%
  dplyr::arrange(desc(median))

K.scores$rank <- 1:nrow(K.scores)

K.scores <- K.scores %>%
  dplyr::select(rank, Full.Name, median, fifth.percentile, ninety.fifth.percentile, n)

KiK.csv(K.scores, "~/Documents/R/FantasyFootball/Data/K_gamma_scores.csv", row.names = F)
