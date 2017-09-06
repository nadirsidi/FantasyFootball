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

# Filter for the TE data, only players with at least 8 games (half a season)
# and at least one run per game
TE.data <- ffdata.recent %>%
  dplyr::filter(position == "TE") %>%
  dplyr::add_count(First.Name, Last.Name) %>%
  dplyr::filter(n >= 8) %>%
  dplyr::filter(Receiving.Target >= 1)

# Create a histogram of all the TE weekly scores
TE.FPts.Hist <- ggplot(data = TE.data) +
  geom_histogram(aes(x = Fantasy.FPts))

print(TE.FPts.Hist)

# Walking throught the fitdistrplus package
descdist(TE.data$Fantasy.FPts, boot = 100, discrete = F)

# Reverse the data and try the fit
ScaleFPts <- function(x){ (x-min(x)) / (max(x)-min(x))}
TE.data$scaled.FPts <- ScaleFPts(TE.data$Fantasy.FPts)
fit.beta <- fitdist(TE.data$scaled.FPts, "beta", method = "mme")

par(mar = rep(2,4))
plot(fit.beta)

# Try the fit on the data without a flip (linearly transform to remove negatives)
fit.gamma <- fitdist(TE.data$Fantasy.FPts + 10, "gamma")
plot(fit.gamma)

# Median Fantasy Points for All TEs
qgamma(0.5, shape = fit.gamma$estimate[1], rate = fit.gamma$estimate[2]) - 10

TE.data.fit <- TE.data %>%
  dplyr::group_by(Full.Name) %>%
  dplyr::add_count() %>%
  dplyr::filter(n > 8) %>%
  dplyr::mutate(shape = fitdist(100 - Fantasy.FPts, "gamma")[1][1][[1]][1]) %>%
  dplyr::mutate(rate = fitdist(100 - Fantasy.FPts, "gamma")[1][1][[1]][2]) %>%
  dplyr::mutate(median = 100 - qgamma(0.5, shape, rate)) %>%
  dplyr::mutate(fifth.percentile = 100 - qgamma(0.05, shape, rate)) %>%
  dplyr::mutate(ninety.fifth.percentile = 100 - qgamma(0.95, shape, rate))

TE.scores <- TE.data.fit %>%
  dplyr::select(Full.Name, median, fifth.percentile, ninety.fifth.percentile, n) %>%
  dplyr::distinct() %>%
  dplyr::arrange(desc(median))

TE.scores$rank <- 1:nrow(TE.scores)

TE.scores <- TE.scores %>%
  dplyr::select(rank, Full.Name, median, fifth.percentile, ninety.fifth.percentile, n)

TEite.csv(TE.scores, "~/Documents/R/FantasyFootball/Data/TE_gamma_scores.csv", row.names = F)
