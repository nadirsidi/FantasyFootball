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

# Filter for the RB data, only players with at least 8 games (half a season)
# and at least one run per game
RB.data <- ffdata.recent %>%
  dplyr::filter(position == "RB") %>%
  dplyr::add_count(First.Name, Last.Name) %>%
  dplyr::filter(n >= 8) %>%
  dplyr::filter(Rushing.Att >= 1)

# Create a histogram of all the RB weekly scores
RB.FPts.Hist <- ggplot(data = RB.data) +
  geom_histogram(aes(x = Fantasy.FPts))

print(RB.FPts.Hist)

# Walking throught the fitdistrplus package
descdist(RB.data$Fantasy.FPts, boot = 100, discrete = F)

# Reverse the data and try the fit
ScaleFPts <- function(x){ (x-min(x)) / (max(x)-min(x))}
RB.data$scaled.FPts <- ScaleFPts(RB.data$Fantasy.FPts)
fit.beta <- fitdist(RB.data$scaled.FPts, "beta", method = "mme")

par(mar = rep(2,4))
plot(fit.beta)

# Try the fit on the data without a flip (linearly transform to remove negatives)
fit.gamma <- fitdist(RB.data$Fantasy.FPts + 10, "gamma")
plot(fit.gamma)

# Median Fantasy Points for All RBs
100 - qgamma(0.5, shape = fit.gamma.flip$estimate[1], rate = fit.gamma.flip$estimate[2])

RB.data.fit <- RB.data %>%
  dplyr::group_by(Full.Name) %>%
  dplyr::mutate(shape = fitdist(100 - Fantasy.FPts, "gamma")[1][1][[1]][1]) %>%
  dplyr::mutate(rate = fitdist(100 - Fantasy.FPts, "gamma")[1][1][[1]][2]) %>%
  dplyr::mutate(median = 100 - qgamma(0.5, shape, rate)) %>%
  dplyr::mutate(fifth.percentile = 100 - qgamma(0.05, shape, rate)) %>%
  dplyr::mutate(ninety.fifth.percentile = 100 - qgamma(0.95, shape, rate)) %>%
  dplyr::add_count()

RB.scores <- RB.data.fit %>%
  dplyr::select(Full.Name, median, fifth.percentile, ninety.fifth.percentile, n) %>%
  dplyr::distinct() %>%
  dplyr::arrange(desc(median))

write.csv(RB.scores, "~/Documents/R/FantasyFootball/Data/RB_gamma_scores.csv", row.names = F)
