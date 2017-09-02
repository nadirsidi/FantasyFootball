library(plyr)
library(dplyr)
library(ggplot2)
library(MASS)
library(scales)

# Read in formatted data
ffdata.raw <- readRDS("~/Documents/R/FantasyFootball/Data/FFtoday_formatted.RDS")

# Rules --
 
# Passing - 
PY = 0.04 # Passing Yard
INT = -2 # Interception
PTD = 4 # Passing TD
TWOPC = 2 # 2-Pt Conversion
  
# Rushing -
RY = 0.1 # Rushing Yard
TWOPR = 2 # 2-Pt Conversion Rushing
RTD = 6 # Rushing TD
     
# Receiving - 
REY = 0.1 # Recieving Yard
TWOPRE = 2 # 2-Pt Conversion Reception
RETD = 6 # Recieving TD

# Miscellaneous -
# KRTD = 6 # Kick-Return TD
# FTD = 6 # Fumbled Recovered for TD
# INTTD = 6 # Pick-Six
# BLKKRTD = 6 # Blocked Kick Returned for TD
# PRTD = 6 # Punt Return TD
# FRTD = 6 # Fumbled Returned for TD
DTD = 6 # All Defensive TDs
FUML = -2 # Fumble

# Kicking -
PAT = 1 # Successful point-after
FG0 = 3 # 0-39 yd FG
FG50 = 5 # 50+ FG
FG40 = 4 # 40-49 yd FG
FGM = -1 # Missed Field Goal

# Add in the Fantasy Points with Custom Scoring
# QB Scoring
ffdata$GrnMtn.FPts = 0
QB.index = (ffdata$position == "QB")

ffdata$GrnMtn.FPts[QB.index] = ffdata$Passing.Yard[QB.index] * PY + 
  ffdata$Passing.INT[QB.index] * INT +
  ffdata$Passing.TD[QB.index] * PTD +
  ffdata$Rushing.Yard[QB.index] * RY +
  ffdata$Rushing.TD[QB.index] * RTD

# It looks like my league's scoring is close to ESPN standard-- using the 
# ESPN standard values from FFToday

ffdata <- dplyr::select(ffdata, -GrnMtn.FPts)

# Filter for all QB
ffdata.QB <- ffdata %>%
  dplyr::filter(position == "QB") %>%
  dplyr::filter(Passing.Att > 0)

# Create a histogram of all the QB weekly scores
QB.Fantasy.FPts.Hist <- ggplot(data = ffdata.QB) +
  geom_histogram(aes(x = Fantasy.FPts)) +
  geom_line(aes(x = 0:50, y = dgamma(0:50,6.736,0.3)),color="red")

# Linearly Transform the scores, fit a gamma, graph a non-transformed density function
QB.FPts.Trans = ffdata.QB$Fantasy.FPts + 10
QB.FPts.Trans.Gamma = fitdistr(QB.FPts.Trans, "gamma")

QB.Fantasy.FPts.Hist <- ggplot(data = ffdata.QB) +
  geom_line(aes(x = Fantasy.FPts, y = dgamma(Fantasy.FPts+10,6.736,0.3)),color="red")

print(QB.Fantasy.FPts.Hist)

# Create a histrogram for Aaron Rogers
A.Rodgers <- ffdata.QB %>%
  dplyr::filter(First.Name == "Aaron" & Last.Name == "Rodgers")

hist(A.Rodgers$Fantasy.FPts)
A.Rodgers.Gamma = fitdistr(A.Rodgers$Fantasy.FPts + 10, "gamma")
A.Rodgers.Plot <- ggplot(data = A.Rodgers) +
  geom_line(aes(x = A.Rodgers$Fantasy.FPts, 
                y = dgamma(A.Rodgers$Fantasy.FPts+10, A.Rodgers.Gamma[[1]][1], A.Rodgers.Gamma[[1]][2])))

print(A.Rodgers.Plot)

# Quantiles for A.Rodgers
qgamma(c(0.05, 0.25, 0.5, 0.75, 0.95), shape = A.Rodgers.Gamma[[1]][1], rate = A.Rodgers.Gamma[[1]][2]) - 10


# Looking at Matt Ryan
M.Ryan <- ffdata.QB %>%
  dplyr::filter(First.Name == "Matt" & Last.Name == "Ryan")

hist(M.Ryan$Fantasy.FPts)
M.Ryan.Gamma = fitdistr(M.Ryan$Fantasy.FPts + 10, "gamma")
M.Ryan.Plot <- ggplot(data = M.Ryan) +
  # geom_histogram(aes(x = M.Ryan$Fantasy.FPts / nrow(M.Ryan) + 10)) + 
  geom_line(aes(x = M.Ryan$Fantasy.FPts, 
                y = dgamma(M.Ryan$Fantasy.FPts+10, M.Ryan.Gamma[[1]][1], M.Ryan.Gamma[[1]][2]),
                color = "red"))

print(M.Ryan.Plot)

qgamma(c(0.05, 0.25, 0.5, 0.75, 0.95), shape = M.Ryan.Gamma[[1]][1], rate = M.Ryan.Gamma[[1]][2]) - 10

# Plot pass attempts versus recieving yards

passing.data <- ffdata %>%
  dplyr::filter(season == "2016") %>%
  dplyr::filter(position == "QB" | position == "WR") %>%
  dplyr::filter(position == "WR" | Passing.Att > 0) %>%
  dplyr::select(Team, week, season, Passing.Att, Receiving.Yard) %>%
  dplyr::group_by(Team, week, season) %>%
  dplyr::mutate(Tot.Receiving.Yard = sum(Receiving.Yard, na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-Receiving.Yard) %>%
  dplyr::distinct()

passing.attempts.v.ReYd <- ggplot(data = passing.data) +
  geom_point(aes(x = Passing.Att, y = Tot.Receiving.Yard, color = Team)) +
  facet_wrap(~season) 
  # theme(legend.position="none")

print(passing.attempts.v.ReYd)
