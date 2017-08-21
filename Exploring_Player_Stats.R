library(dplyr)
library(ggplot2)

# Format the columns
player.stats.formatted <- player.stats %>%
  dplyr::mutate(FGPct = as.numeric(gsub("[%]", "", `FG%`))) %>%
  dplyr::select(-G, -`FG%`, -`Fantasy.FPts/G`) %>%
  dplyr::rename(PaYd = `PaYd/G`,
                RuYd = `RuYd/G`) %>%
  dplyr::mutate(FGPct = FGPct/100)

# Rearrange the columns
player.stats.formatted <- player.stats.formatted[c("First.Name", "Last.Name","position", "Team", "week", 
                                                   colnames(player.stats.formatted)[-c(1,2,12,13,15)])]

# Format the columns as numeric
cols.num <- c(6:32)
player.stats.formatted[cols.num] <- sapply(player.stats.formatted[cols.num],as.numeric)

# Look at all RB Rushing Yards by season --

RB.stats <- player.stats.formatted %>%
  dplyr::filter(position == "RB") %>%
  dplyr::group_by(season, Last.Name, First.Name) %>%
  dplyr::summarise(season.RuYd = sum(Rushing.Yard, na.rm = T)) %>%
  dplyr::ungroup()

# Boxplots of rushing yards by season
plot.season.RB.RuYd <- ggplot(data = RB.stats) +
  geom_boxplot(aes(season,season.RuYd, group = season))

print(plot.season.RB.RuYd)

# Distribution of rushing yards by season for 2016
RB.stats.2016 <- RB.stats %>%
  dplyr::filter(season == 2016)

plot.2016.RB.RuYd <- ggplot(data = RB.stats.2016) +
  geom_histogram(aes(x = season.RuYd), binwidth = 50)

print(plot.2016.RB.RuYd)
