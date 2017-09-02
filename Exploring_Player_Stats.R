library(dplyr)
library(ggplot2)
library(tidyr)

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

# Look at all RB Rushing Yards by regular season --

RB.stats <- player.stats.formatted %>%
  dplyr::filter(position == "RB") %>%
  dplyr::filter(grepl("\\d+", week)) %>%
  dplyr::mutate(week = as.numeric(week)) %>%
  dplyr::group_by(season, Last.Name, First.Name) %>%
  dplyr::mutate(regular.season.RuYd = sum(Rushing.Yard, na.rm = T)) %>%
  dplyr::ungroup()

# Boxplots of rushing yards by season
plot.season.RB.RuYd <- ggplot(data = RB.stats) +
  geom_boxplot(aes(season,regular.season.RuYd, group = season))
print(plot.season.RB.RuYd)

# Explore the top rushers from 2016
RB.stats.2016 <- RB.stats %>%
  dplyr::filter(season == 2016) %>%
  dplyr::mutate(Full.Name = paste(First.Name, Last.Name))

top.RB.rushers.2016 <- RB.stats.2016 %>%
  dplyr::select(Full.Name, regular.season.RuYd) %>%
  dplyr::distinct() %>%
  dplyr::top_n(25, regular.season.RuYd)

top.RB.rushers.stats.2016 <- RB.stats.2016 %>%
  dplyr::filter(Full.Name %in% top.RB.rushers.2016$Full.Name) %>%
  dplyr::group_by(Full.Name) %>%
  dplyr::mutate(min.RuYd = min(Rushing.Yard),
                max.RuYd = max(Rushing.Yard),
                mean.RuYd = mean(Rushing.Yard),
                median.RuYd = median(Rushing.Yard)) %>%
  tidyr::gather(stat.RuYd, value.RuYd, min.RuYd, max.RuYd, mean.RuYd, median.RuYd) 

plot.2016.RB.RuYd <- ggplot(data = top.RB.rushers.stats.2016) +
  geom_point(aes(x = value.RuYd, y = Full.Name, color = stat.RuYd))
                
print(plot.2016.RB.RuYd)

plot.2016.RB.RuYd.boxplot <- ggplot(data = top.RB.rushers.stats.2016) +
  geom_boxplot(aes(reorder(Full.Name, regular.season.RuYd), Rushing.Yard, group = Full.Name),
               position = "identity")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "#D2D2D2", size = 0.5, linetype = "dotted"))+
  labs(x = "Full Name", y = "Weekly Rushing Yards") +
  ggtitle("Top 25 RB by Rushing Yards", "Regular Season 2016") 
print(plot.2016.RB.RuYd.boxplot)
