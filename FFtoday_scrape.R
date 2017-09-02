library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)

# Load rvest for scraping
library(rvest)

# Game Weeks
game.weeks <- c(seq(1, 17), seq(21,24))
names(game.weeks) <- c(seq(1, 17), "wild.card", "divisional", "conference", "super.bowl")

# Seasons
seasons <- seq(2000, 2016)

# Positions
positions <- c(seq(10,40, by = 10), 80, 99) 
names(positions) <- c('QB', 'RB', 'WR', 'TE', 'K', 'DEF')

player.stats <- data.frame()

for(season in seasons) {
  for(week in game.weeks) {
    for(position in positions) {
      print(paste("Starting position ", position, "for week", week, "in season", season))
      fftoday.page <- read_html(sprintf("http://www.fftoday.com/stats/playerstats.php?Season=%s&GameWeek=%s&PosID=%s&LeagueID=26955", 
                                        season, week, position))
      table <- fftoday.page %>%
        html_nodes("table") %>%
        .[[11]] %>%
        html_table()
      
      # Concat the first and second rows to make the column headings
      colnames(table) <- sapply(seq(1:ncol(table)), function(x) { paste(table[1,x], table[2,x], sep = ".") } )
      
      # Remove the first and second rows
      table <- table[-(1:2),]
      
      # Remove any leading periods from the column names
      colnames(table) <- sub("^[.]", "", colnames(table))
      
      # Set the first column title
      colnames(table)[1] <- "First.Name"
      
      # Account for instances where the table is blank (early seasons, playoff stats missing)
      if(nrow(table) == 0) {
        break()
      }
      
      # Split into first and last names, account for defensive players = teams
      
      if(names(positions)[which(positions == position)] == "DEF") {
        table$Team <- sapply(table[,1], function(x) {gsub("^\\d+[.]\\s+", "", x)})
        table <- table[,-1]
      } else {
        table$Last.Name <- sapply(table$First.Name, function(x) {strsplit(x, split = "[ ]")[[1]][3]})
        table$First.Name <- sapply(table$First.Name, function(x) {strsplit(x, split = "[ ]")[[1]][2]})
      }
        
      # Add the position, season, and week
      table$position <- names(positions)[which(positions == position)]
      table$season <- season
      table$week <- names(game.weeks)[which(game.weeks == week)]
      
      player.stats <- rbind.fill(player.stats, table)
      # browser()
    }
  }
}

# Read in the saved data
# player.stats <- read.csv("/home/nadirsidi/Documents/R/FantasyFootball/Data/FFtoday_raw_data.csv",
#                          stringsAsFactors = F)

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

# Combine the defensive stats to match the column names of the offensive players
player.stats.formatted <- player.stats.formatted %>%
  dplyr::mutate(Rushing.Yard = ifelse(is.na(RuYd), Rushing.Yard, RuYd)) %>%
  dplyr::mutate(Passing.Yard = ifelse(is.na(PaYd), Passing.Yard, PaYd)) %>%
  dplyr::select(-c(RuYd, PaYd))

# Save the data (including RDS to avoid re-formatting columns)
write.csv(player.stats.formatted, file = "~/Documents/R/FantasyFootball/Data/FFtoday_formatted.csv",
          row.names = F)
saveRDS(player.stats.formatted, file = "~/Documents/R/FantasyFootball/Data/FFtoday_formatted.RDS")
