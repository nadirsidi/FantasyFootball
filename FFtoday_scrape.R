# Load rvest for scraping
library(rvest)

# Set the url for the page
# TODO(nadir.sidi): Loop through the seasons, weeks, and positions to create a master dataset
fftoday.qb <- read_html("http://www.fftoday.com/stats/playerstats.php?Season=2016&GameWeek=1&PosID=10&LeagueID=26955")

# Pull out all html table tags, then reference the desired table (index 11)
table <- fftoday.qb %>%
  html_nodes("table") %>%
  .[[11]] %>%
  html_table()

# TODO(nadir.sidi): Format the table, add columns for season, week and position
