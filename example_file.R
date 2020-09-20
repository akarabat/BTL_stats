#Example of how working file could look
#value of having all the previous calls is that if we update any of the programs we can just run everything
#and have up to date values for all games


library(rvest)
library(stringr)
library(tidyverse)
library(reshape)

#Replace this line with your directory
topdir <-"C:/Users/adamk/OneDrive/Documents/BTL_scrape"

setwd(topdir)

#Replace this line with your API key
mykey <- "RGAPI-2c72589e-f35f-4334-a64b-5a4e7c6f3627"

source("btl_functions.R")


#This is how a season with 4 teams might start
#We can comment out the lines that are for already run games by adding # at the start of the lines
#Then those lines won't run
#But we still have them if we want to change anything later

#Week 1
#w1g1 <- game_data(blueTeam="AAA", redTeam="BBB", gameID="3470119125" , key=mykey, week=1)
#w1g2 <- game_data(blueTeam="CCC", redTeam="DDD", gameID="3486391449" , key=mykey, week=1)

#export_data(w1g1)
#export_data(w1g2)


#Week 2
w2g1 <- game_data(blueTeam="AAA", redTeam="CCC", gameID="3487388654" , key=mykey, week=2)
w2g2 <- game_data(blueTeam="BBB", redTeam="DDD", gameID="3487388654" , key=mykey, week=2)

export_data(w2g1)
export_data(w2g2)


##### After running export_data() lines, go to files and add player names / positions
##### Then save the sheets with _named before running combine_data()
combine_data()

#the combine data call now creates a folder called AggData if one doesn't already exist
#And adds to it up to date PlayerData and ChampionData sheets

#OTHER NOTES
#For playoff games
  #week is going to just keep incrementing up (10, 11, ...)
  #Add to the game_data call playoff=T and playoff_game = 1 (or 2 or 3 or 4 or 5, depending on which game in the series it is)








