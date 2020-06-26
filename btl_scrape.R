#btl_scrape.R
#Version 1.0
#6/25/2020
#Adam Karabatakis


#Program for scraping BTL games to get aggregated data
#
#Step 1: write function that adds a new game to a dataset of all games, "games"
#Step 2: Create champion/player level dataset of all new games, "new_games"
#Step 3: Add new games data to all games data, "all_games"
#Step 4: From "all_games" create "Champion_data", "Player_data", "Team_data"
#will want an "update_all" command as well that does Steps 2 - 4
#So workflow will be: (1) add_data(team1=, team2=, matchID=, playerID=, playoff=F, week=); (2) update_all()


#match history is https://matchhistory.na.leagueoflegends.com/en/#match-details/NA1/[matchID]/[playerID]?tab=overview

###############
# Defining add_data()
###############

#team1 must be a string of a team's 3 letter abbreviation
#team2 must be a string of the other team's 3 letter abbreviation
#matchID must be an integer, the match ID of the most recent btl game
#playerID must be the ID of ONE OF THE PLAYERS IN THE GAME (CAN BE ANY) <- would like to change this but don't know how yet
#playoff is automatically false, will update this program if we decide we want to keep regular season data separate
#week must be an integer, the week of the game (1 through 10)

add_data <- function(team1, team2, matchID, playerID, playoff=F, week) {
  
  match_source1 <- "https://matchhistory.na.leagueoflegends.com/en/#match-details/NA1/"
  match_source2 <- toString(matchID)
  match_source3 <- toString(playerID)
  
  match_source <- paste(match_source1, match_source2, match_source3, sep = "")
  
  
  
  
  
  
  
}

#list of current champions

champlist <- html_text(read_html("http://ddragon.leagueoflegends.com/cdn/10.13.1/data/en_US/champion.json"))

num_champs <- str_count(champlist, pattern= 'id\\":')

champ_names <- vector(mode="character", length=num_champs)
champ_ids <- vector(mode="character", length=num_champs)

current <- champlist
for (x in 1:num_champs) {
  startname <- str_locate(current, 'id\\":\\"')[2]
  endname <- str_locate(current, '\\",\\"key')[1]
  
  startid <- str_locate(current, 'key\\":\\"')[2]
  endid <- str_locate(current, '\\",\\"name')[1]
  
  champ_names[x] <- substr(current, startname+1, endname-1)
  champ_ids[x] <- substr(current, startid+1, endid-1)
  
  
  current <- substr(current, endid + 20, nchar(current))
}
  
#some manual changes to names
for (x in 1:num_champs) {
  champ_names[x] <- gsub("([a-z])([A-Z])", "\\1 \\2", champ_names[x])
}

replace(champ_names, champ_names == "Monkey King", "Wukong")

champ_info <- data.frame(  champ_names, champ_ids)  

names(champ_info) <- c("champ_names", "champ_ids")










test <- read_html("https://na1.api.riotgames.com/lol/match/v4/matches/3470119125?api_key=RGAPI-ee2c2921-8ad4-490a-88da-05c1fd102847")
current <- html_text(test)


#getting each players data separated

rawdata <- vector(mode="character", length=10)

for (x in 1:10) {
  phrase1 <- '\\{\\"participantId\\":'
  phrase2 <- toString(x)
  phrase <- paste(phrase1, phrase2, sep="")
  
  start <- str_locate(current, phrase)
  end <- str_locate(current, "\\}\\}")
  
  rawdata[x] <- substr(current, start, end)
  
  current <- substr(current, end+2, nchar(current))

}




#which team is each player on

teams <- vector(mode="character", length=10)

for (x in 1:10) {
  start <- str_locate(rawdata[x], "teamId")[2]
  
  teamId <- substr(rawdata[x], start+3,start+5)
  if (teamId == "100") {
    teams[x] <- "Blue"
  }
  else {
    teams[x] <- "Red"
  }
}



#did the player win

win <- vector(mode="character", length=10)

for (x in 1:10) {
  start <- str_locate(rawdata[x], "win")[2]
  
  winner <- substr(rawdata[x], start+3, start+3)
  if (winner == "t") {
    win[x] <- 1
  }
  else {
    win[x] <- 0
  }
}



#which champion did the player play

champ_id <- vector(mode="character", length=10)


for (x in 1:10) {
  
  start <- str_locate(rawdata[x], "championId")[2]
  end <- str_locate(rawdata[x], "spell1Id")[1]
  
  champ_id[x] <- substr(rawdata[x], start+3, end-3)

  
}
  
champs_in_game <- data.frame(  champ_id, c(1:10))
names(champs_in_game) <- c("champ_ids", "order")

champs_in_game <- merge(x=champs_in_game, y=champ_info, by="champ_ids", all.x=TRUE)
champs_in_game <- champs_in_game[order(champs_in_game$order),]

champ_named <- champs_in_game[,3]  



#number of kills
kills <- vector(mode="character", length=10)


for (x in 1:10) {
  
  start <- str_locate(rawdata[x], "kills")[2]
  end <- str_locate(rawdata[x], "deaths")[1]
  
  kills[x] <- substr(rawdata[x], start+3, end-3)
  
  
}


#number of deaths

deaths <- vector(mode="character", length=10)


for (x in 1:10) {
  
  start <- str_locate(rawdata[x], "deaths")[2]
  end <- str_locate(rawdata[x], "assists")[1]
  
  deaths[x] <- substr(rawdata[x], start+3, end-3)
  
  
}

#number of assists


assists <- vector(mode="character", length=10)


for (x in 1:10) {
  
  start <- str_locate(rawdata[x], "assists")[2]
  end <- str_locate(rawdata[x], "largestKillingSpree")[1]
  
  assists[x] <- substr(rawdata[x], start+3, end-3)
  
  
}




#total damage done
damage <- vector(mode="character", length=10)


for (x in 1:10) {
  
  start <- str_locate(rawdata[x], "totalDamageDealtToChampions")[2]
  end <- str_locate(rawdata[x], "magicDamageDealtToChampions")[1]
  
  damage[x] <- substr(rawdata[x], start+3, end-3)
  
  
}

empty <- vector(mode="character", length=10)


out_game_data <- data.frame(empty, empty, champ_named, teams, win, kills, deaths, assists, damage)
names(out_game_data) <- c("Player", "Team", "Champion", "Side", "Win", "Kills", "Deaths", "Assists", "Damage")



