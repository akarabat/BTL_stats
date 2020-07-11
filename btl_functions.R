
library(rvest)
library(stringr)
library(pracma)


###Pull champion data from Riot API
###Source is currently the 10.13 patch champion list

champ_data <- function( source = "http://ddragon.leagueoflegends.com/cdn/10.13.1/data/en_US/champion.json") {
  
  
  champlist <- html_text(read_html(source))
  
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
  
  return(champ_info)
}



###############
# Defining game_data() to get one game's data in nice form
###############

#blueTeam must be a string of the blue side team's 3 letter abbreviation
#redTeam must be a string of the red side team's 3 letter abbreviation
#matchID must be an a link to the Riot API result of the match data
#playoff is automatically false, will update this program if we decide we want to keep regular season data separate
#week must be an integer, the week of the game (1 through 10)

game_data <- function(blueTeam, redTeam, key, gameID, playoff=F, week) {
  
  #Save Game ID in list of Game IDs
  setwd(topdir)
  #Check if already have gameID list file
  if (max( grepl("gameIDs.csv", list.files())) == 1) {
    IDlist <- read.csv("gameIDs.csv")
    
    #check if alreeady have game ID in this list
    IDlist <- IDlist[,1]
    if (max( grepl(gameID, IDlist)) == 0) {
      IDlist <- c(IDlist, gameID)
      write.csv(IDlist, "gameIDs.csv", row.names=F)
    }
  }
  else {
    write.csv(gameID, "gameIDs.csv", row.names=F)
  }
    

  
  
  ##Starting parcing of new game's data
  
  source <- paste("https://na1.api.riotgames.com/lol/match/v4/matches/", gameID, "?api_key=", key, sep="")
  
  
  game_rawdata <- read_html(source)
  current <- html_text(game_rawdata)
  
  
  #Grabbing some player independant data (game date, game time)

  
  start <- str_locate(current, '\\"gameCreation\\":')[2]
  end <- str_locate(current,'\\"gameDuration')[1]
  game_creation <- substr(current, start+1, end-2)
  date <- as.Date(as.POSIXct(as.numeric(game_creation)/1000, origin="1970-01-01"))
  year <- substr(date, 1, 4)
  month <- substr(date, 6,7)
  day <- substr(date,9,10)
  
  
  start <- str_locate(current, '\\"gameDuration\\":')[2]
  end <- str_locate(current,'\\"queueId')[1]
  game_time <- substr(current, start+1, end-2)
  
  minutes <- floor(as.numeric(game_time)/60)
  seconds <- mod(as.numeric(game_time), 60)

  
  
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
  all_champs <- champ_data()
  champs_in_game <- merge(x=champs_in_game, y=all_champs, by="champ_ids", all.x=TRUE)
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
  
  #Gold earned
  gold <- vector(mode="character", length=10)
  
  
  for (x in 1:10) {
    
    start <- str_locate(rawdata[x], "goldEarned")[2]
    end <- str_locate(rawdata[x], "goldSpent")[1]
    
    gold[x] <- substr(rawdata[x], start+3, end-3)
    
    
  }
  
  #Creep Score
  cs1 <- vector(mode="character", length=10)
  cs2 <- vector(mode="character", length=10)
  cs <- vector(mode="character", length=10)
  
  
  for (x in 1:10) {
    
    start <- str_locate(rawdata[x], "totalMinionsKilled")[2]
    end <- str_locate(rawdata[x], "neutralMinionsKilled")[1]
    
    cs1[x] <- substr(rawdata[x], start+3, end-3)
    
    
  }
  for (x in 1:10) {
    
    start <- str_locate(rawdata[x], 'neutralMinionsKilled\\"')[2]
    end <- str_locate(rawdata[x], "neutralMinionsKilledTeam")[1]
    
    cs2[x] <- substr(rawdata[x], start+2, end-3)
    
    
  }
  
  for (x in 1:10) {
    
    cs[x] <- toString( as.numeric(cs1[x]) + as.numeric(cs2[x]))
    
  }
  
  
  
  
  game_week <- rep(week, 10)
  
  game_playoffs <- rep(playoff, 10)
  
  team_id <- teams
  
  team_id <- replace(team_id, team_id == "Blue", blueTeam)
  team_id <- replace(team_id, team_id == "Red", redTeam)
  
  
  date_vec <- rep(date, 10)
  month_vec <- rep(month, 10)
  day_vec <- rep(day, 10)
  year_vec <- rep(year, 10)
  timer_vec <- rep(game_time, 10)
  minutes_vec <- rep(minutes, 10)
  seconds_vec <- rep(seconds,10)
  
  
  
  empty <- vector(mode="character", length=10)
  
  
  out_game_data <- data.frame(empty, empty, team_id, game_playoffs, game_week, champ_named, teams, win, 
                              kills, deaths, assists, damage, gold, cs, 
                              date_vec, month_vec, day_vec, year_vec, timer_vec)
  names(out_game_data) <- c("Player", "Position", "Team", "Playoffs", "Week", "Champion", "Side", "Win", 
                            "Kills", "Deaths", "Assists", "Damage", "Gold", "CS",
                            "GameDate", "GameMonth", "GameDay", "GameYear", "gameTime"
                            )
  
  return(out_game_data)
}


#export_data
#export game data file

export_data <- function( dataset ) {
  setwd(topdir)
  
  name1 <- dataset$Team[1]
  name2 <- dataset$Team[6]
  name3 <- dataset$GameMonth[1]
  name4 <- dataset$GameDay[1]
  name5 <- ".csv"
  name <- paste( paste(name1, name2, name3, name4, sep="_"), name5, sep = "")
  
  week_out <- dataset$Week[1]
  folder <- paste( "Week", week_out, sep="")
  dir.create(file.path(getwd(), folder), showWarnings = FALSE)
  
  setwd(folder)
  write.csv(dataset, name, row.names = FALSE)
  
  setwd(topdir)
}
  

combine_data <- function() {
  setwd(topdir)
  
  alldata <- data.frame( )

  weeks <- paste("Week", seq(1,10), sep="")
  maxwk <- 1
  
  for (x in 1:10) {
    if (file.exists(weeks[x])) {
      maxwk <- as.numeric(x)
    }
  }
  
  for (x in 1:maxwk) {
    setwd(weeks[x])
    games <- list.files()[grepl("_named", list.files())]
    for (y in 1:length(games)) {
      tempdata <- read.csv(games[y])
      alldata <- rbind(alldata, tempdata)
    }
    setwd('..')
  }
    
  return(alldata)  
  
}

  