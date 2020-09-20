
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
  
  champ_names <- replace(champ_names, champ_names == "Monkey King", "Wukong")
  
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

game_data <- function(blueTeam, redTeam, key, gameID, playoff=F, playoff_game=NA, week) {
  
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
  
  #bans (team 1)
  
  start <- str_locate(current,'\\"bans\\":')[2]
  end <- str_locate( substr(current, start, str_length(current)), '\\}\\]\\},')[2]
  team1bans <- substr(current, start, start+end)
  start_hold <- start
  end_hold <- end
  bans <- rep("", 10)
  
  for (x in 1:5) {
    start <- str_locate(team1bans, 'championId')[2]
    end <- str_locate(team1bans, 'pickTurn')[1]
    bans[x] <- substr(team1bans, start+3, end-3)
    team1bans <- substr(team1bans, end + 5, str_length(team1bans))
  }
    
  
  current_sub <- substr(current, start_hold+end_hold, str_length(current))
  start <- str_locate(current_sub,'\\"bans\\":')[2]
  end <- str_locate( current_sub, '\\}\\]\\}\\],')[2]
  team2bans <- substr(current_sub, start, start+end)
  
  for (x in 6:10) {
    start <- str_locate(team2bans, 'championId')[2]
    end <- str_locate(team2bans, 'pickTurn')[1]
    bans[x] <- substr(team2bans, start+3, end-3)
    team2bans <- substr(team2bans, end + 5, str_length(team2bans))
  }
  
  bans_in_game <- data.frame(  bans, c(1:10))
  names(bans_in_game) <- c("champ_ids", "order")
  all_champs <- champ_data()
  bans_in_game <- merge(x=bans_in_game, y=all_champs, by="champ_ids", all.x=TRUE)
  bans_in_game <- bans_in_game[order(bans_in_game$order),]
  
  bans_named <- bans_in_game[,3]  
  
  

  
  
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
  
  firstblood <- vector(mode="character", length=10)
  
  for (x in 1:10) {
    
    start <- str_locate(rawdata[x], "firstBloodKill")[2]
    end <- str_locate(rawdata[x], "firstBloodAssist")[1]
    
    firstblood[x] <- substr(rawdata[x], start+3, end-3)
    
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
  
  #Vision Score
  vision <- vector(mode="character", length=10)
  
  
  for (x in 1:10) {
    
    start <- str_locate(rawdata[x], "visionScore")[2]
    end <- str_locate(rawdata[x], "timeCCingOthers")[1]
    
    vision[x] <- substr(rawdata[x], start+3, end-3)
    
    
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
  playoff_gamenum <- rep(playoff_game, 10)
  
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
  present_vec <- rep(1,10)
  
  for (x in 1:10) {
    eval(parse( text = paste("ban", x, "_vec <- rep(bans_named[", x, "], 10)", sep="")))
  }
  
  
  
  empty <- vector(mode="character", length=10)
  
  
  out_game_data <- data.frame(empty, empty, team_id, game_playoffs, playoff_gamenum, game_week, champ_named, teams, win, 
                              kills, deaths, assists, firstblood, damage, gold, vision, cs, 
                              date_vec, month_vec, day_vec, year_vec, timer_vec, minutes_vec, seconds_vec, present_vec,
                              ban1_vec, ban2_vec, ban3_vec, ban4_vec, ban5_vec, ban6_vec, ban7_vec, ban8_vec, ban9_vec, ban10_vec
                              )
  names(out_game_data) <- c("Player", "Position", "Team", "Playoffs", "PlayoffGameNumber", "Week", "Champion", "Side", "Win", 
                            "Kills", "Deaths", "Assists", "FirstBlood", "Damage", "Gold", "Vision", "CS",
                            "GameDate", "GameMonth", "GameDay", "GameYear", "gameTime", "Minutes", "Seconds", "Present", 
                            "Ban1", "Ban2", "Ban3", "Ban4", "Ban5", "Ban6", "Ban7", "Ban8", "Ban9", "Ban10"
                            )
  
  return(out_game_data)
}


##champion_data()
champion_data <- function( dataset) {
  #All champs
  champnames <- champ_data()
  colnames(champnames) <- c("Champion", "champ_ids")
  
  #Champs played
  z <- dataset %>% group_by(Champion) %>% summarise(totpick = sum(Present), totwin = sum(Win), avgdam = mean(Damage))
  z <- as.data.frame(z)
  z$totloss <- z$totpick - z$totwin
  z$winpct <- z$totwin / z$totpick
  
  #Champs banned
  baninfo <- dataset[seq(10,nrow(dataset), by=10), 26:35]
  baninfo <- cbind(rep(1, nrow(baninfo)), baninfo)
  names(baninfo)[1] <- "melt"
  baninfo <- melt(baninfo, id="melt")
  baninfo <- baninfo[-c(1,2)]
  baninfo$banned <- rep(1, nrow(baninfo))
  bans_ <- baninfo %>% group_by(value) %>% summarise(totban = sum(banned))
  bans <- as.data.frame(bans_)
  colnames(bans) <- c("Champion", "totban")
  
  outframe <- merge(champnames, z, all.x=TRUE, by="Champion")
  outframe <- merge(outframe, bans, all.x=TRUE, by="Champion")
  
  outframe$totpick[is.na(outframe$totpick)] <- 0
  outframe$totban[is.na(outframe$totban)] <- 0
  
  outframe$totpres <- outframe$totban + outframe$totpick
  numgames = sum(outframe$totpres,na.rm=TRUE) / 20
  outframe$fracpres <- (outframe$totpres / numgames)
  
  setwd(topdir)
  
  folder <- "AggData"
  dir.create(file.path(getwd(), folder), showWarnings = FALSE)
  
  setwd(folder)
  
  write.csv(outframe, "ChampionData.csv", row.names = FALSE)
  
  setwd(topdir)
  
  return(outframe)
  
}
  


##player_data()
player_data <- function( dataset) {
  #All players
  dataset$minsec = dataset$Minutes + dataset$Seconds / 60
  dataset$FirstBloodBool = dataset$FirstBlood == "true"
  x <- dataset %>% group_by(Player) %>% summarise(killtot = sum(Kills), deathtot = sum(Deaths), assisttot =sum(Assists), FBtot = sum(FirstBloodBool), CStot = sum(CS), dmgtot = sum(Damage), 
                                                  vistot = sum(Vision), gamestot = sum(Present), goldtot = sum(Gold), mintot = sum(minsec))
  x <- as.data.frame(x)
  
  x$cspermin <- x$CStot / x$mintot
  x$dmgpermin <- x$dmgtot / x$mintot
  x$goldpermin <- x$goldtot / x$mintot
  x$avggametime <- x$mintot / x$gamestot
  x$avgwardscore <- x$vistot / x$gamestot
  x$kda <- (x$killtot + x$assisttot) / x$deathtot
  
  setwd(topdir)
  
  folder <- "AggData"
  dir.create(file.path(getwd(), folder), showWarnings = FALSE)
  
  setwd(folder)
  
  write.csv(x, "PlayerData.csv", row.names = FALSE)
  
  setwd(topdir)
  
  return(x)
}




##team_data()






#export_data
#export game data file

export_data <- function( dataset ) {
  setwd(topdir)
  
  if (dataset$Playoffs[1] == F) {
  
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
  else {
    name1 <- dataset$Team[1]
    name2 <- dataset$Team[6]
    name3 <- dataset$GameMonth[1]
    name4 <- dataset$GameDay[1]
    name5 <- paste0( "game", dataset$PlayoffGameNumber[1])
    name6 <- ".csv"
    name <- paste( paste(name1, name2, name3, name4, name5, sep="_"), name6, sep = "")
    
    week_out <- dataset$Week[1]
    folder <- paste( "Week", week_out, sep="")
    dir.create(file.path(getwd(), folder), showWarnings = FALSE)
    
    setwd(folder)
    write.csv(dataset, name, row.names = FALSE)
    
    setwd(topdir)
  }
    
}


combine_data <- function() {
  setwd(topdir)
  
  alldata <- data.frame( )

  weeks <- paste("Week", seq(1,100), sep="")
  maxwk <- 1
  
  for (x in 1:100) {
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
  
  champion_data(alldata)
  player_data(alldata)
  #team_data(alldata)
    
  return(alldata)  
  
}

  