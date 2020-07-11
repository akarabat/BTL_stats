
library(rvest)
library(stringr)

topdir <-"C:/Users/adamk/OneDrive/Documents/BTL_scrape"

setwd(topdir)

mykey <- "RGAPI-5063558e-ab2e-437d-82b9-6df8e2f9d8eb"


source("btl_functions.R")


a <- game_data(blueTeam="AAA", redTeam="BBB", gameID="3470119125" , key=mykey, week=1)
b <- game_data(blueTeam="AAA", redTeam="BBB", gameID="3486391449" , key=mykey, week=1)
c <- game_data(blueTeam="AAA", redTeam="BBB", gameID="3487388654" , key=mykey, week=2)

a
b
c

export_data(a)
export_data(b)
export_data(c)

d <- combine_data()