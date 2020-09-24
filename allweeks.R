
library(rvest)
library(stringr)
library(tidyverse)
library(reshape)

#Replace this line with your directory
topdir <-"C:/Users/adamk/OneDrive/Documents/BTL_scrape"

setwd(topdir)

#Replace this line with your API key
mykey <- "RGAPI-b4f9f6fb-dad6-4ad9-820c-0862c30741b2"

source("btl_functions.R")

#week 1
w1g1 <- game_data(blueTeam="XO", redTeam="THD", gameID="3486391449" , key=mykey, week=1)
w1g2 <- game_data(blueTeam="TSC", redTeam="MBB", gameID="3487328064" , key=mykey, week=1)
w1g3 <- game_data(blueTeam="GB", redTeam="PIO", gameID="3487513311" , key=mykey, week=1)
w1g4 <- game_data(blueTeam="AR", redTeam="SB", gameID="3487479029" , key=mykey, week=1)
w1g5 <- game_data(blueTeam="DD", redTeam="NBS", gameID="3484006702" , key=mykey, week=1)

export_data(w1g1)
export_data(w1g2)
export_data(w1g3)
export_data(w1g4)
export_data(w1g5)



#week 2
w2g1 <- game_data(blueTeam="THD", redTeam="GB", gameID="3491728976" , key=mykey, week=2)
w2g2 <- game_data(blueTeam="MBB", redTeam="XO", gameID="3495980046" , key=mykey, week=2)
w2g3 <- game_data(blueTeam="NBS", redTeam="AR", gameID="3496154532" , key=mykey, week=2)
w2g4 <- game_data(blueTeam="SB", redTeam="DD", gameID="3493889233" , key=mykey, week=2)
w2g5 <- game_data(blueTeam="PIO", redTeam="TSC", gameID="3491559080" , key=mykey, week=2)

export_data(w2g1)
export_data(w2g2)
export_data(w2g3)
export_data(w2g4)
export_data(w2g5)



#week 3
w3g1 <- game_data(blueTeam="GB", redTeam="DD", gameID="3505576524" , key=mykey, week=3)
w3g2 <- game_data(blueTeam="PIO", redTeam="AR", gameID="3505541463" , key=mykey, week=3)
w3g3 <- game_data(blueTeam="SB", redTeam="XO", gameID="3505561715" , key=mykey, week=3)
w3g4 <- game_data(blueTeam="TSC", redTeam="NBS", gameID="3508461584" , key=mykey, week=3)
w3g5 <- game_data(blueTeam="MBB", redTeam="THD", gameID="3502640996" , key=mykey, week=3)

export_data(w3g1)
export_data(w3g2)
export_data(w3g3)
export_data(w3g4)
export_data(w3g5)



#week 4
w4g1 <- game_data(blueTeam="NBS", redTeam="GB", gameID="3510578884" , key=mykey, week=4)
w4g2 <- game_data(blueTeam="THD", redTeam="SB", gameID="3513513983" , key=mykey, week=4)
w4g3 <- game_data(blueTeam="AR", redTeam="TSC", gameID="3513608858" , key=mykey, week=4)
w4g4 <- game_data(blueTeam="DD", redTeam="MBB", gameID="3515803644" , key=mykey, week=4)
w4g5 <- game_data(blueTeam="XO", redTeam="PIO", gameID="3510932723" , key=mykey, week=4)

export_data(w4g1)
export_data(w4g2)
export_data(w4g3)
export_data(w4g4)
export_data(w4g5)



#week 5
w5g1 <- game_data(blueTeam="TSC", redTeam="GB", gameID="3520495036" , key=mykey, week=5)
w5g2 <- game_data(blueTeam="SB", redTeam="MBB", gameID="3523895559" , key=mykey, week=5)
w5g3 <- game_data(blueTeam="XO", redTeam="AR", gameID="3524034660" , key=mykey, week=5)
w5g4 <- game_data(blueTeam="THD", redTeam="NBS", gameID="3520520336" , key=mykey, week=5)
w5g5 <- game_data(blueTeam="PIO", redTeam="DD", gameID="3522423550" , key=mykey, week=5)

export_data(w5g1)
export_data(w5g2)
export_data(w5g3)
export_data(w5g4)
export_data(w5g5)



#week 6
w6g1 <- game_data(blueTeam="NBS", redTeam="SB", gameID="3531609707" , key=mykey, week=6)
w6g2 <- game_data(blueTeam="GB", redTeam="XO", gameID="3533719758" , key=mykey, week=6)
w6g3 <- game_data(blueTeam="MBB", redTeam="PIO", gameID="3531194506" , key=mykey, week=6)
w6g4 <- game_data(blueTeam="DD", redTeam="TSC", gameID="3531344626" , key=mykey, week=6)
w6g5 <- game_data(blueTeam="AR", redTeam="THD", gameID="3533719758" , key=mykey, week=6)

export_data(w6g1)
export_data(w6g2)
export_data(w6g3)
export_data(w6g4)
export_data(w6g5)



#week 7
w7g1 <- game_data(blueTeam="XO", redTeam="DD", gameID="3538531567" , key=mykey, week=7)
w7g2 <- game_data(blueTeam="MBB", redTeam="NBS", gameID="3541017471" , key=mykey, week=7)
w7g3 <- game_data(blueTeam="AR", redTeam="GB", gameID="3540218653" , key=mykey, week=7)
w7g4 <- game_data(blueTeam="SB", redTeam="PIO", gameID="3540424088" , key=mykey, week=7)
w7g5 <- game_data(blueTeam="THD", redTeam="TSC", gameID="3538178396" , key=mykey, week=7)

export_data(w7g1)
export_data(w7g2)
export_data(w7g3)
export_data(w7g4)
export_data(w7g5)



#week 8
w8g1 <- game_data(blueTeam="NBS", redTeam="XO", gameID="3549084430" , key=mykey, week=8)
w8g2 <- game_data(blueTeam="GB", redTeam="MBB", gameID="3552008295" , key=mykey, week=8)
w8g3 <- game_data(blueTeam="TSC", redTeam="SB", gameID="3551215465" , key=mykey, week=8)
w8g4 <- game_data(blueTeam="DD", redTeam="AR", gameID="3551345717" , key=mykey, week=8)
w8g5 <- game_data(blueTeam="PIO", redTeam="THD", gameID="3551061616" , key=mykey, week=8)

export_data(w8g1)
export_data(w8g2)
export_data(w8g3)
export_data(w8g4)
export_data(w8g5)



#week 9
w9g1 <- game_data(blueTeam="PIO", redTeam="NBS", gameID="3569429790" , key=mykey, week=9)
w9g2 <- game_data(blueTeam="TSC", redTeam="XO", gameID="3556136905" , key=mykey, week=9)
w9g3 <- game_data(blueTeam="MBB", redTeam="AR", gameID="3559638574" , key=mykey, week=9)
w9g4 <- game_data(blueTeam="THD", redTeam="DD", gameID="3561633822" , key=mykey, week=9)
w9g5 <- game_data(blueTeam="SB", redTeam="GB", gameID="3559558686" , key=mykey, week=9)

export_data(w9g1)
export_data(w9g2)
export_data(w9g3)
export_data(w9g4)
export_data(w9g5)


