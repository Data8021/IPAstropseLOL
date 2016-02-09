## Function to fetch game data from a list of matches

fetchGameHash <- function(gamesDF = leagueGames, environment = .GlobalEnv){
  suppressMessages(suppressWarnings(library(jsonlite)))
  suppressMessages(suppressWarnings(library(dplyr)))
  
  ## Remove games without gameHash
  gamesDF <- filter(gamesDF, gameHash != "NA", blueTeamID !="NA") 
  
  team1 <- vector()
  
  ## Loop through all games
  for (i in 1:nrow(gamesDF)){
    print(i)
    
    ## Fetch game JSON file
    gameDataTemp <- fromJSON(paste0("https://acs.leagueoflegends.com/v1/stats/game/",
                                           gamesDF[i, "gameRealm"], "/", gamesDF[i, "gameCode"], "?gameHash=", 
                                           gamesDF[i, "gameHash"]))
    
  
    
  }
  
  ## Put final DF in specified env
  assign("leagueGames", leagueGames, envir = environment)
  
}




## Load functions

extractPlayerData <- function(num = i, name, firstLink, secondLink, thirdLink = "skip"){
  
  if (thirdLink == "skip") {
    allGameData[num, paste0("t1player1", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[1]]
    allGameData[num, paste0("t1player2", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[2]]
    allGameData[num, paste0("t1player3", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[3]]
    allGameData[num, paste0("t1player4", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[4]]
    allGameData[num, paste0("t1player5", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[5]]
    allGameData[num, paste0("t2player1", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[6]]
    allGameData[num, paste0("t2player2", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[7]]
    allGameData[num, paste0("t2player3", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[8]]
    allGameData[num, paste0("t2player4", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[9]]
    allGameData[num, paste0("t2player5", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[10]]
  } else {
    allGameData[num, paste0("t1player1", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[thirdLink]][[1]]
    allGameData[num, paste0("t1player2", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[thirdLink]][[2]]
    allGameData[num, paste0("t1player3", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[thirdLink]][[3]]
    allGameData[num, paste0("t1player4", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[thirdLink]][[4]]
    allGameData[num, paste0("t1player5", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[thirdLink]][[5]]
    allGameData[num, paste0("t2player1", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[thirdLink]][[6]]
    allGameData[num, paste0("t2player2", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[thirdLink]][[7]]
    allGameData[num, paste0("t2player3", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[thirdLink]][[8]]
    allGameData[num, paste0("t2player4", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[thirdLink]][[9]]
    allGameData[num, paste0("t2player5", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[thirdLink]][[10]]
  }
  
}


extractTimelineData <- function(num = i, name, link){
  
  ## Test to see if variable present
  if (link %in% names(fullGameList[[num]][["participants"]][["timeline"]])){
    
    ## Load 0-10 data
    allGameData[num, paste0("t1player1", name, "0to10")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][1, "0-10"]
    allGameData[num, paste0("t1player2", name, "0to10")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][2, "0-10"]
    allGameData[num, paste0("t1player3", name, "0to10")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][3, "0-10"]
    allGameData[num, paste0("t1player4", name, "0to10")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][4, "0-10"]
    allGameData[num, paste0("t1player5", name, "0to10")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][5, "0-10"]
    allGameData[num, paste0("t2player1", name, "0to10")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][6, "0-10"]
    allGameData[num, paste0("t2player2", name, "0to10")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][7, "0-10"]
    allGameData[num, paste0("t2player3", name, "0to10")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][8, "0-10"]
    allGameData[num, paste0("t2player4", name, "0to10")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][9, "0-10"]
    allGameData[num, paste0("t2player5", name, "0to10")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][10, "0-10"]
    
    ## Load 10-20 data
    allGameData[num, paste0("t1player1", name, "10to20")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][1, "10-20"]
    allGameData[num, paste0("t1player2", name, "10to20")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][2, "10-20"]
    allGameData[num, paste0("t1player3", name, "10to20")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][3, "10-20"]
    allGameData[num, paste0("t1player4", name, "10to20")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][4, "10-20"]
    allGameData[num, paste0("t1player5", name, "10to20")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][5, "10-20"]
    allGameData[num, paste0("t2player1", name, "10to20")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][6, "10-20"]
    allGameData[num, paste0("t2player2", name, "10to20")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][7, "10-20"]
    allGameData[num, paste0("t2player3", name, "10to20")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][8, "10-20"]
    allGameData[num, paste0("t2player4", name, "10to20")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][9, "10-20"]
    allGameData[num, paste0("t2player5", name, "10to20")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][10, "10-20"]
    
    ## Test for 20-30
    if (length(names(fullGameList[[num]][["participants"]][["timeline"]][[link]])) > 2){
      
      ## Load 20-30 data
      allGameData[num, paste0("t1player1", name, "20to30")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][1, "20-30"]
      allGameData[num, paste0("t1player2", name, "20to30")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][2, "20-30"]
      allGameData[num, paste0("t1player3", name, "20to30")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][3, "20-30"]
      allGameData[num, paste0("t1player4", name, "20to30")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][4, "20-30"]
      allGameData[num, paste0("t1player5", name, "20to30")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][5, "20-30"]
      allGameData[num, paste0("t2player1", name, "20to30")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][6, "20-30"]
      allGameData[num, paste0("t2player2", name, "20to30")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][7, "20-30"]
      allGameData[num, paste0("t2player3", name, "20to30")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][8, "20-30"]
      allGameData[num, paste0("t2player4", name, "20to30")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][9, "20-30"]
      allGameData[num, paste0("t2player5", name, "20to30")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][10, "20-30"]
      
    }
    
    ## Test for 30-end
    if (length(names(fullGameList[[num]][["participants"]][["timeline"]][[link]])) > 3){
      
      ## Load 20-30 data
      allGameData[num, paste0("t1player1", name, "30toend")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][1, "30-end"]
      allGameData[num, paste0("t1player2", name, "30toend")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][2, "30-end"]
      allGameData[num, paste0("t1player3", name, "30toend")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][3, "30-end"]
      allGameData[num, paste0("t1player4", name, "30toend")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][4, "30-end"]
      allGameData[num, paste0("t1player5", name, "30toend")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][5, "30-end"]
      allGameData[num, paste0("t2player1", name, "30toend")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][6, "30-end"]
      allGameData[num, paste0("t2player2", name, "30toend")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][7, "30-end"]
      allGameData[num, paste0("t2player3", name, "30toend")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][8, "30-end"]
      allGameData[num, paste0("t2player4", name, "30toend")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][9, "30-end"]
      allGameData[num, paste0("t2player5", name, "30toend")] <<- fullGameList[[num]][["participants"]][["timeline"]][[link]][10, "30-end"]
      
    }
    
    
  }
  
}

## Load raw game details
load("data/fullGameList.Rda")

## Initialize data frame
allGameData <- data.frame(gameHash = as.character(),
                          gameId = as.numeric(),
                          platformId = as.character(),
                          gameCreation = as.numeric(),
                          gameDuration = as.numeric(),
                          queueId = as.numeric(),
                          mapId = as.numeric(),
                          seasonId = as.numeric(),
                          gameVersion = as.numeric(),
                          gameMode = as.character(),
                          gameType = as.character(),
                          t1Win = as.character(),
                          t1FirstBlood = as.logical(),
                          t1FirstTower = as.logical(), 
                          t1FirstInhibitor = as.logical(),
                          t1FirstBaron = as.logical(),
                          t1FirstDragon = as.logical(),
                          t1FirstRiftHerald = as.logical(),
                          t1TowerKills = as.numeric(),
                          t1InhibitorKills = as.numeric(),
                          t1BaronKills = as.numeric(),
                          t1DragonKills = as.numeric(),
                          t1VilemawKills = as.numeric(),
                          t1RiftHeraldKills = as.numeric(),
                          t1DominionVictoryScore = as.numeric(),
                          t2Win = as.character(),
                          t2FirstBlood = as.logical(),
                          t2FirstTower = as.logical(), 
                          t2FirstInhibitor = as.logical(),
                          t2FirstBaron = as.logical(),
                          t2FirstDragon = as.logical(),
                          t2FirstRiftHerald = as.logical(),
                          t2TowerKills = as.numeric(),
                          t2InhibitorKills = as.numeric(),
                          t2BaronKills = as.numeric(),
                          t2DragonKills = as.numeric(),
                          t2VilemawKills = as.numeric(),
                          t2RiftHeraldKills = as.numeric(),
                          t2DominionVictoryScore = as.numeric(),
                          t1player1Name = as.character(),
                          t1player2Name = as.character(),
                          t1player3Name = as.character(),
                          t1player4Name = as.character(),
                          t1player5Name = as.character(),
                          t2player1Name = as.character(),
                          t2player2Name = as.character(),
                          t2player3Name = as.character(),
                          t2player4Name = as.character(),
                          t2player5Name = as.character(),
                          t1player1Champion = as.numeric(),
                          t1player2Champion = as.numeric(),
                          t1player3Champion = as.numeric(),
                          t1player4Champion = as.numeric(),
                          t1player5Champion = as.numeric(),
                          t2player1Champion = as.numeric(),
                          t2player2Champion = as.numeric(),
                          t2player3Champion = as.numeric(),
                          t2player4Champion = as.numeric(),
                          t2player5Champion = as.numeric(),
                          t1player1Role = as.character(),
                          t1player2Role = as.character(),
                          t1player3Role = as.character(),
                          t1player4Role = as.character(),
                          t1player5Role = as.character(),
                          t2player1Role = as.character(),
                          t2player2Role = as.character(),
                          t2player3Role = as.character(),
                          t2player4Role = as.character(),
                          t2player5Role = as.character(),
                          t1player1Lane = as.character(),
                          t1player2Lane = as.character(),
                          t1player3Lane = as.character(),
                          t1player4Lane = as.character(),
                          t1player5Lane = as.character(),
                          t2player1Lane = as.character(),
                          t2player2Lane = as.character(),
                          t2player3Lane = as.character(),
                          t2player4Lane = as.character(),
                          t2player5Lane = as.character(),
                          t1player1Spell1 = as.numeric(),
                          t1player2Spell1 = as.numeric(),
                          t1player3Spell1 = as.numeric(),
                          t1player4Spell1 = as.numeric(),
                          t1player5Spell1 = as.numeric(),
                          t2player1Spell1 = as.numeric(),
                          t2player2Spell1 = as.numeric(),
                          t2player3Spell1 = as.numeric(),
                          t2player4Spell1 = as.numeric(),
                          t2player5Spell1 = as.numeric(),
                          t1player1Spell2 = as.numeric(),
                          t1player2Spell2 = as.numeric(),
                          t1player3Spell2 = as.numeric(),
                          t1player4Spell2 = as.numeric(),
                          t1player5Spell2 = as.numeric(),
                          t2player1Spell2 = as.numeric(),
                          t2player2Spell2 = as.numeric(),
                          t2player3Spell2 = as.numeric(),
                          t2player4Spell2 = as.numeric(),
                          t2player5Spell2 = as.numeric(),
                          t1player1Masteries = I(list()),
                          t1player2Masteries = I(list()),
                          t1player3Masteries = I(list()),
                          t1player4Masteries = I(list()),
                          t1player5Masteries = I(list()),
                          t2player1Masteries = I(list()),
                          t2player2Masteries = I(list()),
                          t2player3Masteries = I(list()),
                          t2player4Masteries = I(list()),
                          t2player5Masteries = I(list()),
                          t1player1Runes = I(list()),
                          t1player2Runes = I(list()),
                          t1player3Runes = I(list()),
                          t1player4Runes = I(list()),
                          t1player5Runes = I(list()),
                          t2player1Runes = I(list()),
                          t2player2Runes = I(list()),
                          t2player3Runes = I(list()),
                          t2player4Runes = I(list()),
                          t2player5Runes = I(list()),
                          t1player1Item0 = as.numeric(),
                          t1player2Item0 = as.numeric(),
                          t1player3Item0 = as.numeric(),
                          t1player4Item0 = as.numeric(),
                          t1player5Item0 = as.numeric(),
                          t2player1Item0 = as.numeric(),
                          t2player2Item0 = as.numeric(),
                          t2player3Item0 = as.numeric(),
                          t2player4Item0 = as.numeric(),
                          t2player5Item0 = as.numeric(),
                          t1player1Item1 = as.numeric(),
                          t1player2Item1 = as.numeric(),
                          t1player3Item1 = as.numeric(),
                          t1player4Item1 = as.numeric(),
                          t1player5Item1 = as.numeric(),
                          t2player1Item1 = as.numeric(),
                          t2player2Item1 = as.numeric(),
                          t2player3Item1 = as.numeric(),
                          t2player4Item1 = as.numeric(),
                          t2player5Item1 = as.numeric(),
                          t1player1Item2 = as.numeric(),
                          t1player2Item2 = as.numeric(),
                          t1player3Item2 = as.numeric(),
                          t1player4Item2 = as.numeric(),
                          t1player5Item2 = as.numeric(),
                          t2player1Item2 = as.numeric(),
                          t2player2Item2 = as.numeric(),
                          t2player3Item2 = as.numeric(),
                          t2player4Item2 = as.numeric(),
                          t2player5Item2 = as.numeric(),
                          t1player1Item3 = as.numeric(),
                          t1player2Item3 = as.numeric(),
                          t1player3Item3 = as.numeric(),
                          t1player4Item3 = as.numeric(),
                          t1player5Item3 = as.numeric(),
                          t2player1Item3 = as.numeric(),
                          t2player2Item3 = as.numeric(),
                          t2player3Item3 = as.numeric(),
                          t2player4Item3 = as.numeric(),
                          t2player5Item3 = as.numeric(),
                          t1player1Item4 = as.numeric(),
                          t1player2Item4 = as.numeric(),
                          t1player3Item4 = as.numeric(),
                          t1player4Item4 = as.numeric(),
                          t1player5Item4 = as.numeric(),
                          t2player1Item4 = as.numeric(),
                          t2player2Item4 = as.numeric(),
                          t2player3Item4 = as.numeric(),
                          t2player4Item4 = as.numeric(),
                          t2player5Item4 = as.numeric(),
                          t1player1Item5 = as.numeric(),
                          t1player2Item5 = as.numeric(),
                          t1player3Item5 = as.numeric(),
                          t1player4Item5 = as.numeric(),
                          t1player5Item5 = as.numeric(),
                          t2player1Item5 = as.numeric(),
                          t2player2Item5 = as.numeric(),
                          t2player3Item5 = as.numeric(),
                          t2player4Item5 = as.numeric(),
                          t2player5Item5 = as.numeric(),
                          t1player1Item6 = as.numeric(),
                          t1player2Item6 = as.numeric(),
                          t1player3Item6 = as.numeric(),
                          t1player4Item6 = as.numeric(),
                          t1player5Item6 = as.numeric(),
                          t2player1Item6 = as.numeric(),
                          t2player2Item6 = as.numeric(),
                          t2player3Item6 = as.numeric(),
                          t2player4Item6 = as.numeric(),
                          t2player5Item6 = as.numeric(),
                          t1player1Kills = as.numeric(),
                          t1player2Kills = as.numeric(),
                          t1player3Kills = as.numeric(),
                          t1player4Kills = as.numeric(),
                          t1player5Kills = as.numeric(),
                          t2player1Kills = as.numeric(),
                          t2player2Kills = as.numeric(),
                          t2player3Kills = as.numeric(),
                          t2player4Kills = as.numeric(),
                          t2player5Kills = as.numeric(),
                          t1player1Deaths = as.numeric(),
                          t1player2Deaths = as.numeric(),
                          t1player3Deaths = as.numeric(),
                          t1player4Deaths = as.numeric(),
                          t1player5Deaths = as.numeric(),
                          t2player1Deaths = as.numeric(),
                          t2player2Deaths = as.numeric(),
                          t2player3Deaths = as.numeric(),
                          t2player4Deaths = as.numeric(),
                          t2player5Deaths = as.numeric(),
                          t1player1Assists = as.numeric(),
                          t1player2Assists = as.numeric(),
                          t1player3Assists = as.numeric(),
                          t1player4Assists = as.numeric(),
                          t1player5Assists = as.numeric(),
                          t2player1Assists = as.numeric(),
                          t2player2Assists = as.numeric(),
                          t2player3Assists = as.numeric(),
                          t2player4Assists = as.numeric(),
                          t2player5Assists = as.numeric(),
                          t1player1LargestKillingSpree = as.numeric(),
                          t1player2LargestKillingSpree = as.numeric(),
                          t1player3LargestKillingSpree = as.numeric(),
                          t1player4LargestKillingSpree = as.numeric(),
                          t1player5LargestKillingSpree = as.numeric(),
                          t2player1LargestKillingSpree = as.numeric(),
                          t2player2LargestKillingSpree = as.numeric(),
                          t2player3LargestKillingSpree = as.numeric(),
                          t2player4LargestKillingSpree = as.numeric(),
                          t2player5LargestKillingSpree = as.numeric(),
                          t1player1LargestMultiKill = as.numeric(),
                          t1player2LargestMultiKill = as.numeric(),
                          t1player3LargestMultiKill = as.numeric(),
                          t1player4LargestMultiKill = as.numeric(),
                          t1player5LargestMultiKill = as.numeric(),
                          t2player1LargestMultiKill = as.numeric(),
                          t2player2LargestMultiKill = as.numeric(),
                          t2player3LargestMultiKill = as.numeric(),
                          t2player4LargestMultiKill = as.numeric(),
                          t2player5LargestMultiKill = as.numeric(),
                          t1player1KillingSprees = as.numeric(),
                          t1player2KillingSprees = as.numeric(),
                          t1player3KillingSprees = as.numeric(),
                          t1player4KillingSprees = as.numeric(),
                          t1player5KillingSprees = as.numeric(),
                          t2player1KillingSprees = as.numeric(),
                          t2player2KillingSprees = as.numeric(),
                          t2player3KillingSprees = as.numeric(),
                          t2player4KillingSprees = as.numeric(),
                          t2player5KillingSprees = as.numeric(),
                          t1player1LongestTimeSpentLiving = as.numeric(),
                          t1player2LongestTimeSpentLiving = as.numeric(),
                          t1player3LongestTimeSpentLiving = as.numeric(),
                          t1player4LongestTimeSpentLiving = as.numeric(),
                          t1player5LongestTimeSpentLiving = as.numeric(),
                          t2player1LongestTimeSpentLiving = as.numeric(),
                          t2player2LongestTimeSpentLiving = as.numeric(),
                          t2player3LongestTimeSpentLiving = as.numeric(),
                          t2player4LongestTimeSpentLiving = as.numeric(),
                          t2player5LongestTimeSpentLiving = as.numeric(),
                          t1player1DoubleKills = as.numeric(),
                          t1player2DoubleKills = as.numeric(),
                          t1player3DoubleKills = as.numeric(),
                          t1player4DoubleKills = as.numeric(),
                          t1player5DoubleKills = as.numeric(),
                          t2player1DoubleKills = as.numeric(),
                          t2player2DoubleKills = as.numeric(),
                          t2player3DoubleKills = as.numeric(),
                          t2player4DoubleKills = as.numeric(),
                          t2player5DoubleKills = as.numeric(),
                          t1player1TripleKills = as.numeric(),
                          t1player2TripleKills = as.numeric(),
                          t1player3TripleKills = as.numeric(),
                          t1player4TripleKills = as.numeric(),
                          t1player5TripleKills = as.numeric(),
                          t2player1TripleKills = as.numeric(),
                          t2player2TripleKills = as.numeric(),
                          t2player3TripleKills = as.numeric(),
                          t2player4TripleKills = as.numeric(),
                          t2player5TripleKills = as.numeric(),
                          t1player1QuadraKills = as.numeric(),
                          t1player2QuadraKills = as.numeric(),
                          t1player3QuadraKills = as.numeric(),
                          t1player4QuadraKills = as.numeric(),
                          t1player5QuadraKills = as.numeric(),
                          t2player1QuadraKills = as.numeric(),
                          t2player2QuadraKills = as.numeric(),
                          t2player3QuadraKills = as.numeric(),
                          t2player4QuadraKills = as.numeric(),
                          t2player5QuadraKills = as.numeric(),
                          t1player1PentaKills = as.numeric(),
                          t1player2PentaKills = as.numeric(),
                          t1player3PentaKills = as.numeric(),
                          t1player4PentaKills = as.numeric(),
                          t1player5PentaKills = as.numeric(),
                          t2player1PentaKills = as.numeric(),
                          t2player2PentaKills = as.numeric(),
                          t2player3PentaKills = as.numeric(),
                          t2player4PentaKills = as.numeric(),
                          t2player5PentaKills = as.numeric(),
                          t1player1UnrealKills = as.numeric(),
                          t1player2UnrealKills = as.numeric(),
                          t1player3UnrealKills = as.numeric(),
                          t1player4UnrealKills = as.numeric(),
                          t1player5UnrealKills = as.numeric(),
                          t2player1UnrealKills = as.numeric(),
                          t2player2UnrealKills = as.numeric(),
                          t2player3UnrealKills = as.numeric(),
                          t2player4UnrealKills = as.numeric(),
                          t2player5UnrealKills = as.numeric(),
                          t1player1TotalDamageDealt = as.numeric(),
                          t1player2TotalDamageDealt = as.numeric(),
                          t1player3TotalDamageDealt = as.numeric(),
                          t1player4TotalDamageDealt = as.numeric(),
                          t1player5TotalDamageDealt = as.numeric(),
                          t2player1TotalDamageDealt = as.numeric(),
                          t2player2TotalDamageDealt = as.numeric(),
                          t2player3TotalDamageDealt = as.numeric(),
                          t2player4TotalDamageDealt = as.numeric(),
                          t2player5TotalDamageDealt = as.numeric(),
                          t1player1MagicDamageDealt = as.numeric(),
                          t1player2MagicDamageDealt = as.numeric(),
                          t1player3MagicDamageDealt = as.numeric(),
                          t1player4MagicDamageDealt = as.numeric(),
                          t1player5MagicDamageDealt = as.numeric(),
                          t2player1MagicDamageDealt = as.numeric(),
                          t2player2MagicDamageDealt = as.numeric(),
                          t2player3MagicDamageDealt = as.numeric(),
                          t2player4MagicDamageDealt = as.numeric(),
                          t2player5MagicDamageDealt = as.numeric(),
                          t1player1PhysicalDamageDealt = as.numeric(),
                          t1player2PhysicalDamageDealt = as.numeric(),
                          t1player3PhysicalDamageDealt = as.numeric(),
                          t1player4PhysicalDamageDealt = as.numeric(),
                          t1player5PhysicalDamageDealt = as.numeric(),
                          t2player1PhysicalDamageDealt = as.numeric(),
                          t2player2PhysicalDamageDealt = as.numeric(),
                          t2player3PhysicalDamageDealt = as.numeric(),
                          t2player4PhysicalDamageDealt = as.numeric(),
                          t2player5PhysicalDamageDealt = as.numeric(),
                          t1player1TrueDamageDealt = as.numeric(),
                          t1player2TrueDamageDealt = as.numeric(),
                          t1player3TrueDamageDealt = as.numeric(),
                          t1player4TrueDamageDealt = as.numeric(),
                          t1player5TrueDamageDealt = as.numeric(),
                          t2player1TrueDamageDealt = as.numeric(),
                          t2player2TrueDamageDealt = as.numeric(),
                          t2player3TrueDamageDealt = as.numeric(),
                          t2player4TrueDamageDealt = as.numeric(),
                          t2player5TrueDamageDealt = as.numeric(),
                          t1player1LargestCriticalStrike = as.numeric(),
                          t1player2LargestCriticalStrike = as.numeric(),
                          t1player3LargestCriticalStrike = as.numeric(),
                          t1player4LargestCriticalStrike = as.numeric(),
                          t1player5LargestCriticalStrike = as.numeric(),
                          t2player1LargestCriticalStrike = as.numeric(),
                          t2player2LargestCriticalStrike = as.numeric(),
                          t2player3LargestCriticalStrike = as.numeric(),
                          t2player4LargestCriticalStrike = as.numeric(),
                          t2player5LargestCriticalStrike = as.numeric(),
                          t1player1TotalDamageDealtToChampions = as.numeric(),
                          t1player2TotalDamageDealtToChampions = as.numeric(),
                          t1player3TotalDamageDealtToChampions = as.numeric(),
                          t1player4TotalDamageDealtToChampions = as.numeric(),
                          t1player5TotalDamageDealtToChampions = as.numeric(),
                          t2player1TotalDamageDealtToChampions = as.numeric(),
                          t2player2TotalDamageDealtToChampions = as.numeric(),
                          t2player3TotalDamageDealtToChampions = as.numeric(),
                          t2player4TotalDamageDealtToChampions = as.numeric(),
                          t2player5TotalDamageDealtToChampions = as.numeric(),
                          t1player1MagicDamageDealtToChampions = as.numeric(),
                          t1player2MagicDamageDealtToChampions = as.numeric(),
                          t1player3MagicDamageDealtToChampions = as.numeric(),
                          t1player4MagicDamageDealtToChampions = as.numeric(),
                          t1player5MagicDamageDealtToChampions = as.numeric(),
                          t2player1MagicDamageDealtToChampions = as.numeric(),
                          t2player2MagicDamageDealtToChampions = as.numeric(),
                          t2player3MagicDamageDealtToChampions = as.numeric(),
                          t2player4MagicDamageDealtToChampions = as.numeric(),
                          t2player5MagicDamageDealtToChampions = as.numeric(),
                          t1player1PhysicalDamageDealtToChampions = as.numeric(),
                          t1player2PhysicalDamageDealtToChampions = as.numeric(),
                          t1player3PhysicalDamageDealtToChampions = as.numeric(),
                          t1player4PhysicalDamageDealtToChampions = as.numeric(),
                          t1player5PhysicalDamageDealtToChampions = as.numeric(),
                          t2player1PhysicalDamageDealtToChampions = as.numeric(),
                          t2player2PhysicalDamageDealtToChampions = as.numeric(),
                          t2player3PhysicalDamageDealtToChampions = as.numeric(),
                          t2player4PhysicalDamageDealtToChampions = as.numeric(),
                          t2player5PhysicalDamageDealtToChampions = as.numeric(),
                          t1player1TrueDamageDealtToChampions = as.numeric(),
                          t1player2TrueDamageDealtToChampions = as.numeric(),
                          t1player3TrueDamageDealtToChampions = as.numeric(),
                          t1player4TrueDamageDealtToChampions = as.numeric(),
                          t1player5TrueDamageDealtToChampions = as.numeric(),
                          t2player1TrueDamageDealtToChampions = as.numeric(),
                          t2player2TrueDamageDealtToChampions = as.numeric(),
                          t2player3TrueDamageDealtToChampions = as.numeric(),
                          t2player4TrueDamageDealtToChampions = as.numeric(),
                          t2player5TrueDamageDealtToChampions = as.numeric(),
                          t1player1TotalHeal = as.numeric(),
                          t1player2TotalHeal = as.numeric(),
                          t1player3TotalHeal = as.numeric(),
                          t1player4TotalHeal = as.numeric(),
                          t1player5TotalHeal = as.numeric(),
                          t2player1TotalHeal = as.numeric(),
                          t2player2TotalHeal = as.numeric(),
                          t2player3TotalHeal = as.numeric(),
                          t2player4TotalHeal = as.numeric(),
                          t2player5TotalHeal = as.numeric(),
                          t1player1TotalUnitsHealed = as.numeric(),
                          t1player2TotalUnitsHealed = as.numeric(),
                          t1player3TotalUnitsHealed = as.numeric(),
                          t1player4TotalUnitsHealed = as.numeric(),
                          t1player5TotalUnitsHealed = as.numeric(),
                          t2player1TotalUnitsHealed = as.numeric(),
                          t2player2TotalUnitsHealed = as.numeric(),
                          t2player3TotalUnitsHealed = as.numeric(),
                          t2player4TotalUnitsHealed = as.numeric(),
                          t2player5TotalUnitsHealed = as.numeric(),
                          t1player1TotalDamageTaken = as.numeric(),
                          t1player2TotalDamageTaken = as.numeric(),
                          t1player3TotalDamageTaken = as.numeric(),
                          t1player4TotalDamageTaken = as.numeric(),
                          t1player5TotalDamageTaken = as.numeric(),
                          t2player1TotalDamageTaken = as.numeric(),
                          t2player2TotalDamageTaken = as.numeric(),
                          t2player3TotalDamageTaken = as.numeric(),
                          t2player4TotalDamageTaken = as.numeric(),
                          t2player5TotalDamageTaken = as.numeric(),
                          t1player1MagicalDamageTaken = as.numeric(),
                          t1player2MagicalDamageTaken = as.numeric(),
                          t1player3MagicalDamageTaken = as.numeric(),
                          t1player4MagicalDamageTaken = as.numeric(),
                          t1player5MagicalDamageTaken = as.numeric(),
                          t2player1MagicalDamageTaken = as.numeric(),
                          t2player2MagicalDamageTaken = as.numeric(),
                          t2player3MagicalDamageTaken = as.numeric(),
                          t2player4MagicalDamageTaken = as.numeric(),
                          t2player5MagicalDamageTaken = as.numeric(),
                          t1player1PhysicalDamageTaken = as.numeric(),
                          t1player2PhysicalDamageTaken = as.numeric(),
                          t1player3PhysicalDamageTaken = as.numeric(),
                          t1player4PhysicalDamageTaken = as.numeric(),
                          t1player5PhysicalDamageTaken = as.numeric(),
                          t2player1PhysicalDamageTaken = as.numeric(),
                          t2player2PhysicalDamageTaken = as.numeric(),
                          t2player3PhysicalDamageTaken = as.numeric(),
                          t2player4PhysicalDamageTaken = as.numeric(),
                          t2player5PhysicalDamageTaken = as.numeric(), 
                          t1player1TrueDamageTaken = as.numeric(),
                          t1player2TrueDamageTaken = as.numeric(),
                          t1player3TrueDamageTaken = as.numeric(),
                          t1player4TrueDamageTaken = as.numeric(),
                          t1player5TrueDamageTaken = as.numeric(),
                          t2player1TrueDamageTaken = as.numeric(),
                          t2player2TrueDamageTaken = as.numeric(),
                          t2player3TrueDamageTaken = as.numeric(),
                          t2player4TrueDamageTaken = as.numeric(),
                          t2player5TrueDamageTaken = as.numeric(), 
                          t1player1GoldEarned = as.numeric(),
                          t1player2GoldEarned = as.numeric(),
                          t1player3GoldEarned = as.numeric(),
                          t1player4GoldEarned = as.numeric(),
                          t1player5GoldEarned = as.numeric(),
                          t2player1GoldEarned = as.numeric(),
                          t2player2GoldEarned = as.numeric(),
                          t2player3GoldEarned = as.numeric(),
                          t2player4GoldEarned = as.numeric(),
                          t2player5GoldEarned = as.numeric(), 
                          t1player1GoldSpent = as.numeric(),
                          t1player2GoldSpent = as.numeric(),
                          t1player3GoldSpent = as.numeric(),
                          t1player4GoldSpent = as.numeric(),
                          t1player5GoldSpent = as.numeric(),
                          t2player1GoldSpent = as.numeric(),
                          t2player2GoldSpent = as.numeric(),
                          t2player3GoldSpent = as.numeric(),
                          t2player4GoldSpent = as.numeric(),
                          t2player5GoldSpent = as.numeric(), 
                          t1player1TurretKills = as.numeric(),
                          t1player2TurretKills = as.numeric(),
                          t1player3TurretKills = as.numeric(),
                          t1player4TurretKills = as.numeric(),
                          t1player5TurretKills = as.numeric(),
                          t2player1TurretKills = as.numeric(),
                          t2player2TurretKills = as.numeric(),
                          t2player3TurretKills = as.numeric(),
                          t2player4TurretKills = as.numeric(),
                          t2player5TurretKills = as.numeric(),
                          t1player1InhibitorKills = as.numeric(),
                          t1player2InhibitorKills = as.numeric(),
                          t1player3InhibitorKills = as.numeric(),
                          t1player4InhibitorKills = as.numeric(),
                          t1player5InhibitorKills = as.numeric(),
                          t2player1InhibitorKills = as.numeric(),
                          t2player2InhibitorKills = as.numeric(),
                          t2player3InhibitorKills = as.numeric(),
                          t2player4InhibitorKills = as.numeric(),
                          t2player5InhibitorKills = as.numeric(),
                          t1player1TotalMinionsKilled = as.numeric(),
                          t1player2TotalMinionsKilled = as.numeric(),
                          t1player3TotalMinionsKilled = as.numeric(),
                          t1player4TotalMinionsKilled = as.numeric(),
                          t1player5TotalMinionsKilled = as.numeric(),
                          t2player1TotalMinionsKilled = as.numeric(),
                          t2player2TotalMinionsKilled = as.numeric(),
                          t2player3TotalMinionsKilled = as.numeric(),
                          t2player4TotalMinionsKilled = as.numeric(),
                          t2player5TotalMinionsKilled = as.numeric(),
                          t1player1NeutralMinionsKilled = as.numeric(),
                          t1player2NeutralMinionsKilled = as.numeric(),
                          t1player3NeutralMinionsKilled = as.numeric(),
                          t1player4NeutralMinionsKilled = as.numeric(),
                          t1player5NeutralMinionsKilled = as.numeric(),
                          t2player1NeutralMinionsKilled = as.numeric(),
                          t2player2NeutralMinionsKilled = as.numeric(),
                          t2player3NeutralMinionsKilled = as.numeric(),
                          t2player4NeutralMinionsKilled = as.numeric(),
                          t2player5NeutralMinionsKilled = as.numeric(),
                          t1player1NeutralMinionsKilledtJungle = as.numeric(),
                          t1player2NeutralMinionsKilledtJungle = as.numeric(),
                          t1player3NeutralMinionsKilledtJungle = as.numeric(),
                          t1player4NeutralMinionsKilledtJungle = as.numeric(),
                          t1player5NeutralMinionsKilledtJungle = as.numeric(),
                          t2player1NeutralMinionsKilledtJungle = as.numeric(),
                          t2player2NeutralMinionsKilledtJungle = as.numeric(),
                          t2player3NeutralMinionsKilledtJungle = as.numeric(),
                          t2player4NeutralMinionsKilledtJungle = as.numeric(),
                          t2player5NeutralMinionsKilledtJungle = as.numeric(),
                          t1player1NeutralMinionsKilledEnemyJungle = as.numeric(),
                          t1player2NeutralMinionsKilledEnemyJungle = as.numeric(),
                          t1player3NeutralMinionsKilledEnemyJungle = as.numeric(),
                          t1player4NeutralMinionsKilledEnemyJungle = as.numeric(),
                          t1player5NeutralMinionsKilledEnemyJungle = as.numeric(),
                          t2player1NeutralMinionsKilledEnemyJungle = as.numeric(),
                          t2player2NeutralMinionsKilledEnemyJungle = as.numeric(),
                          t2player3NeutralMinionsKilledEnemyJungle = as.numeric(),
                          t2player4NeutralMinionsKilledEnemyJungle = as.numeric(),
                          t2player5NeutralMinionsKilledEnemyJungle = as.numeric(),
                          t1player1TotalTimeCrowdControlDealt = as.numeric(),
                          t1player2TotalTimeCrowdControlDealt = as.numeric(),
                          t1player3TotalTimeCrowdControlDealt = as.numeric(),
                          t1player4TotalTimeCrowdControlDealt = as.numeric(),
                          t1player5TotalTimeCrowdControlDealt = as.numeric(),
                          t2player1TotalTimeCrowdControlDealt = as.numeric(),
                          t2player2TotalTimeCrowdControlDealt = as.numeric(),
                          t2player3TotalTimeCrowdControlDealt = as.numeric(),
                          t2player4TotalTimeCrowdControlDealt = as.numeric(),
                          t2player5TotalTimeCrowdControlDealt = as.numeric(),
                          t1player1ChampLevel = as.numeric(),
                          t1player2ChampLevel = as.numeric(),
                          t1player3ChampLevel = as.numeric(),
                          t1player4ChampLevel = as.numeric(),
                          t1player5ChampLevel = as.numeric(),
                          t2player1ChampLevel = as.numeric(),
                          t2player2ChampLevel = as.numeric(),
                          t2player3ChampLevel = as.numeric(),
                          t2player4ChampLevel = as.numeric(),
                          t2player5ChampLevel = as.numeric(),
                          t1player1VisionWardsBoughtInGame = as.numeric(),
                          t1player2VisionWardsBoughtInGame = as.numeric(),
                          t1player3VisionWardsBoughtInGame = as.numeric(),
                          t1player4VisionWardsBoughtInGame = as.numeric(),
                          t1player5VisionWardsBoughtInGame = as.numeric(),
                          t2player1VisionWardsBoughtInGame = as.numeric(),
                          t2player2VisionWardsBoughtInGame = as.numeric(),
                          t2player3VisionWardsBoughtInGame = as.numeric(),
                          t2player4VisionWardsBoughtInGame = as.numeric(),
                          t2player5VisionWardsBoughtInGame = as.numeric(),
                          t1player1SightWardsBoughtInGame = as.numeric(),
                          t1player2SightWardsBoughtInGame = as.numeric(),
                          t1player3SightWardsBoughtInGame = as.numeric(),
                          t1player4SightWardsBoughtInGame = as.numeric(),
                          t1player5SightWardsBoughtInGame = as.numeric(),
                          t2player1SightWardsBoughtInGame = as.numeric(),
                          t2player2SightWardsBoughtInGame = as.numeric(),
                          t2player3SightWardsBoughtInGame = as.numeric(),
                          t2player4SightWardsBoughtInGame = as.numeric(),
                          t2player5SightWardsBoughtInGame = as.numeric(),
                          t1player1WardsPlaced = as.numeric(),
                          t1player2WardsPlaced = as.numeric(),
                          t1player3WardsPlaced = as.numeric(),
                          t1player4WardsPlaced = as.numeric(),
                          t1player5WardsPlaced = as.numeric(),
                          t2player1WardsPlaced = as.numeric(),
                          t2player2WardsPlaced = as.numeric(),
                          t2player3WardsPlaced = as.numeric(),
                          t2player4WardsPlaced = as.numeric(),
                          t2player5WardsPlaced = as.numeric(),
                          t1player1WardsKilled = as.numeric(),
                          t1player2WardsKilled = as.numeric(),
                          t1player3WardsKilled = as.numeric(),
                          t1player4WardsKilled = as.numeric(),
                          t1player5WardsKilled = as.numeric(),
                          t2player1WardsKilled = as.numeric(),
                          t2player2WardsKilled = as.numeric(),
                          t2player3WardsKilled = as.numeric(),
                          t2player4WardsKilled = as.numeric(),
                          t2player5WardsKilled = as.numeric(),
                          t1player1FirstBloodKill = as.logical(),
                          t1player2FirstBloodKill = as.logical(),
                          t1player3FirstBloodKill = as.logical(),
                          t1player4FirstBloodKill = as.logical(),
                          t1player5FirstBloodKill = as.logical(),
                          t2player1FirstBloodKill = as.logical(),
                          t2player2FirstBloodKill = as.logical(),
                          t2player3FirstBloodKill = as.logical(),
                          t2player4FirstBloodKill = as.logical(),
                          t2player5FirstBloodKill = as.logical(),
                          t1player1FirstBloodAssist = as.logical(),
                          t1player2FirstBloodAssist = as.logical(),
                          t1player3FirstBloodAssist = as.logical(),
                          t1player4FirstBloodAssist = as.logical(),
                          t1player5FirstBloodAssist = as.logical(),
                          t2player1FirstBloodAssist = as.logical(),
                          t2player2FirstBloodAssist = as.logical(),
                          t2player3FirstBloodAssist = as.logical(),
                          t2player4FirstBloodAssist = as.logical(),
                          t2player5FirstBloodAssist = as.logical(),
                          t1player1FirstTowerKill = as.logical(),
                          t1player2FirstTowerKill = as.logical(),
                          t1player3FirstTowerKill = as.logical(),
                          t1player4FirstTowerKill = as.logical(),
                          t1player5FirstTowerKill = as.logical(),
                          t2player1FirstTowerKill = as.logical(),
                          t2player2FirstTowerKill = as.logical(),
                          t2player3FirstTowerKill = as.logical(),
                          t2player4FirstTowerKill = as.logical(),
                          t2player5FirstTowerKill = as.logical(),
                          t1player1FirstTowerAssist = as.logical(),
                          t1player2FirstTowerAssist = as.logical(),
                          t1player3FirstTowerAssist = as.logical(),
                          t1player4FirstTowerAssist = as.logical(),
                          t1player5FirstTowerAssist = as.logical(),
                          t2player1FirstTowerAssist = as.logical(),
                          t2player2FirstTowerAssist = as.logical(),
                          t2player3FirstTowerAssist = as.logical(),
                          t2player4FirstTowerAssist = as.logical(),
                          t2player5FirstTowerAssist = as.logical(),
                          t1player1FirstInhibitorKill = as.logical(),
                          t1player2FirstInhibitorKill = as.logical(),
                          t1player3FirstInhibitorKill = as.logical(),
                          t1player4FirstInhibitorKill = as.logical(),
                          t1player5FirstInhibitorKill = as.logical(),
                          t2player1FirstInhibitorKill = as.logical(),
                          t2player2FirstInhibitorKill = as.logical(),
                          t2player3FirstInhibitorKill = as.logical(),
                          t2player4FirstInhibitorKill = as.logical(),
                          t2player5FirstInhibitorKill = as.logical(),
                          t1player1FirstInhibitorAssist = as.logical(),
                          t1player2FirstInhibitorAssist = as.logical(),
                          t1player3FirstInhibitorAssist = as.logical(),
                          t1player4FirstInhibitorAssist = as.logical(),
                          t1player5FirstInhibitorAssist = as.logical(),
                          t2player1FirstInhibitorAssist = as.logical(),
                          t2player2FirstInhibitorAssist = as.logical(),
                          t2player3FirstInhibitorAssist = as.logical(),
                          t2player4FirstInhibitorAssist = as.logical(),
                          t2player5FirstInhibitorAssist = as.logical(),
                          t1player1CombatPlayerScore = as.numeric(),
                          t1player2CombatPlayerScore = as.numeric(),
                          t1player3CombatPlayerScore = as.numeric(),
                          t1player4CombatPlayerScore = as.numeric(),
                          t1player5CombatPlayerScore = as.numeric(),
                          t2player1CombatPlayerScore = as.numeric(),
                          t2player2CombatPlayerScore = as.numeric(),
                          t2player3CombatPlayerScore = as.numeric(),
                          t2player4CombatPlayerScore = as.numeric(),
                          t2player5CombatPlayerScore = as.numeric(),
                          t1player1ObjectivePlayerScore = as.numeric(),
                          t1player2ObjectivePlayerScore = as.numeric(),
                          t1player3ObjectivePlayerScore = as.numeric(),
                          t1player4ObjectivePlayerScore = as.numeric(),
                          t1player5ObjectivePlayerScore = as.numeric(),
                          t2player1ObjectivePlayerScore = as.numeric(),
                          t2player2ObjectivePlayerScore = as.numeric(),
                          t2player3ObjectivePlayerScore = as.numeric(),
                          t2player4ObjectivePlayerScore = as.numeric(),
                          t2player5ObjectivePlayerScore = as.numeric(),
                          t1player1TotalPlayerScore = as.numeric(),
                          t1player2TotalPlayerScore = as.numeric(),
                          t1player3TotalPlayerScore = as.numeric(),
                          t1player4TotalPlayerScore = as.numeric(),
                          t1player5TotalPlayerScore = as.numeric(),
                          t2player1TotalPlayerScore = as.numeric(),
                          t2player2TotalPlayerScore = as.numeric(),
                          t2player3TotalPlayerScore = as.numeric(),
                          t2player4TotalPlayerScore = as.numeric(),
                          t2player5TotalPlayerScore = as.numeric(),
                          t1player1TotalScoreRank = as.numeric(),
                          t1player2TotalScoreRank = as.numeric(),
                          t1player3TotalScoreRank = as.numeric(),
                          t1player4TotalScoreRank = as.numeric(),
                          t1player5TotalScoreRank = as.numeric(),
                          t2player1TotalScoreRank = as.numeric(),
                          t2player2TotalScoreRank = as.numeric(),
                          t2player3TotalScoreRank = as.numeric(),
                          t2player4TotalScoreRank = as.numeric(),
                          t2player5TotalScoreRank = as.numeric(),
                          t1player1CreepsPerMinDeltas0to10 = as.numeric(),
                          t1player2CreepsPerMinDeltas0to10 = as.numeric(),
                          t1player3CreepsPerMinDeltas0to10 = as.numeric(),
                          t1player4CreepsPerMinDeltas0to10 = as.numeric(),
                          t1player5CreepsPerMinDeltas0to10 = as.numeric(),
                          t2player1CreepsPerMinDeltas0to10 = as.numeric(),
                          t2player2CreepsPerMinDeltas0to10 = as.numeric(),
                          t2player3CreepsPerMinDeltas0to10 = as.numeric(),
                          t2player4CreepsPerMinDeltas0to10 = as.numeric(),
                          t2player5CreepsPerMinDeltas0to10 = as.numeric(),
                          t1player1CreepsPerMinDeltas10to20 = as.numeric(),
                          t1player2CreepsPerMinDeltas10to20 = as.numeric(),
                          t1player3CreepsPerMinDeltas10to20 = as.numeric(),
                          t1player4CreepsPerMinDeltas10to20 = as.numeric(),
                          t1player5CreepsPerMinDeltas10to20 = as.numeric(),
                          t2player1CreepsPerMinDeltas10to20 = as.numeric(),
                          t2player2CreepsPerMinDeltas10to20 = as.numeric(),
                          t2player3CreepsPerMinDeltas10to20 = as.numeric(),
                          t2player4CreepsPerMinDeltas10to20 = as.numeric(),
                          t2player5CreepsPerMinDeltas10to20 = as.numeric(),
                          t1player1CreepsPerMinDeltas20to30 = as.numeric(),
                          t1player2CreepsPerMinDeltas20to30 = as.numeric(),
                          t1player3CreepsPerMinDeltas20to30 = as.numeric(),
                          t1player4CreepsPerMinDeltas20to30 = as.numeric(),
                          t1player5CreepsPerMinDeltas20to30 = as.numeric(),
                          t2player1CreepsPerMinDeltas20to30 = as.numeric(),
                          t2player2CreepsPerMinDeltas20to30 = as.numeric(),
                          t2player3CreepsPerMinDeltas20to30 = as.numeric(),
                          t2player4CreepsPerMinDeltas20to30 = as.numeric(),
                          t2player5CreepsPerMinDeltas20to30 = as.numeric(),
                          t1player1CreepsPerMinDeltas30toend = as.numeric(),
                          t1player2CreepsPerMinDeltas30toend = as.numeric(),
                          t1player3CreepsPerMinDeltas30toend = as.numeric(),
                          t1player4CreepsPerMinDeltas30toend = as.numeric(),
                          t1player5CreepsPerMinDeltas30toend = as.numeric(),
                          t2player1CreepsPerMinDeltas30toend = as.numeric(),
                          t2player2CreepsPerMinDeltas30toend = as.numeric(),
                          t2player3CreepsPerMinDeltas30toend = as.numeric(),
                          t2player4CreepsPerMinDeltas30toend = as.numeric(),
                          t2player5CreepsPerMinDeltas30toend = as.numeric(),
                          t1player1XPPerMinDeltas0to10 = as.numeric(),
                          t1player2XPPerMinDeltas0to10 = as.numeric(),
                          t1player3XPPerMinDeltas0to10 = as.numeric(),
                          t1player4XPPerMinDeltas0to10 = as.numeric(),
                          t1player5XPPerMinDeltas0to10 = as.numeric(),
                          t2player1XPPerMinDeltas0to10 = as.numeric(),
                          t2player2XPPerMinDeltas0to10 = as.numeric(),
                          t2player3XPPerMinDeltas0to10 = as.numeric(),
                          t2player4XPPerMinDeltas0to10 = as.numeric(),
                          t2player5XPPerMinDeltas0to10 = as.numeric(),
                          t1player1XPPerMinDeltas10to20 = as.numeric(),
                          t1player2XPPerMinDeltas10to20 = as.numeric(),
                          t1player3XPPerMinDeltas10to20 = as.numeric(),
                          t1player4XPPerMinDeltas10to20 = as.numeric(),
                          t1player5XPPerMinDeltas10to20 = as.numeric(),
                          t2player1XPPerMinDeltas10to20 = as.numeric(),
                          t2player2XPPerMinDeltas10to20 = as.numeric(),
                          t2player3XPPerMinDeltas10to20 = as.numeric(),
                          t2player4XPPerMinDeltas10to20 = as.numeric(),
                          t2player5XPPerMinDeltas10to20 = as.numeric(),
                          t1player1XPPerMinDeltas20to30 = as.numeric(),
                          t1player2XPPerMinDeltas20to30 = as.numeric(),
                          t1player3XPPerMinDeltas20to30 = as.numeric(),
                          t1player4XPPerMinDeltas20to30 = as.numeric(),
                          t1player5XPPerMinDeltas20to30 = as.numeric(),
                          t2player1XPPerMinDeltas20to30 = as.numeric(),
                          t2player2XPPerMinDeltas20to30 = as.numeric(),
                          t2player3XPPerMinDeltas20to30 = as.numeric(),
                          t2player4XPPerMinDeltas20to30 = as.numeric(),
                          t2player5XPPerMinDeltas20to30 = as.numeric(),
                          t1player1XPPerMinDeltas30toend = as.numeric(),
                          t1player2XPPerMinDeltas30toend = as.numeric(),
                          t1player3XPPerMinDeltas30toend = as.numeric(),
                          t1player4XPPerMinDeltas30toend = as.numeric(),
                          t1player5XPPerMinDeltas30toend = as.numeric(),
                          t2player1XPPerMinDeltas30toend = as.numeric(),
                          t2player2XPPerMinDeltas30toend = as.numeric(),
                          t2player3XPPerMinDeltas30toend = as.numeric(),
                          t2player4XPPerMinDeltas30toend = as.numeric(),
                          t2player5XPPerMinDeltas30toend = as.numeric(),
                          t1player1GoldPerMinDeltas0to10 = as.numeric(),
                          t1player2GoldPerMinDeltas0to10 = as.numeric(),
                          t1player3GoldPerMinDeltas0to10 = as.numeric(),
                          t1player4GoldPerMinDeltas0to10 = as.numeric(),
                          t1player5GoldPerMinDeltas0to10 = as.numeric(),
                          t2player1GoldPerMinDeltas0to10 = as.numeric(),
                          t2player2GoldPerMinDeltas0to10 = as.numeric(),
                          t2player3GoldPerMinDeltas0to10 = as.numeric(),
                          t2player4GoldPerMinDeltas0to10 = as.numeric(),
                          t2player5GoldPerMinDeltas0to10 = as.numeric(),
                          t1player1GoldPerMinDeltas10to20 = as.numeric(),
                          t1player2GoldPerMinDeltas10to20 = as.numeric(),
                          t1player3GoldPerMinDeltas10to20 = as.numeric(),
                          t1player4GoldPerMinDeltas10to20 = as.numeric(),
                          t1player5GoldPerMinDeltas10to20 = as.numeric(),
                          t2player1GoldPerMinDeltas10to20 = as.numeric(),
                          t2player2GoldPerMinDeltas10to20 = as.numeric(),
                          t2player3GoldPerMinDeltas10to20 = as.numeric(),
                          t2player4GoldPerMinDeltas10to20 = as.numeric(),
                          t2player5GoldPerMinDeltas10to20 = as.numeric(),
                          t1player1GoldPerMinDeltas20to30 = as.numeric(),
                          t1player2GoldPerMinDeltas20to30 = as.numeric(),
                          t1player3GoldPerMinDeltas20to30 = as.numeric(),
                          t1player4GoldPerMinDeltas20to30 = as.numeric(),
                          t1player5GoldPerMinDeltas20to30 = as.numeric(),
                          t2player1GoldPerMinDeltas20to30 = as.numeric(),
                          t2player2GoldPerMinDeltas20to30 = as.numeric(),
                          t2player3GoldPerMinDeltas20to30 = as.numeric(),
                          t2player4GoldPerMinDeltas20to30 = as.numeric(),
                          t2player5GoldPerMinDeltas20to30 = as.numeric(),
                          t1player1GoldPerMinDeltas30toend = as.numeric(),
                          t1player2GoldPerMinDeltas30toend = as.numeric(),
                          t1player3GoldPerMinDeltas30toend = as.numeric(),
                          t1player4GoldPerMinDeltas30toend = as.numeric(),
                          t1player5GoldPerMinDeltas30toend = as.numeric(),
                          t2player1GoldPerMinDeltas30toend = as.numeric(),
                          t2player2GoldPerMinDeltas30toend = as.numeric(),
                          t2player3GoldPerMinDeltas30toend = as.numeric(),
                          t2player4GoldPerMinDeltas30toend = as.numeric(),
                          t2player5GoldPerMinDeltas30toend = as.numeric(),
                          t1player1CSDiffPerMinDeltas0to10 = as.numeric(),
                          t1player2CSDiffPerMinDeltas0to10 = as.numeric(),
                          t1player3CSDiffPerMinDeltas0to10 = as.numeric(),
                          t1player4CSDiffPerMinDeltas0to10 = as.numeric(),
                          t1player5CSDiffPerMinDeltas0to10 = as.numeric(),
                          t2player1CSDiffPerMinDeltas0to10 = as.numeric(),
                          t2player2CSDiffPerMinDeltas0to10 = as.numeric(),
                          t2player3CSDiffPerMinDeltas0to10 = as.numeric(),
                          t2player4CSDiffPerMinDeltas0to10 = as.numeric(),
                          t2player5CSDiffPerMinDeltas0to10 = as.numeric(),
                          t1player1CSDiffPerMinDeltas10to20 = as.numeric(),
                          t1player2CSDiffPerMinDeltas10to20 = as.numeric(),
                          t1player3CSDiffPerMinDeltas10to20 = as.numeric(),
                          t1player4CSDiffPerMinDeltas10to20 = as.numeric(),
                          t1player5CSDiffPerMinDeltas10to20 = as.numeric(),
                          t2player1CSDiffPerMinDeltas10to20 = as.numeric(),
                          t2player2CSDiffPerMinDeltas10to20 = as.numeric(),
                          t2player3CSDiffPerMinDeltas10to20 = as.numeric(),
                          t2player4CSDiffPerMinDeltas10to20 = as.numeric(),
                          t2player5CSDiffPerMinDeltas10to20 = as.numeric(),
                          t1player1CSDiffPerMinDeltas20to30 = as.numeric(),
                          t1player2CSDiffPerMinDeltas20to30 = as.numeric(),
                          t1player3CSDiffPerMinDeltas20to30 = as.numeric(),
                          t1player4CSDiffPerMinDeltas20to30 = as.numeric(),
                          t1player5CSDiffPerMinDeltas20to30 = as.numeric(),
                          t2player1CSDiffPerMinDeltas20to30 = as.numeric(),
                          t2player2CSDiffPerMinDeltas20to30 = as.numeric(),
                          t2player3CSDiffPerMinDeltas20to30 = as.numeric(),
                          t2player4CSDiffPerMinDeltas20to30 = as.numeric(),
                          t2player5CSDiffPerMinDeltas20to30 = as.numeric(),
                          t1player1CSDiffPerMinDeltas30toend = as.numeric(),
                          t1player2CSDiffPerMinDeltas30toend = as.numeric(),
                          t1player3CSDiffPerMinDeltas30toend = as.numeric(),
                          t1player4CSDiffPerMinDeltas30toend = as.numeric(),
                          t1player5CSDiffPerMinDeltas30toend = as.numeric(),
                          t2player1CSDiffPerMinDeltas30toend = as.numeric(),
                          t2player2CSDiffPerMinDeltas30toend = as.numeric(),
                          t2player3CSDiffPerMinDeltas30toend = as.numeric(),
                          t2player4CSDiffPerMinDeltas30toend = as.numeric(),
                          t2player5CSDiffPerMinDeltas30toend = as.numeric(),
                          t1player1XPDiffPerMinDeltas0to10 = as.numeric(),
                          t1player2XPDiffPerMinDeltas0to10 = as.numeric(),
                          t1player3XPDiffPerMinDeltas0to10 = as.numeric(),
                          t1player4XPDiffPerMinDeltas0to10 = as.numeric(),
                          t1player5XPDiffPerMinDeltas0to10 = as.numeric(),
                          t2player1XPDiffPerMinDeltas0to10 = as.numeric(),
                          t2player2XPDiffPerMinDeltas0to10 = as.numeric(),
                          t2player3XPDiffPerMinDeltas0to10 = as.numeric(),
                          t2player4XPDiffPerMinDeltas0to10 = as.numeric(),
                          t2player5XPDiffPerMinDeltas0to10 = as.numeric(),
                          t1player1XPDiffPerMinDeltas10to20 = as.numeric(),
                          t1player2XPDiffPerMinDeltas10to20 = as.numeric(),
                          t1player3XPDiffPerMinDeltas10to20 = as.numeric(),
                          t1player4XPDiffPerMinDeltas10to20 = as.numeric(),
                          t1player5XPDiffPerMinDeltas10to20 = as.numeric(),
                          t2player1XPDiffPerMinDeltas10to20 = as.numeric(),
                          t2player2XPDiffPerMinDeltas10to20 = as.numeric(),
                          t2player3XPDiffPerMinDeltas10to20 = as.numeric(),
                          t2player4XPDiffPerMinDeltas10to20 = as.numeric(),
                          t2player5XPDiffPerMinDeltas10to20 = as.numeric(),
                          t1player1XPDiffPerMinDeltas20to30 = as.numeric(),
                          t1player2XPDiffPerMinDeltas20to30 = as.numeric(),
                          t1player3XPDiffPerMinDeltas20to30 = as.numeric(),
                          t1player4XPDiffPerMinDeltas20to30 = as.numeric(),
                          t1player5XPDiffPerMinDeltas20to30 = as.numeric(),
                          t2player1XPDiffPerMinDeltas20to30 = as.numeric(),
                          t2player2XPDiffPerMinDeltas20to30 = as.numeric(),
                          t2player3XPDiffPerMinDeltas20to30 = as.numeric(),
                          t2player4XPDiffPerMinDeltas20to30 = as.numeric(),
                          t2player5XPDiffPerMinDeltas20to30 = as.numeric(),
                          t1player1XPDiffPerMinDeltas30toend = as.numeric(),
                          t1player2XPDiffPerMinDeltas30toend = as.numeric(),
                          t1player3XPDiffPerMinDeltas30toend = as.numeric(),
                          t1player4XPDiffPerMinDeltas30toend = as.numeric(),
                          t1player5XPDiffPerMinDeltas30toend = as.numeric(),
                          t2player1XPDiffPerMinDeltas30toend = as.numeric(),
                          t2player2XPDiffPerMinDeltas30toend = as.numeric(),
                          t2player3XPDiffPerMinDeltas30toend = as.numeric(),
                          t2player4XPDiffPerMinDeltas30toend = as.numeric(),
                          t2player5XPDiffPerMinDeltas30toend = as.numeric(),
                          t1player1DamageTakenPerMinDeltas0to10 = as.numeric(),
                          t1player2DamageTakenPerMinDeltas0to10 = as.numeric(),
                          t1player3DamageTakenPerMinDeltas0to10 = as.numeric(),
                          t1player4DamageTakenPerMinDeltas0to10 = as.numeric(),
                          t1player5DamageTakenPerMinDeltas0to10 = as.numeric(),
                          t2player1DamageTakenPerMinDeltas0to10 = as.numeric(),
                          t2player2DamageTakenPerMinDeltas0to10 = as.numeric(),
                          t2player3DamageTakenPerMinDeltas0to10 = as.numeric(),
                          t2player4DamageTakenPerMinDeltas0to10 = as.numeric(),
                          t2player5DamageTakenPerMinDeltas0to10 = as.numeric(),
                          t1player1DamageTakenPerMinDeltas10to20 = as.numeric(),
                          t1player2DamageTakenPerMinDeltas10to20 = as.numeric(),
                          t1player3DamageTakenPerMinDeltas10to20 = as.numeric(),
                          t1player4DamageTakenPerMinDeltas10to20 = as.numeric(),
                          t1player5DamageTakenPerMinDeltas10to20 = as.numeric(),
                          t2player1DamageTakenPerMinDeltas10to20 = as.numeric(),
                          t2player2DamageTakenPerMinDeltas10to20 = as.numeric(),
                          t2player3DamageTakenPerMinDeltas10to20 = as.numeric(),
                          t2player4DamageTakenPerMinDeltas10to20 = as.numeric(),
                          t2player5DamageTakenPerMinDeltas10to20 = as.numeric(),
                          t1player1DamageTakenPerMinDeltas20to30 = as.numeric(),
                          t1player2DamageTakenPerMinDeltas20to30 = as.numeric(),
                          t1player3DamageTakenPerMinDeltas20to30 = as.numeric(),
                          t1player4DamageTakenPerMinDeltas20to30 = as.numeric(),
                          t1player5DamageTakenPerMinDeltas20to30 = as.numeric(),
                          t2player1DamageTakenPerMinDeltas20to30 = as.numeric(),
                          t2player2DamageTakenPerMinDeltas20to30 = as.numeric(),
                          t2player3DamageTakenPerMinDeltas20to30 = as.numeric(),
                          t2player4DamageTakenPerMinDeltas20to30 = as.numeric(),
                          t2player5DamageTakenPerMinDeltas20to30 = as.numeric(),
                          t1player1DamageTakenPerMinDeltas30toend = as.numeric(),
                          t1player2DamageTakenPerMinDeltas30toend = as.numeric(),
                          t1player3DamageTakenPerMinDeltas30toend = as.numeric(),
                          t1player4DamageTakenPerMinDeltas30toend = as.numeric(),
                          t1player5DamageTakenPerMinDeltas30toend = as.numeric(),
                          t2player1DamageTakenPerMinDeltas30toend = as.numeric(),
                          t2player2DamageTakenPerMinDeltas30toend = as.numeric(),
                          t2player3DamageTakenPerMinDeltas30toend = as.numeric(),
                          t2player4DamageTakenPerMinDeltas30toend = as.numeric(),
                          t2player5DamageTakenPerMinDeltas30toend = as.numeric(),
                          t1player1DamageTakenDiffPerMinDeltas0to10 = as.numeric(),
                          t1player2DamageTakenDiffPerMinDeltas0to10 = as.numeric(),
                          t1player3DamageTakenDiffPerMinDeltas0to10 = as.numeric(),
                          t1player4DamageTakenDiffPerMinDeltas0to10 = as.numeric(),
                          t1player5DamageTakenDiffPerMinDeltas0to10 = as.numeric(),
                          t2player1DamageTakenDiffPerMinDeltas0to10 = as.numeric(),
                          t2player2DamageTakenDiffPerMinDeltas0to10 = as.numeric(),
                          t2player3DamageTakenDiffPerMinDeltas0to10 = as.numeric(),
                          t2player4DamageTakenDiffPerMinDeltas0to10 = as.numeric(),
                          t2player5DamageTakenDiffPerMinDeltas0to10 = as.numeric(),
                          t1player1DamageTakenDiffPerMinDeltas10to20 = as.numeric(),
                          t1player2DamageTakenDiffPerMinDeltas10to20 = as.numeric(),
                          t1player3DamageTakenDiffPerMinDeltas10to20 = as.numeric(),
                          t1player4DamageTakenDiffPerMinDeltas10to20 = as.numeric(),
                          t1player5DamageTakenDiffPerMinDeltas10to20 = as.numeric(),
                          t2player1DamageTakenDiffPerMinDeltas10to20 = as.numeric(),
                          t2player2DamageTakenDiffPerMinDeltas10to20 = as.numeric(),
                          t2player3DamageTakenDiffPerMinDeltas10to20 = as.numeric(),
                          t2player4DamageTakenDiffPerMinDeltas10to20 = as.numeric(),
                          t2player5DamageTakenDiffPerMinDeltas10to20 = as.numeric(),
                          t1player1DamageTakenDiffPerMinDeltas20to30 = as.numeric(),
                          t1player2DamageTakenDiffPerMinDeltas20to30 = as.numeric(),
                          t1player3DamageTakenDiffPerMinDeltas20to30 = as.numeric(),
                          t1player4DamageTakenDiffPerMinDeltas20to30 = as.numeric(),
                          t1player5DamageTakenDiffPerMinDeltas20to30 = as.numeric(),
                          t2player1DamageTakenDiffPerMinDeltas20to30 = as.numeric(),
                          t2player2DamageTakenDiffPerMinDeltas20to30 = as.numeric(),
                          t2player3DamageTakenDiffPerMinDeltas20to30 = as.numeric(),
                          t2player4DamageTakenDiffPerMinDeltas20to30 = as.numeric(),
                          t2player5DamageTakenDiffPerMinDeltas20to30 = as.numeric(),
                          t1player1DamageTakenDiffPerMinDeltas30toend = as.numeric(),
                          t1player2DamageTakenDiffPerMinDeltas30toend = as.numeric(),
                          t1player3DamageTakenDiffPerMinDeltas30toend = as.numeric(),
                          t1player4DamageTakenDiffPerMinDeltas30toend = as.numeric(),
                          t1player5DamageTakenDiffPerMinDeltas30toend = as.numeric(),
                          t2player1DamageTakenDiffPerMinDeltas30toend = as.numeric(),
                          t2player2DamageTakenDiffPerMinDeltas30toend = as.numeric(),
                          t2player3DamageTakenDiffPerMinDeltas30toend = as.numeric(),
                          t2player4DamageTakenDiffPerMinDeltas30toend = as.numeric(),
                          t2player5DamageTakenDiffPerMinDeltas30toend = as.numeric(),
                          stringsAsFactors = FALSE)

## Load game data from each game
for (i in 1:length(fullGameList)) {
  
  ## Generic game data
  allGameData[i, "gameHash"] <- names(fullGameList)[i]
  allGameData[i, "gameId"] <- fullGameList[[i]][["gameId"]]
  allGameData[i, "platformId"] <- fullGameList[[i]][["platformId"]]
  allGameData[i, "gameCreation"] <- fullGameList[[i]][["gameCreation"]]
  allGameData[i, "gameDuration"] <- fullGameList[[i]][["gameDuration"]]
  allGameData[i, "queueId"] <- fullGameList[[i]][["queueId"]] ## Unnecesary
  allGameData[i, "mapId"] <- fullGameList[[i]][["mapId"]] ## likely not necesary
  allGameData[i, "seasonId"] <- fullGameList[[i]][["seasonId"]] ## likely not Unnecesary
  allGameData[i, "gameVersion"] <- fullGameList[[i]][["gameVersion"]] ## Not sure how to handle yet
  allGameData[i, "gameMode"] <- fullGameList[[i]][["gameMode"]] ## Unnecesary
  allGameData[i, "gameType"] <- fullGameList[[i]][["gameType"]] ## Unnecesary
  
  ## Team 1 data
  allGameData[i, "t1Win"] <- fullGameList[[i]][["teams"]][["win"]][[1]]
  allGameData[i, "t1FirstBlood"] <- fullGameList[[i]][["teams"]][["firstBlood"]][[1]]
  allGameData[i, "t1FirstTower"] <- fullGameList[[i]][["teams"]][["firstTower"]][[1]]
  allGameData[i, "t1FirstInhibitor"] <- fullGameList[[i]][["teams"]][["firstInhibitor"]][[1]]
  allGameData[i, "t1FirstBaron"] <- fullGameList[[i]][["teams"]][["firstBaron"]][[1]]
  allGameData[i, "t1FirstDragon"] <- fullGameList[[i]][["teams"]][["firstDragon"]][[1]]
  allGameData[i, "t1TowerKills"] <- fullGameList[[i]][["teams"]][["towerKills"]][[1]]
  allGameData[i, "t1InhibitorKills"] <- fullGameList[[i]][["teams"]][["inhibitorKills"]][[1]]
  allGameData[i, "t1BaronKills"] <- fullGameList[[i]][["teams"]][["baronKills"]][[1]]
  allGameData[i, "t1DragonKills"] <- fullGameList[[i]][["teams"]][["dragonKills"]][[1]]
  allGameData[i, "t1VilemawKills"] <- fullGameList[[i]][["teams"]][["vilemawKills"]][[1]]
  allGameData[i, "t1DominionVictoryScore"] <- fullGameList[[i]][["teams"]][["dominionVictoryScore"]][[1]]
  
  ## Test for Rift Herald
  if ("firstRiftHerald" %in% names(fullGameList[[i]][["teams"]])) {
    allGameData[i, "t1FirstRiftHerald"] <- fullGameList[[i]][["teams"]][["firstRiftHerald"]][[1]]
  } else {
    allGameData[i, "t1FirstRiftHerald"] <- NA
  }
  
  if ("riftHeraldKills" %in% names(fullGameList[[i]][["teams"]])) {
    allGameData[i, "t1RiftHeraldKills"] <- fullGameList[[i]][["teams"]][["riftHeraldKills"]][[1]]
  } else {
    allGameData[i, "t1RiftHeraldKills"] <- NA
  }
  
  ## Team 2 data
  allGameData[i, "t2Win"] <- fullGameList[[i]][["teams"]][["win"]][[2]]
  allGameData[i, "t2FirstBlood"] <- fullGameList[[i]][["teams"]][["firstBlood"]][[2]]
  allGameData[i, "t2FirstTower"] <- fullGameList[[i]][["teams"]][["firstTower"]][[2]]
  allGameData[i, "t2FirstInhibitor"] <- fullGameList[[i]][["teams"]][["firstInhibitor"]][[2]]
  allGameData[i, "t2FirstBaron"] <- fullGameList[[i]][["teams"]][["firstBaron"]][[2]]
  allGameData[i, "t2FirstDragon"] <- fullGameList[[i]][["teams"]][["firstDragon"]][[2]]
  allGameData[i, "t2TowerKills"] <- fullGameList[[i]][["teams"]][["towerKills"]][[2]]
  allGameData[i, "t2InhibitorKills"] <- fullGameList[[i]][["teams"]][["inhibitorKills"]][[2]]
  allGameData[i, "t2BaronKills"] <- fullGameList[[i]][["teams"]][["baronKills"]][[2]]
  allGameData[i, "t2DragonKills"] <- fullGameList[[i]][["teams"]][["dragonKills"]][[2]]
  allGameData[i, "t2VilemawKills"] <- fullGameList[[i]][["teams"]][["vilemawKills"]][[2]]
  allGameData[i, "t2DominionVictoryScore"] <- fullGameList[[i]][["teams"]][["dominionVictoryScore"]][[2]]
  
  ## Test for Rift Herald
  if ("firstRiftHerald" %in% names(fullGameList[[i]][["teams"]])) {
    allGameData[i, "t2FirstRiftHerald"] <- fullGameList[[i]][["teams"]][["firstRiftHerald"]][[2]]
  } else {
    allGameData[i, "t2FirstRiftHerald"] <- NA
  }
  
  if ("riftHeraldKills" %in% names(fullGameList[[i]][["teams"]])) {
    allGameData[i, "t2RiftHeraldKills"] <- fullGameList[[i]][["teams"]][["riftHeraldKills"]][[2]]
  } else {
    allGameData[i, "t2RiftHeraldKills"] <- NA
  }
  
  ## Load player names
  allGameData[i, "t1player1Name"] <- fullGameList[[i]][["participantIdentities"]][["player"]][[1]][[1]]
  allGameData[i, "t1player2Name"] <- fullGameList[[i]][["participantIdentities"]][["player"]][[1]][[2]]
  allGameData[i, "t1player3Name"] <- fullGameList[[i]][["participantIdentities"]][["player"]][[1]][[3]]
  allGameData[i, "t1player4Name"] <- fullGameList[[i]][["participantIdentities"]][["player"]][[1]][[4]]
  allGameData[i, "t1player5Name"] <- fullGameList[[i]][["participantIdentities"]][["player"]][[1]][[5]]
  allGameData[i, "t2player1Name"] <- fullGameList[[i]][["participantIdentities"]][["player"]][[1]][[6]]
  allGameData[i, "t2player2Name"] <- fullGameList[[i]][["participantIdentities"]][["player"]][[1]][[7]]
  allGameData[i, "t2player3Name"] <- fullGameList[[i]][["participantIdentities"]][["player"]][[1]][[8]]
  allGameData[i, "t2player4Name"] <- fullGameList[[i]][["participantIdentities"]][["player"]][[1]][[9]]
  allGameData[i, "t2player5Name"] <- fullGameList[[i]][["participantIdentities"]][["player"]][[1]][[10]]
  
  ## Load player champion
  extractPlayerData(num = i,
                    name = "Champion",
                    firstLink = "participants",
                    secondLink = "championId")
  
  ## Load player spells
  extractPlayerData(num = i,
                    name = "Spell1",
                    firstLink = "participants",
                    secondLink = "spell1Id")
  
  extractPlayerData(num = i,
                    name = "Spell2",
                    firstLink = "participants",
                    secondLink = "spell2Id")
  
  ## Load masteries and runes
  allGameData[[i, "t1player1Masteries"]] <- fullGameList[[i]][["participants"]][["masteries"]][[1]]
  allGameData[[i, "t1player2Masteries"]] <- fullGameList[[i]][["participants"]][["masteries"]][[2]]
  allGameData[[i, "t1player3Masteries"]] <- fullGameList[[i]][["participants"]][["masteries"]][[3]]
  allGameData[[i, "t1player4Masteries"]] <- fullGameList[[i]][["participants"]][["masteries"]][[4]]
  allGameData[[i, "t1player5Masteries"]] <- fullGameList[[i]][["participants"]][["masteries"]][[5]]
  allGameData[[i, "t2player1Masteries"]] <- fullGameList[[i]][["participants"]][["masteries"]][[6]]
  allGameData[[i, "t2player2Masteries"]] <- fullGameList[[i]][["participants"]][["masteries"]][[7]]
  allGameData[[i, "t2player3Masteries"]] <- fullGameList[[i]][["participants"]][["masteries"]][[8]]
  allGameData[[i, "t2player4Masteries"]] <- fullGameList[[i]][["participants"]][["masteries"]][[9]]
  allGameData[[i, "t2player5Masteries"]] <- fullGameList[[i]][["participants"]][["masteries"]][[10]]
  
  allGameData[[i, "t1player1Runes"]] <- fullGameList[[i]][["participants"]][["runes"]][[1]]
  allGameData[[i, "t1player2Runes"]] <- fullGameList[[i]][["participants"]][["runes"]][[2]]
  allGameData[[i, "t1player3Runes"]] <- fullGameList[[i]][["participants"]][["runes"]][[3]]
  allGameData[[i, "t1player4Runes"]] <- fullGameList[[i]][["participants"]][["runes"]][[4]]
  allGameData[[i, "t1player5Runes"]] <- fullGameList[[i]][["participants"]][["runes"]][[5]]
  allGameData[[i, "t2player1Runes"]] <- fullGameList[[i]][["participants"]][["runes"]][[6]]
  allGameData[[i, "t2player2Runes"]] <- fullGameList[[i]][["participants"]][["runes"]][[7]]
  allGameData[[i, "t2player3Runes"]] <- fullGameList[[i]][["participants"]][["runes"]][[8]]
  allGameData[[i, "t2player4Runes"]] <- fullGameList[[i]][["participants"]][["runes"]][[9]]
  allGameData[[i, "t2player5Runes"]] <- fullGameList[[i]][["participants"]][["runes"]][[10]]
  
  ## Load player items
  extractPlayerData(num = i,
                    name = "Item0",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "item0")
  
  extractPlayerData(num = i,
                    name = "Item1",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "item1")
  
  extractPlayerData(num = i,
                    name = "Item2",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "item2")
  
  extractPlayerData(num = i,
                    name = "Item3",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "item3")
  
  extractPlayerData(num = i,
                    name = "Item4",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "item4")
  
  extractPlayerData(num = i,
                    name = "Item5",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "item5")
  
  extractPlayerData(num = i,
                    name = "Item6",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "item6")
  
  ## Load KDAs
  extractPlayerData(num = i,
                    name = "Kills",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "kills")
  
  extractPlayerData(num = i,
                    name = "Deaths",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "deaths")
  
  extractPlayerData(num = i,
                    name = "Assists",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "assists")
  
  ## Load sprees
  extractPlayerData(num = i,
                    name = "LargestKillingSpree",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "largestKillingSpree")
  
  extractPlayerData(num = i,
                    name = "LargestMultiKill",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "largestMultiKill")
  
  extractPlayerData(num = i,
                    name = "KillingSprees",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "killingSprees")
  
  extractPlayerData(num = i,
                    name = "LongestTimeSpentLiving",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "longestTimeSpentLiving")
  
  ## Load multi kills
  extractPlayerData(num = i,
                    name = "DoubleKills",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "doubleKills")
  
  extractPlayerData(num = i,
                    name = "TripleKills",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "tripleKills")
  
  extractPlayerData(num = i,
                    name = "QuadraKills",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "quadraKills")
  
  extractPlayerData(num = i,
                    name = "PentaKills",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "pentaKills")
  
  extractPlayerData(num = i,
                    name = "UnrealKills",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "unrealKills")
  
  ## Load damage data
  extractPlayerData(num = i,
                    name = "TotalDamageDealt",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "totalDamageDealt")
  
  extractPlayerData(num = i,
                    name = "MagicDamageDealt",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "magicDamageDealt")
  
  extractPlayerData(num = i,
                    name = "PhysicalDamageDealt",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "physicalDamageDealt")
  
  extractPlayerData(num = i,
                    name = "TrueDamageDealt",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "trueDamageDealt")
  
  extractPlayerData(num = i,
                    name = "LargestCriticalStrike",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "largestCriticalStrike")
  
  extractPlayerData(num = i,
                    name = "TotalDamageDealtToChampions",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "totalDamageDealtToChampions")
  
  extractPlayerData(num = i,
                    name = "MagicDamageDealtToChampions",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "magicDamageDealtToChampions")
  
  extractPlayerData(num = i,
                    name = "PhysicalDamageDealtToChampions",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "physicalDamageDealtToChampions")
  
  extractPlayerData(num = i,
                    name = "TrueDamageDealtToChampions",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "trueDamageDealtToChampions")
  
  ## Load Heal data
  extractPlayerData(num = i,
                    name = "TotalHeal",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "totalHeal")
  
  extractPlayerData(num = i,
                    name = "TotalUnitsHealed",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "totalUnitsHealed")
  
  ## Load damage taken data
  extractPlayerData(num = i,
                    name = "TotalDamageTaken",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "totalDamageTaken")
  
  extractPlayerData(num = i,
                    name = "MagicalDamageTaken",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "magicalDamageTaken")
  
  extractPlayerData(num = i,
                    name = "PhysicalDamageTaken",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "physicalDamageTaken")
  
  extractPlayerData(num = i,
                    name = "TrueDamageTaken",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "trueDamageTaken")
  
  ## Load gold data
  extractPlayerData(num = i,
                    name = "GoldEarned",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "goldEarned")
  
  extractPlayerData(num = i,
                    name = "GoldSpent",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "goldSpent")
  
  ## Load turret/inhibitor kills
  extractPlayerData(num = i,
                    name = "TurretKills",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "turretKills")
  
  extractPlayerData(num = i,
                    name = "InhibitorKills",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "inhibitorKills")
  
  ## Load minions killed data
  extractPlayerData(num = i,
                    name = "TotalMinionsKilled",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "totalMinionsKilled")
  
  extractPlayerData(num = i,
                    name = "NeutralMinionsKilled",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "neutralMinionsKilled")
  
  extractPlayerData(num = i,
                    name = "NeutralMinionsKilledTeamJungle",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "neutralMinionsKilledTeamJungle")
  
  extractPlayerData(num = i,
                    name = "NeutralMinionsKilledEnemyJungle",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "neutralMinionsKilledEnemyJungle")
  
  extractPlayerData(num = i,
                    name = "TotalTimeCrowdControlDealt",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "totalTimeCrowdControlDealt")
  
  ## Load champ level and ward data
  extractPlayerData(num = i,
                    name = "ChampLevel",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "champLevel")
  
  extractPlayerData(num = i,
                    name = "VisionWardsBoughtInGame",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "visionWardsBoughtInGame")
  
  extractPlayerData(num = i,
                    name = "SightWardsBoughtInGame",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "sightWardsBoughtInGame")
  
  extractPlayerData(num = i,
                    name = "WardsPlaced",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "wardsPlaced")
  
  extractPlayerData(num = i,
                    name = "WardsKilled",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "wardsKilled")
  
  ## Load first kill/assists
  extractPlayerData(num = i,
                    name = "FirstBloodKill",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "firstBloodKill")
  
  extractPlayerData(num = i,
                    name = "FirstBloodAssist",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "firstBloodAssist")
  
  extractPlayerData(num = i,
                    name = "FirstTowerKill",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "firstTowerKill")
  
  extractPlayerData(num = i,
                    name = "FirstTowerAssist",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "firstTowerAssist")
  
  ## test for first inhibitor stats
  if ("firstInhibitorKill" %in% names(fullGameList[[i]][["participants"]][["stats"]])) {
    extractPlayerData(num = i,
                      name = "FirstInhibitorKill",
                      firstLink = "participants",
                      secondLink = "stats",
                      thirdLink = "firstInhibitorKill")
    
    extractPlayerData(num = i,
                      name = "FirstInhibitorAssist",
                      firstLink = "participants",
                      secondLink = "stats",
                      thirdLink = "firstInhibitorAssist")
    
  } else {
    allGameData[i, "t1player1FirstInhibitorKill"] <- NA
    allGameData[i, "t1player2FirstInhibitorKill"] <- NA
    allGameData[i, "t1player3FirstInhibitorKill"] <- NA
    allGameData[i, "t1player4FirstInhibitorKill"] <- NA
    allGameData[i, "t1player5FirstInhibitorKill"] <- NA
    allGameData[i, "t2player1FirstInhibitorKill"] <- NA
    allGameData[i, "t2player2FirstInhibitorKill"] <- NA
    allGameData[i, "t2player3FirstInhibitorKill"] <- NA
    allGameData[i, "t2player4FirstInhibitorKill"] <- NA
    allGameData[i, "t2player5FirstInhibitorKill"] <- NA
    allGameData[i, "t1player1FirstInhibitorAssist"] <- NA
    allGameData[i, "t1player2FirstInhibitorAssist"] <- NA
    allGameData[i, "t1player3FirstInhibitorAssist"] <- NA
    allGameData[i, "t1player4FirstInhibitorAssist"] <- NA
    allGameData[i, "t1player5FirstInhibitorAssist"] <- NA
    allGameData[i, "t2player1FirstInhibitorAssist"] <- NA
    allGameData[i, "t2player2FirstInhibitorAssist"] <- NA
    allGameData[i, "t2player3FirstInhibitorAssist"] <- NA
    allGameData[i, "t2player4FirstInhibitorAssist"] <- NA
    allGameData[i, "t2player5FirstInhibitorAssist"] <- NA
  }
  
  ## Load player scores
  extractPlayerData(num = i,
                    name = "CombatPlayerScore",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "combatPlayerScore")
  
  extractPlayerData(num = i,
                    name = "ObjectivePlayerScore",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "objectivePlayerScore")
  
  extractPlayerData(num = i,
                    name = "TotalPlayerScore",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "totalPlayerScore")
  
  extractPlayerData(num = i,
                    name = "TotalScoreRank",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "totalScoreRank")
  
  ## Load role and lane
  extractPlayerData(num = i,
                    name = "Role",
                    firstLink = "participants",
                    secondLink = "timeline",
                    thirdLink = "role")
  
  extractPlayerData(num = i,
                    name = "Lane",
                    firstLink = "participants",
                    secondLink = "timeline",
                    thirdLink = "lane")
  
  ## Load timeline data
  extractTimelineData(num = i,
                      name = "CreepsPerMinDeltas",
                      link = "creepsPerMinDeltas")
  
  extractTimelineData(num = i,
                      name = "XPPerMinDeltas",
                      link = "xpPerMinDeltas")
  
  extractTimelineData(num = i,
                      name = "GoldPerMinDeltas",
                      link = "goldPerMinDeltas")
  
  extractTimelineData(num = i,
                      name = "CSDiffPerMinDeltas",
                      link = "csDiffPerMinDeltas")
  
  extractTimelineData(num = i,
                      name = "XPDiffPerMinDeltas",
                      link = "xpDiffPerMinDeltas")
  
  extractTimelineData(num = i,
                      name = "DamageTakenPerMinDeltas",
                      link = "damageTakenPerMinDeltas")
  
  extractTimelineData(num = i,
                      name = "DamageTakenDiffPerMinDeltas",
                      link = "damageTakenDiffPerMinDeltas")
  
  
  ## Track progress
  print(round((i/length(fullGameList))*100,1))  
}
