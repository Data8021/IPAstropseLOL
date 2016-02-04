## Function to fetch player stats based on a list of league tournaments

fetchPlayerStats <- function(tournamentList = tournamentList, environment = .GlobalEnv){
  
  suppressMessages(suppressWarnings(library(jsonlite)))
  suppressMessages(suppressWarnings(library(dplyr)))
  
  ## Initialize df for player stats
  playerStatsTourn <- data.frame(matrix(ncol = 15, nrow = 0))

  ## Loop through each tournament
  for (i in 1:nrow(tournamentList)) {
    
    playerStatsTournTemp <- fromJSON(paste0("http://api.lolesports.com/api/v2/tournamentPlayerStats?tournamentId=", tournamentList[i, 1]))[[1]]
    
    ## Extract df from list
    playerStatsTournTemp <- playerStatsTournTemp
    
    ## Bind the temp df onto full one
    playerStatsTourn <- rbind(playerStatsTourn, playerStatsTournTemp)
  }
  
  ## Rename variables
  playerStatsTourn <- rename(playerStatsTourn,
                             playerID = id,
                             playerName = name,
                             playerPosition = position,
                             teamAcro = team)
  
  ## Put final DF in specified env
  assign("playerStatsTourn", playerStatsTourn, envir = environment)
  
}