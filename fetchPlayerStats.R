## Function to fetch player stats based on a list of league tournaments

fetchPlayerStats <- function(tournamentList = tournamentList, environment = .GlobalEnv){
  
  suppressMessages(suppressWarnings(library(jsonlite)))
  suppressMessages(suppressWarnings(library(dplyr)))
  
  ## Initialize df for player stats
  playerStatsTourn <- data.frame(matrix(ncol = 15, nrow = 0))

  ## Loop through each tournament
  for (i in 1:nrow(tournamentList)) {
    
    ## Fetch player stats and conver to df
    playerStatsTournTemp <- fromJSON(paste0("http://api.lolesports.com/api/v2/tournamentPlayerStats?tournamentId=", tournamentList[i, 1]))[[1]]
    
    ## Test if any player stats exist
    if (!(length(playerStatsTournTemp) == 0)) {
        
        ## Add tournament id
        playerStatsTournTemp <- mutate(playerStatsTournTemp, tournamentID = tournamentList[i, 1])
        
        ## Bind the temp df onto full one
        playerStatsTourn <- rbind(playerStatsTourn, playerStatsTournTemp)
        
    }
    

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