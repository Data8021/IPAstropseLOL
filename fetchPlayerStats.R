## Function to fetch player stats based on a list of league tournaments

fetchPlayerStats <- function(rawTournList){
  
  suppressMessages(suppressWarnings(library(jsonlite)))
  suppressMessages(suppressWarnings(library(dplyr)))
  
  ## Initialize df for player stats
  playerStatsTourn <- data.frame(matrix(ncol = 15, nrow = 0))

  ## Loop through each league
  for (i in 1:length(rawTournList)) {
    
    ## Loop through each tournament
    for (j in 1:length(rawTournList[[i]][["highlanderTournaments"]])) {
      
      ## Fetch player stats and conver to df
      playerStatsTournTemp <- fromJSON(paste0("http://api.lolesports.com/api/v2/tournamentPlayerStats?tournamentId=",
                                              rawTournList[[i]][["highlanderTournaments"]][[j]][["id"]]))[[1]]
      
      ## Test if any player stats exist
      if (!(length(playerStatsTournTemp) == 0)) {
        
        ## Add tournament id
        playerStatsTournTemp <- mutate(playerStatsTournTemp,
                                       tournamentID = rawTournList[[i]][["highlanderTournaments"]][[j]][["id"]])
        
        ## Bind the temp df onto full one
        playerStatsTourn <- rbind(playerStatsTourn, playerStatsTournTemp)
        
      }
    
    print(paste0("i = ",i,", j = ",j))
    }
    
  }
  
  ## Rename variables
  playerStatsTourn <- rename(playerStatsTourn,
                             playerID = id,
                             playerName = name,
                             playerPosition = position,
                             teamAcro = team)
  
  ## Put final DF in specified env
  return(playerStatsTourn)
  
}