## Function to determine the player id, role and team, from a game data JSON

mergeRoster <- function(gameData = gameDataTemp, tournID, environment = .GlobalEnv){
   
    suppressMessages(suppressWarnings(library(dplyr)))  
    
    
   
  
  
    ## Put final DF in specified environment
    assign("leagueGames", gamesDF, envir = environment)
        
}