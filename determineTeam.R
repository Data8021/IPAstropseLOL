## Function to determine the player id, role and team, from a game data JSON

determineTeam <- function(playerTest,
                          tournID,
                          playerDB = playersDatabase,
                          teamDB = teamsDatabase,
                          teamPlayersDB = teamPlayersDatabase,
                          environment = .GlobalEnv){
   
    suppressMessages(suppressWarnings(library(dplyr)))  
    
    playerDB = filter(playerDB,
                      tournamentID == tournID,
                      playerName == playerTestBlue)
    
  
  
    ## Put final DF in specified environment
    assign("leagueGames", gamesDF, envir = environment)
        
}