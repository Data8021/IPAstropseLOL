## Function to merge a roster df with high level game information df

mergeRoster <- function(rosterDF = tournamentRosters, gamesDF = leagueGames, environment = .GlobalEnv){
   
    suppressMessages(suppressWarnings(library(dplyr)))  
    
    
    ## Downselect variables and remove rows with NA in rosterDF
    rosterDF <- select(rosterDF, -rosterPlayerName,
                                 -rosterPlayerID) %>%
        filter(!is.na(rosterTeamID)) %>%
        distinct()
    
    ## Rename roster variables for blue team
    rosterDF <- rename(rosterDF, rosterBlue = rosterID,
                                 blueTeamAcro = rosterTeamAcro,
                                 blueTeamID = rosterTeamID)
    
    ## Merge for blue team
    gamesDF <- suppressMessages(left_join(gamesDF, rosterDF))
    
    ## Rename roster variables for red team
    rosterDF <- rename(rosterDF, rosterRed = rosterBlue,
                                 redTeamAcro = blueTeamAcro,
                                 redTeamID = blueTeamID)
    
    ## Merge for red team
    gamesDF <- suppressMessages(left_join(gamesDF, rosterDF))
    
    ## Put final DF in specified environment
    assign("leagueGames", gamesDF, envir = environment)
        
}