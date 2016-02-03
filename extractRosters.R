## Function to extract all rosters from a complete list
## of league tournament files

extractRosters <- function(rawTournList = leagueTournamentList){
  
  ## Create dataframe to hold roster information
  tournamentRosters <- data.frame(rosterID = character(),
                                  rosterTeamAcro = character(),
                                  rosterTeamID = numeric(),
                                  rosterPlayerName = character(),
                                  rosterPlayerID = numeric(),
                                  tournamentID = character(),
                                  stringsAsFactors = FALSE)
  
  ## Loop through each league
  for (i in 1:length(rawTournList)){
    
    ## Loop through each tournament
    for (j in 1:length(rawTournList[[i]][["highlanderTournaments"]])) {
      
      ## Store tournament ID
      tournamentID <- rawTournList[[i]][["highlanderTournaments"]][[j]][["id"]]
      
      ## Extract rosters 
      rosterList <- as.list(rawTournList[[i]][["highlanderTournaments"]][[j]][["rosters"]])
      
      ## Loop through each roster
      for (k in 1:length(rosterList)){
        
        ## Determine if team or player roster
        if ("player" %in% names(rosterList[[k]])) {
          
          rosterID <- rosterList[[k]][["id"]]
          rosterPlayerName <- rosterList[[k]][["name"]]
          rosterPlayerID <- rosterList[[k]][["player"]]
          
          numRosters <-length(rosterList[[k]][["id"]])
          rosterTeamAcro <- rep(NA_character_, times=numRosters)
          rosterTeamID <- rep(NA_real_, times=numRosters)
          
          tournamentRostersTemp <- data.frame(rosterID,
                                              rosterTeamAcro,
                                              rosterTeamID,
                                              rosterPlayerName,
                                              rosterPlayerID,
                                              tournamentID,
                                              stringsAsFactors = FALSE)
          
        } else {
          
          rosterID <- rosterList[[k]][["id"]]
          rosterTeamAcro <- rosterList[[k]][["name"]]
          rosterTeamID <- rosterList[[k]][["team"]]
          
          numRosters <-length(rosterList[[k]][["id"]])
          rosterPlayerName <- rep(NA_character_, times=numRosters)
          rosterPlayerID <- rep(NA_real_, times=numRosters)
          
          tournamentRostersTemp <- data.frame(rosterID,
                                              rosterTeamAcro,
                                              rosterTeamID,
                                              rosterPlayerName,
                                              rosterPlayerID,
                                              tournamentID,
                                              stringsAsFactors = FALSE)
          
        }
        
        ## Remove rows with no rosterID
        tournamentRostersTemp <- tournamentRostersTemp[!(is.na(tournamentRostersTemp$rosterID)),]
        
        ## Bind with full roster list
        tournamentRosters <- rbind(tournamentRosters, tournamentRostersTemp)
        
      }
      
    }
    
  }
  
  ## Put final DF in global env
  assign("tournamentRosters", tournamentRosters, envir = .GlobalEnv)
  
}

## Save tournament roster df
save(tournamentRosters, file="data/tournamentRosters.Rda")

## Load tournament roster df
load("data/tournamentRosters.Rda")
