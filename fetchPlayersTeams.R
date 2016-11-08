## Function to fetch player and team details based on a list of league tournaments

fetchPlayersTeams <- function(rawTournList){
  
  suppressMessages(suppressWarnings(library(jsonlite)))
  suppressMessages(suppressWarnings(library(dplyr)))
  
  ## Initialize dataframes
  players <- data.frame(matrix(ncol=9, nrow=0))
  teams <- data.frame(matrix(ncol=5, nrow=0))
  
  ## Loop through each league
  for (i in 1:length(rawTournList)) {
    
    ## Loop through each tournament
    for (j in 1:length(rawTournList[[i]][["highlanderTournaments"]])) {
      
      tournamentID <- rawTournList[[i]][["highlanderTournaments"]][[j]][["id"]]
      
      ## Loop through each bracket
      for (k in 1:length(rawTournList[[i]][["highlanderTournaments"]][[j]][["brackets"]])) {
        
        ## Loop through each match
        for (l in 1:length(rawTournList[[i]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["matches"]])) {
          
          ## Store match information
          matchID <- rawTournList[[i]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["matches"]][[l]][["id"]]
          
          ## Initialize error flag
          flag <- TRUE
          
          ## Fetch match information
          matchDataJSON <- tryCatch({
            fromJSON(paste0("http://api.lolesports.com/api/v2/highlanderMatchDetails?tournamentId=", tournamentID, "&matchId=", matchID))
          }, error=function(e){
            message(paste0("Error on row ",i,": ",e))
            flag <<- FALSE
          })
          if (!flag) next
          
         
          if(length(matchDataJSON[["teams"]]) > 0) {
            
            ## Store team information
            teamsTemp <- matchDataJSON[[1]][,c("id","slug","name","acronym","homeLeague")]
            teamsTemp$homeLeague <- gsub("urn:rg:lolesports:global:league:league:", "", teamsTemp$homeLeague)
            teamsTemp$tournamentID <- tournamentID
            teamsTemp$matchID <- matchID
            
            ## Bind temp DFs on final DFs
            teams <- rbind(teams, teamsTemp)
          
          }
          
          
          if(length(matchDataJSON[["players"]]) > 0) {
            
            
            if("liveGameTeam" %in% names(matchDataJSON[["players"]])) {
              
              ## Store player information
              playersTemp <- matchDataJSON[["players"]][,c("id","slug","name","firstName","lastName", "roleSlug", "liveGameTeam")]
              playersTemp$tournamentID <- tournamentID
              playersTemp$matchID <- matchID

            } else {
              
              ## Store player information
              playersTemp <- matchDataJSON[["players"]][,c("id","slug","name","firstName","lastName", "roleSlug")]
              playersTemp$liveGameTeam <- NA_integer_
              playersTemp$tournamentID <- tournamentID
              playersTemp$matchID <- matchID
              
            }

            ## Bind temp df on final df
            players <- rbind(players, playersTemp)
            
          }

          print(paste0("i = ",i,", j = ",j,", k = ",k,", l = ", l))
          
        }

      }
      
    }
  
  }
  
  ## Rename variables
  teams <- rename(teams,
                  teamID = id,
                  teamSlug = slug,
                  teamName = name,
                  teamAcro = acronym,
                  teamLeague = homeLeague)
  
  players <- rename(players,
                    playerID = id,
                    playerSlug = slug,
                    playerName = name,
                    playerFirstName = firstName,
                    playerLastName = lastName,
                    playerRole = roleSlug,
                    teamID = liveGameTeam)
  
  ## Combine into one list
  playersTeamsList <- list(teams = teams, players = players)
  
  ## Return list
  return(playersTeamsList)
  
}