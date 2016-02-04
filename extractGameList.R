## Function to extract high level game information from
## a complete list of league tournament files

extractGameList <- function(league = "all", rawTournList = leagueTournamentList, environment = .GlobalEnv){
  
  ## set up vector of leagues
  leagues = c(1,2,3,4,5,6,7,8,9,12,14,17,18)
  leagueNames <- c("all-star",
                   "na-lcs",
                   "eu-lcs",
                   "na-cs",
                   "eu-cs",
                   "lck",
                   "lpl",
                   "lms",
                   "world-championship",
                   "iwc",
                   "cblol",
                   "copa-south",
                   "copa-north")
  names(leagues) <- leagueNames
  
  ## Populate league if "all"
  if (any(league == "all")) {
    league <- c("all-star",
                "na-lcs",
                "eu-lcs",
                "na-cs",
                "eu-cs",
                "lck",
                "lpl",
                "lms",
                "world-championship",
                "iwc",
                "cblol",
                "copa-south",
                "copa-north")
  }
  
  ## Test if league exists
  if (any(!(league %in% leagueNames))) {
    stop("League does not exist.")
  }
  
  ## Initialize df to hold games
  leagueGames <- data.frame(tournamentID = character(),
                            tournamentName = character(),
                            tournamentDescription = character(),
                            tournamentLeague = character(),
                            tournamentStartDate = character(),
                            tournamentEndDate = character(),
                            bracketID = character(),
                            bracketName = character(),
                            matchID = character(),
                            matchName = character(),
                            matchPosition = numeric(),
                            gameID = character(),
                            gameName = character(),
                            gameRealm = character(),
                            gameCode = character(),
                            rosterBlue = character(),
                            rosterRed = character(),
                            teamBlueWin = character(),
                            teamRedWin = character(),
                            stringsAsFactors = FALSE)
  
  ## Loop through each league
  for (i in 1:length(league)) {

    
    ## Loop through each tournament
    for (j in 1:length(rawTournList[[league[i]]][["highlanderTournaments"]])) {
      
      ## Store tournament information
      tournamentID <- rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["id"]]
      tournamentName <- rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["title"]]
      tournamentDescription <- rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["description"]]
      tournamentLeague <- rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["league"]]
      
      ## Test if start and end dates are included
      if ("startDate" %in% names(rawTournList[[i]][["highlanderTournaments"]][[j]])) {
        
        tournamentStartDate = rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["startDate"]]
        tournamentEndDate = rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["endDate"]]
        
      } else {
        
        tournamentStartDate = NA_character_
        tournamentEndDate = NA_character_
      }
      
      ## Loop through each bracket
      for (k in 1:length(rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]])) {
        
        ## Store bracket information
        bracketID <- rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["id"]]
        
        ## Test if bracket name is present
        if ("name" %in% names(rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]][[k]])) {
          
          ## Assign bracket name
          bracketName <- rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["name"]]
          
        } else {
          
          ## Assign no bracket name
          bracketName <- NA_character_
          
        }
        
        
        ## Loop through each match
        for (l in 1:length(rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["matches"]])) {
          
          ## Store match information
          matchID <- rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["matches"]][[l]][["id"]]
          matchName <- rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["matches"]][[l]][["name"]]
          matchPosition <- rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["matches"]][[l]][["position"]]
          
          ## Loop through each game
          for (m in 1:length(rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["matches"]][[l]][["games"]])) {
            
            ## Store game information
            gameID <- rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["matches"]][[l]][["games"]][[m]][["id"]] 
            gameName <- rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["matches"]][[l]][["games"]][[m]][["generatedName"]] 
            
            ## Test if input present
            if ("input" %in% names(rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["matches"]][[l]][["games"]][[m]])) {
              
              if ("roster" %in%names(rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["matches"]][[l]][["games"]][[m]][["input"]][[1]])) {
              
                ## Assign rosters
                rosterBlue <- rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["matches"]][[l]][["games"]][[m]][["input"]][[1]][["roster"]]
                rosterRed <- rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["matches"]][[l]][["games"]][[m]][["input"]][[2]][["roster"]]

              } else {
              
                ## Assign no rosters
                rosterBlue <- NA_character_
                rosterRed <- NA_character_
                
              }
              
            } else {
              
              ## Assign no rosters
              rosterBlue <- NA_character_
              rosterRed <- NA_character_

            }
                  
            ## Test if winner identified
            if ("standings" %in% names(rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["matches"]][[l]][["games"]][[m]])) {
              
              ## Determine winner
              if (rosterBlue == rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["matches"]][[l]][["games"]][[m]][["standings"]][["result"]][[1]][[1]][["roster"]]) {
                
                ## Assign winner
                teamBlueWin <- "YES"
                teamRedWin <- "NO"
                
              } else {
                
                ## Assign winner
                teamBlueWin <- "NO"
                teamRedWin <- "YES"
              }
              
            } else {
              
              ## Assign no winner
              teamBlueWin <- NA_character_
              teamRedWin <- NA_character_
              
            }
            
            ## Test for gameRealm and gameId
            if ("gameRealm" %in% names(rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["matches"]][[l]][["games"]][[m]])) {
              
              ## Store gameRealm/gameCode
              gameRealm <- rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["matches"]][[l]][["games"]][[m]][["gameRealm"]]
              gameCode <- rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["matches"]][[l]][["games"]][[m]][["gameId"]]
              
            } else {
              
              ## Store no gameRealm/gameCode
              gameRealm <- NA_character_
              gameCode <- NA_character_
              
            }
          
            ## Create tempDF
            leagueGamesTemp <- data.frame(tournamentID,
                                          tournamentName,
                                          tournamentDescription,
                                          tournamentLeague,
                                          tournamentStartDate,
                                          tournamentEndDate,
                                          bracketID,
                                          bracketName,
                                          matchID,
                                          matchName,
                                          matchPosition,
                                          gameID,
                                          gameName,
                                          gameRealm,
                                          gameCode,
                                          rosterBlue,
                                          rosterRed,
                                          teamBlueWin,
                                          teamRedWin,
                                          stringsAsFactors = FALSE)
            
            ## Bind on to primary df
            leagueGames <- rbind(leagueGames, leagueGamesTemp)  
            
          }
            
        }
        
      }
          
    }
    
  }
  
  ## Put final DF in specified env
  assign("leagueGames", leagueGames, envir = environment)
  
}


