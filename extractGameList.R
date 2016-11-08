## Function to extract high level game information from
## a complete list of league tournament files

extractGameList <- function(league = "all", rawTournList){
  
  ## set up vector of leagues
  leagues = c(1,2,3,4,5,6,7,8,9,10,12,13,14,18,20,23,24,26,28,29)
  leagueNames <- c("all-star",
                   "na-lcs",
                   "eu-lcs",
                   "na-cs",
                   "eu-cs",
                   "lck",
                   "lpl",
                   "lms",
                   "world-championship",
                   "msi",
                   "iwc",
                   "opl",
                   "cblol",
                   #"copa-south" formerly #17 missing now,
                   "copa-north",
                   "ulol",
                   "cdl",
                   "cls",
                   "fr",
                   "sdp",
                   "tpc")
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
                "msi",
                "iwc",
                "opl",
                "cblol",
                #"copa-south"??,
                "copa-north",
                "ulol",
                "cdl",
                "cls",
                "fr",
                "sdp",
                "tpc")
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
                            stringsAsFactors = FALSE)
  
  ## Loop through each league
  for (i in 1:length(league)) {

    
    ## Loop through each tournament
    for (j in 1:length(rawTournList[[league[i]]][["highlanderTournaments"]])) {
      
      ## Store Roster information
      blueRosterResults <- data.frame(blueRosterID = as.character(),
                                      blueTeamAcro = as.character(),
                                      blueTeamNum = as.character(),
                                      stringsAsFactors = FALSE)
      
      for (p in 1:length(rawTournList[[i]][["highlanderTournaments"]][[j]][["rosters"]])) {
        if("team" %in% names(rawTournList[[i]][["highlanderTournaments"]][[j]][["rosters"]][[p]])) {
          blueRosterResults[p,1] <- rawTournList[[i]][["highlanderTournaments"]][[j]][["rosters"]][[p]][["id"]]
          blueRosterResults[p,2] <- rawTournList[[i]][["highlanderTournaments"]][[j]][["rosters"]][[p]][["name"]]
          blueRosterResults[p,3] <- rawTournList[[i]][["highlanderTournaments"]][[j]][["rosters"]][[p]][["team"]]
        }
      }
      
      ## Dupliucate it for red team
      redRosterResults <- blueRosterResults
      redRosterResults <- rename(redRosterResults,
                                 redRosterID = blueRosterID,
                                 redTeamAcro = blueTeamAcro,
                                 redTeamNum = blueTeamNum)
      
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
            
            ## Test if rosters exist
            if("input" %in% names(rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["matches"]][[l]][["games"]][[m]]) &
               "roster" %in% names(rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["matches"]][[l]][["games"]][[m]][["input"]][[1]]) &
               "roster" %in% names(rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["matches"]][[l]][["games"]][[m]][["input"]][[2]])) {
              
              ## Store teams
              blueRosterID <- rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["matches"]][[l]][["games"]][[m]][["input"]][[1]][["roster"]]
              redRosterID <- rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["matches"]][[l]][["games"]][[m]][["input"]][[2]][["roster"]]
            
            } else {
              
              ## Store no teams
              blueRosterID <- NA_character_
              redRosterID <- NA_character_
            }  
            
            ## Test if game actually played
            if("standings" %in% names(rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["matches"]][[l]][["games"]][[m]])) {
              
              ## Store winner
              blueWinner <- blueRosterID == rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["matches"]][[l]][["games"]][[m]][["standings"]][[1]][[1]][[1]][["roster"]]  
              redWinner <- redRosterID ==rawTournList[[league[i]]][["highlanderTournaments"]][[j]][["brackets"]][[k]][["matches"]][[l]][["games"]][[m]][["standings"]][[1]][[1]][[1]][["roster"]]
              
            } else {
              
              ## Store no winners
              blueWinner <- FALSE
              redWinner <- FALSE
            
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
                                          blueRosterID,
                                          redRosterID,
                                          blueWinner,
                                          redWinner,
                                          stringsAsFactors = FALSE)
            
            ## Join with Blue/Red Rosters
            leagueGamesTemp <- left_join(leagueGamesTemp, blueRosterResults, by = "blueRosterID")
            leagueGamesTemp <- left_join(leagueGamesTemp, redRosterResults, by = "redRosterID")
            
            ## Bind on to primary df
            leagueGames <- rbind(leagueGames, leagueGamesTemp)
            
          }
            
        }
        
      }
          
    }
    
  }
  
  ## return final df
  return(leagueGames)
  
}


