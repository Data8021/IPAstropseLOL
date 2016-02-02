## Function to fetch one or more league tournaments


fetchLeagueTournaments <- function(league = "all") {
  
  suppressMessages(suppressWarnings(library(jsonlite)))
  suppressMessages(suppressWarnings(library(dplyr)))
  
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
  
  ## Initialize list to hold league tournaments
  leagueTournamentList <- vector("list", length(league))
  names(leagueTournamentList) <- league
  
  ## Loop through each league requested and download faw tournament file from API
  for (i in 1:length(league)) {
    leagueTournamentList[[i]] <- fromJSON(paste0("http://api.lolesports.com/api/v2/highlanderTournaments?league=", leagues[league[i]]),
                                          simplifyVector = FALSE,
                                          simplifyDataFrame = FALSE,
                                          simplifyMatrix = FALSE)
    
    ## return i for tracking status
    print(paste0("leagueTournamentList Loop ", i))
  }
  
  ## Put final DF in global env
  assign("leagueTournamentList", leagueTournamentList, envir = .GlobalEnv)
}

## Save raw tournament list files
save(leagueTournamentList, file="data/leagueTournamentList.Rda")

## Load raw tournament list files
load("data/leagueTournamentList.Rda")
  
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
    
    ## Extract rosters 
    rosterList <- as.list(rawTournList[[i]][["highlanderTournaments"]][["rosters"]])
    
    ## Store tournament IDs
    tournamentID <- rawTournList[[i]][["highlanderTournaments"]][["id"]]
    
    ## Loop through each roster
    for (j in 1:length(rosterList)){
      
      ## Determine if team or player roster
      if ("player" %in% names(rosterList[[j]])) {
        
        rosterID <- rosterList[[j]][["id"]]
        rosterPlayerName <- rosterList[[j]][["name"]]
        rosterPlayerID <- rosterList[[j]][["player"]]
        
        numRosters <-length(rosterList[[j]][["id"]])
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
        
        rosterID <- rosterList[[j]][["id"]]
        rosterTeamAcro <- rosterList[[j]][["name"]]
        rosterTeamID <- rosterList[[j]][["team"]]
        
        numRosters <-length(rosterList[[j]][["id"]])
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
      
      # return j for tracking status
      print(paste0("roster Loop", j))
    
    }
    
    ## return i for tracking status
    print(paste0("rawTournList Loop", i))
    
  }
  
  ## Put final DF in global env
  assign("tournamentRosters", tournamentRosters, envir = .GlobalEnv)
  
}

## Save tournament roster df
save(tournamentRosters, file="data/tournamentRosters.Rda")

## Load tournament roster df
load("data/tournamentRosters.Rda")


extractGameList <- function(league = "all", rawTournList = leagueTournamentList){
  
  suppressMessages(suppressWarnings(library(jsonlite)))
  suppressMessages(suppressWarnings(library(dplyr)))
  
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
  tournamentGames <- data.frame(tournamentID = character(),
                                tournamentName = character(),
                                tournamentDescription = character(),
                                tournamentLeague = numeric(),
                                tournamentStartDate = character(),
                                tournamentEndDate = character(),
                                bracketID = character(),
                                matchID = character(),
                                gameName = character(),
                                gameID = character(),
                                gameRealm = character(),
                                gameCode = numeric(),
                                stringsAsFactors = FALSE)
  
  ## Begin looping through each league
  for (i in 1:length(league)) {
    
    ## Begin looping through each tournament
    for (j in 1:length(rawTournList[[league[i]]][["highlanderTournaments"]][["id"]])) {
      
      ## Store tournament information
      tournamentID <- rawTournList[[league[i]]][["highlanderTournaments"]][[j, "id"]]
      tournamentName <- rawTournList[[league[i]]][["highlanderTournaments"]][[j, "title"]]
      tournamentDescription <- rawTournList[[league[i]]][["highlanderTournaments"]][[j, "description"]]
      tournamentLeague <- rawTournList[[league[i]]][["highlanderTournaments"]][[j, "league"]]
      
      ## Test if start and end dates are included
      if ("startDate" %in% names(rawTournList[[i]][["highlanderTournaments"]])) {
        
        tournamentStartDate = rawTournList[[league[i]]][["highlanderTournaments"]][[j, "startDate"]]
        tournamentEndDate = rawTournList[[league[i]]][["highlanderTournaments"]][[j, "endDate"]]
        
      }
      
      ## Extract the tournament brackets
      bracketsList <- as.list(rawTournList[[league[i]]][["highlanderTournaments"]]["brackets"])
      
    }
    
  }
  
}

for (i in 1:length(rawTournList)) {
  
  print(paste0(names(rawTournList)[i], " - ",
         length(rawTournList[[i]][["highlanderTournaments"]][["id"]]), " - ",
         length(rawTournList[[i]][["highlanderTournaments"]][["brackets"]])))
  
}


     
##NOTES  
# http://api.lolesports.com/api/v2/highlanderTournaments?league=2
# 
# THEN
# 
# http://api.lolesports.com/api/v2/tournamentPlayerStats?tournamentId=6090e92b-d565-41c4-8548-06570ab26fb7
# 
# THEN
# 
# http://api.lolesports.com/api/v1/teams?slug=team-solomid&&tournament=6090e92b-d565-41c4-8548-06570ab26fb7
# 
# THEN MAYBE
# 
# http://api.lolesports.com/api/v1/players?slug=faker&tournament=91be3d78-874a-44e0-943f-073d4c9d7bf6
# 
#   
# ## Loop through schedules???
# for (i in 1:length(leagues)) {
#   
#   scheduleList[[i]] <- fromJSON(paste0("http://api.lolesports.com/api/v1/scheduleItems?leagueId=", leagues[i]))
#   
# }
