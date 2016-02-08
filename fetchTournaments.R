## Function to fetch one or more league's tournaments


fetchLeagueTournaments <- function(league = "all") {
  suppressMessages(suppressWarnings(library(jsonlite)))
  suppressMessages(suppressWarnings(library(dplyr)))
  
  ## Get environment
  funcEnv <- environment()
  
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
  }
  
  ## Extract game data from tournament files
  extractGameList(league = league, rawTournList = leagueTournamentList, environment = funcEnv)
  
  ## Extract roster data from tournament files
  extractRosters(rawTournList = leagueTournamentList, environment = funcEnv)
  
  ## Merge in Roster information
  mergeRoster(rosterDF = tournamentRosters, gamesDF = leagueGames, environment = funcEnv)

  ## Extract list of unique tournamentIDs
  tournamentList <- select(leagueGames, tournamentID) %>%
    distinct()
  
  ## Fetch player tournament stats
  fetchPlayerStats(tournamentList = tournamentList, environment = funcEnv)
  
  ## Create player and team databases
  createTeamPlayerDatabase(tournamentList = tournamentList, environment = funcEnv)
  
  ## Update leageGames with blue team names
  teamNames <- select(teamsDatabase,
                      blueTeamID = teamID,
                      blueTeamName = teamName) %>%
    distinct()
  teamNames$blueTeamID <- as.character(teamNames$blueTeamID)
  leagueGames <- left_join(leagueGames, teamNames)
  
  ## Update leagueGames with red team names
  teamNames <- rename(teamNames,
                      redTeamID = blueTeamID,
                      redTeamName = blueTeamName)
  leagueGames <- left_join(leagueGames, teamNames)
  
  ## Fetch gamehash and merge with leagueGames
  fetchGameHash(gamesDF = leagueGames, environment = funcEnv)
  
  
  ## Put processed work into global env
  assign("leagueTournamentList", leagueTournamentList, envir = .GlobalEnv)
  ## assign("tournamentRosters", tournamentRosters, envir = .GlobalEnv)  
  assign("leagueGames", leagueGames, envir = .GlobalEnv)  
  assign("playerStatsTourn", playerStatsTourn, envir = .GlobalEnv)
  assign("playersDatabase", playersDatabase, envir = .GlobalEnv)
  assign("teamsDatabase", teamsDatabase, envir = .GlobalEnv)  
  assign("teamPlayersDatabase", teamPlayersDatabase, envir = .GlobalEnv)
  
}

## Save processed data
save(leagueTournamentList, file="data/leagueTournamentList.Rda")
save(leagueGames, file="data/leagueGames.Rda")
save(tournamentRosters, file="data/tournamentRosters.Rda")
save(playerStatsTourn, file="data/playerStatsTourn.Rda")
save(playersDatabase, file="data/playersDatabase.Rda")
save(teamsDatabase, file="data/teamsDatabase.Rda")
save(teamPlayersDatabase, file="data/teamPlayersDatabase.Rda")

## Load processed data
load("data/leagueTournamentList.Rda")
load("data/leagueGames.Rda")
load("data/tournamentRosters.Rda")
load("data/playerStatsTourn.Rda")
load("data/playersDatabase.Rda")
load("data/teamsDatabase.Rda")
load("data/teamPlayersDatabase.Rda")
