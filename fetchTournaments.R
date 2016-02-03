## Function to fetch one or more league's tournaments


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
  }
  
  ## Put final DF in global env
  assign("leagueTournamentList", leagueTournamentList, envir = .GlobalEnv)
  
  ## Extract game data from tournament files
  extractGameList(league = league, rawTournList = leagueTournamentList)
  
}

## Save raw tournament list files and the games DF
save(leagueTournamentList, file="data/leagueTournamentList.Rda")
save(leagueGames, file="data/leagueGames.Rda")

## Load raw tournament list files and the games DF
load("data/leagueTournamentList.Rda")
load("data/leagueGames.Rda")
