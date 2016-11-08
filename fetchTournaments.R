## Function to fetch one or more league's tournaments

fetchLeagueTournaments <- function(league = "all") {
  suppressMessages(suppressWarnings(library(jsonlite)))
  suppressMessages(suppressWarnings(library(dplyr)))
  
  ## Get environment
  funcEnv <- environment()
  
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
  
  ## Initialize list to hold league tournaments
  leagueTournamentList <- vector("list", length(league))
  names(leagueTournamentList) <- league
  
  ## Loop through each league requested and download raw tournament file from API
  for (i in 1:length(league)) {
    leagueTournamentList[[i]] <- fromJSON(paste0("http://api.lolesports.com/api/v2/highlanderTournaments?league=", leagues[league[i]]),
                                          simplifyVector = FALSE,
                                          simplifyDataFrame = FALSE,
                                          simplifyMatrix = FALSE)
  }
  
  return(leagueTournamentList)
}