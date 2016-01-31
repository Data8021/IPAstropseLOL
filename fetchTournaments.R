## Function to fetch one or more league tournaments

fetchLeagueTournaments <- function(league = "all") {
  
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
  
  
  leagues <- leagues[league]
  return(leagues)
}

  
  
     
  






leagues <- 1:50


## Initialize list to hold schedules
scheduleList <- vector("list", length(leagues))
names(scheduleList) <- leagues

for (i in 1:length(leagues)) {
  scheduleList[[i]] <- fromJSON(paste0("http://api.lolesports.com/api/v2/highlanderTournaments?league=", leagues[i]))
}

for (j in 1:length(scheduleList)) {
  
  if (!is.null(names(scheduleList[[j]][[1]]))) {
    print(paste(names(scheduleList[j]), scheduleList[[j]][[2]][[2]], sep=" - "))
  }
  
}


leagues <- 2

## Initialize list to hold schedules
scheduleList2 <- vector("list", length(leagues))
names(scheduleList2) <- leagues

for (i in 1:length(leagues)) {
  scheduleList2[[i]] <- fromJSON(paste0("http://api.lolesports.com/api/v2/highlanderTournaments?league=", leagues[i]))
}

## Identify leagues to scrape schedules of
leagues <- c(1,2,3,4,5,6,7,8,9,12,14,17,18)

## Initialize list to hold schedules
scheduleList <- vector("list", length(leagues))
names(scheduleList) <- leagues

## Loop through schedules
for (i in 1:length(leagues)) {
  
  scheduleList[[i]] <- fromJSON(paste0("http://api.lolesports.com/api/v1/scheduleItems?leagueId=", leagues[i]))
  
}

for (j in 1:length(scheduleList)) {
  
  
}
