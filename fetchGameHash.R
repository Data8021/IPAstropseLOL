## Function to fetch gameHash from a list of matches

fetchGameHash <- function(gamesData){
  suppressMessages(suppressWarnings(library(jsonlite)))
  suppressMessages(suppressWarnings(library(dplyr)))
  
  ## Extract list of matches
  matchList <- filter(gamesData, gameRealm != "NA") %>%
    select(tournamentID, matchID) %>%
    distinct()
  
  ## Initialize gameID/gameHash df
  gameIDMap <- data.frame(matrix(ncol=2, nrow=0))
  
  ## Loop through matchList
  for (i in 1:nrow(matchList)){
    ## Initialize error flag
    flag <- TRUE
    
    ## Fetch match information
    matchDataJSON <- tryCatch({
      fromJSON(paste0("http://api.lolesports.com/api/v2/highlanderMatchDetails?tournamentId=", matchList[i, "tournamentID"], "&matchId=", matchList[i, "matchID"]))
    }, error=function(e){
      message(paste0("Error on row ",i,": ",e))
      flag <<- FALSE
    })
    if (!flag) next
    
    ## Extract gameID/gameHash df
    gameIDMapTemp <- matchDataJSON[[4]]
    gameIDMapTemp <- rename(gameIDMapTemp, gameID = id)
    
    ## Bind temp df onto full df
    gameIDMap <- rbind(gameIDMap, gameIDMapTemp)
  }
  
  ## Merge gameHash into gamesDF
  suppressMessages(leagueGames <- left_join(gamesData, gameIDMap))

  ## Put final DF in specified env
  return(leagueGames)
  
}


