## Function to fetch player stats based on a list of league tournaments

fetchTeamStats <- function(tournamentList = tournamentList, environment = .GlobalEnv) {
    
    suppressMessages(suppressWarnings(library(jsonlite)))
    suppressMessages(suppressWarnings(library(dplyr)))

    ## Get environment
    funcEnv <- environment()
    
    ## Get list of every team at each tournament
    fetchPlayerStats(tournamentList = tournamentList, environment = funcEnv)
    teamTournDF <- select(playerStatsTourn, teamSlug, teamAcro, tournamentID) %>%
        distinct()
    
    ## Initialize df for team stats
    teamStatsTourn <- data.frame(matrix(ncol = 15, nrow = 0))
        
    ## Loop through each team in each tournament
    for (i in 1:nrow(teamTournDF)) {
        
        teamTournTempJSON <- fromJSON(paste0("http://api.lolesports.com/api/v1/teams?slug=", teamTournDF[i, "teamSlug"], "&tournament=", teamTournDF[i, "tournamentID"]))
        
        ## Initialize temp df for team stats
        teamStatsTournTemp <- data.frame
        
    }
    
    http://api.lolesports.com/api/v1/teams?slug=team-solomid&&tournament=6090e92b-d565-41c4-8548-06570ab26fb7
                        
    http://api.lolesports.com/api/v1/players?slug=faker&tournament=91be3d78-874a-44e0-943f-073d4c9d7bf6
                        
                        
}
