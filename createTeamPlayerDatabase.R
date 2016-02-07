## Function to fetch player stats based on a list of league tournaments

createTeamPlayerDatabase <- function(tournamentList = tournamentList, environment = .GlobalEnv) {
    
    suppressMessages(suppressWarnings(library(jsonlite)))
    suppressMessages(suppressWarnings(library(dplyr)))

    ## Get environment
    funcEnv <- environment()
    
    ## Get list of every team at each tournament
    fetchPlayerStats(tournamentList = tournamentList, environment = funcEnv)
    teamTournDF <- select(playerStatsTourn, teamSlug, teamAcro, tournamentID) %>%
        distinct()
    
    ## Initialize dfs
    playersDatabase <- data.frame(matrix(ncol = 12, nrow = 0))
    teamsDatabase <- data.frame(matrix(ncol = 7, nrow = 0))
    teamPlayersDatabase <- data.frame(matrix(ncol = 3, nrow = 0))
    
    ## Loop through each team in each tournament
    for (i in 1:nrow(teamTournDF)) {
        
        print(paste0("Team/Tourn Loop = ", i))  
      
        teamTournTempJSON <- fromJSON(paste0("http://api.lolesports.com/api/v1/teams?slug=", teamTournDF[i, "teamSlug"], "&tournament=", teamTournDF[i, "tournamentID"]))
        
        ## Extract players
        playersTemp <- teamTournTempJSON[[1]]
        
        ## Rename and downselect variables
        playersTemp <- select(playersTemp,
                              playerID = id,
                              playerSlug = slug,
                              playerName = name,
                              playerFristName = firstName,
                              playerLastName = lastName,
                              playerPosition = roleSlug,
                              playerPhotoURL = photoUrl,
                              playerHometown = hometown,
                              playerRegion = region)
        

        ## Add tournamentID and team information
        playersTemp[,10] <- teamTournDF[i, "tournamentID"]
        playersTemp[,11] <- teamTournDF[i, "teamSlug"]
        playersTemp[,12] <- teamTournDF[i, "teamAcro"]
        
        ## Rename new variables
        playersTemp <- rename(playersTemp,
                              tournamentID = V10,
                              teamSlug = V11,
                              teamAcro = V12)
        
        ## Bind players temp df onto full df
        playersDatabase <- rbind(playersDatabase, playersTemp)
        
        ## Extract teams
        teamsTemp <- teamTournTempJSON[[7]]
        
        ## Rename and downselect variables
        teamsTemp <- select(teamsTemp,
                            teamID = id,
                            teamSlug = slug,
                            teamName = name,
                            teamLogoUrl = logoUrl,
                            teamAcro = acronym,
                            teamHomeLeague = homeLeague,
                            teamPlayers = players)
        
        ## Extract league number
        teamsTemp$teamHomeLeague <- sapply(teamsTemp$teamHomeLeague,
                                           function(x) {
                                             x <- gsub("urn:rg:lolesports:global:league:league:","",x)
                                             })
        
        ## Add tournamentID and rename
        teamsTemp[,8] <- teamTournDF[i, "tournamentID"]
        teamsTemp <- rename(teamsTemp, tournamentID = V8)

        ## Loop through teams extracting players
        for (j in 1:nrow(teamsTemp)){
            
            print(paste0("Team/Player Loop = ", j))
          
            ## Initialize teamPlayersTemp
            teamPlayersTemp <- data.frame(matrix(ncol = 2, nrow = 0))
            
            ## Populate df with tournament, teamID and all players
            teamPlayersTemp[1:length(teamsTemp[j, "teamPlayers"][[1]]), 1] <- teamTournDF[i, "tournamentID"]
            teamPlayersTemp[1:length(teamsTemp[j, "teamPlayers"][[1]]), 2] <- teamsTemp[j, "teamID"]
            teamPlayersTemp <- cbind(teamPlayersTemp, teamsTemp[j, "teamPlayers"][[1]])
            
            ## Rename variables
            colnames(teamPlayersTemp) <- c("tournamentID",
                                           "teamID",
                                           "playerID")
            
            ## Bind teamPlayers temp df onto full df
            teamPlayersDatabase <- rbind(teamPlayersDatabase, teamPlayersTemp)
        }

        ## Remove teamPlayers variable
        teamsTemp <- select(teamsTemp, -teamPlayers)
        
        ## Bind teams temp df onto full df
        teamsDatabase <- rbind(teamsDatabase, teamsTemp)
                                                  
    }
    
    ## Remove duplicates
    playersDatabase <- distinct(playersDatabase)
    teamsDatabase <- distinct(teamsDatabase)
    teamPlayersDatabase <- distinct(teamPlayersDatabase)
    
    ## Put processed work into global env
    assign("playersDatabase", playersDatabase, envir = environment)
    assign("teamsDatabase", teamsDatabase, envir = environment)  
    assign("teamPlayersDatabase", teamPlayersDatabase, envir = environment)
                        
}
