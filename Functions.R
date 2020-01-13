####Read in Libraries####
library(dplyr)
library(data.table)
library(Hmisc)
library(httr)
library(jsonlite)
library(readxl)  


####Helper Functions####
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


####Pull All Players from NFL Fantasy API####
nfl_players_scrape <- function(season, week, ppr){
  if(ppr == T){
    position_vec <- c("QB","RB-ppr","WR-ppr","TE-ppr","K","DEF")
  }else if(ppr == F){
    position_vec <- c("QB","RB","WR","TE","K","DEF")
  }
  position_vec_order <- c("QB","RB","WR","TE","K","DEF")
  get_player_data_by_pos <- function(pos){
    nfl_players_url_base <- "https://api.fantasy.nfl.com/v1/players/stats?"
    stat_type = "seasonStats"
    format = "json"
    nfl_players_url <- paste0(nfl_players_url_base, "statType=", stat_type, "&season=", season, "&week=", week, "&position=", pos, "&format=", format)
    nfl_players_DF_list <- jsonlite::fromJSON(nfl_players_url)
    nfl_players_DF <- nfl_players_DF_list$players
    nfl_players_DF <- subset(nfl_players_DF, select = -c(stats))
  } 
  pos_list <- list()
  for(i in seq_along(position_vec)){
    pos_list[[i]] <- get_player_data_by_pos(pos = position_vec[i])
  }
  nfl_players_DF <- do.call(rbind, pos_list)
  nfl_players_DF$position <- factor(nfl_players_DF$position, position_vec_order)
  nfl_players_DF <- nfl_players_DF[order(nfl_players_DF$position, -nfl_players_DF$weekProjectedPts), ]
  nfl_players_DF$position <- as.character(nfl_players_DF$position)
  nfl_players_DF$teamAbbr[which(nfl_players_DF$teamAbbr == "")] = rep("FA", length(which(nfl_players_DF$teamAbbr == "")))
  row.names(nfl_players_DF) <- NULL
  nfl_players_DF$season <- rep(season, nrow(nfl_players_DF))
  nfl_players_DF$week <- rep(week, nrow(nfl_players_DF))
  nfl_players_DF$PPR <- ifelse(ppr == T, rep(TRUE, nrow(nfl_players_DF)), rep(FALSE, nrow(nfl_players_DF)))
  return(nfl_players_DF)
}
#nfl_players_scrape(season = 2019, week = 10, ppr = T)


####Functions to Get Available Players for Each User####
get_available_players <- function(user, opposing_user, week, season, ppr = T){
  player_usage <- read_excel_allsheets("PlayerUsage.xlsx")
  user_players <- player_usage[[which(names(player_usage) == paste(user, "Usage", sep = " "))]]
  opposing_user_players <- player_usage[[which(names(player_usage) == paste(opposing_user, "Usage", sep = " "))]]
  full_player_DF <- nfl_players_scrape(season, week, ppr)
  user_available_players <- filter(full_player_DF, name %nin% user_players$Players)
  opposing_user_available_players <- filter(full_player_DF, name %nin% opposing_user_players$Players)
  user_available_players$opponent_used <- user_available_players$name %in% opposing_user_players$Players
  user_available_players_return <- subset(user_available_players, select = c(name, position, teamAbbr, seasonPts, seasonProjectedPts, weekPts, weekProjectedPts, opponent_used))
  return(user_available_players_return)
}
basils_available_players <- get_available_players(user = "Basil", opposing_user = "Geoff", week = 10, season = 2019, ppr = T)




