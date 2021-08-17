## update data sets
# This script scrapes new (or all) game records from goblin spy

## start remote session
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "chrome")
remDr$open()


# get each game summary
game <- get_game_summary_details(compurl)

try(load("game_summary.RData"))
if(exists("game_old")) {
  game_compare <- game %>% anti_join(game_old, by = "Game_ID") 
  game_old <- game # overwrites reference file with most recent set of games
  game <- game_compare # ensure only scrapping dice for new games
  
} else { #if no saved data run through here
  game_old <- game
}
save(game_old, file = "game_summary.RData")


## get dice table for each game
game_combined <- get_d6_rolls(game_summary = game)

# get the team names
teams <- unique(game_combined$Team)

# get block die results
remDr$open()
block_combined <- get_block_rolls(game_summary = game)

game_combined <- game_combined %>% bind_rows(block_combined) %>% mutate(Total_blocks = rowSums(select(.,c(11:15)), na.rm = T)) 

try(load("game_dice.RData"))
if(exists("game_combined_save")){
  game_combined <- game_combined %>% bind_rows(game_combined_save)
  game_combined_save <- game_combined
} else {
  game_combined_save <- game_combined
}

save(game_combined_save, file = "game_dice.RData")
