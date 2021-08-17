## main script to run to create or update the dataset, with very limited analysis at the bottom

## Set-up ## 
# first time using - run this line to set up docker
#shell('docker pull selenium/standalone-chrome')

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
shell('docker run -d -p 4445:4444 selenium/standalone-chrome')

# update for whatever league you want needs to be results page e.g: https://www.mordrek.com/gspy/comp/#####/results
compurl <- "https://www.mordrek.com/gspy/comp/28067/results" 

source("required_packages.R")
source("required_functions.R")
source("update_data.R")


rm(list=ls())


## start of analysis
load("game_dice.RData")

game_combined_save %>% #filter(Game_ID == "game7") %>% 
  group_by(Team) %>%
  summarise(rolls = sum(as.numeric(Rolls), na.rm = T), expect = sum(as.numeric(Expected), na.rm = T), success = sum(as.numeric(Successes), na.rm = T), 
            dd = sum(`Defender Down`, na.rm = T), totb = sum(Total_blocks, na.rm = T)) %>% 
  mutate(luck = (success/expect) * 100, block_success = dd/totb *100) %>% arrange(luck)

