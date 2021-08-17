######### Functions #########

# could you better commenting 


#generic functions
wetest <- function(sleepmin,sleepmax){
  remDr <- get("remDr",envir=globalenv())
  webElemtest <-NULL
  while(is.null(webElemtest)){
    webElemtest <- tryCatch({remDr$findElement(using = 'class', "GSpyTable")},
                            error = function(e){NULL})
    #loop until element with name <value> is found in <webpage url>
  }
  randsleep <- sample(seq(sleepmin, sleepmax, by = 0.01), 1)
  Sys.sleep(randsleep)
}
wetestblock <- function(sleepmin,sleepmax){
  remDr <- get("remDr",envir=globalenv())
  webElemtest <-NULL
  while(is.null(webElemtest)){
    webElemtest <- tryCatch({remDr$findElement(using = 'class', "replay-log-row")},
                            error = function(e){NULL})
    #loop until element with name <value> is found in <webpage url>
  }
  randsleep <- sample(seq(sleepmin, sleepmax, by = 0.01), 1)
  Sys.sleep(randsleep)
}
f4 <- function(x, blank = is.na) {
  # Find the values
  if (is.function(blank)) {
    isnotblank <- !blank(x)
  } else {
    isnotblank <- x != blank
  }
  # Fill down
  x[which(isnotblank)][cumsum(isnotblank)]
}

#Blood bowl 2 based functions
get_game_summary_details <- function(url){
  ## get game urls 
  remDr$navigate(url)
  
  wetest(sleepmin=.5,sleepmax=1)
  doc <- htmlParse(remDr$getPageSource()[[1]])
  
  games <- as.data.frame(getHTMLLinks(doc))
  colnames(games) <- "urls"
  games <- games %>% filter(grepl("match", urls))
  
  game_players <- as.data.frame(readHTMLTable(doc)) %>% select(3,5,7,10,15,18)
  games <- cbind(games, game_players)
  colnames(games) <- c("urls", "round", "date", "player_one", "team_one", "team_two", "player_two")
  
  #drop AI games
  games <- games %>% filter(player_one != "AI" & player_two != "AI") %>% mutate(Game_ID = paste0("game",max(row_number())-row_number()+1))
  
  return(games)
  
}
get_d6_rolls <- function(game_summary){
  
  y <- max(parse_number(game$Game_ID)) 
  for(i in game$urls){
    
    x <- paste0("https://www.mordrek.com",i,"/dice")  
    
    remDr$navigate(x)
    
    wetest(sleepmin=.5,sleepmax=1)
    
    doc <- htmlParse(remDr$getPageSource()[[1]])
    test <- readHTMLTable(doc)
    assign(paste0("game",y), as.data.frame(test[1]))
    y <- y - 1
  }
  
  remDr$close()
  
  
  ## combine into one dataset
  df_names <- ls(pattern = "game\\d+")
  
  game_combined <- do.call(rbind, mget(df_names))
  game_combined$id <- sub("\\.\\d+$", "", rownames(game_combined))
  row.names(game_combined) <- NULL
  
  rm(df_names)
  
  colnames(game_combined) <- c("Team","Action","Rolls","Expected","Successes","Failures","Percentage_success","Luck","Risk","Game_ID")
  return(game_combined)
  
}
get_block_locations <- function(htmltxt){
  ## This finds all the block image pngs
  blocklocations <- gregexpr("Select", htmltxt)[[1]]
  blocklocations <- blocklocations + 40  
  
  pngnum <- 5 # not 0 as it is in a png string
  for (i in 1:length(blocklocations)){
    
    new <- substr(htmltxt, blocklocations[i],blocklocations[i])
    pngnum <- c(pngnum, new)
  }
  
  blocklocations <- as.data.frame(blocklocations) 
  pngnum <- as.data.frame(pngnum) %>% filter(pngnum!=5) 
  
  blocks <- cbind(blocklocations,pngnum)
  colnames(blocks) <- c("location","pngnum")
  return(blocks)
}
get_turn_locations <- function(htmltxt){
  teams_in_game <- sapply(teams, grepl, htmltxt)
  teams_in_game <- teams_in_game[teams_in_game == TRUE]
  teams_in_game <- names(teams_in_game)
  
  # to find turn location
  team_1 <- as.data.frame(gregexpr(teams_in_game[1], htmltxt)[[1]])
  team_2 <- as.data.frame(gregexpr(teams_in_game[2], htmltxt)[[1]])
  
  colnames(team_1) <- "location"
  colnames(team_2) <- "location"
  
  team_1$team_name <- teams_in_game[1] 
  team_2$team_name <- teams_in_game[2]
  
  turns <- rbind(team_1, team_2)
  
  turns <- turns %>% arrange(location)
  return(turns)
}
get_game_blocks <- function(blocks, turns){
  game_state <- bind_rows(blocks,turns) %>% 
    arrange(location)
  
  game_state$team_name <- f4(game_state$team_name)
  
  game_state <- game_state %>% 
    mutate(pngnum = case_when(pngnum == 0 ~ "Attacker Down",
                              pngnum == 1 ~ "Both Down",
                              pngnum == 2 ~ "Push",
                              pngnum == 3 ~ "Defender Stumbles",
                              pngnum == 4 ~ "Defender Down"
    ))
  
  output <- game_state %>% filter(!is.na(pngnum)) %>% group_by(team_name, pngnum) %>% summarise(count = n()) %>% dcast(team_name ~ pngnum, value.var = "count") %>% mutate(Action = "Block")
  return(output)
}
get_block_rolls <- function(game_summary){
  
  y <- max(parse_number(game$Game_ID)) 
  for (i in game$urls) { 
    x <- paste0("https://www.mordrek.com",i,"/replay")  
    
    remDr$navigate(x)
    wetestblock(sleepmin=.5,sleepmax=1)
    
    doc <- htmlParse(remDr$getPageSource()[[1]])
    htmltxt <- paste(capture.output(doc, file=NULL), collapse="\n")
    
    block_game <- get_block_locations(htmltxt)
    turn_game <- get_turn_locations(htmltxt)
    assign(paste0("game",y),get_game_blocks(block_game, turn_game))
    
    y <- y - 1
    
  }
  
  df_names <- ls(pattern = "game\\d+")
  
  game_combined <- do.call(rbind, mget(df_names))
  game_combined$id <- sub("\\.\\d+$", "", rownames(game_combined))
  row.names(game_combined) <- NULL
  
  rm(df_names)
  
  colnames(game_combined) <- c("Team","Attacker Down","Both Down","Defender Down","Defender Stumbles","Push","Action","Game_ID")
  return(game_combined)
  
}