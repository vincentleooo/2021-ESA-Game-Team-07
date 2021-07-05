
###########################################################################
###########################################################################
###                                                                     ###
###                    FUNCTIONS FOR THE APPLICATION                    ###
###                                                                     ###
###########################################################################
###########################################################################


#' Registers a player.
#' 
#' @param user_id New user ID of the player.
#' @param password New password of the player.
#' 
#' @return User ID if successful, NULL if not.
#' 
#' @author Alex.
#' @references Modified version taken from ESA lectures.
#' 
registerPlayer <- function(user_id, password) {
  conn <- getAWSConnection()
  # print("Connection Established")
  query <- createNewPlayerQuery(conn, user_id, password)
  # print("Created Query")
  success <- FALSE
  tryCatch(
    {
      result <- dbExecute(conn, query)
      # print("Executed Query")
      success <- TRUE
    }, 
    error = function(cond) {
      # print("registerPlayer: ERROR")
      # print(cond)
    }, 
    warning = function(cond) {
      # print("registerPlayer: WARNING")
      # print(cond)
    },
    finally = {
      # print("Creating New Player Query Done")
    }
  )
  
  if (!success) {
    user_id <- NULL
  }
  
  dbDisconnect(conn)
  return(user_id)
}


#' Gets player ID
#' 
#' @param username User name of the player.
#' @param password Password of the player.
#' 
#' @return User ID if successful, if not it is 0.
#' 
#' @author Alex.
#' @references Modified from ESA lectures.
#' 
getPlayerID <- function(username, password) {
  conn <- getAWSConnection()

  querytemplate <- 
    "SELECT * FROM User WHERE username=?id1;"
  query <- sqlInterpolate(conn, querytemplate, id1 = username)
  # print(query)
  result <- dbGetQuery(conn,query)
  
  # Use bcrypt to check salted and hashed password
  if (nrow(result) == 1 & checkpw(password, result$password[1])) {
    
    user_id <- result$user_id[1]
    
  } else {
    
    user_id <- 0
    
  }

  dbDisconnect(conn)
  return(user_id)
}


#' Changes password in the database.
#' 
#' @param playerid User ID of the player.
#' @param oldpassword Current password.
#' @param newpassword New password.
#' 
#' @return NULL.
#' 
#' @author Vincent.
#' @references Taken from one of Vincent's assignment answers.
#' 
changePassword <- function(playerid, oldpassword, newpassword) {
  conn <- getAWSConnection()
  
  querytemplate <- 
    "UPDATE User set password = ?id2 where User.user_id = ?id3"
  query <- sqlInterpolate(conn, querytemplate,id2=newpassword,id3=playerid)
  
  result <- dbExecute(conn,query)
  print("Password updated")
  
  dbDisconnect(conn)
}

#' Renders board cells.
#' 
#' @param gridrow Grid row for the grid on the board.
#' @param gridcol Grid column for the grid on the board.
#' @param tile_id Which tile the player is at.
#' @param player_id The current user ID.
#' 
#' @return Shows the red/blue squares on the board denoting position
#' 
#' @author Vincent
#' @references Starter code taken from ESA lectures
#' 
renderCell <- function(gridrow, gridcol, tile_id, player_id) {
  renderImage({
    
    imageid <- 1
    
    if (gridrow==0 & gridcol==0) {
      
      imageid <- 1
      
      } else if (tile_id[1]==tile_id[2]) {
        
      if (tile_id[player_id] %in% c(1:12)) {
        
        if (gridrow == 13 - tile_id[player_id] & gridcol == 1) {
          imageid <- 4
        }
      } else if (tile_id[player_id] %in% c(13:20)) {
        if (gridcol == tile_id[player_id]-11 & gridrow == 1) {
          imageid <- 4
        }
      } else if (tile_id[player_id] %in% c(21:32)) {
        if (gridrow == tile_id[player_id]-20 & gridcol == 10) {
          imageid <- 4
        }
      } else if (tile_id[player_id] %in% c(33:40)) {
        if (gridcol == 42-tile_id[player_id] & gridrow == 12) {
          imageid <- 4
        }
      }
        
    } else {
      
      if (tile_id[player_id] %in% c(1:12)) {
        if (gridrow == 13 - tile_id[player_id] & gridcol == 1) {
          imageid <- 2
        }
      } else if (tile_id[player_id] %in% c(13:20)) {
        if (gridcol == tile_id[player_id]-11 & gridrow == 1) {
          imageid <- 2
        }
      } else if (tile_id[player_id] %in% c(21:32)) {
        if (gridrow == tile_id[player_id]-20 & gridcol == 10) {
          imageid <- 2
        }
      } else if (tile_id[player_id] %in% c(33:40)) {
        if (gridcol == 42-tile_id[player_id] & gridrow == 12) {
          imageid <- 2
        }
      } 
      
      if (tile_id[if_else(player_id==1, 2, 1)] %in% c(1:12)) {
        if (gridrow == 13 - tile_id[if_else(player_id == 1, 2, 1)] &
            gridcol == 1) {
          imageid <- 3
        }
      } else if (tile_id[if_else(player_id==1, 2, 1)] %in% c(13:20)) {
        if (gridcol == tile_id[if_else(player_id == 1, 2, 1)] - 11 &
            gridrow == 1) {
          imageid <- 3
        }
      } else if (tile_id[if_else(player_id==1, 2, 1)] %in% c(21:32)) {
        if (gridrow == tile_id[if_else(player_id == 1, 2, 1)] - 20 &
            gridcol == 10) {
          imageid <- 3
        }
      } else if (tile_id[if_else(player_id==1, 2, 1)] %in% c(33:40)) {
        if (gridcol == 42 - tile_id[if_else(player_id == 1, 2, 1)] &
            gridrow == 12) {
          imageid <- 3
        }
      }
      
    }
    
    imgsrc = switch(imageid,
                    "www/blank.png",
                    "www/red.png",
                    "www/blue.png",
                    "www/redblue.png")
    list(src = imgsrc, style = "position:relative;z-order:999")
  }, deleteFile = FALSE)
}


#' Starts the game by trying to send signals to one another that they are ready.
#' 
#' @note It is a Two Generals situation.
#' 
#' @param user_id User ID of the player.
#' @param turn_state Whose turn it is.
#' @param game_state List of things relating to game mechanics.
#' @param avatar Avatar ID of the player.
#' 
#' @return List of turn state and player IDs.
#' 
#' @author Vincent.
#' @references Some parts inspired by the various ESA lectures.
#' 
startGame <- function(user_id, turn_state, game_state, avatar) {
  
  player_id <- 0
  jsongamestate <- paste0("'",toJSON(game_state),"'")
  
  conn <- getAWSConnection()
  
  querytemplate <- 
    "SELECT * FROM GameState WHERE GameState.user_id=?id1;"
  query <- sqlInterpolate(conn, querytemplate, id1=user_id)
  
  result <- dbGetQuery(conn,query)
  
  if (nrow(result)==1) { # Player requesting is Player 2
    
    # print(result)
    game_state <- fromJSON(result$json_game_state[1])
    
    game_state_1 <- list(
      cash_balance=game_state$cash_balance,
      tile_id=game_state$tile_id,
      player_joined=2,
      avatar=c(game_state$avatar, avatar)
    )
    
    jsongamestate <-  paste0("'",toJSON(game_state_1),"'")
    turn_state <- 1
    
    query <- 
      paste0(
        "UPDATE GameState SET turn_state=",
        turn_state,
        ", json_game_state=",
        jsongamestate,
        " WHERE GameState.user_id=",
        user_id
      )
    
    result <- dbExecute(conn,query)
    player_id <- 2
    
  } else { # Player requesting is Player 1
    
    query <- paste0("INSERT INTO GameState 
                    (GameState.user_id,turn_state,json_game_state) VALUES (")
    query <- paste0(query,user_id,",",turn_state,",",jsongamestate,");")
    # print(query)
    result <- dbExecute(conn,query)
    player_id <- 1
    
  }
  
  return_list <- list(
    turn_state=turn_state,
    player_id=player_id
  )
  
  dbDisconnect(conn)
  return(return_list)
}


#' Checks if both players have joined.
#' 
#' @param user_id User ID of the player requesting.
#' 
#' @return The number of players joined.
#' 
#' @author Chester
playersJoined <- function(user_id) {
  conn <- getAWSConnection()
  
  querytemplate <- 
    "SELECT * FROM GameState WHERE GameState.user_id=?id1;"
  query <- sqlInterpolate(conn, querytemplate, id1 = user_id)
  
  result <- dbGetQuery(conn, query)
  game_state <- fromJSON(result$json_game_state[1])
  # print(game_state)
  
  dbDisconnect(conn)
  
  return(game_state$player_joined)
}

#' Deletes the game instance from GameState database.
#' 
#' @param user_id User ID of the player.
#' 
#' @return NULL
#' 
#' @author Chester
#' 
deleteGame <- function(user_id) {
  conn <- getAWSConnection()
  
  querytemplate <- 
    "SELECT * FROM GameState WHERE GameState.user_id=?id1;"
  query <- sqlInterpolate(conn, querytemplate, id1=user_id)
  result <- dbGetQuery(conn,query)
  if (nrow(result)==1) {
    query <- paste0("DELETE FROM GameState WHERE GameState.user_id=",user_id)
    result <- dbGetQuery(conn,query)
  }
  dbDisconnect(conn)
}


#' Checks turn state from the database
#' 
#' @param user_id User ID of the player.
#' 
#' @return Turn state of the game.
#' 
#' @author Devid.
#' 
checkTurnState <- function(user_id) {
  
  conn <- getAWSConnection()
  
  querytemplate <- 
    "SELECT * FROM GameState WHERE GameState.user_id=?id1;"
  query <- sqlInterpolate(conn, querytemplate, id1=user_id)
  result <- dbGetQuery(conn,query)
  
  turn_state <- result$turn_state
  
  dbDisconnect(conn)
  return(turn_state)
}


#' Checks game state.
#' 
#' @param user_id User ID of the player.
#' 
#' @return Game state list.
#' 
#' @author Devid.
#' 
checkGameState <- function(user_id) {
  conn <- getAWSConnection()
  
  querytemplate <- 
    "SELECT * FROM GameState WHERE GameState.user_id=?id1;"
  query <- sqlInterpolate(conn, querytemplate, id1=user_id)
  result <- dbGetQuery(conn,query)
  
  game_state <- fromJSON(result$json_game_state[1])
  
  dbDisconnect(conn)
  return(game_state)
}


#' Gets tile details.
#' 
#' @param user_id User ID of the player.
#' 
#' @return Tile details list.
#' 
#' @author Devid.
#' 
getTileTypeTopic <- function(tile_id) {
  conn <- getAWSConnection()
  
  querytemplate <- 
    "SELECT * FROM Tile WHERE Tile.tile_id=?id1;"
  query <- sqlInterpolate(conn, querytemplate, id1=tile_id-1)
  
  result <- dbGetQuery(conn,query)
  dbDisconnect(conn)
  return(result)
  
}


#' Grabs a scenario from the database and shows the scenario modal.
#' 
#' @param tile_topic_id Tile topic of the tile the player is on.
#' 
#' @return Cost associated with the scenario.
#' 
#' @author Devid.
#' 
getScenario <- function(tile_topic_id) {
  conn <- getAWSConnection()
  
  querytemplate <- 
    "SELECT * FROM ScenarioBank WHERE ScenarioBank.tile_topic_id=?id1 
  ORDER BY RAND() LIMIT 1;"
  query <- sqlInterpolate(conn, querytemplate, id1=tile_topic_id)
  
  result <- dbGetQuery(conn,query)
  
  querytemplate <- 
    "SELECT tile_topic FROM TileTopic WHERE TileTopic.tile_topic_id=?id1;"
  query <- sqlInterpolate(conn, querytemplate, id1=tile_topic_id)
  
  tile_topic <- dbGetQuery(conn,query)
  
  showModal(scenarioModal(tile_topic, result$scenario))
  
  dbDisconnect(conn)
  return(result$cost)
  
  
}


#' Grabs a question from the database and shows the question modal.
#' 
#' @param tile_topic_id Tile topic of the tile the player is on.
#' 
#' @return Correct answer placement and the prize associated.
#' 
#' @author Devid.
#' 
getQuestion <- function(tile_topic_id) {
  conn <- getAWSConnection()
  
  querytemplate <- 
    "SELECT * FROM QuestionBank WHERE QuestionBank.tile_topic_id=?id1 
  ORDER BY RAND() LIMIT 1;"
  query <- sqlInterpolate(conn, querytemplate, id1=tile_topic_id)
  
  result <- dbGetQuery(conn,query)
  
  # print(result)
  
  querytemplate <- 
    "SELECT tile_topic FROM TileTopic WHERE TileTopic.tile_topic_id=?id1;"
  query <- sqlInterpolate(conn, querytemplate, id1=tile_topic_id)
  
  tile_topic <- dbGetQuery(conn,query)
  
  answers <- strsplit(result$answer[1], ",")
  
  answers <- answers[[1]]
  
  correct_answer <- answers[1]
  
  prize <- result$prize
  
  rand_answers_index <- sample(c(1,2,3))
  
  rand_answers <- c(
    answers[rand_answers_index[1]],
    answers[rand_answers_index[2]],
    answers[rand_answers_index[3]]
  )
  
  correct_answer_index <- which(rand_answers_index==1)
  
  # print(correct_answer_index)
  
  showModal(questionModal(tile_topic, result$question, rand_answers, prize))
  
  
  dbDisconnect(conn)
  
  # print(correct_answer_index)
  # print(prize)
  
  return(c(correct_answer_index, prize))
}


#' Gets the opponent avatar.
#' 
#' @param user_id User ID of the player.
#' @param player_id Player ID for the avatar being queried.
#' 
#' @return Avatar ID of the specified Player ID.
#' 
#' @author Chester.
#' 
getAvatar <- function(user_id, player_id) {
  conn <- getAWSConnection()
  
  querytemplate <- 
    "SELECT * FROM GameState WHERE GameState.user_id=?id1;"
  query <- sqlInterpolate(conn, querytemplate, id1=user_id)
  
  result <- dbGetQuery(conn,query)
  game_state <- fromJSON(result$json_game_state[1])
  
  dbDisconnect(conn)
  
  return(game_state$avatar[player_id])
}


#' Updates the game state list to the database.
#' 
#' @param user_id User ID of the player.
#' @param turn_state Turn state of the game.
#' @param game_state List of game state.
#' 
#' @return NULL.
#' 
#' @author Vincent.
#' 
updateGameState <- function(user_id, turn_state, game_state) {
  conn <- getAWSConnection()
  
  jsongamestate <-  paste0("'",toJSON(game_state),"'")
  query <- 
    paste0(
      "UPDATE GameState SET turn_state=",
      turn_state,
      ", json_game_state=",
      jsongamestate,
      " WHERE GameState.user_id=",
      user_id
    )
  
  result <- dbExecute(conn,query)
  
  dbDisconnect(conn)
}