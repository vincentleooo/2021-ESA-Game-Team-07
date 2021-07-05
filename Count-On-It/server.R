############################################################################
############################################################################
###                                                                      ###
###                     DEFINE SERVER LOGIC REQUIRED                     ###
###                                                                      ###
############################################################################
############################################################################


shinyServer(function(input, output, session) {
    
    
    #################################################################
    ##                       Reactive Values                       ##
    #################################################################
    
    # Values that are related to the player attributes
    vals <- reactiveValues(
        user_id = NULL,
        username = NULL,
        cash_balance = c(10000,10000),
        turn_state = 0,
        tile_id = c(1,1),
        player_id = 0
    )
    
    # Values that are related to the game mechanics
    game_vals <- reactiveValues(
        avatar = NULL,
        avatar_id = NULL,
        die_number = 1,
        start = NULL,
        correct_answer_index = NULL,
        question_prize = NULL
    )
    
    
    ##################################################################
    ##                            Inputs                            ##
    ##################################################################
    
    # Click on login button
    observeEvent(
        input$login,
        {
            showModal(loginModal())
        }
    ) 
    
    # Click on register button
    observeEvent(
        input$register,
        {
            showModal(newUserModal())
        }
    ) 
    
    # Click on change password button
    observeEvent(
        input$password_change,
        {
            showModal(newPasswordModal())
        }
    )
    
    # Click on logout button
    observeEvent(
        input$logout,
        {
            showModal(logoutModal(vals$user_id))
        }
    )
    
    # Click on OK button when the game ends
    observeEvent(
        input$win,
        {
            # Initial idea is to reset values, but it seems rather buggy
            
            # vals$cash_balance <- c(0,0)
            # vals$turn_state <- 0
            # vals$tile_id <- c(1,1)
            # vals$player_id <-  0
            # 
            # game_vals$avatar <-  NULL
            # game_vals$avatar_id <- NULL
            # game_vals$die_number <-  1
            # game_vals$start <-  NULL
            # game_vals$correct_answer_index <-  NULL
            # game_vals$question_prize <-  NULL
            
            # Instead, we delete the GameState database entry and stop the app
            deleteGame(vals$user_id)
            stopApp()
        }
    )
    
    # Click on logout button
    observeEvent(
        input$logoutok,
        {
            # Brings back to original state
            # Just in case something fails during the process
            tryCatch(
                {
                    
                    vals$user_id <- NULL
                    vals$username <- NULL
                    vals$cash_balance <- c(0,0)
                    vals$turn_state <- 0
                    vals$tile_id <- c(1,1)
                    vals$player_id <-  0
                    
                    game_vals$avatar <-  NULL
                    game_vals$avatar_id <- NULL
                    game_vals$die_number <-  1
                    game_vals$start <-  NULL
                    game_vals$correct_answer_index <-  NULL
                    game_vals$question_prize <-  NULL
                    
                },
                error = function(cond) {
                    
                    print("Logout Error")
                    showModal(logoutModal(vals$user_id, failed = TRUE))
                    
                }
            )
            
            # Resetting menu bar outputs
            output$logged_in_tabs <- renderMenu({
                req(vals$user_id)
                if (is.null(game_vals$avatar)) {
                    
                    sidebarMenu(
                        menuItem(
                            "Avatars",
                            tabName = "avatars",
                            icon = icon("user-tie")
                        )
                        
                        # Some remnants of the original multiplayer plan
                        
                        # menuItem(
                        #     "Choose Opponents",
                        #     tabName = "choose_opponents",
                        #     icon = icon("people-carry")
                        # ),
                        
                    )
                    
                } else {
                    
                    sidebarMenu(
                        menuItem(
                            "Avatars",
                            tabName = "avatars",
                            icon = icon("user-tie"),
                            selected = T
                        ),
                        menuItem(
                            "Start Game",
                            tabName = "start_game",
                            icon = icon("play-circle")
                        )
                    )
                    
                }
            })
            
            removeModal()
        }
    )
    
    # Click on OK button when registering   
    observeEvent(
        input$newuserok,
        {
            # Check that password1 exists and it matches password2
            # No requirements on password length, but will need Regex for that
            if (
                str_length(input$password1) > 0 && 
                (input$password1 == input$password2)
            ) {
                
                vals$user_id = registerPlayer(
                    input$username_new, 
                    hashpw(input$password1)
                )
                
                if (is.null(vals$user_id)) {
                    
                    showModal(newUserModal(failed_username = TRUE))
                    
                } else {
                    
                    vals$username <- input$username_new
                    logged_in <- 1
                    
                    removeModal()
                }
                
            } else {
                
                showModal(newUserModal(failed_password = TRUE))
                
            }
        }
    )
    
    # Click on OK button when logging in
    observeEvent(
        input$loginok,
        {
            user_id <- getPlayerID(input$username,input$password)
            
            if (user_id > 0) {
                
                vals$user_id <- user_id
                vals$username <- input$username
                
                logged_in <- 1
                
                # Clears residual GameState entry just in case
                deleteGame(vals$user_id)

                removeModal()
                
            } else {
                
                showModal(loginModal(failed = TRUE))
                
            }
        }
    )
    
    # Click on OK when changing password
    observeEvent(
        input$newpasswordok, 
        {
            conn <- getAWSConnection()
            querytemplate <- 
                "SELECT * FROM User WHERE username=?id1;"
            query <- sqlInterpolate(conn, querytemplate, id1 = vals$username)
            result <- dbGetQuery(conn,query)
            
            dbDisconnect(conn)
            
            if (nrow(result)!=1) {
                
                showModal(newPasswordModal(error = TRUE))
                
            } else if (!checkpw(input$oldpassword, result$password[1])) {
                
                showModal(newPasswordModal(failed = TRUE))
                
            } else if (
                str_length(input$newpassword) > 0 &&
                (input$newpassword == input$newpassword1)
            ) {
                
                changePassword(
                    vals$user_id, 
                    result$password[1], 
                    hashpw(input$newpassword)
                )
                removeModal()
                showModal(passwordChangedModal())
                
            } else {
                
                showModal(newPasswordModal(failed = TRUE))
                
            }
        }
    )
    
    # Avatar 1 is chosen
    observeEvent(
        input$avatar1,
        {
            game_vals$avatar <- img(src="avatar1.png", height=36)
            game_vals$avatar_id <- 1
        }
    )
    
    # Avatar 2 is chosen
    observeEvent(
        input$avatar2,
        {
            game_vals$avatar <- img(src="avatar2.png", height=36)
            game_vals$avatar_id <- 2
        }
    )
    
    # Avatar 3 is chosen
    observeEvent(
        input$avatar3,
        {
            game_vals$avatar <- img(src="avatar3.png", height=36)
            game_vals$avatar_id <- 3
        }
    )
    
    # Avatar 4 is chosen
    observeEvent(
        input$avatar4,
        {
            game_vals$avatar <- img(src="avatar4.png", height=36)
            game_vals$avatar_id <- 4
        }
    )
    
    # Avatar 5 is chosen
    observeEvent(
        input$avatar5,
        {
            game_vals$avatar <- img(src="avatar5.png", height=36)
            game_vals$avatar_id <- 5
        }
    )
    
    # Click on rechoose avatar button
    observeEvent(
        input$rechoose_avatar,
        {
            game_vals$avatar <- NULL
            game_vals$avatar_id <- NULL
            output$logged_in_tabs <- renderMenu({
                req(vals$user_id)
                
                if (is.null(game_vals$avatar)) {
                    
                    sidebarMenu(
                        menuItem(
                            "Avatars",
                            tabName = "avatars",
                            icon = icon("user-tie"),
                            selected = T
                        )
                        
                        # menuItem(
                        #     "Choose Opponents",
                        #     tabName = "choose_opponents",
                        #     icon = icon("people-carry")
                        # ),
                        
                    )
                    
                } else {
                    
                    sidebarMenu(
                        
                        menuItem(
                            "Avatars",
                            tabName = "avatars",
                            icon = icon("user-tie"),
                            selected = T
                        ),
                        
                        menuItem(
                            "Start Game",
                            tabName = "start_game",
                            icon = icon("play-circle")
                        )
                        
                    )
                    
                }
            })
        }
    )
    
    # Click on start game button
    observeEvent(
        input$startok,
        {
            game_state <- list(
                cash_balance=vals$cash_balance,
                tile_id=vals$tile_id,
                player_joined=1,
                avatar=game_vals$avatar_id
            )
            
            game_init <- startGame(
                vals$user_id,
                vals$turn_state,
                game_state, 
                game_vals$avatar_id
            )
            
            vals$turn_state <- game_init$turn_state
            vals$player_id <- game_init$player_id
            
            showModal(waitingPlayerModal(failed = FALSE))
        }
    )
    
    # Click on refresh button to ensure handshake between two players
    observeEvent(
        input$refresh,
        {
            number <- playersJoined(vals$user_id)
            print(number)
            
            if (number == 2) {
                removeModal()
                game_vals$start <- 1
            } else {
                showModal(waitingPlayerModal(failed = TRUE))
            }
        }
    )
    
    # Observe game state and update the UI during the game
    observe({
        invalidateLater(2000, session)
        
        if (!is.null(game_vals$start)) {
            vals$turn_state <- checkTurnState(vals$user_id)
            update_game_state <- checkGameState(vals$user_id)
            vals$cash_balance <- update_game_state$cash_balance
            vals$tile_id <- update_game_state$tile_id
            
            output$your_cash <- renderText({
                # invalidateLater(1000, session)
                
                paste0("$", vals$cash_balance[vals$player_id])
            })
            
            output$opponent_cash <- renderText({
                # invalidateLater(1000, session)
                
                paste0("$", vals$cash_balance[if_else(vals$player_id==1, 2, 1)])
            })
            
            output$cell0101 <- renderCell(1,1,vals$tile_id,vals$player_id)
            output$cell0102 <- renderCell(1,2,vals$tile_id,vals$player_id)
            output$cell0103 <- renderCell(1,3,vals$tile_id,vals$player_id)
            output$cell0104 <- renderCell(1,4,vals$tile_id,vals$player_id)
            output$cell0105 <- renderCell(1,5,vals$tile_id,vals$player_id)
            output$cell0106 <- renderCell(1,6,vals$tile_id,vals$player_id)
            output$cell0107 <- renderCell(1,7,vals$tile_id,vals$player_id)
            output$cell0108 <- renderCell(1,8,vals$tile_id,vals$player_id)
            output$cell0109 <- renderCell(1,9,vals$tile_id,vals$player_id)
            output$cell0110 <- renderCell(1,10,vals$tile_id,vals$player_id)
            
            output$cell0201 <- renderCell(2,1,vals$tile_id,vals$player_id)
            output$cell0210 <- renderCell(2,10,vals$tile_id,vals$player_id)
            
            output$cell0301 <- renderCell(3,1,vals$tile_id,vals$player_id)
            output$cell0310 <- renderCell(3,10,vals$tile_id,vals$player_id)
            
            output$cell0401 <- renderCell(4,1,vals$tile_id,vals$player_id)
            output$cell0410 <- renderCell(4,10,vals$tile_id,vals$player_id)
            
            output$cell0501 <- renderCell(5,1,vals$tile_id,vals$player_id)
            output$cell0510 <- renderCell(5,10,vals$tile_id,vals$player_id)
            
            output$cell0601 <- renderCell(6,1,vals$tile_id,vals$player_id)
            output$cell0610 <- renderCell(6,10,vals$tile_id,vals$player_id)
            
            output$cell0701 <- renderCell(7,1,vals$tile_id,vals$player_id)
            output$cell0710 <- renderCell(7,10,vals$tile_id,vals$player_id)
            
            output$cell0801 <- renderCell(8,1,vals$tile_id,vals$player_id)
            output$cell0810 <- renderCell(8,10,vals$tile_id,vals$player_id)
            
            output$cell0901 <- renderCell(9,1,vals$tile_id,vals$player_id)
            output$cell0910 <- renderCell(9,10,vals$tile_id,vals$player_id)
            
            output$cell1001 <- renderCell(10,1,vals$tile_id,vals$player_id)
            output$cell1010 <- renderCell(10,10,vals$tile_id,vals$player_id)
            
            output$cell1101 <- renderCell(11,1,vals$tile_id,vals$player_id)
            output$cell1110 <- renderCell(11,10,vals$tile_id,vals$player_id)
            
            output$cell1201 <- renderCell(12,1,vals$tile_id,vals$player_id)
            output$cell1202 <- renderCell(12,2,vals$tile_id,vals$player_id)
            output$cell1203 <- renderCell(12,3,vals$tile_id,vals$player_id)
            output$cell1204 <- renderCell(12,4,vals$tile_id,vals$player_id)
            output$cell1205 <- renderCell(12,5,vals$tile_id,vals$player_id)
            output$cell1206 <- renderCell(12,6,vals$tile_id,vals$player_id)
            output$cell1207 <- renderCell(12,7,vals$tile_id,vals$player_id)
            output$cell1208 <- renderCell(12,8,vals$tile_id,vals$player_id)
            output$cell1209 <- renderCell(12,9,vals$tile_id,vals$player_id)
            output$cell1210 <- renderCell(12,10,vals$tile_id,vals$player_id)
        }
    })
    
    # Observe the cash balance for win situation
    observe({
        invalidateLater(5000, session)
        
        if (vals$cash_balance[1] < 0 | vals$cash_balance[2] >= 15000) {
            
            showModal(winModal(2))
            
        } else if (vals$cash_balance[2] < 0 | vals$cash_balance[1] >= 15000) {
            
            showModal(winModal(1))
            
        }
    })
    
    # Click on first answer option for a question
    observeEvent(
        input$answer1,
        {
            if (game_vals$correct_answer_index==1) {
                
                vals$cash_balance[vals$player_id] <- 
                    vals$cash_balance[vals$player_id] + game_vals$question_prize
                showModal(correctModal())
                
            } else {
                
                showModal(falseModal())
                
            }
            
            game_state <- list(
                cash_balance=vals$cash_balance,
                tile_id=vals$tile_id,
                player_joined=2,
                avatar=c(
                    getAvatar(vals$user_id,1),
                    getAvatar(vals$user_id,2)
                )
            )
            
            updateGameState(vals$user_id, 
                            if_else(vals$player_id == 1, 2, 1),
                            game_state)
        }
    )
    
    # Click on second answer option for a question
    observeEvent(
        input$answer2,
        {
            if (game_vals$correct_answer_index == 2) {
                
                vals$cash_balance[vals$player_id] <- 
                    vals$cash_balance[vals$player_id] + game_vals$question_prize
                showModal(correctModal())
                
            } else {
                
                showModal(falseModal())
                
            }
            
            game_state <- list(
                cash_balance=vals$cash_balance,
                tile_id=vals$tile_id,
                player_joined=2,
                avatar=c(
                    getAvatar(vals$user_id, 1),
                    getAvatar(vals$user_id, 2)
                )
            )
            
            updateGameState(vals$user_id, 
                            if_else(vals$player_id == 1, 2, 1),
                            game_state)
        }
    )
    
    # Click on third answer option for a question
    observeEvent(
        input$answer3,
        {
            if (game_vals$correct_answer_index == 3) {
                
                vals$cash_balance[vals$player_id] <- 
                    vals$cash_balance[vals$player_id] + game_vals$question_prize
                showModal(correctModal())
                
            } else {
                
                showModal(falseModal())
                
            }
            
            game_state <- list(
                cash_balance=vals$cash_balance,
                tile_id=vals$tile_id,
                player_joined=2,
                avatar=c(
                    getAvatar(vals$user_id, 1),
                    getAvatar(vals$user_id, 2)
                )
            )
            
            updateGameState(vals$user_id, 
                            if_else(vals$player_id == 1, 2, 1),
                            game_state)
        }
    )

    # Click on the die during the game
    observeEvent(input$clickdie, {
        
        # Ensure it's the player whose turn is theirs that get to be responded
        if (vals$turn_state == vals$player_id) {
            
            game_vals$die_number <- as.integer(runif(1,1,7))
            
            # print("reached here")
            
            vals$tile_id[vals$player_id] <-
                vals$tile_id[vals$player_id] + game_vals$die_number
            
            # print(vals$tile_id)
            
            if (vals$tile_id[vals$player_id] >= 41) {
                
                vals$tile_id[vals$player_id] <-
                    42 - vals$tile_id[vals$player_id]
                
                vals$cash_balance[vals$player_id] <-
                    1000 + vals$cash_balance[vals$player_id]
                
                showModal(payDayModal())
                
            }
            
            game_state <- list(
                cash_balance = vals$cash_balance,
                tile_id = vals$tile_id,
                player_joined = 2,
                avatar = c(getAvatar(vals$user_id, 1),
                           getAvatar(vals$user_id, 2)
                )
            )
            
            # print(game_state)
            
            updateGameState(vals$user_id,
                            vals$player_id,
                            game_state)
            
            # Re-renders the board when the position is updated
            output$cell0101 <- renderCell(1,1,vals$tile_id,vals$player_id)
            output$cell0102 <- renderCell(1,2,vals$tile_id,vals$player_id)
            output$cell0103 <- renderCell(1,3,vals$tile_id,vals$player_id)
            output$cell0104 <- renderCell(1,4,vals$tile_id,vals$player_id)
            output$cell0105 <- renderCell(1,5,vals$tile_id,vals$player_id)
            output$cell0106 <- renderCell(1,6,vals$tile_id,vals$player_id)
            output$cell0107 <- renderCell(1,7,vals$tile_id,vals$player_id)
            output$cell0108 <- renderCell(1,8,vals$tile_id,vals$player_id)
            output$cell0109 <- renderCell(1,9,vals$tile_id,vals$player_id)
            output$cell0110 <- renderCell(1,10,vals$tile_id,vals$player_id)
            
            output$cell0201 <- renderCell(2,1,vals$tile_id,vals$player_id)
            output$cell0202 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0203 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0204 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0205 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0206 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0207 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0208 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0209 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0210 <- renderCell(2,10,vals$tile_id,vals$player_id)
            
            output$cell0301 <- renderCell(3,1,vals$tile_id,vals$player_id)
            output$cell0302 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0303 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0304 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0305 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0306 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0307 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0308 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0309 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0310 <- renderCell(3,10,vals$tile_id,vals$player_id)
            
            output$cell0401 <- renderCell(4,1,vals$tile_id,vals$player_id)
            output$cell0402 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0403 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0404 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0405 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0406 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0407 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0408 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0409 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0410 <- renderCell(4,10,vals$tile_id,vals$player_id)
            
            output$cell0501 <- renderCell(5,1,vals$tile_id,vals$player_id)
            output$cell0502 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0503 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0504 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0505 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0506 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0507 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0508 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0509 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0510 <- renderCell(5,10,vals$tile_id,vals$player_id)
            
            output$cell0601 <- renderCell(6,1,vals$tile_id,vals$player_id)
            output$cell0602 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0603 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0604 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0605 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0606 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0607 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0608 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0609 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0610 <- renderCell(6,10,vals$tile_id,vals$player_id)
            
            output$cell0701 <- renderCell(7,1,vals$tile_id,vals$player_id)
            output$cell0702 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0703 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0704 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0705 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0706 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0707 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0708 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0709 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0710 <- renderCell(7,10,vals$tile_id,vals$player_id)
            
            output$cell0801 <- renderCell(8,1,vals$tile_id,vals$player_id)
            output$cell0802 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0803 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0804 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0805 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0806 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0807 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0808 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0809 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0810 <- renderCell(8,10,vals$tile_id,vals$player_id)
            
            output$cell0901 <- renderCell(9,1,vals$tile_id,vals$player_id)
            output$cell0902 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0903 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0904 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0905 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0906 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0907 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0908 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0909 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell0910 <- renderCell(9,10,vals$tile_id,vals$player_id)
            
            output$cell1001 <- renderCell(10,1,vals$tile_id,vals$player_id)
            output$cell1002 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell1003 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell1004 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell1005 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell1006 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell1007 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell1008 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell1009 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell1010 <- renderCell(10,10,vals$tile_id,vals$player_id)
            
            output$cell1101 <- renderCell(11,1,vals$tile_id,vals$player_id)
            output$cell1102 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell1103 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell1104 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell1105 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell1106 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell1107 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell1108 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell1109 <- renderCell(0,0,vals$tile_id,vals$player_id)
            output$cell1110 <- renderCell(11,10,vals$tile_id,vals$player_id)
            
            output$cell1201 <- renderCell(12,1,vals$tile_id,vals$player_id)
            output$cell1202 <- renderCell(12,2,vals$tile_id,vals$player_id)
            output$cell1203 <- renderCell(12,3,vals$tile_id,vals$player_id)
            output$cell1204 <- renderCell(12,4,vals$tile_id,vals$player_id)
            output$cell1205 <- renderCell(12,5,vals$tile_id,vals$player_id)
            output$cell1206 <- renderCell(12,6,vals$tile_id,vals$player_id)
            output$cell1207 <- renderCell(12,7,vals$tile_id,vals$player_id)
            output$cell1208 <- renderCell(12,8,vals$tile_id,vals$player_id)
            output$cell1209 <- renderCell(12,9,vals$tile_id,vals$player_id)
            output$cell1210 <- renderCell(12,10,vals$tile_id,vals$player_id)
            
            result <- getTileTypeTopic(vals$tile_id[vals$player_id])
            
            # If scenario tile
            if (result$tile_type == 1) {
                
                vals$cash_balance[vals$player_id] <-
                    vals$cash_balance[vals$player_id] + 
                    getScenario(result$tile_topic_id)
                
                game_state <- list(
                    cash_balance=vals$cash_balance,
                    tile_id=vals$tile_id,
                    player_joined=2,
                    avatar=c(
                        getAvatar(vals$user_id,1),
                        getAvatar(vals$user_id,2)
                    )
                )
                
                updateGameState(vals$user_id, 
                                if_else(vals$player_id==1, 2, 1),
                                game_state)
                
            } else { # If question tile
                
                result <- getQuestion(result$tile_topic_id)
                game_vals$correct_answer_index <- result[1]
                game_vals$question_prize <- result[2]
                
                
            }
        }
    })
    
    #################################################################
    ##                           Outputs                           ##
    #################################################################
    
    # Shows login and register button if not logged in
    output$not_logged_in <- renderUI({
        if (is.null(vals$user_id)) {
            
            tagList(
                actionButton("login", "Login"),
                actionButton("register", "Register")
            )
            
        }
    })
    
    # Shows logout and change password button if logged in
    output$login_privilege <- renderUI({
        req(vals$user_id) # if NULL, the UI below will not be visible
        
        tagList(
            actionButton("logout", "Log Out"),
            actionButton("password_change", "Change Password")
        )
    })
    
    # Shows username on home page
    output$username <- renderUI({
        if (is.null(vals$user_id)) {
            
            "Not logged in yet."
            
        } else {
            
            paste("Logged in as", vals$username, ".")
            
        }
    })
    
    # Show Avatars tab when logged in and also the Start Game tab when an avatar
    # is already chosen
    output$logged_in_tabs <- renderMenu({
        req(vals$user_id)
        
        if (is.null(game_vals$avatar)) {
            
            sidebarMenu(
                menuItem(
                    "Avatars", 
                    tabName = "avatars", 
                    icon = icon("user-tie")
                )
                # menuItem(
                #     "Choose Opponents", 
                #     tabName = "choose_opponents", 
                #     icon = icon("people-carry")
                # ),
            )
            
        } else {
            
            sidebarMenu(
                menuItem(
                    "Avatars",
                    tabName = "avatars",
                    icon = icon("user-tie"),
                    selected = T
                ),
                menuItem(
                    "Start Game",
                    tabName = "start_game",
                    icon = icon("play-circle")
                )
            )
            
        }
    })
    
    # Choose avatar page
    output$choose_avatar <- renderUI({
        if (is.null(game_vals$avatar)) {
            
            fluidRow(
                box(
                    title = "Businessman",
                    status = "primary",
                    img(src="avatar1.png", height=36),
                    actionButton("avatar1", "Choose This!")
                ),
                box(
                    title = "Blue-Collar Worker",
                    status = "primary",
                    img(src="avatar2.png", height=36),
                    actionButton("avatar2", "Choose This!")
                ),
                box(
                    title = "Banker",
                    status = "primary",
                    img(src="avatar3.png", height=36),
                    actionButton("avatar3", "Choose This!")
                ),
                box(
                    title = "University Student",
                    status = "primary",
                    img(src="avatar4.png", height=36),
                    actionButton("avatar4", "Choose This!")
                ),
                box(
                    title = "Retiree",
                    status = "primary",
                    img(src="avatar5.png", height=36),
                    actionButton("avatar5", "Choose This!")
                )
            )    
            
        }
    })
    
    # If avatar is chosen, the page changes to this
    output$chosen_avatar <- renderUI({
        req(game_vals$avatar)
        
        fluidRow(
            box(
                title = "You have chosen this avatar!",
                status = "primary",
                game_vals$avatar,
                actionButton("rechoose_avatar", "Choose Another Avatar")
            )
        )
    })
    
    # Click start game button
    output$start_game <- renderUI({
        if (is.null(game_vals$start)) {
            fluidRow(
                box(
                    title = "Count On It!",
                    p("Both players must press start, one after the other. 
                      The first to do so will be the first player, and so on."),
                    actionButton("startok", "Press to Start!")
                )
            )
        }
    })
    
    # After player handshake, show this UI
    output$board_game <- renderUI({
        req(game_vals$start)
        fluidRow(
            box(
                title = "Count On It! Your Turn!",
                "You will be represented by the red square on the board.",
                tags$br(),
                h4("Your goal is to reach $15000 and not to go bankrupt."),
                width = 12,
                
                img(src="board.png",
                    style="position:absolute;z-order:0",
                    height="535px"),
                
                imageOutput("cell0101",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0102",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0103",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0104",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0105",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0106",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0107",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0108",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0109",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0110",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                tags$br(),
                imageOutput("cell0201",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0202",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0203",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0204",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0205",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0206",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0207",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0208",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0209",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0210",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                tags$br(),
                imageOutput("cell0301",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0302",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0303",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0304",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0305",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0306",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0307",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0308",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0309",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0310",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                tags$br(),
                imageOutput("cell0401",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0402",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0403",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0404",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0405",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0406",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0407",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0408",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0409",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0410",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                tags$br(),
                imageOutput("cell0501",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0502",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0503",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0504",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0505",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0506",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0507",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0508",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0509",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0510",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                tags$br(),
                imageOutput("cell0601",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0602",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0603",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0604",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0605",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0606",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0607",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0608",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0609",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0610",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                tags$br(),
                imageOutput("cell0701",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0702",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0703",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0704",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0705",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0706",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0707",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0708",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0709",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0710",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                tags$br(),
                imageOutput("cell0801",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0802",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0803",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0804",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0805",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0806",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0807",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0808",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0809",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0810",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                tags$br(),
                imageOutput("cell0901",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0902",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0903",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0904",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0905",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0906",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0907",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0908",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0909",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell0910",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                tags$br(),
                imageOutput("cell1001",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell1002",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell1003",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell1004",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell1005",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell1006",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell1007",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell1008",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell1009",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell1010",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                tags$br(),
                imageOutput("cell1101",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell1102",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell1103",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell1104",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell1105",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell1106",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell1107",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell1108",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell1109",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell1110",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                tags$br(),
                imageOutput("cell1201",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell1202",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell1203",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell1204",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell1205",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell1206",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell1207",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell1208",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell1209",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE),
                imageOutput("cell1210",
                            height = "44.6px",
                            width = "42px",
                            inline = TRUE), 
                
                tags$br(),
                tags$br(),#for more space
                textOutput("player_turn"),
                imageOutput(
                    "die",
                    height = "410px",
                    width = "410px",
                    click = "clickdie",
                    inline = TRUE
                )
            ),
            box(
                title = "You",
                game_vals$avatar,
                width = 6,
                textOutput("your_cash")
            ),
            box(
                title = "Opponent",
                img(src = paste0("avatar", getAvatar(
                    vals$user_id, if_else(vals$player_id == 1, 2, 1)
                ), ".png"),
                height = 36),
                textOutput("opponent_cash")
            )
        )
        
        
    })
    
    # Shows turn state to the players
    output$player_turn <- renderText({
        invalidateLater(4000, session)
        
        if (vals$turn_state == vals$player_id) {
            
            text <- "It is your turn. Click on the die below."
            
        } else {
            
            text <- "It is not your turn. Please wait a while for your turn."
            
        }
        
        text
    })
    
    # Shows player cash, invalidated by Observe on line 415
    output$your_cash <- renderText({
        # invalidateLater(1000, session)
        
        paste0("$", vals$cash_balance[vals$player_id])
    })
    
    # Shows opponent cash, invalidated by Observe on line 415
    output$opponent_cash <- renderText({
        # invalidateLater(1000, session)
        
        paste0("$", vals$cash_balance[if_else(vals$player_id==1, 2, 1)])
    })
    
    # Change die image accordingly
    output$die <- renderImage({
        # invalidateLater(2000, session)
        
        imageid <- game_vals$die_number
        imgsrc = switch(
            imageid,
            "www/Die1.png",
            "www/Die2.png",
            "www/Die3.png",
            "www/Die4.png",
            "www/Die5.png",
            "www/Die6.png",
            "www/blank.png"
        )
        
        list(src = imgsrc, style = "position:relative;z-order:999;")
        
    }, deleteFile = FALSE)
    
    # Initial board output
    output$cell0101 <- renderCell(1,1,vals$tile_id,vals$player_id)
    output$cell0102 <- renderCell(1,2,vals$tile_id,vals$player_id)
    output$cell0103 <- renderCell(1,3,vals$tile_id,vals$player_id)
    output$cell0104 <- renderCell(1,4,vals$tile_id,vals$player_id)
    output$cell0105 <- renderCell(1,5,vals$tile_id,vals$player_id)
    output$cell0106 <- renderCell(1,6,vals$tile_id,vals$player_id)
    output$cell0107 <- renderCell(1,7,vals$tile_id,vals$player_id)
    output$cell0108 <- renderCell(1,8,vals$tile_id,vals$player_id)
    output$cell0109 <- renderCell(1,9,vals$tile_id,vals$player_id)
    output$cell0110 <- renderCell(1,10,vals$tile_id,vals$player_id)
    
    output$cell0201 <- renderCell(2,1,vals$tile_id,vals$player_id)
    output$cell0202 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0203 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0204 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0205 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0206 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0207 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0208 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0209 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0210 <- renderCell(2,10,vals$tile_id,vals$player_id)
    
    output$cell0301 <- renderCell(3,1,vals$tile_id,vals$player_id)
    output$cell0302 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0303 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0304 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0305 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0306 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0307 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0308 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0309 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0310 <- renderCell(3,10,vals$tile_id,vals$player_id)
    
    output$cell0401 <- renderCell(4,1,vals$tile_id,vals$player_id)
    output$cell0402 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0403 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0404 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0405 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0406 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0407 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0408 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0409 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0410 <- renderCell(4,10,vals$tile_id,vals$player_id)
    
    output$cell0501 <- renderCell(5,1,vals$tile_id,vals$player_id)
    output$cell0502 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0503 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0504 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0505 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0506 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0507 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0508 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0509 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0510 <- renderCell(5,10,vals$tile_id,vals$player_id)
    
    output$cell0601 <- renderCell(6,1,vals$tile_id,vals$player_id)
    output$cell0602 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0603 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0604 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0605 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0606 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0607 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0608 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0609 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0610 <- renderCell(6,10,vals$tile_id,vals$player_id)
    
    output$cell0701 <- renderCell(7,1,vals$tile_id,vals$player_id)
    output$cell0702 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0703 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0704 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0705 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0706 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0707 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0708 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0709 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0710 <- renderCell(7,10,vals$tile_id,vals$player_id)
    
    output$cell0801 <- renderCell(8,1,vals$tile_id,vals$player_id)
    output$cell0802 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0803 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0804 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0805 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0806 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0807 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0808 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0809 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0810 <- renderCell(8,10,vals$tile_id,vals$player_id)
    
    output$cell0901 <- renderCell(9,1,vals$tile_id,vals$player_id)
    output$cell0902 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0903 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0904 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0905 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0906 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0907 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0908 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0909 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell0910 <- renderCell(9,10,vals$tile_id,vals$player_id)
    
    output$cell1001 <- renderCell(10,1,vals$tile_id,vals$player_id)
    output$cell1002 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell1003 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell1004 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell1005 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell1006 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell1007 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell1008 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell1009 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell1010 <- renderCell(10,10,vals$tile_id,vals$player_id)
    
    output$cell1101 <- renderCell(11,1,vals$tile_id,vals$player_id)
    output$cell1102 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell1103 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell1104 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell1105 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell1106 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell1107 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell1108 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell1109 <- renderCell(0,0,vals$tile_id,vals$player_id)
    output$cell1110 <- renderCell(11,10,vals$tile_id,vals$player_id)
    
    output$cell1201 <- renderCell(12,1,vals$tile_id,vals$player_id)
    output$cell1202 <- renderCell(12,2,vals$tile_id,vals$player_id)
    output$cell1203 <- renderCell(12,3,vals$tile_id,vals$player_id)
    output$cell1204 <- renderCell(12,4,vals$tile_id,vals$player_id)
    output$cell1205 <- renderCell(12,5,vals$tile_id,vals$player_id)
    output$cell1206 <- renderCell(12,6,vals$tile_id,vals$player_id)
    output$cell1207 <- renderCell(12,7,vals$tile_id,vals$player_id)
    output$cell1208 <- renderCell(12,8,vals$tile_id,vals$player_id)
    output$cell1209 <- renderCell(12,9,vals$tile_id,vals$player_id)
    output$cell1210 <- renderCell(12,10,vals$tile_id,vals$player_id)

})

##::::::::::::::::::::::::::::::
##  Credits and Acknowledgement  
##::::::::::::::::::::::::::::::

#' @author Vincent & Chester

#' Line 18 and 28
#' Idea of using vals and game_vals as reactive values from the ESA lectures.
#' 
#' Line 131 et cetera
#' Using the argument select in the menu tabs output to prevent it from changing
#' to the home page all the time when it refreshes.
#' https://stackoverflow.com/questions/55492979
#' 
#' Line 750
#' Grids for the board game is taken from various ESA lectures.
#' 
#' Line 1317 and 1337
#' Die output and switching is taken from various ESA lectures.