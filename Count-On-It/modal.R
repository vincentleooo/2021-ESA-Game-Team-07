source("server.R")


############################################################################
############################################################################
###                                                                      ###
###                      MODALS FOR THE APPLICATION                      ###
###                                                                      ###
############################################################################
############################################################################



#################################################################
##                     Administrative Jobs                     ##
#################################################################


#' @author Alex
#' @references Taken from the ESA Lectures
newUserModal <- function(failed_username = FALSE, failed_password = FALSE) {
  modalDialog(
    title = "Create a new account",
    textInput("username_new", "Enter a new username:"),
    passwordInput("password1", "Enter a new password:"),
    passwordInput("password2", "Confirm by re-entering the new password:"),
    
    if (failed_username) {
      
      div(tags$b(
        "The username already exists. Try again.",
        style = "color: red;"
      ))
      
    },
    
    if (failed_password) {
      
      div(tags$b(
        "The passwords do not match. Try again.",
        style = "color: red;"
      ))
      
    },
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("newuserok", "OK")
    )
  )
}


#' @author Alex
#' @references Taken from the ESA Lectures
newPasswordModal <- function(failed = FALSE, error = FALSE) {
  modalDialog(
    title = "Change your password",
    "If you just registered and have not logged out yet, 
    please logout and login again to be able to change your password.",
    tags$br(),
    tags$br(),
    passwordInput("oldpassword", "Enter your current password:"),
    passwordInput("newpassword", "Enter a new password:"),
    passwordInput("newpassword1", "Confirm by re-entering the new password:"),
    "If successful, this will be your new password.",
    
    if (error) {
      div(tags$b(
        "An error occured. Please try again.",
        style = "color: red;"
      ))
    },
    
    if (failed)
      div(tags$b(
        "Either the current password is wrong or the new passwords do not
        match. Try again.",
        style = "color: red;"
      )),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("newpasswordok", "OK")
    )
  )
}

#' @author Vincent
passwordChangedModal <- function() {
  modalDialog(
    title = "Password changed",
    "Click OK to continue.",
    footer = tagList(
      modalButton("OK")
    )
  )
}

#' @author Vincent
notLoggedInModal <- function() {
  modalDialog(
    title = "Please log in first",
    "Click OK to continue.",
    footer = tagList(
      modalButton("OK")
    )
  )
}

#' @author Vincent
loginModal <- function(failed = FALSE) {
  modalDialog(
    title = "Login",
    textInput("username", "Enter your username:"),
    passwordInput("password", "Enter your password:"),
    
    if (failed)
      div(
        tags$b(
          "There is no registered player with that name and 
          password. Try again or re-register.", 
          style = "color: red;"
        )
      ),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("loginok", "OK")
    )
  )
}

#' @author Vincent
logoutModal <- function(user_id, failed = FALSE) {
  if (!is.null(user_id)) {
    modalDialog(
      title = "Logout",
      "Are you sure you would like to logout?",
      if (failed) {
        div(
          tags$b(
            "An error occured. Please try again.", 
            style = "color: red;"
          )
        )
      },
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("logoutok", "OK")
      )
    )
  } else {
    modalDialog(
      title = "Logout",
      "You are not logged in.",
      footer = tagList(
        modalButton("OK")
      )
    )
  }
  
}


#################################################################
##                          Game Jobs                          ##
#################################################################


#' @author Devid
waitingPlayerModal <- function(failed = FALSE) {
  modalDialog(
    title = "Waiting for the other player",
    "If you are Player 1, ensure the other player pressed Start, 
    and then press Refresh. If you are Player 2, press Refresh.",
    if (failed) {
      div(
        tags$b(
          "The other player have not pressed Start. Try again.", 
          style = "color: red;"
        )
      )
    },
    footer = tagList(
      actionButton("refresh", "Refresh")
    )
  )
}


#' @author Devid
notPlayerTurnModal <- function() {
  modalDialog(
    title = "Not your turn yet! Please wait for the other player.",
    "If this takes longer than expected, please restart the game.",
    footer = NULL
  )
}

#' @author Chester
scenarioModal <- function(tile_topic, scenario) {
  modalDialog(
    title = paste(tile_topic, "Scenario"),
    scenario
  )
}

#' @author Chester
questionModal <- function(tile_topic, question, answers, prize) {
  modalDialog(
    title = paste(tile_topic, "Question"),
    question,
    tags$br(),
    paste0("Option 1: ", answers[1]),
    tags$br(),
    paste0("Option 2: ", answers[2]),
    tags$br(),
    paste0("Option 3: ", answers[3]),
    tags$br(),
    paste("Prize if correct: ", prize),
    footer = tagList(
      actionButton("answer1", "Option 1"),
      actionButton("answer2", "Option 2"),
      actionButton("answer3", "Option 3")
    )
  )
}

#' @author Devid
payDayModal <- function() {
  modalDialog(
    title="Pay Day",
    "Pay Day has come! You have got an extra $1000 in the bank."
  )
}

#' @author Chester
correctModal <- function() {
  modalDialog(
    "You are correct!"
  )
}

#' @author Chester
falseModal <- function() {
  modalDialog(
    "You are wrong, try again next time!"
  )
}

#' @author Chester
winModal <- function(winner_id) {
  modalDialog(
    title = "Game Ended",
    paste("Player", winner_id, "has won!"),
    footer = tagList(
      actionButton("win", "OK")
    )
  )
}