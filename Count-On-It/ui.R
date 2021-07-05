
##################################################################
##                  Linking the Files Together                  ##
##################################################################


source("usePackages.R")
source("modal.R")
source("sql.R")
source("getAWSConnection.R")
source("functions.R")


#################################################################
##              Uncomment when Uploading to Shiny              ##
#################################################################


# library(tidyverse)
# library(shiny)
# library(DBI)
# library(jsonlite)
# library(shinydashboard)
# library(bcrypt)
# library(shinyjs)

pkgnames <-
    c("tidyverse",
      "shiny",
      "DBI",
      "jsonlite",
      "shinydashboard",
      "bcrypt",
      "shinyjs")

loadPkgs(pkgnames) # Comment when uploading to Shiny




###########################################################################
###########################################################################
###                                                                     ###
###                      DEFINE UI FOR APPLICATION                      ###
###                                                                     ###
###########################################################################
###########################################################################


shinyUI(
    
    # Dashboard Page
    dashboardPage(
        
        # Header
        dashboardHeader(
            title = "Count On It!"
            # dropdownMenuOutput("messageMenu")
        ),
        
        # Sidebar
        dashboardSidebar(
            
            # Sidebar menu
            sidebarMenu(
                menuItem(
                    "Home Page",
                    tabName = "home_page", 
                    icon = icon("home")
                )
            ),
            sidebarMenuOutput("logged_in_tabs")
        ),
        
        # Body
        dashboardBody(
            
            tabItems(
                
                # Login Tab
                tabItem(
                    tabName = "home_page",
                    h1("Home Page"),
                    h4(htmlOutput("username")),
                    uiOutput("not_logged_in"),
                    uiOutput("login_privilege"),
                    # htmlOutput("logged-in-as"),
                    h2("Disclaimer"),
                    p("The game may lag for a bit. 
                      Please only click buttons once to 
                      prevent double-clicking."),
                    h2("How to Play"),
                    p("To play, you can register or sign in. Both players must 
                      sign in to the same account and choose their avatars 
                      before starting the game. This game's goal is to reach 
                      the $15000 starting from $10000 while answering some 
                      accounting related-questions and dealing with real-life 
                      scenarios along the way."),
                    tags$br(),
                    tags$br(),
                    img(src="questiontypes.png", width="500px"),
                    h3("Acknowledgments"),
                    p("Die face images by Nein Arimasen, CC BY-SA 3.0 
                      <http://creativecommons.org/licenses/by-sa/3.0/>, 
                      via Wikimedia Commons"),
                    p("Assortment of icons found from 
                      https://www.flaticon.com/")
                ),
                
                # Avatars Tab
                tabItem(
                    tabName = "avatars",
                    h1("Choose Your Avatar"),
                    uiOutput("choose_avatar"),
                    uiOutput("chosen_avatar")
                ),
                
                # # Choose Opponents Tab
                # tabItem(
                #     tabName = "choose_opponents",
                #     h1("Choose Your Opponent"),
                #     selectInput(
                #         "opponent-choice",
                #         "Select a logged-on player who is not 
                #         playing a game:",
                #         list(
                #             "A",
                #             "B",
                #             "C"
                #         )
                #     )
                # ),
                
                # Start Game Tab
                tabItem(
                    useShinyjs(),
                    tabName = "start_game",
                    h1("Start the Game"),
                    uiOutput("board_game"),
                    uiOutput("start_game")
                )
            )
        )
    )
)


##::::::::::::::::::::::::::::::
##  Credits and Acknowledgement  
##::::::::::::::::::::::::::::::

#' @author Devid & Alex
#' usePackages.R is from the ESA lectures.
#' 
#' @note The whole game was merged by Vincent.