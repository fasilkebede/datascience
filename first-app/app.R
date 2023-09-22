####################################
####################################
## https://github.com/fasilkebede/datascience#
#################################### #
####################################

# Modified from Winston Chang, 
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Load R packages
library(shiny)
library(shinythemes)


  # Define UI
  ui <- fluidPage(theme = shinytheme("superhero"),
    navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "TPGS Dashboard",
      tabPanel("Survival",
               sidebarPanel(
                 tags$h3("Country:"),
                 textInput("txt1", "Region:", " Amhara"),
                 textInput("txt2", "Trait:", " Body weight"),
                 
               ), # sidebarPanel
               mainPanel(
                            h1("Sasso"),
                            
                            h4("Body weight"),
                            verbatimTextOutput("txtout"),

               ) # mainPanel
               
      ), # Navbar 1, tabPanel
      tabPanel("Egg productivity", "This panel is intentionally left blank"),
      tabPanel("Body weight", "This panel is intentionally left blank")
  
    ) # navbarPage
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output) {
    
    output$txtout <- renderText({
      paste( input$txt1, input$txt2, sep = " " )
    })
  } # server
  
  
  # Create Shiny object
  shinyApp(ui = ui, server = server)