#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plotly)
set.seed(2019)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    
    dashboardHeader(title = "World Values Survey"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home Page", tabName = "Home", icon = icon("dashboard")),
            menuItem("Questions-V4-V5", tabName = "V4-V5", icon = icon("th"))
        )## sidebarMenu
        
    ),## dashboardSidebar
    dashboardBody(
        tabItems(
            tabItem(tabName = "Home", 
                    #Greetings
                    h2("Questionnaire_Taiwan_2012")
            )
            
            ,
            tabItem(tabName = "V4-V5",
                    fluidRow(
                        column(9,wellPanel(h4("Your Output:"),
                                           plotlyOutput("performance")
                                           
                        ))
                        
                    )) ## tabItem 2
        ))) ## tabItems


server <- function(input, output) {
    
    output$performance <- renderPlotly({
        p <-ggplot(testdf)+
            aes(x = answer)+
            geom_bar(position = "dodge" )+
            facet_wrap(as.factor(testdf$Question),ncol = 2)
        plotly_build(p)
    })
}

# Run the application 
shinyApp(ui,server)