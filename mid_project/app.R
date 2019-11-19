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

tw_num<- read_excel("Data_Taiwan_2012_Excel_v20180912.xlsx")

tw_text<- read_excel("text_Data_Taiwan_2012_Excel_v20180912.xlsx")
## select questions that we will analyze. 
tw_select<-tw_num%>%select(5,6,7,8,9,10,11,12,24,62,106:111,161,162,163,306,307,308,309)
tw_select_all<-tw_select
# column name
tw_select_col <- colnames(tw_select)
tw_select_col_copy <- tw_select_col
# split comlumn name 
tw_select_col_code <- str_split_fixed(tw_select_col_copy , ": ", 2)[,1]

# replace column name make it easy to use in filter function
tw_select_col <- str_replace_all(tw_select_col,": ","_")
tw_select_col <- str_replace_all(tw_select_col," ","_")
colnames(tw_select) <- tw_select_col_code

colnames(tw_select_all) <- tw_select_col

tw_select_all$V240_Sex <- factor(tw_select_all$V240_Sex,levels = c(1,2),labels = c("male","female"))
testdf <- tw_select_all%>%select(1:4,V240_Sex)%>%
    pivot_longer(cols = 1:4,names_to = "Question",values_to = "answer") %>%
    filter(answer>=0)
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

}

# Run the application 
shinyApp(ui,server)
