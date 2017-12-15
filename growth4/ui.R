library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme=shinytheme("flatly"),
  # Application title
  titlePanel("Size Distribution"),
#For strains: Blackwater=1,Carp=2,Gerrard=3,Multiple=4,Pennask=5
  # Sidebar with a slider input for the number of bins
#sden<-c(20,50,100,200,500)
  sidebarLayout(
      sidebarPanel(
          radioButtons("straintype", "Strain:",
             c(
                 "Blackwater" = 1,
                  "Carp" = 2,
                  "Fraser Valley"=3,
                  "Gerrard"=4,
                  "Multiple"=5,
                  "Pennask"=6
),inline=TRUE),
          radioButtons("sden", "Stocking Density:",
             c("20" = 20,
               "50" = 50,
                "100"=100,
                 "200"=200,
                 "500"=500
            ),inline=TRUE),

dateInput("eta", "eta", value = NULL, min = NULL, max = NULL,
  format = "yyyy-mm-dd", startview = "month", weekstart = 0,
  language = "en", width = NULL)),

    # Show a plot of the generated distribution
      mainPanel(
verbatimTextOutput("info",placeholder=TRUE),
          tabsetPanel(type = "tabs",
                      tabPanel("Map Fry ", plotOutput("mainPlot1",width="100%",click = "plot_click")),
                      tabPanel("Map Yearling ", plotOutput("mainPlot2",width="100%",click = "plot_click"))
)

))))

