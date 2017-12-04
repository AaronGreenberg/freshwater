library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme=shinytheme("flatly"),
  # Application title
  titlePanel("Size Distribution"),
#For strains: Blackwater=1,Carp=2,Gerrard=3,Multiple=4,Pennask=5
  # Sidebar with a slider input for the number of bins
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
          radioButtons("ploidy", "Ploidy:",
             c("3n" = 1,
               "2n" = 3,
                "Multiple"=2
),inline=TRUE),
##           radioButtons("lfs", "LFS:",
##              c("1" = 1,
##                "2" = 2,
##                 "3"=3,
##                  "4"=4
## ),inline=TRUE),

          sliderInput("stockdensity",
                      "Stocked Density:",
                      sep="",
                      min = 0.1,
                      max =1000,
                      value = 1000),
          
   textInput("wbid", "Water Body Id", "00372KOTR")),
    # Show a plot of the generated distribution
      mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Something", plotOutput("mainPlot"))
)))))

