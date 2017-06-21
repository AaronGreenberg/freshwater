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

          sliderInput("lwts",
                      "Stocking Weight(g):",
                      sep="",
                      min = .999,
                      max =25,
                      value = 1),

          sliderInput("stockdensity",
                      "Stocked Density:",
                      sep="",
                      min = 0.1,
                      max =10000,
                      value = 3500),
          
radioButtons("age", "Age:",
             c("2" = "2",
               "3" = "3",
               "4" = "4",
               "5" = "5"
),inline=TRUE),
          sliderInput("targ",
                      "Target Size:",
                      sep="",
                      min = 0.1,
                      max =70,
                      value = 11),

   textInput("wbid", "Water Body Id", "00372KOTR")),
    # Show a plot of the generated distribution
      mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Age Dist", plotOutput("mainPlot")),
                      tabPanel("Age Dist", tableOutput("mainTab")),
                      tabPanel("Age Dist", plotOutput("inversePlot")),
                      tabPanel("Age Dist", tableOutput("inverseTab"))

)))))

