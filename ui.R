library(shiny)
library(shinythemes)
masterlist<-read.csv("RB_tool_master_list.csv")
waterbody=list(masterlist$wbid)
print(waterbody)
# Define UI for application that draws a histogram
shinyUI(fluidPage(theme=shinytheme("flatly"),

                  
  # Application title
  titlePanel("Size Distribution"),
#For strains: Blackwater=1,Carp=2,Gerrard=3,Multiple=4,Pennask=5
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
      sidebarPanel(
          radioButtons("straintype", "Stock:",
             c("Carp" = "Carp",
               "Blackwater" = "Blackwater",
                "Gerrard"="Gerrard",
                 "Multiple"="Multiple",
                 "Pennask"="Pennask"
),inline=TRUE),
          radioButtons("ploidy", "Ploidy:",
             c("3n" = "n3",
               "2n" = "n2",
                 "Multiple"="Multiple"
),inline=TRUE),

          sliderInput("lwts",
                      "lwts:",
                      sep="",
                      min = 1,
                      max =4,
                      value = 1),


          sliderInput("kf",
                      "kf:",
                      sep="",
                      min = 0.1,
                      max =5,
                      value = .25),

          sliderInput("stockdensity",
                      "Stocking Density:",
                      sep="",
                      min = 0.1,
                      max =10000,
                      value = 200),
    dateRangeInput("dates", label = h3("Date range")),
   textInput("wbid", "Water Body Id", "00001ALBN")),
    # Show a plot of the generated distribution
      mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Size Dist", plotOutput("countPlot")),
                      tabPanel("Box Plot", plotOutput("boxPlot"))

)))))

