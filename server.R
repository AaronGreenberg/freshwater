library(shiny)
library(dplyr)
shinyServer(function(input, output){
source("RB_tool.R")
dataInput <- reactive({
out <- model(input$wbid,lwts,kf,input$straintype,input$ploidy,input$dates[1],input$dates[2],input$stockdensity)
return(out)
})


output$countPlot <- renderPlot({
    out <- dataInput()
plotdist(out,input$wbid,input$straintype,input$ploidy,input$dates[1],input$dates[2])

})


output$boxPlot <- renderPlot({
    out <- dataInput()
plotbox(out,input$wbid,straintype,input$ploidy,input$dates[1],input$dates[2])

})

})




