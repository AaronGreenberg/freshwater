library(shiny)
shinyServer(function(input, output){

source("growth_tab3.R")
print("Hi")
kf <- 1.1
dataInput <- reactive({
    straintype <- as.integer(input$straintype)
    ploidy <- as.integer(input$ploidy)
    mainout <-main(input$wbid,straintype,ploidy)
out=list(mainout=mainout)

return(out)
})


output$mainPlot <- renderPlot({
    out <- dataInput()
    print("silly")
    print(head(out$mainout))
plottab2(out$mainout,input$wbid)

})


})


