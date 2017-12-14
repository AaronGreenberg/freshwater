library(shiny)
shinyServer(function(input, output){

source("growth_tab4.R")
print("Hi")
kf <- 1.1
dataInput <- reactive({
    mainout <-main(input$sden,input$eta)
out=list(mainout=mainout)
return(out)
})


output$mainPlot1 <- renderPlot({
    out <- dataInput()
    print("silly")
   plotfry(out$mainout,as.numeric(input$straintype))
}
,height = 600,width=600)

output$mainPlot2 <- renderPlot({
    out <- dataInput()
    print("silly")
    plotye(out$mainout,as.numeric(input$straintype))
}
,height = 600,width=600)

output$info <- renderPrint({
    # With base graphics, need to tell it what the x and y variables are.
    nearPoints(mtcars, input$plot_click, xvar = "wt", yvar = "mpg")
    # nearPoints() also works with hover and dblclick events
  })
    
})


