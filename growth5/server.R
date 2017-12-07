library(shiny)
shinyServer(function(input, output){

source("growth_tab5.R")
print("Hi")
kf <- 1.1
dataInput <- reactive({
    mainout <-main(input$eta,as.numeric(input$target))
out=list(mainout=mainout)
return(out)
})


output$mainPlot1 <- renderPlot({
    out <- dataInput()
    print("silly")
   plots6fry(out$mainout)
}
,height = 600,width=600)

output$mainPlot2 <- renderPlot({
    out <- dataInput()
    print("silly")
    plots6ye(out$mainout)
}
,height = 600,width=600)

})


