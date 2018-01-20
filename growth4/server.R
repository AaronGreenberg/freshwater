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
   plotfry(out$mainout)
}
,height = 600,width=600)

output$mainPlot2 <- renderPlot({
    out <- dataInput()
    print("silly")
    plotye(out$mainout)
}
,height = 600,width=600)

output$info <- renderPrint({
                                        # With base graphics, need to tell it what the x and y variables are.
    haspclist<-read.csv("RB_haspc2017_master_list.csv",stringsAsFactors=FALSE)
    out <- nearPoints(haspclist, input$plot_click, xvar = "LONGITUDE", yvar = "LATITUDE")
    print(out$WATERBODY_IDENTIFIER)
    print(out$GAZETTED_NAME)

  })
    
})


