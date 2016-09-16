library(shiny)
library(dplyr)
shinyServer(function(input, output){
source("RB_tool.R")
dataInput <- reactive({
main <- model(input$wbid,lwts,kf,input$straintype,input$ploidy,input$dates[1],input$dates[2],input$stockdensity)
stockdensity=seq(1000,10000,by=1000)
vec <- matrix(nrow=length(main),ncol=length(stockdensity))
for(i in 1:length(stockdensity))
{
vec[,i] <-model(input$wbid,lwts,kf,input$straintype,input$ploidy,input$dates[1],input$dates[2],stockdensity[i])
}
out=list(main=main,vec=vec)

return(out)
})


output$countPlot <- renderPlot({
    out <- dataInput()
plotdist(out$main,input$wbid,input$straintype,input$ploidy,input$dates[1],input$dates[2])

})


output$boxPlot <- renderPlot({
    out <- dataInput()
plotbox(out$vec,input$wbid,straintype,input$ploidy,input$dates[1],input$dates[2])

})

})




