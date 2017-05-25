library(shiny)
shinyServer(function(input, output){

source("growth_tab1.R")
print("Hi")

dataInput <- reactive({
    straintype <- as.integer(input$straintype)
    ploidy <- as.integer(input$ploidy)
    age <- as.integer(input$age)
    
    mainout <-main(input$wbid,input$lwts,input$kf,input$stockdensity,straintype,ploidy)
    print("ran")
    print(head(mainout))
    print("What's up")
    sdentmp <-main3(input$wbid,input$lwts,input$kf,input$targ,straintype,ploidy,age)
    maintargout <-main(input$wbid,input$lwts,input$kf,sdentmp$root,straintype,ploidy)
out=list(mainout=mainout,maintargout=maintargout,sdentmp=sdentmp$root)

return(out)
})


output$mainPlot <- renderPlot({
    out <- dataInput()
    print("silly")
    print(head(out$mainout))
fig1(out$mainout,input$stockdensity)

})


output$mainTab <- renderTable({
    out <- dataInput()
tab1(out$mainout)

},rownames=TRUE)

output$inverseTab <- renderTable({
out <- dataInput()
tab1(out$maintargout)

},rownames=TRUE)


output$inversePlot <- renderPlot({
    out <- dataInput()
    fig1(out$maintargout,out$sdentmp,input$targ)

})


})


