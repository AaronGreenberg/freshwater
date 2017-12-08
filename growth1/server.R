library(shiny)
shinyServer(function(input, output,session){

source("growth_tab1.R")
print("Hi")
kf <- 1.1
dataInput <- reactive({
    straintype <- as.integer(input$straintype)
    ploidy <- as.integer(input$ploidy)
    age <- as.integer(input$age)

    wbidlist <- read.csv("RB_haspc2017_master_list.csv", header = T)$WATERBODY_IDENTIFIER
    regionlist <- read.csv("RB_haspc2017_master_list.csv", header = T)$Region
    
    ## input$AREA=5
    print("Region")
    print(input$region)
    print(wbidlist[which(regionlist==input$region)])
    updateSelectInput(session, 'wbid', choices = wbidlist[which(regionlist==input$region)])
    
    mainout <-main(input$wbid,input$lwts,kf,input$stockdensity,straintype,ploidy)
    print("ran")
    print(head(mainout))
    print("What's up")
    sdentmp <-main3(input$wbid,input$lwts,kf,input$targ,straintype,ploidy,age)
    maintargout <-main(input$wbid,input$lwts,kf,sdentmp$root,straintype,ploidy)
out=list(mainout=mainout,maintargout=maintargout,sdentmp=sdentmp$root)

return(out)
})


output$mainPlot <- renderPlot({
    out <- dataInput()
    print("silly")
    print(head(out$mainout))
fig1(out$mainout,input$wbid,input$stockdensity)

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
    fig1(out$maintargout,input$wbid,out$sdentmp,input$targ)

})


})


