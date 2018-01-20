library(shiny)
library(data.table)
shinyServer(function(input, output,session){

source("growth_tab2.R")
print("Hi")
kf <- 1.1

kf <- 1.1
dataInputR <- reactive({

    main <- fread("RB_haspc2017_master_list.csv",sep=",",data.table=FALSE)
    wbidlist <- main$WATERBODY_IDENTIFIER
    regionlist <- main$Region
    nameslist <- main$GAZETTED_NAME
    names(wbidlist) <- paste(wbidlist,nameslist,regionlist,sep="____")

    ## input$AREA=5
    input$region
    print("Region")
    print(input$region)
    #print(wbidlist[which(regionlist==input$region)])
    updateSelectInput(session, 'wbid', choices = wbidlist[which(regionlist==input$region)])
})


dataInput <- reactive({
    t=dataInputR()#needed to make sure that we do not update the list unless the list has changed
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


