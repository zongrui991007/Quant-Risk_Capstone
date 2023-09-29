library(shiny)
library(DT)
library(data.table)

function(input, output, session){
  
  fileData <- reactiveFileReader(intervalMillis = 1000, session = session, 
                                 filePath = "ptf_return.csv",
                                 readFunc = read.csv)
  
  dataAggregation <- reactive({
    
    aggData <- data.table(fileData())[,list(total_returns=sum(qtr_return)), by=list(Code, Date)]
    
    return(aggData)
    
  })
 
  output$updatedData <- renderDT(datatable(dataAggregation())) 
  
  output$updatedPlot <- renderPlot({
    
    ggplot(data=dataAggregation(), aes(x=Code, y=Return, fill =Code))+geom_col()
    
  })
}


