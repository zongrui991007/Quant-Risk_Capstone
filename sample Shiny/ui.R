library(shiny)
library(DT)

fluidPage(
  
    textInput(inputId = "text", label = "Enter text", placeholder = "Placeholder text"),
    numericInput(inputId = "number", label="Enter a number:", value = 5),
    selectInput(inputId = "select", label="Select a choice", choices = c("Conservative","Mediate","Aggressive"), multiple=T, selectize = T),
    radioButtons(inputId="radio", label="Select one", choices = c("Radio1","Radio2","Radio3")),
    checkboxGroupInput(inputId = "check", label="Check some boxed:", choices = c("fixed income","money market","equity")),
    sliderInput(inputId = "Slider", label= "Select a number:", min =1, max =10, value =5, step=1),
    #sliderInput(inputId = "Slider", label= "Select a number:", min ="conservative", max ="aggressive", value ="middle", step=1),
    dateInput(input="date", label = "Select a date:")
    

    #DTOutput(outputId = "updatedData"),
    #plotOutput(outputId = "updatedPlot")
    
)