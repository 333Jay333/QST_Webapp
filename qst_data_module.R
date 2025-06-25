# qst_data_module.R

library(rio)
library(plyr) #has to come before here package -> have same function
library(here)

df.qst.z <- import(here("data","qst_z_values.csv")) # what is this doing? get the working directory where my R project is with here(), then go to the subdirectory. Thanks to https://github.com/jennybc/here_here and https://epirhandbook.com/en/new_pages/importing.html

qstDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(
      inputId = ns("cdt1"),
      label = "CDT Value 1 (°C)",
      value = 30,
      min = 0,
      max = 32),
    
    numericInput(
      inputId = ns("cdt2"),
      label = "CDT Value 2 (°C)",
      value = 30,
      min = 0,
      max = 32),
    
    numericInput(
      inputId = ns("cdt3"),
      label = "CDT Value 3 (°C)",
      value = 30,
      min = 0,
      max = 32),
    
    numericInput(
      inputId = ns("wdt1"),
      label = "WDT Value 1 (°C)",
      value = 34,
      min = 32,
      max = 50),
    
    numericInput(
      inputId = ns("wdt2"),
      label = "WDT Value 2 (°C)",
      value = 34,
      min =32,
      max = 50),
    
    numericInput(
      inputId = ns("wdt3"),
      label = "WDT Value 3 (°C)",
      value = 34,
      min = 32,
      max = 50),
    
    actionButton(
      inputId = ns("bt.analyse"),
      label = "Analyse"
    )
  )
}

qstDataServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- eventReactive(input$bt.analyse, {
      req(input$cdt1, input$cdt2, input$cdt3)
      cdt.sum <- ((32-input$cdt1) + (32-input$cdt2) + (32-input$cdt3))/3
      cdt.sum.log <- log10(cdt.sum)
      
      req(input$wdt1, input$wdt2, input$wdt3)
      wdt.sum <- ((input$wdt1-32) + (input$wdt2-32) + (input$wdt3-32))/3
      wdt.sum.log <- log10(wdt.sum)
      
      df <- data.frame(test = c("CDT","WDT","TSL","CPT","HPT","PPT","MPT","MPS","WUR","MDT","VDT"), value = c(cdt.sum.log, wdt.sum.log, rep(0,9)))
      df
    })
    
    # Return the reactive dataframe
    return(data)
    
    # Return the button press
    eventReactive(input$bt.analyse, {
      TRUE
    })
  })
}

# qstServer <- function(id, data) {
#   moduleServer(id, function(input, output, session) {
# 
#     cdt.z.score <- reactive({
#       req(input$cdt1, input$cdt2, input$cdt3)
# 
#       cdt.sum <- ((32-input$cdt1) + (32-input$cdt2) + (32-input$cdt3))/3
#       #paste("Arithmetischer Mittelwert:", cdt.sum)
# 
#       cdt.sum.log <- log10(cdt.sum)
# 
#       # RETURN
#       -((cdt.sum.log - data$mean) / data$sd)
#       # for CDT, the z-score needs to be inverted
#     })
#     
#     output$cdt.output <- renderText({
#       cdt.z.score()
#     })
#     
#     reactive({
#       df.qst.export <- data.frame(x = c(1:11), z = c(cdt.z.score(), rep(0,10)), lables = c("CDT","WDT","TSL","CPT","HPT","PPT","MPT","MPS","WUR","MDT","VDT"))
#     })
#   })
# }