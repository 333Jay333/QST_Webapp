# qst_module.R

library(rio)
library(plyr) #has to come before here package -> have same funciton
library(here)

df.qst.z <- import(here("data","qst_z_values.csv")) # what is this doing? get the working directory where my R project is with here(), then go to the subdirectory. Thanks to https://github.com/jennybc/here_here and https://epirhandbook.com/en/new_pages/importing.html


qstUI <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(
      inputId = ns("cdt1"),
      label = "CDT Value 1 (°C)",
      value = NULL,
      min = 0,
      max = 32),
    
    numericInput(
      inputId = ns("cdt2"),
      label = "CDT Value 2 (°C)",
      value = NULL,
      min = 0,
      max = 32),
    
    numericInput(
      inputId = ns("cdt3"),
      label = "CDT Value 3 (°C)",
      value = NULL,
      min = 0,
      max = 32),
    
    verbatimTextOutput(ns("cdt.output"))
  )
}

#function(id, gender, age, area)
qstServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    cdt.z.score <- reactive({
      req(input$cdt1, input$cdt2, input$cdt3)

      cdt.sum <- ((32-input$cdt1) + (32-input$cdt2) + (32-input$cdt3))/3
      #paste("Arithmetischer Mittelwert:", cdt.sum)

      cdt.sum.log <- log10(cdt.sum)

      # RETURN
      -((cdt.sum.log - data$mean) / data$sd)
      # for CDT, the z-score needs to be inverted
    })
    
    output$cdt.output <- renderText({
      cdt.z.score()
    })
  })
}