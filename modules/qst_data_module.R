# qst_data_module.R

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
    
    selectInput(
      inputId = ns("mdtNot1"),
      label = "MDT Value 1 Not Felt (mN)",
      choices = 2^(-2:9)
    ),
    
    selectInput(
      inputId = ns("mdt1"),
      label = "MDT Value 1 Felt (mN)",
      choices = 2^(-2:9)
    ),
    
    selectInput(
      inputId = ns("mdtNot2"),
      label = "MDT Value 2 Not Felt (mN)",
      choices = 2^(-2:9)
    ),
    
    selectInput(
      inputId = ns("mdt2"),
      label = "MDT Value 2 Felt (mN)",
      choices = 2^(-2:9)
    ),
    
    selectInput(
      inputId = ns("mdtNot3"),
      label = "MDT Value 3 Not Felt (mN)",
      choices = 2^(-2:9)
    ),
    
    selectInput(
      inputId = ns("mdt3"),
      label = "MDT Value 3 Felt (mN)",
      choices = 2^(-2:9)
    ),
    
    selectInput(
      inputId = ns("mdtNot4"),
      label = "MDT Value 4 Not Felt (mN)",
      choices = 2^(-2:9)
    ),
    
    selectInput(
      inputId = ns("mdt4"),
      label = "MDT Value 4 Felt (mN)",
      choices = 2^(-2:9)
    ),
    
    selectInput(
      inputId = ns("mdtNot5"),
      label = "MDT Value 5 Not Felt (mN)",
      choices = 2^(-2:9)
    ),
    
    selectInput(
      inputId = ns("mdt5"),
      label = "MDT Value 5 Felt (mN)",
      choices = 2^(-2:9)
    )
  )
}

qstDataServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      req(input$cdt1, input$cdt2, input$cdt3)
      cdt.sum <- ((32-input$cdt1) + (32-input$cdt2) + (32-input$cdt3))/3
      cdt.sum.log <- log10(cdt.sum)
      
      req(input$wdt1, input$wdt2, input$wdt3)
      wdt.sum <- ((input$wdt1-32) + (input$wdt2-32) + (input$wdt3-32))/3
      wdt.sum.log <- log10(wdt.sum)
      
      req(input$mdtNot1, input$mdtNot2, input$mdtNot3, input$mdtNot4, input$mdtNot5, input$mdt1, input$mdt2, input$mdt3, input$mdt4, input$mdt5)
      mdt <- as.numeric(c(input$mdtNot1, input$mdtNot2, input$mdtNot3, input$mdtNot4, input$mdtNot5, input$mdt1, input$mdt2, input$mdt3, input$mdt4, input$mdt5))
      mdt.mean <- exp(mean(log(mdt)))
      mdt.mean.log <- log10(mdt.mean)
      
      df <- data.frame(parameter = c("CDT","WDT","TSL","CPT","HPT","PPT","MPT","MPS","WUR","MDT","VDT","PHS","DMA"), logValue = c(cdt.sum.log, wdt.sum.log, rep(0,7), mdt.mean.log, rep(0,3)))
      df
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