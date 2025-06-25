#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)

source("qst_data_module.R")

# Define UI for application that draws a histogram
ui <- page_sidebar(

    # Application title
    title = "QST",

    # Sidebar 
    sidebar = sidebar(
          
          selectInput("gender",
                      label = "Gender",
                      choices = list("",
                                     "Male" = "m",
                                     "Female" = "f"),
                      selected = NULL),
          
          sliderInput(
            inputId = "age",
            label = "Age",
            min = 20,
            max = 100,
            value = 60
          ),

          # numericInput("age",
          #              "Age",
          #              value = 0,
          #              min = 20,
          #              max = 120),
          # 
          # # selectInput("multiple",
          # #             label = "Multiple Areas tested?",
          # #             choices = list("",
          # #                            "Yes" = TRUE,
          # #                            "No" = FALSE),
          # #             selected = NULL),
          # 
          selectInput("area",
                      label = "Area Tested",
                      choices = list("",
                                     "Face" = "face",
                                     "Hand" = "hand",
                                     "Feet" = "feet"),
                      #multiple = TRUE,
                      selected = NULL)
        ),

        # Show a plot of the generated distribution
        navset_card_underline(
          
          nav_panel(
            title = "Face",
            
            qstDataUI("name1"),
            tableOutput("result")
          )
        )
)   
            # numericInput("cdt1",
            #    "CDT Wert 1 (°C)",
            #    value = 0,
            #    min = 0,
            #    max = 32),
            # 
            # numericInput("cdt2",
            #    "CDT Wert 2 (°C)",
            #    value = 0,
            #    min = 0,
            #    max = 32),
            # 
            # numericInput("cdt3",
            #    "CDT Wert 3 (°C)",
            #    value = 0,
            #    min = 0,
            #    max = 32)
            # )
           # plotOutput("distPlot"),#,
           # 
           # verbatimTextOutput("cdt.output")

        # )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  filtered.data <- reactive({
    req(input$gender, input$age, input$area)
    # round_any(29, 10, f = floor) -> returns 20
    
    subset(df.qst.z, age.low == round_any(input$age, 10, f = floor) &
             gender == input$gender[[1]] &
             area == input$area[[1]])
    
  })
  
  df.qst.data <- qstDataServer("name1")
  
  output$result <- renderTable({
    req(df.qst.data())
    df.qst.data()
  })
  # 
  # export_data <- qstServer(id = "name1", data = filtered.data())
  # 
  # # Pass it to summary module
  # summaryServer("name1", data = export_data)
  
    #age <- round_any(input$age, 10, f = floor)
  
  
    # 
    # cdt.z.score <- reactive({
    #     req(input$cdt1, input$cdt2, input$cdt3)
    #   
    #     cdt.sum <- ((32-input$cdt1) + (32-input$cdt2) + (32-input$cdt3))/3
    #     #paste("Arithmetischer Mittelwert:", cdt.sum)
    #     
    #     cdt.sum.log <- log10(cdt.sum)
    #     
    #     df.filtered <- filtered.data()
    #     
    #     # RETURN
    #     -((cdt.sum.log - df.filtered$mean) / df.filtered$sd)
    #     # for CDT, the z-score needs to be inverted
    # })
    # 
    # output$cdt.output <- renderText({
    #     cdt.z.score()
    # })
    # 
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
