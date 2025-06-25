library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# Load modules
module_files <- list.files("modules", pattern = "\\.R$", 
                           full.names = TRUE, recursive = TRUE)
lapply(module_files, source)

# Define UI for application
ui <- navbarPage(
  id = "qst",

  # Application title
  title = "QST",
  
  tabPanel(
    title = "Setup",
    
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
    selectInput("area",
                label = "Area Tested",
                choices = list("",
                               "Face" = "face",
                               "Hand" = "hand",
                               "Feet" = "feet"),
                #multiple = TRUE,
                selected = NULL),
    
    actionButton(
      inputId = "bt.done",
      label = "Done"
    )
  ),
  
  tabPanel(
    title = "Inputs",
    
    qstDataUI("qst1")
  ),
  
  tabPanel(
    title = "Table",
    
    tableOutput("result")
  ),
  
  tabPanel(
    title = "Plot",
    
    plotOutput("test")
  )
)
          
          

          
          # 
          # # selectInput("multiple",
          # #             label = "Multiple Areas tested?",
          # #             choices = list("",
          # #                            "Yes" = TRUE,
          # #                            "No" = FALSE),
          # #             selected = NULL),
          # 
          
          # plotOutput("test")
       
        
    
            # plotOutput(
            #   outputId = "test"
            # )
        #   )
        # )
 
            


# Define server logic
server <- function(input, output, session) {
  
  hideTab(
    inputId = "qst",
    target = "Inputs"
  )
  
  hideTab(
    inputId = "qst",
    target = "Table"
  )
  
  hideTab(
    inputId = "qst",
    target = "Plot"
  )
  
  observeEvent(input$bt.done, {
    showTab(
      inputId = "qst",
      target = "Inputs"
    )
  })
  
  # Reactives
  gender <- reactive({input$gender[[1]]})
  age <- reactive({input$age})
  area <- reactive({input$area[[1]]})
  
  # Get the logged data from the QST UI
  df.qst.data <- qstDataServer("name1")
  
  observe({
    req(df.qst.data())
    
    showTab(
      inputId = "qst",
      target = "Table"
    )
    
    showTab(
      inputId = "qst",
      target = "Plot"
    )
  })
  
  # Get the z-scores
  df.qst.zScores <- qstZScoreServer("zscore",df.qst.data,gender,age,area)
  
  output$result <- renderTable({
    req(df.qst.zScores())
    df.qst.zScores()
  })
  
  # Get the z-Scores
  output$test <- renderPlot({
    # z-Scores
    # Get the z-scores
    # df.qst.zScores <- qstZScore(
    #   data = df.qst.data(),
    #   gender = input$gender[[1]],
    #   age = input$age,
    #   area = input$area[[1]])
    
    req(df.qst.zScores())
    ggplot(df.qst.zScores(), aes(x = parameter, y = logValue)) +
      geom_point()
    
    # ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
    #   geom_point()
  })
  
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
