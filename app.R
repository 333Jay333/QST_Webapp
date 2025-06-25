library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# Load modules
module_files <- list.files("modules", pattern = "\\.R$", 
                           full.names = TRUE, recursive = TRUE)
lapply(module_files, source)

# Define UI for application
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
                      selected = NULL),
          plotOutput("test")
        ),

        # Show a plot of the generated distribution
        # navset_card_underline(
          
          # nav_panel(
          #   title = "Face",
            
            qstDataUI("name1"),
            tableOutput("result")
            # plotOutput(
            #   outputId = "test"
            # )
        #   )
        # )
)   
            


# Define server logic
server <- function(input, output, session) {
  
  # Get the logged data from the QST UI
  df.qst.data <- qstDataServer("name1")
  
  output$result <- renderTable({
    req(df.qst.data())
    df.qst.data()
  })
  
  # Get the z-Scores
  output$test <- renderPlot({
    # z-Scores
    req(df.qst.data())
    df.qst.zScores <- qstZScore(
      data = df.qst.data(),
      gender = input$gender[[1]],
      age = input$age,
      area = input$area[[1]])

    req(df.qst.zScores)
    ggplot(df.qst.zScores, aes(x = parameter, y = logValue)) +
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
