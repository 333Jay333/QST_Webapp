#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(rio)
library(plyr) #has to come before here package -> have same funciton
library(here)

df.qst.z <- import(here("data","qst_z_values.csv")) # what is this doing? get the working directory where my R project is with here(), then go to the subdirectory. Thanks to https://github.com/jennybc/here_here and https://epirhandbook.com/en/new_pages/importing.html

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            
            selectInput("gender",
                        label = "Gender",
                        choices = list("",
                                       "Male" = "m",
                                       "Female" = "f"),
                        selected = NULL),
            
            numericInput("age",
                         "Age",
                         value = 0,
                         min = 20,
                         max = 120),
            
            # selectInput("multiple",
            #             label = "Multiple Areas tested?",
            #             choices = list("",
            #                            "Yes" = TRUE,
            #                            "No" = FALSE),
            #             selected = NULL),
            
            selectInput("area",
                        label = "Area Tested",
                        choices = list("",
                                       "Face" = "face",
                                       "Hand" = "hand",
                                       "Feet" = "feet"),
                        multiple = TRUE,
                        selected = NULL),
        
            numericInput("cdt1",
                         "CDT Wert 1 (°C)",
                         value = 0,
                         min = 0,
                         max = 32),
            
            numericInput("cdt2",
                         "CDT Wert 2 (°C)",
                         value = 0,
                         min = 0,
                         max = 32),
            
            numericInput("cdt3",
                         "CDT Wert 3 (°C)",
                         value = 0,
                         min = 0,
                         max = 32)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),#,
           
           verbatimTextOutput("cdt.output")

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    #age <- round_any(input$age, 10, f = floor)
    
    output$cdt.output <- renderText({
      cdt.sum <- ((32-input$cdt1) + (32-input$cdt2) + (32-input$cdt3))/3
      paste("Arithmetischer Mittelwert:", cdt.sum)
      
      cdt.sum.log <- log10(cdt.sum)
      
      # round_any(29, 10, f = floor) -> returns 20
      df.control <- subset(df.qst.z, gender == input$gender & age.low == round_any(input$age, 10, f = floor))
      
      
    })
  
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
