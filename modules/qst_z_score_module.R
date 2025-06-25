# qst_z_score_module.R

library(shiny)
library(rio)
library(plyr)
library(here)

# Load static comparison dataset (once, when the module is sourced)
df.qst.z <- import(here("data", "qst_z_values.csv"))

# SERVER MODULE
qstZScoreServer <- function(id, data, gender, age, area) {
  moduleServer(id, function(input, output, session) {
    
    reactive({
      req(data(), gender(), age(), area())
      
      df.z.scores <- data()
      
      df.qst.z.compare <- subset(
        df.qst.z,
        age.low == round_any(age(), 10, f = floor) &
          gender == gender() &
          area == area()
      )
      
      for (i in 1:nrow(df.z.scores)) {
        df.z.scores[i, 2] <- (df.z.scores[i, 2] - df.qst.z.compare$mean[i]) / df.qst.z.compare$sd[i]
      }
      
      # Flip signs for specific parameters
      flip_rows <- c(1, 2, 3, 5, 6, 7, 10)
      df.z.scores[flip_rows, 2] <- -df.z.scores[flip_rows, 2]
      
      df.z.scores
    })
  })
}



# library(rio)
# library(plyr) #has to come before here package -> have same function
# library(here)
# 
# df.qst.z <- import(here("data","qst_z_values.csv")) # what is this doing? get the working directory where my R project is with here(), then go to the subdirectory. Thanks to https://github.com/jennybc/here_here and https://epirhandbook.com/en/new_pages/importing.html
# 
# qstZScore <- function(data, gender, age, area) {
#   df.z.scores <- data
# 
#   df.qst.z.compare <- subset(df.qst.z, age.low == round_any(age, 10, f = floor) &
#                                gender == gender &
#                                area == area)
# 
#   for (i in 1:nrow(data)) {
#     # calculation of z-score
#     df.z.scores[i,2] <- (data[i,2] - df.qst.z.compare$mean[i]) / df.qst.z.compare$sd[i]
#   }
#   
#   # change of sign for specific parameters
#   df.z.scores[1,2] <- 0-df.z.scores[1,2] #CDT
#   df.z.scores[2,2] <- 0-df.z.scores[2,2] #WDT
#   df.z.scores[3,2] <- 0-df.z.scores[3,2] #TSL
#   df.z.scores[5,2] <- 0-df.z.scores[5,2] #HPT
#   df.z.scores[6,2] <- 0-df.z.scores[6,2] #PPT
#   df.z.scores[7,2] <- 0-df.z.scores[7,2] #MPT
#   df.z.scores[10,2] <- 0-df.z.scores[10,2] #MDT
#   
#   # rename column
#   # df.z.scores %>%
#   #   rename(logValue = zScore)
#   
#   return(df.z.scores)
# }