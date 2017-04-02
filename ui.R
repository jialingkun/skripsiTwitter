library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Hello Shiny!"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("topic", 
                  label = "Choose a topic",
                  choices = c("machung", "ubaya",
                              "stiki", "UMM"),
                  selected = "machung"),
      sliderInput("count",
                  "Number of tweet retrieved:",
                  min = 10,
                  max = 100,
                  value = 30)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      
      tableOutput('table'),
      textOutput("text1")
    )
  )
))
