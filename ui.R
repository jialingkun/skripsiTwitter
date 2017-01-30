library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Hello Shiny!"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("machung", "#machung",
                              "@machung", "Percent Asian"),
                  selected = "machung"),
      plotOutput("distPlot"),
      textOutput("text1")
    )
  )
))
