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
                  choices = c("machung", "gunadarma"),
                  selected = "machung"),
      sliderInput("count",
                  "Number of tweet retrieved:",
                  min = 10,
                  max = 500,
                  value = 50)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Klasifikasi", 
          tableOutput('ClassificationTable')
          ), 
        tabPanel(
          "Faktor Positif",
          "Faktor 1",
          tableOutput('PositiveF1Table'),
          "Faktor 2"
          ), 
        tabPanel(
          "Faktor Negatif", 
          "Faktor 1",
          #tableOutput('NegativeF1Table'),
          "Faktor 2"
          )
      )
    )
  )
))
