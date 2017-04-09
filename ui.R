library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Hello Shiny!"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("topic", 
                  label = "Topik perguruan tinggi",
                  choices = c("machung", "gunadarma"),
                  selected = "machung"),
      sliderInput("tweetscount",
                  "Jumlah tweets yang diambil",
                  min = 0,
                  max = 500,
                  value = 50),
      sliderInput("positivecomponentcount",
                  "Jumlah faktor positif",
                  min = 2,
                  max = 8,
                  value = 3),
      sliderInput("negativecomponentcount",
                  "Jumlah faktor negatif",
                  min = 2,
                  max = 8,
                  value = 3),
      sliderInput("sparsethreshold",
                  "sparse threshold",
                  min = 0.9,
                  max = 0.99,
                  value = 0.95)
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
          h3("Faktor 1"),
          tableOutput('PositiveF1Table'),
          h3("Faktor 2"),
          tableOutput('PositiveF2Table'),
          h3("Faktor 3"),
          tableOutput('PositiveF3Table')
          ), 
        tabPanel(
          "Faktor Negatif", 
          h3("Faktor 1"),
          tableOutput('NegativeF1Table'),
          h3("Faktor 2"),
          tableOutput('NegativeF2Table')
          )
      )
    )
  )
))
