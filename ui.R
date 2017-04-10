library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Twitter Sentimen Analisis Perguruan Tinggi"),

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
                  max = 5,
                  value = 3),
      sliderInput("negativecomponentcount",
                  "Jumlah faktor negatif",
                  min = 2,
                  max = 5,
                  value = 3),
      sliderInput("sparsethresholdpositive",
                  "sparse threshold positif",
                  min = 0.9,
                  max = 0.99,
                  value = 0.95),
      sliderInput("sparsethresholdnegative",
                  "sparse threshold negatif",
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
          h3(textOutput("PositiveF3Text")),
          tableOutput('PositiveF3Table'),
          h3(textOutput("PositiveF4Text")),
          tableOutput('PositiveF4Table'),
          h3(textOutput("PositiveF5Text")),
          tableOutput('PositiveF5Table')
          ), 
        tabPanel(
          "Faktor Negatif", 
          h3("Faktor 1"),
          tableOutput('NegativeF1Table'),
          h3("Faktor 2"),
          tableOutput('NegativeF2Table'),
          h3(textOutput("NegativeF3Text")),
          tableOutput('NegativeF3Table'),
          h3(textOutput("NegativeF4Text")),
          tableOutput('NegativeF4Table'),
          h3(textOutput("NegativeF5Text")),
          tableOutput('NegativeF5Table')
          )
      )
    )
  )
))
