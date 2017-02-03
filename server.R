library(shiny)
library(devtools)
library(httr)
library(twitteR)

consumerKey <- 'TOB0CV5G8er5yVpucjYLK6Vvn'
consumerSecret <- 'ZstnN6E3cv5Uwysps3fFGlTxAorFWbWIliZTreUbBFgoQRilWd'
accessSecret <- 'WBMZ0OFzCWtrwma0imN3EppFWwrqFoGgekTiIj2qgMPHU'
accessToken <- '871695572-H0oNk72XgoKGGVBpkcgHNo7ZRKERggsvsd5jY5Si'
setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessSecret)

search <- function(searchterm)
{
  #access tweets and create cumulative file
  
  list <- searchTwitter(searchterm, n=50, lang="id")
  if(length(list)>0){
    df <- twListToDF(list)
    df <- df[, order(names(df))]
    df$created <- strftime(df$created, '%Y-%m-%d')
    if (file.exists(paste('cumtw.csv'))==FALSE) write.csv(df, file=paste('cumtw.csv'), row.names=F)
    
    #merge last access with cumulative file and remove duplicates
    stack <- read.csv(file=paste('cumtw.csv'))
    stack <- rbind(stack, df)
    stack <- subset(stack, !duplicated(stack$text))
    write.csv(stack, file=paste('cumtw.csv'), row.names=F)
    return(stack)
  }
}
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  output$text1 <- renderText({ 
    paste("You have selected", input$var)
  })
  output$table <- renderTable(search(input$var))
})
