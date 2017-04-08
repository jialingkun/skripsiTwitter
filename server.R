library(shiny)
library(devtools)
library(httr)
library(twitteR)

library(tm)
library(katadasaR)
library(stringr)


consumerKey <- 'TOB0CV5G8er5yVpucjYLK6Vvn'
consumerSecret <- 'ZstnN6E3cv5Uwysps3fFGlTxAorFWbWIliZTreUbBFgoQRilWd'
accessSecret <- 'WBMZ0OFzCWtrwma0imN3EppFWwrqFoGgekTiIj2qgMPHU'
accessToken <- '871695572-H0oNk72XgoKGGVBpkcgHNo7ZRKERggsvsd5jY5Si'
setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessSecret)

search <- function(searchterm, count)
{
  #access tweets and create cumulative file
  
  list <- searchTwitter(searchterm, n=count, lang="id")
  if(length(list)>0){
    df <- twListToDF(list)
    df <- df[, order(names(df))]
    df$created <- strftime(df$created, "%d/%m/%Y")
    
    #clean things that could cause bug
    df$text = gsub("(\r\n)+|\r+|\n+", " ", df$text) # remove line break
    df$text = iconv(df$text, "latin1", "ASCII", sub="") # remove emoticon
    df$text = gsub("<[^ ]*>", "", df$text) # remove other format (<564+> emoticon bug)
    
    #customize data to csv
    tweetToCsv <- cbind(created=df$created,text=df$text)
    filenamesplit = c(searchterm,"DataTestRaw.csv")
    filename = str_c(filenamesplit,collapse='')
    if (file.exists(paste(filename))==FALSE) write.csv(tweetToCsv, file=paste(filename), row.names=F)
    
    #merge last access with cumulative file and remove duplicates
    stack <- read.csv(file=paste(filename))
    stack <- rbind(stack, tweetToCsv)
    stack <- subset(stack, !duplicated(stack$text))
    #order by date
    stack <- stack[rev(order(as.Date(stack$created, format="%d/%m/%Y"))),]
    write.csv(stack, file=paste(filename), row.names=F)
    return(stack)
  }
}

clean_text = function(x)
{
  #tolower function often return error on certain character, need try catch
  try.error = function(z) #To convert the text in lowercase
  {
    y = NA
    try_error = tryCatch(tolower(z), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(z)
    return(y)
  }
  
  #case folding
  x = gsub("(\r\n)+|\r+|\n+", " ", x) # remove line break
  x = gsub("@\\w+", "", x) # remove at(@)
  x = gsub("&[^ ]*", "", x) # remove other format (&amp)
  x = gsub("http[^ ]*", "", x)  # remove http
  x = iconv(x, "latin1", "ASCII", sub="") # remove emoticon
  x = gsub("<[^ ]*>", "", x) # remove other format (<564+> emoticon bug)
  x = gsub("[[:punct:]]", " ", x) # remove punctuation
  x = gsub("[[:digit:]]", "", x) # remove numbers/Digits
  x = sapply(x, try.error) #lower case
  x = gsub("rt ", "", x) # remove Retweet tag
  
  #filtering
  cStopwordID = readLines("stopword-ID.txt") #read stopword file
  corpus = Corpus(VectorSource(x)) #tm package only support data type corpus
  x = tm_map(corpus, removeWords, c(stopwords("english"),cStopwordID))
  
  #case folding
  x = gsub("[ |\t]{2,}", " ", x) # remove tabs
  x = gsub("^ ", "", x)  # remove blank spaces at the beginning
  x = gsub(" $", "", x) # remove blank spaces at the end
  
  #tokenizing
  token = str_split(x, '\\s+')
  
  
  size = as.numeric(length(x))
  
  for(i in 1:size){
    token[[i]] = sapply(token[[i]], katadasaR)
    token[[i]] = str_c(token[[i]],collapse=' ') #untokenize
    
  }
  
  return(token)
}

probabilityMatrix <-function(docMatrix)
{
  # Sum up the term frequencies
  termSums<-cbind(colnames(as.matrix(docMatrix)),as.numeric(colSums(as.matrix(docMatrix))))
  # Add one
  termSums<-cbind(termSums,as.numeric(termSums[,2])+1)
  # Calculate the probabilties
  termSums<-cbind(termSums,(as.numeric(termSums[,3])/sum(as.numeric(termSums[,3]))))
  # Calculate the natural log of the probabilities
  termSums<-cbind(termSums,log(as.numeric(termSums[,4])))
  # Add pretty names to the columns
  colnames(termSums)<-c("term","count","additive","probability","lnProbability")
  termSums
}

getProbability <- function(testChars,probabilityMatrix)
{
  charactersFound<-probabilityMatrix[probabilityMatrix[,1] %in% testChars,"term"] #in is a matching function
  # Count how many words were not found in the positive matrix
  charactersNotFound<-length(testChars)-length(charactersFound)
  # Add the normalized probabilities for the words founds together
  charactersFoundSum<-sum(as.numeric(probabilityMatrix[probabilityMatrix[,1] %in% testChars,"lnProbability"]))
  # We use ln(1/total smoothed words) for words not found
  charactersNotFoundSum<-charactersNotFound*log(1/sum(as.numeric(probabilityMatrix[,"additive"])))
  #This is our probability
  prob<-charactersFoundSum+charactersNotFoundSum 
  prob
}
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  retrieveRawTweet <- reactive({
      search(input$topic,input$count)
  })
  
  classification <- reactive({
    withProgress(message = 'Classification :', value = 0, {
      searchterm = input$topic
      filenameNegative = c(searchterm,"TrainNegative.csv")
      filenameNegative = str_c(filenameNegative,collapse='')
      filenamePositive = c(searchterm,"TrainPositive.csv")
      filenamePositive = str_c(filenamePositive,collapse='')
      
      # Collect data
      tweetsTrain.positive<-read.csv(filenamePositive,header=T)
      setProgress(0.1, detail = "Cleaning data train")
      tweetsTrain.positive$Tweet<- clean_text(tweetsTrain.positive$Tweet)
      tweetsTrain.negative<-read.csv(filenameNegative,header=T)
      setProgress(0.35)
      tweetsTrain.negative$Tweet<- clean_text(tweetsTrain.negative$Tweet)
      setProgress(0.55, detail = "Retrieve Tweets")
      tweetsTestRaw<-retrieveRawTweet()
      setProgress(0.65, detail = "Cleaning data test")
      tweetsTest<-clean_text(tweetsTestRaw$text)

      
      setProgress(0.95, detail = "Computing")
      tweetsTrain.positive["class"]<-rep("positif",nrow(tweetsTrain.positive))
      tweetsTrain.negative["class"]<-rep("negatif",nrow(tweetsTrain.negative))
      
      # Create corpus
      tweetsTrain.positive.corpus <- Corpus(VectorSource(as.vector(tweetsTrain.positive$Tweet)))
      tweetsTrain.negative.corpus <- Corpus(VectorSource(as.vector(tweetsTrain.negative$Tweet)))
      tweetsTest.corpus <- Corpus(VectorSource(as.vector(tweetsTest)))
      
      # Create term document matrix
      tweetsTrain.positive.matrix <- t(TermDocumentMatrix(tweetsTrain.positive.corpus,control = list(wordLengths=c(3,Inf)))); #exclude word less than 3 character
      tweetsTrain.negative.matrix <- t(TermDocumentMatrix(tweetsTrain.negative.corpus,control = list(wordLengths=c(3,Inf))));
      tweetsTest.matrix <- t(TermDocumentMatrix(tweetsTest.corpus,control = list(wordLengths=c(3,Inf))));
      
      tweetsTrain.positive.pMatrix<-probabilityMatrix(tweetsTrain.positive.matrix)
      tweetsTrain.negative.pMatrix<-probabilityMatrix(tweetsTrain.negative.matrix)
      
      # Get the matrix
      tweetsTest.matrix<-as.matrix(tweetsTest.matrix)
      
      # A holder for classification 
      classified<-NULL
      
      for(documentNumber in 1:nrow(tweetsTest.matrix))
      {
        # Extract the test words
        tweetsTest.chars<-names(tweetsTest.matrix[documentNumber,tweetsTest.matrix[documentNumber,] >= 1])
        # Get the probabilities
        positiveProbability <- getProbability(tweetsTest.chars,tweetsTrain.positive.pMatrix)
        negativeProbability <- getProbability(tweetsTest.chars,tweetsTrain.negative.pMatrix)
        # Add it to the classification list
        classified<-c(classified,ifelse(positiveProbability>negativeProbability,"positive","negative"))
      }
      
      resultRaw <- cbind(tweetsTestRaw,classified)
      resultClean <-cbind(text=tweetsTest,classified)
      resultnegative <- resultClean[resultClean[, "classified"] == 'negative',"text"]
      resultpositive <- resultClean[resultClean[, "classified"] == 'positive',"text"]
      setProgress(1)
      result <- list(raw=resultRaw,negative=resultnegative,positive=resultpositive)
      result
    })
  })
  
  #ComponentAnalysis <- reactive({})
  
  output$ClassificationTable <- renderTable({
    classification()$raw
  })
  
  output$PositiveF1Table <- renderTable({
    NULL
  })
  
})
