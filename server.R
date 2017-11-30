#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


library(shiny)
options(stringsAsFactors = F)
library(RMySQL) # for extracting data 
library(psych) # for plotting 
library(tm) # for NLP
library(ggplot2) # for plotting 
library(ggthemes) # for plotting
library(qdap) # for sentiment analysis
library(stringr)
library(Rstem)
library(sentiment)
library(wordcloud)
library(tidytext)
library(tidyr)
library(dplyr)
library(Hmisc)
library(mgcv)

# data for emotions, sentiments
data(emotions)
data(sentiments)
source("ScoreSentiment.R")
# subset the sentiment data into different lexicons
afinn<-subset(sentiments,sentiments$lexicon=='AFINN')
bing<-subset(sentiments,sentiments$lexicon=='bing')
nrc<-subset(sentiments,sentiments$lexicon=='nrc')



# SSH


# # for local use setwd("~/Documents/Projects/Shiny/asthma_tweet/asthma_tweet_analysis/")
# #setwd("~/Documents/Projects/Shiny/asthma_tweet/asthma_tweet_analysis/")
# #setwd('/srv/shiny-server/asthma_tweet_analysis')
# # Change permissions on SSH keys
# pubkey<-paste0(getwd(),"/data/id_rsa.pub")
# pvtkey<-paste0(getwd(),"/data/id_rsa")
# 
# system(paste0("chmod 600 ",pubkey))
# system(paste0("chmod 600 ",pvtkey))
# 
# # Open SSH connection
# 
# system(paste0("ssh -v -f -N -o StrictHostKeyChecking=no -i ",pvtkey," -R 3307:localhost:3306 ck@140.226.57.147 sleep 20"))
# 
# #MySQL connection here we set it up as a local host connection but in reality it will connect 
# 
# con <- dbConnect(MySQL(),
#                  user = 'kimchon',
#                  password = 'CkMj1527!%@&',
#                  host = '127.0.0.1',
#                  dbname='testdb',
#                  port =3307)
# 
# # Create twitter dataframe containing text regarding SABA
# twitter <- dbReadTable(conn = con, name = 'twitter')
# dbDisconnect(con)

twitter <- read.csv("twitter.csv")

############ FUNCTIONS

#Lowercase for everything
tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

# clean corpus
clean.corpus<-function(corpus){
  custom.stopwords <- c(stopwords('english'), 'lol',
                        'smh', 'delta', 'amp', 'asthma',"pussy")
  corpus <- tm_map(corpus,
                   content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords,
                   custom.stopwords) #Here we are not using any custom stop words... if we deem it necessary then we can add custom stop words 
  #custom.stopwords <- c(stopwords('english'), 'lol', 'smh', 'delta')
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  return(corpus)
}



meta.data.reader <- readTabular(mapping=list(content='text', id='id'))



# ShinyServer({})


shinyServer(function(input, output) {
  

  passData <- reactive({
    # choose twitter data based on date
    twitter <- twitter[as.Date(twitter$created_at)  %in%
                         seq.Date(input$dateRange[1],
                                  input$dateRange[2], by = "days"),]
    # choose twitter data based on the number of tweets 
    twitter <- twitter[((nrow(twitter)-(input$nTweet-1)):(nrow(twitter))), ]
    
    corpus = VCorpus(DataframeSource(twitter),
                     readerControl=list(reader=meta.data.reader))
    corpus <- clean.corpus(corpus)
    tdm<-TermDocumentMatrix(corpus,control=list(weighting=weightTf))
    
    tdm.tweets.m<-as.matrix(tdm)
    term.freq<-rowSums(tdm.tweets.m)
    freq.df<-data.frame(word= names(term.freq),frequency=term.freq)
    freq.df<-freq.df[order(freq.df[,2], decreasing=T),]
    
    freq.df$word<-factor(freq.df$word,
                         levels=unique(as.character(freq.df$word)))
    
    freq.df #final dataset for ggploting
    
  })
  
  passData2 <- reactive({
    
    twitter <- twitter[as.Date(twitter$created_at)  %in%
                         seq.Date(input$dateRange[1],
                                  input$dateRange[2], by = "days"),]
    
    twitter <- twitter[((nrow(twitter)-(input$nTweet-1)):(nrow(twitter))), ]
    
    
    clean_tweet = gsub("&amp", "", twitter$text)
    clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
    clean_tweet = gsub("@\\w+", "", clean_tweet)
    clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
    clean_tweet = gsub("[[:digit:]]", "", clean_tweet)
    clean_tweet = gsub("http\\w+", "", clean_tweet)
    clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
    clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet) 
    clean_tweet=str_replace_all(clean_tweet,"[^[:graph:]]", " ") 
    
    new.pos<-c('rofl','lol') # define positive words
    old.pos<-subset(as.data.frame(key.pol),key.pol$y==1) # key.pol has polarity lookup from 
    all.pos<-c(new.pos,old.pos[,1]) # positive words!
    new.neg<-c('kappa','meh') # define negative words
    old.neg<-subset(as.data.frame(key.pol),key.pol$y==-1)
    all.neg<-c(new.neg,old.neg[,1])
    all.polarity<-sentiment_frame(all.pos,all.neg,1,-1)
    
    twit_pol <- polarity(clean_tweet)
    
    twitter$polarity<-scale(twit_pol$all$polarity)
    
    # get positive comments 
    pos.comments<-subset(twitter$text,
                         twitter$polarity>0)
    neg.comments<-subset(twitter$text,
                         twitter$polarity<0)
    
    # pass all through VCorpus
    pos.terms<-paste(pos.comments,collapse = " ")
    neg.terms<-paste(neg.comments,collapse = " ")
    all.terms<-c(pos.terms,neg.terms)
    all.corpus<-VCorpus(VectorSource(all.terms))
    
    all.tdm<-TermDocumentMatrix(all.corpus,
                                control=list(weighting=weightTfIdf, removePunctuation =
                                               TRUE,stopwords=stopwords(kind='en')))
    # switch to matrix form
    all.tdm.m<-as.matrix(all.tdm)
    colnames(all.tdm.m)<-c('positive','negative')
    all.tdm.m
  })
 
  
  passData3 <- reactive({
    
    twitter <- twitter[as.Date(twitter$created_at)  %in%
                         seq.Date(input$dateRange[1],
                                  input$dateRange[2], by = "days"),]
    
    twitter <- twitter[((nrow(twitter)-(input$nTweet-1)):(nrow(twitter))), ]
    
    
    clean_tweet = gsub("&amp", "", twitter$text)
    clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
    clean_tweet = gsub("@\\w+", "", clean_tweet)
    clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
    clean_tweet = gsub("[[:digit:]]", "", clean_tweet)
    clean_tweet = gsub("http\\w+", "", clean_tweet)
    clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
    clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet) 
    clean_tweet=str_replace_all(clean_tweet,"[^[:graph:]]", " ") 
    
    emotions.df<-as.data.frame(classify_emotion(clean_tweet))
    twitter$emotions <-(emotions.df$BEST_FIT)
    emotion.reviews <-split(twitter$text,
                            twitter$emotions)
    emotion.reviews <-lapply(emotion.reviews,
                             paste,collapse=" ")
    emotion.reviews <-do.call(c,emotion.reviews)
    emotion.reviews <-VCorpus(VectorSource(
      emotion.reviews))
    all.tdm <-TermDocumentMatrix(emotion.reviews,control=
                                   list(weighting=weightTfIdf, removePunctuation = TRUE,
                                        stopwords=stopwords(kind='en')))
    all.tdm.m<-as.matrix(all.tdm)
    colnames(all.tdm.m)<-levels(addNA(twitter$emotions))[-7]
    
    all.tdm.m
    
  })
  
  passData4 <- reactive({
    
    twitter <- twitter[as.Date(twitter$created_at)  %in%
                         seq.Date(input$dateRange[1],
                                  input$dateRange[2], by = "days"),]
    
    twitter <- twitter[((nrow(twitter)-(input$nTweet-1)):(nrow(twitter))), ]
    
    twitter <- twitter[as.Date(twitter$created_at)  %in%
                         seq.Date(input$dateRange[1],
                                  input$dateRange[2], by = "days"),]
    
    clean_tweet = gsub("&amp", "", twitter$text)
    clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
    clean_tweet = gsub("@\\w+", "", clean_tweet)
    clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
    clean_tweet = gsub("[[:digit:]]", "", clean_tweet)
    clean_tweet = gsub("http\\w+", "", clean_tweet)
    clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
    clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet) 
    twitter$clean_text <- str_replace_all(clean_tweet,"[^[:graph:]]", " ") 
    
    # Here we need to do the cleaning operation per time point...
    twitter$scores <- score.sentiment(twitter$clean_text, positive.words, negative.words)[,1]
    twitter$hours <- difftime(as.POSIXct(twitter$created_at) , input$dateRange[1], units = 'hours')
    
    twitter
    # corpus = VCorpus(DataframeSource(twitter),
    #                  readerControl=list(reader=meta.data.reader))
    # corpus <- clean.corpus(corpus)
    # twit_dtm<-DocumentTermMatrix(corpus)
    # twit_tidy <- tidy(twit_dtm)
    # colnames(twit_tidy)<-c('line_number','word','count')
    # twit_tidy$line_number<-as.numeric(twit_tidy$line_number)
    # 
    # # use nrc to get joy words 
    # nrc.joy <- sentiments %>%
    #   filter(lexicon == "nrc", sentiment == "joy")
    # 
    # joy.words<-twit_tidy %>%
    #   inner_join(nrc.joy) %>%
    #   count(word, sort = TRUE)
    # 
    # bing <- sentiments %>%
    #   filter(lexicon == "bing") %>%
    #   select(-score)
    # 
    # twit_sentiment <- twit_tidy %>%
    #   inner_join(bing) %>%
    #   count(line_number, index = line_number, sentiment) %>%
    #   spread(sentiment, n, fill = 0) %>%
    #   mutate(polarity = positive - negative) %>%
    #   mutate(pos=ifelse(polarity >= 0, "pos",
    #                     "neg"))
    # 
    # twit_sentiment
  
  })  
  
  
  
    output$textdisplay <- renderText({
      
      paste(
        length(seq.Date(input$dateRange[1], input$dateRange[2], by = "days")),
        " days are summarized. Within the Number of Tweets Selected by Yourself, there were", dim(unique(passData()))[1], "unique words tweeted in this time period."
      )
      
    })
  
  output$tfGraph <- renderPlot({

    graphData <- passData()
    theGraph <- ggplot(graphData[1:20,], aes(x=word, y=frequency))+
      geom_bar(stat="identity", fill='darkred')+coord_flip()+theme_gdocs()+
      geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)

    print(theGraph)

  })

  output$swcGraph <- renderPlot({
    
    graphData <- passData()
    theGraph <- wordcloud(graphData$word,graphData$frequency, max.words = input$nWords, colors=brewer.pal(8, "Dark2"))
    
    print(theGraph)
  }, height = 900, width = 900)
  
  
  output$sswcGraph <- renderPlot({
    # make comparison cloud (green positive) (red negative)
    graphData <- passData2()
    theGraph <- comparison.cloud(graphData, 
                                 max.words=input$nWords, 
                                 colors=c('darkgreen','darkred'),
                                 title.size = 3)
    
    
    print(theGraph)
  }, height = 1050, width = 1000)  
  
  output$cswcGraph <- renderPlot({
    
    graphData <- passData3()
    theGraph <- comparison.cloud(graphData, 
                                 max.words = input$nWords, 
                                 random.order = F,
                                 title.size = 3)
    print(theGraph)
  }, height = 1050, width = 900)  
  
  
  output$smoothGraph <- renderPlot({
    
    graphData <- passData4()
    
    twit_smooth <- ggplot(graphData, aes(hours, scores))
    
    theGraph <- twit_smooth + stat_smooth()+theme_gdocs()
    
    print(theGraph)
  }, height = 1000, width = 900)  
  
  }

)

