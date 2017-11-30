# Objective: Create Shiny App that will:
# 1. Extract Asthma Related Tweets from Twitter
#   a. Allow user to select from date OR Number of Tweets
# 2. Clean data then present Simple Word Cloud
# 3. Present Simple Term Frequencies
# 4. Sentiment Analysis with Simple Positive and Negative Word Cloud
# 5. Sentiment with Multiple Emotions

# Load libraries

options(stringsAsFactors = F)
library(RMySQL) # for extracting data 
library(psych) # for plotting 
library(tm) # for NLP
library(ggplot2) # for plotting 
library(ggthemes) # for plotting
library(qdap) # for sentiment analysis
library(stringr)

rm(list=ls())


# connect to server

con <- dbConnect(MySQL(),
                 user = 'kimchon',
                 password = 'CkMj1527!%@&',
                 host = '140.226.57.147',
                 dbname='testdb',
                 port =3306)


# Create twitter dataframe containing text regarding SABA
twitter <- dbReadTable(conn = con, name = 'twitter')

# Options to choose

# 1. Based on number of tweets people want to go back and retrieve
twit1k <- twitter[((nrow(twitter)-(num_tweets_want-1)):(nrow(twitter))), ]

# 2. Based on the number of dates people want to go back and retrieve
twitter_analysis_set <- subset(twitter,  (Sys.Date() - as.Date(created_at)) <= 2 )



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


#clean corpus
clean.corpus<-function(corpus){
  custom.stopwords <- c(stopwords('english'), 'lol',
                        'smh', 'delta', 'amp', 'asthma')
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


meta.data.reader <- readTabular(mapping=list(content=
                                               'text', id='id'))

corpus = VCorpus(DataframeSource(twit1k),
                 readerControl=list(reader=meta.data.reader))

#Clean the corpus using the function
corpus <- clean.corpus(corpus) 

as.list(corpus)[1] #check information regarding ID = 1. 
#First row has been significantly reduced! At least removed half the characters from previous... good or bad?

#Create document term matrix and term document matrix
dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))
tdm<-TermDocumentMatrix(corpus,control=list(weighting
                                            =weightTf))

# find term frequencies
tdm.tweets.m<-as.matrix(tdm)
term.freq<-rowSums(tdm.tweets.m)
freq.df<-data.frame(word=
                      names(term.freq),frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]

freq.df$word<-factor(freq.df$word,
                     levels=unique(as.character(freq.df$word)))

# plot term frequencies
ggplot(freq.df[1:20,], aes(x=word,
                           y=frequency))+geom_bar(stat="identity",
                                                  fill='darkred')+coord_flip()+theme_gdocs()+
  geom_text(aes(label=frequency),
            colour="white",hjust=1.25, size=5.0)


#associations with asthma

# shiny option 1
# changing the document term frequency based on  which associations we want to look at 

associations<-findAssocs(tdm, 'risk', 0.2)
associations<-as.data.frame(associations)
associations$terms<-row.names(associations)
associations$terms<-factor(associations$terms,
                           levels=associations$terms)

# plot words associated with risk
ggplot(associations, aes(y=terms)) +
  geom_point(aes(x=risk), data=associations,
             size=5)+
  theme_gdocs()+ geom_text(aes(x=risk,
                               label=risk),
                           colour="darkred",hjust=-.25,size=8)+
  theme(text=element_text(size=20),
        axis.title.y=element_blank())


#word cloud associated with asthma
library(wordcloud)
head(freq.df)

wordcloud(freq.df$word,freq.df$frequency, max.words =
            100, colors=c('black','darkred'))


# Sentiment analysis 

# CLean twitter comments

clean_tweet = gsub("&amp", "", twit1k$text)
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

polarity('ROFL, look at that!', poliarity.frame = all.polarity)

polarity('whatever you say, kappa.', polarity.frame =
           all.polarity)
polarity('whatever you say, kappa.')

twit_pol <- polarity(clean_tweet)

# check polarity distribution... mostly centered around 0 but more negatives than positive
ggplot(twit_pol$all, aes(x=polarity,
                        y=..density..)) + theme_gdocs() +
  geom_histogram(binwidth=.25,
                 fill="darkred",colour="grey60", size=.2) +
  geom_density(size=.75)

# add polarity back in 
twit1k$polarity<-scale(twit_pol$all$polarity)

# get positive comments 
pos.comments<-subset(twit1k$text,
                     twit1k$polarity>0)
neg.comments<-subset(twit1k$text,
                     twit1k$polarity<0)

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

# make comparison cloud (green positive) (red negative)
comparison.cloud(all.tdm.m, max.words=100,
                 colors=c('darkgreen','darkred'))


# sentiment by emotions
install.packages("https://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz", repos = NULL, type = "source")
install.packages("https://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz", repos = NULL, type = "source")

library(Rstem)
library(sentiment)

data(emotions)
tail(emotions)

emotions.df<-as.data.frame(classify_emotion(
  clean_tweet))

ggplot(emotions.df, aes(x=BEST_FIT)) +
  geom_bar(aes(y=..count.., fill=BEST_FIT)) +
  labs(x="emotion categories",
       y="Asthma Tweets")+theme_gdocs() +
  theme(legend.position="none")


twit1k$emotions <-(emotions.df$BEST_FIT)
emotion.reviews <-split(twit1k$text,
                        twit1k$emotions)
emotion.reviews <-lapply(emotion.reviews,
                         paste,collapse=" ")
emotion.reviews <-do.call(c,emotion.reviews)
emotion.reviews <-VCorpus(VectorSource(
  emotion.reviews))
all.tdm <-TermDocumentMatrix(emotion.reviews,control=
                               list(weighting=weightTfIdf, removePunctuation = TRUE,
                                    stopwords=stopwords(kind='en')))
all.tdm.m<-as.matrix(all.tdm)
colnames(all.tdm.m)<-levels(twit1k$emotions)
comparison.cloud(all.tdm.m)
