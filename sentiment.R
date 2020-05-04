rm(list=ls()); cat("\014") # clear all
#install.packages('rvest')
library(rvest)
#install.packages('tm')
library(tm)
#install.packages("SnowballC")
library(SnowballC)
#install.packages("wordcloud")
library(wordcloud)
#install.packages('ROAuth')
require(ROAuth)
#install.packages('tidyquant')
library(tidyquant)
#install.packages('timetk')
library(timetk)
#install.packages("tidyverse")
library(tidyverse)
#install.packages('bitops')
library("bitops")
#install.packages('RCurl')
library("RCurl")
#install.packages('rjson')
library("rjson")
#install.packages('plotly')
library(plotly)
#install.packages('quantmod')
library(quantmod)
#install.packages('twitteR')
library("twitteR")

# Use your own Twitter Developer API Key and Secret
t.api.key <- "3EFoR1Gl0lGZlRGsoTsucAGrj"
t.api.secret <- "ZdKsGKdM2VcLArTolDWJCCt1wwlNNK976a5cem0T1hhhsiiUU9"
t.access.token <- "1199914297616408577-CGUG5gdJhORzbNXfmJRC4cl7XQf2ig"
t.access.secret <- "jTClBN3lPXQjl3yjAaZswqbpzOJdnmrHYafkKvdHOLnsc"

# ---------  Save Credentials --------- 
setup_twitter_oauth(consumer_key = t.api.key, consumer_secret = t.api.secret, 
                    access_token = t.access.token, 
                    access_secret = t.access.secret)
save(list=(c("t.api.key","t.api.secret","t.access.token","t.access.secret")), file="twitter_credentials.RData")

# load twitter credentials

t.cred <- load("twitter_credentials.RData")
setup_twitter_oauth(t.api.key, t.api.secret, t.access.token, t.access.secret)

# Get top three gainers

# As tweets keep changing I have saved the tweets file(collected on 2019-12-05) and attached in the folder

# A. GET TWEETS OF LOSERS
twtMSFT <- searchTwitter('$MSFT', n = 100)
twtAMZN <- searchTwitter('$AMZN', n = 100)
twtAAPL <- searchTwitter('$AAPL', n = 100)
save(twtMSFT, file="twtMSFT")   
save(twtAMZN, file="twtAMZN")
save(twtAAPL, file="twtAAPL")
load(file = "twtMSFT")
load(file = "twtAMZN")
load(file = "twtAAPL")

losers = c(twtMSFT,twtAMZN,twtAAPL)

# A. GET TWEETS OF GAINERS
twtLBTYB <- searchTwitter('$LBTYB', n = 100)
twtDISCB <- searchTwitter('$DISCB', n = 100)
twtASGN <- searchTwitter('$ASGN', n = 100)
save(twtLBTYB, file="twtLBTYB")
save(twtDISCB, file="twtDISCB")
save(twtASGN, file="twtASGN")
load(file = "twtLBTYB")
load(file = "twtDISCB")
load(file = "twtASGN")

gainers = c(twtLBTYB,twtDISCB,twtASGN)

#display tweets

display.tweet <- function (tweets) 
{
  cat("Screen name:", tweets$getScreenName(), "\nText:", tweets$getText(), "\n\n")
}

for (t in twtASGN)
{
  display.tweet(t)
}

# B. CREATE TWO DATA CORPUS FOR THE TWEETS
getCorpus <- function (tweets) {
  tweets.text <- lapply(tweets, function(t) {t$getText()})
  data.source <- VectorSource(tweets.text)
  data.corpus <- Corpus(data.source)
  return (data.corpus)
}

gainers.corp <- getCorpus(gainers)
gainers.df<-data.frame(text=unlist(sapply(gainers.corp, `[`, "content")),stringsAsFactors=F)
writeCorpus(gainers.corp)

losers.corp = getCorpus(losers)
losers.df<-data.frame(text=unlist(sapply(losers.corp, `[`, "content")),stringsAsFactors=F)
writeCorpus(losers.corp)

# C. PRE-PROCESS DATA
removeURL <- function(x) {  
  gsub("(http[^ ]*)", "", x) 
}

removeNumberWords<-function(x) {
  gsub("([[:digit:]]+)([[:alnum:]])([[:cntrl:]])*", "", x)
  iconv(x, "latin1", "ASCII", sub="")
}

getTransCorpus<-function (data.corpus) {
  data.corpus<-tm_map(data.corpus, content_transformer(removeURL))
  data.corpus<-tm_map(data.corpus, content_transformer(removePunctuation))
  data.corpus<-tm_map(data.corpus, content_transformer(tolower)) 
  english.stopwords<-stopwords("en")
  data.corpus<-tm_map(data.corpus,content_transformer(removeWords),english.stopwords)
  data.corpus<-tm_map(data.corpus, content_transformer(removeNumberWords))
  data.corpus<-tm_map(data.corpus,content_transformer(stemDocument))
  data.corpus<-tm_map(data.corpus,content_transformer(stripWhitespace))
  return (data.corpus)
}

tgainers.corp = getTransCorpus(gainers.corp)
tlosers.corp = getTransCorpus(losers.corp)


# D. CREATE TERM DOCUMENT MATRIX FOR EACH TWEET
tdmGainers <- TermDocumentMatrix(tgainers.corp, control = list(wordLengths = c(3,10)))
tdmLosers <- TermDocumentMatrix(tlosers.corp, control = list(wordLengths = c(3,10)))

save(tdmGainers, file="tdmGainers")
save(tdmLosers, file="tdmLosers")

#Converting TDM to a matrix
m1 <- as.matrix(tdmGainers)

m2 <- as.matrix(tdmLosers)

#Calculate the frequency of words 
wordFreq1 <- rowSums(m1)
wordFreq2 <- rowSums(m2)

#Sort the words by descending order of frequency
wordFreq1 <- sort(wordFreq1, decreasing=TRUE)
wordFreq2 <- sort(wordFreq2, decreasing=TRUE)

#Frequent terms sets
findFreqTerms(tdmGainers, lowfreq=10)
findFreqTerms(tdmLosers, lowfreq=10)

#Top 10 words in Gainers and Losers corpus
wordFreq1[1:10]
wordFreq2[1:10]

par(mfrow=c(1,2))
set.seed(137)
barplot(wordFreq1[1:10], col = 'blue', main='Top 10 words of Gainers stocks', las=2, ylim = c(0,100))
barplot(wordFreq2[1:10], col = 'blue', main='Top 10 words in Losers stocks', las=2)
par(mfrow=c(1,1))

#Word cloud
#install.packages('wordcloud')
library(wordcloud)

palette <- brewer.pal(8,"Dark2")
palette
par(mfrow=c(1,2))
set.seed(137)
wordcloud(words=names(wordFreq1), 
          freq=wordFreq1, 
          min.freq=10, 
          random.order=F,
          colors=palette)

wordcloud(words=names(wordFreq2), 
          freq=wordFreq2, 
          min.freq=10, 
          random.order=F,
          colors=palette)
par(mfrow=c(1,1))

#Lexicons
# positive-words.txt and negative-words.txt attached in the folder
pos.words = scan('positive-words.txt', what='character', comment.char=';')

neg.words = scan('negative-words.txt', what='character', comment.char=';')

head(pos.words)

head(neg.words)

sentiment <- function(text, pos.words, neg.words)
{
  text <- gsub('[[:punct:]]', '', text)
  text <- gsub('[[:cntrl:]]', '', text)
  text <- gsub('\\d+', '', text) # \\d represents digit
  text <- tolower(text)
  
  #Split the text into a vector of words
  words <- strsplit(text, '\\s+')
  words <- unlist(words)
  
  #Find which words are positive
  pos.matches <- match(words, pos.words)
  pos.matches <- !is.na(pos.matches)
  
  #Find which words are negative
  neg.matches <- match(words, neg.words)
  neg.matches <- !is.na(neg.matches)
  
  #Calculate the sentiment score
  score <- sum(pos.matches) - sum(neg.matches)
  cat (" Positive: ", words[pos.matches], "\n")
  cat (" Negative: ", words[neg.matches], "\n")
  return (score)
}


save(list="gainers",
     file="tweetgainer.RData")

save(list="losers",
     file="tweetloser.RData")

load(file="tweetgainer.RData")
load(file="tweetloser.RData")


tweetgainer.texts <- lapply(gainers, function(t) {
  iconv(t$getText(), "latin1", "ASCII", sub="")})


tweetloser.texts <- lapply(losers, 
                           function(t) {
                             iconv(t$getText(), "latin1", "ASCII", sub="")})

sink(tempfile())

#Sapply works on a list or vector of data
scores1 <- sapply(tweetgainer.texts, sentiment, pos.words, neg.words)


scores2 <- sapply(tweetloser.texts, sentiment, pos.words, neg.words)


sink()


table(scores1)
table(scores2)
par(mfrow=c(1,2))

barplot(table(scores1), 
        xlab="Scores", ylab="Count",
        col="cyan", main = 'Sentiment scores of Gainers')

grid(nx=NA,ny=NULL,col=rgb(165,165,165,max=255),lty=1)

barplot(table(scores2), 
        xlab="Score", ylab="Count",
        col="cyan", main= 'Sentiment scores of Losers')

grid(nx=NA,ny=NULL,col=rgb(165,165,165,max=255),lty=1)

par(mfrow=c(1,1))


#CANDLESTICK FOR LBTYB
getSymbols("LBTYB",src='yahoo')

gf1 <- data.frame(Date=index(LBTYB),coredata(LBTYB))
gf1 <- with(gf1, gf1[(gf1$Date >= "2019-12-04" & gf1$Date <= "2019-12-10"),])

lbtybcsc <- gvisCandlestickChart(gf1, xvar = "Date", low = "LBTYB.Low", open = "LBTYB.Open",
                                close = "LBTYB.Close", high = "LBTYB.High",
                                options= list (legend="top", height=400, width=500))
plot(lbtybcsc)

#CANDLESTICK FOR DISCB
getSymbols("DISCB",src='yahoo')

gf2 <- data.frame(Date=index(DISCB),coredata(DISCB))
gf2 <- with(gf2, gf2[(gf2$Date >= "2019-12-04" & gf2$Date <= "2019-12-10"),])

discbcsc <- gvisCandlestickChart(gf2, xvar = "Date", low = "DISCB.Low", open = "DISCB.Open",
                                close = "DISCB.Close", high = "DISCB.High",
                                options= list (legend="top", height=400, width=500))
plot(discbcsc)

#CANDLESTICK FOR ASGN
getSymbols("ASGN",src='yahoo')

gf3 <- data.frame(Date=index(ASGN),coredata(ASGN))
gf3 <- with(gf3, gf3[(gf3$Date >= "2019-12-04" & gf3$Date <= "2019-12-10"),])

asgncsc <- gvisCandlestickChart(gf3, xvar = "Date", low = "ASGN.Low", open = "ASGN.Open",
                              close = "ASGN.Close", high = "ASGN.High",
                              options= list (legend="top", height=400, width=500))
plot(asgncsc)

#CANDLESTICK FOR MSFT
getSymbols("MSFT",src='yahoo')

df <- data.frame(Date=index(MSFT),coredata(MSFT))
df <- with(df, df[(df$Date >= "2019-12-04" & df$Date <= "2019-12-10"),])

msftcsc <- gvisCandlestickChart(df, xvar = "Date", low = "MSFT.Low", open = "MSFT.Open",
                                close = "MSFT.Close", high = "MSFT.High",
                                options= list (legend="top", height=400, width=500))
plot(msftcsc)

#CANDLESTICK FOR AAPL
getSymbols("AAPL",src='yahoo')

df1 <- data.frame(Date=index(AAPL),coredata(AAPL))
df1 <- with(df1, df1[(df1$Date >= "2019-12-04" & df1$Date <= "2019-12-10"),])

aaplcsc <- gvisCandlestickChart(df1, xvar = "Date", low = "AAPL.Low", open = "AAPL.Open",
                                close = "AAPL.Close", high = "AAPL.High",
                                options= list (legend="top", height=400, width=500))
plot(aaplcsc)


#CANDLESTICK FOR AMZN
getSymbols("AMZN",src='yahoo')

df2 <- data.frame(Date=index(AMZN),coredata(AMZN))
df2 <- with(df2, df2[(df2$Date >= "2019-12-04" & df2$Date <= "2019-12-10"),])

amzncsc <- gvisCandlestickChart(df2, xvar = "Date", low = "AMZN.Low", open = "AMZN.Open",
                               close = "AMZN.Close", high = "AMZN.High",
                               options= list (legend="top", height=400, width=500))
plot(amzncsc)


#Cummilative returns for 1$ invested on the stocks on 12-05-2019

#Calculating the daily returns for multiple stocks
tickers <- c("LBTYB","DISCB","ASGN","MSFT","AAPL","AMZN")

multpl_stocks <- tq_get(tickers,
                        from = "2019-12-05",
                        to = "2019-12-18",
                        get = "stock.prices")

multpl_stock_daily_returns <- multpl_stocks %>%
  group_by(symbol) %>%  # Need to group multiple stocks
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'returns')

  


multpl_stock_daily_returns %>%
  group_by(symbol) %>%  # Need to group multiple stocks
  mutate(cr = cumprod(1 + returns)) %>%
  mutate(cumulative_returns = cr - 1) %>%
  ggplot(aes(x = date, y = cumulative_returns+1, color = symbol)) +
  geom_line(size = 1) +
  labs(x = "Date", y = "Cumulative Returns") +
  ggtitle("Value of 1$ invested on 2019/12/05") +
  theme_bw()


