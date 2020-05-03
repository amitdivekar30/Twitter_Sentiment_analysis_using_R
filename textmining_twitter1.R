#assignment on twitter

devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")
library("twitteR")
#install.packages("ROAuth")
library("ROAuth")

cred <- OAuthFactory$new(consumerKey='dO2DaDEn6O93so3nEfRmwut6R', # Consumer Key (API Key)
                         consumerSecret='tlWvCPsoEzmAUj1uUEx0C9B7ZdoatvNLDQXO0P9qmvURPE0jM4', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
#cred$handshake(cainfo="cacert.pem")
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")


#install.packages("base64enc")
library(base64enc)

#install.packages("httpuv")
library(httpuv)

#by creating twtter developer account enter API Key,API Secret,Access Token and Access Token Secret
setup_twitter_oauth("d--aDEn---3so3nEfRmw---", # Consumer Key (API Key)
                    "-lW----zmAUj1uUEx0C9B7Zdoatv---QXO0P9qmvUR---jM4", #Consumer Secret (API Secret)
                    "----92219351367----------CnnWFTmoTQu25fqrt8syB2w",  # Access Token
                    "-----------------------------------")  #Access Token Secret

#registerTwitterOAuth(cred)

Tweets <- userTimeline('BarackObama', n = 1000,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)
a<-TweetsDF$text
write.csv(TweetsDF, "Tweets.csv",row.names = F)
write.table(a,"Tweets.txt")
getwd()


#install.packages("tm")
library(tm)

#install.packages("slam")
library(slam)


#install.packages("topicmodels")
library(topicmodels)

x <- read.csv("Tweets.csv")
x
length(x)

mydata.corpus <- Corpus(VectorSource(x$text))
#inspect(VCorpus(mydata.corpus))
mydata.corpus[1]
as.character(mydata.corpus[[1]])

mydata.corpus <- tm_map(mydata.corpus, removePunctuation)
as.character(mydata.corpus[[1]])
stopwords<-readLines("stop.txt")
my_stopwords <- c(stopwords)
mydata.corpus <- tm_map(mydata.corpus, removeWords, my_stopwords)
as.character(mydata.corpus[[1]])
mydata.corpus <- tm_map(mydata.corpus, removeNumbers)
as.character(mydata.corpus[[1]])
mydata.corpus <- tm_map(mydata.corpus, stripWhitespace)
as.character(mydata.corpus[[1]])
mydata.corpus <- tm_map(mydata.corpus, content_transformer(tolower))
as.character(mydata.corpus[[1]])
mydata.corpus <- tm_map(mydata.corpus, stemDocument)
as.character(mydata.corpus[[1]])



## build a term-document matrix
dtm <- DocumentTermMatrix(mydata.corpus)
tdm<-TermDocumentMatrix((mydata.corpus))

dim(dtm)
dim(tdm)

# rowTotals <- apply(dtm, 1, sum)
# ?apply
# 
# 
# dtm.new   <- dtm[rowTotals > 0, ]
# dim(dtm.new)
# 
# tdm<-t(dtm.new)

#####################wordcloud#####################################

#install.packages("wordcloud")
library(wordcloud)
#install.packages("RColorBrewer")
library(RColorBrewer)

matrix <- as.matrix(tdm) 

words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
View(df)
set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 2,
          max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))       


####################### Sentimet Analysis ##############################

#install.packages("syuzhet")
library("syuzhet")

text <- readLines("Tweets.txt")

s_v <- get_sentences(text)
class(s_v)
str(s_v)
head(s_v)

sentiment_vector <- get_sentiment(s_v, method = "bing")
head(sentiment_vector)

afinn_s_v <- get_sentiment(s_v, method = "afinn")

head(afinn_s_v)

nrc_vector <- get_sentiment(s_v, method="nrc")
head(nrc_vector)

sum(sentiment_vector)
mean(sentiment_vector)

summary(sentiment_vector)

# plot
plot(sentiment_vector, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red")

# To extract the sentence with the most negative emotional valence
negative <- s_v[which.min(sentiment_vector)]
negative

# and to extract the most positive sentence
positive <- s_v[which.max(sentiment_vector)]
positive

# more depth
poa_v <- text
poa_sent <- get_sentiment(poa_v, method="bing")
plot(
  poa_sent, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

# percentage based figures
percent_vals <- get_percentage_values(poa_sent)

plot(
  percent_vals, 
  type="l", 
  main="Throw the ring in the volcano Using Percentage-Based Means", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence", 
  col="red"
)

ft_values <- get_transformed_values(
  poa_sent, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  scale_vals = TRUE,
  scale_range = FALSE
)

plot(
  ft_values, 
  type ="h", 
  main ="LOTR using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)

# categorize each sentence by eight emotions
nrc_data <- get_nrc_sentiment(s_v)
nrc_score_sent <- get_nrc_sentiment(negative)

nrc_score_word <- get_nrc_sentiment('grim')
# subset

sad_items <- which(nrc_data$sadness > 0)
head(s_v[sad_items])

# To view the emotions as a barplot
barplot(sort(colSums(prop.table(nrc_data[, 1:10]))), horiz = T, cex.names = 0.7,
        las = 1, main = "Emotions", xlab = "Percentage",
        col = 1:8)



