rm(list = ls())

install.packages("twitteR")
install.packages("base64enc")
install.packages("devtools")
devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0", force = TRUE)

library(twitteR)
library(base64enc)
library(httr)
library(devtools)

consumer_key <- 'uGlogjo4W072spErUDrAhZIKb'
consumer_secret <- 'u4afHOaFnYBlY9SvWTQsvPQbbeNj47DjKs2OBgtJBJZlL1Y7Zr'
access_token <- '450709215-rEynEuyB46ALbVtcrNEqfOIYIeJDlptSsWo32XEh'
access_secret <- 'gkTuLIoCecSJe6rhSENSYgxs3K0S575SarMeJdUIW4V6k'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


LiverpoolFC.Tweets = searchTwitter('@lfc', n = 500)
length(LiverpoolFC.Tweets)
class(LiverpoolFC.Tweets)

firsttweet = LiverpoolFC.Tweets[[1]]
class(firsttweet)
firstT <- firsttweet$getText()
firsttweet$getScreenName()

liverpoolfc.text = lapply(LiverpoolFC.Tweets, function(t)
  t$getText() ) 
class(liverpoolfc.text)
length(liverpoolfc.text)
head(liverpoolfc.text, 5)
liverpoolfc.text[300:310]

xE2x9DxA4

threeohseventweet = LiverpoolFC.Tweets[[307]]
# Necessity make input into score.sentiment function as extract from -$getText !!!
threetweet = threeohseventweet$getText()
threetweet
threeohseventweet$getScreenName()

# Emoji decoder ???????????????????????? ugh. 

emoji.decoder = scan('~/R materials/emoticonsList.txt')


# Sentiment Lexicon (Hu & Liu)


hu.liu.pos = scan('~/R materials/opinion_lexicon_english/positive-words.txt', 
                  what = ' character', comment.char = ';')
hu.liu.neg = scan('~/R materials/opinion_lexicon_english/negative-words.txt', 
                  what = ' character', comment.char = ';')

pos.words = c(hu.liu.pos, 'upgrade')
neg.words = c(hu.liu.neg, 'stoopid', 'duh')

score.sentiment = function(sentences, pos.words, neg.words, .progress = 'none')
{
  require(plyr)
  require(stringr)
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = tolower(sentence)
    
    
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
    
  }, pos.words, neg.words, .progress = .progress )
  
  scores.df = data.frame(scores = scores, text = sentences)
  return(scores.df)
}


library(plyr)
library(stringr)


# Use with sample string. 
sample = c("You're awesome and I like you", "I love lamp 0x1F4Af")

result = score.sentiment(sample, pos.words, neg.words)
result$scores

# With actual tweet. 
firstT
result2 = score.sentiment(firstT, pos.words, neg.words)
result2$scores


as.character(firsttweet)
Encoding(firsttweet)


result = score.sentiment(threetweet, pos.words, neg.words)
result$scores
result

str(threetweet)

result = score.sentiment(liverpoolfc.text, pos.words, neg.words)
result$scores

result = score.sentiment(liverpoolfc.text[1], pos.words, neg.words)
result$scores

result = score.sentiment(liverpoolfc.text[4], pos.words, neg.words)
result$scores


unicode <- 0x1F4Af
unicode
# Multibyte Version
intToUtf8(unicode)

# Byte-pair Version
hilo <- unicode2hilo(unicode)
intToUtf8(hilo)

?scan()


emoticon.dict <- as.data.frame(read.csv("~/R materials/emdict.csv"))


