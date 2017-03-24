library(rvest)
library(magrittr)
library(dplyr)

# reference website
url <- "http://apps.timwhitlock.info/emoji/tables/unicode"

# get emoticons
emoticons <- url %>%
  html() %>%
  html_nodes(xpath='/html/body/div[2]/div/div/table[1]') %>%
  html_table()
emoticons <- data.frame(emoticons[[1]]$Native, emoticons[[1]]$Bytes, 
                        emoticons[[1]]$Description, stringsAsFactors = FALSE)
names(emoticons) <- c("Native", "Bytes", "Description")

# get additional emoticons
addemoticons <- url %>%
  html() %>%
  html_nodes(xpath='/html/body/div[2]/div/div/table[6]') %>%
  html_table()
addemoticons <- data.frame(addemoticons[[1]]$Native, addemoticons[[1]]$Bytes, 
                           addemoticons[[1]]$Description, stringsAsFactors = FALSE)
names(addemoticons) <- c("Native", "Bytes", "Description")

# get dingbats
dingbats <- url %>%
  html() %>%
  html_nodes(xpath='/html/body/div[2]/div/div/table[2]') %>%
  html_table()
dingbats <- data.frame(dingbats[[1]]$Native, dingbats[[1]]$Bytes, 
                       dingbats[[1]]$Description, stringsAsFactors = FALSE)
names(dingbats) <- c("Native", "Bytes", "Description")

# get transports
transport <- url %>%
  html() %>%
  html_nodes(xpath='/html/body/div[2]/div/div/table[3]') %>%
  html_table()
transport <- data.frame(transport[[1]]$Native, transport[[1]]$Bytes, 
                        transport[[1]]$Description, stringsAsFactors = FALSE)
names(transport) <- c("Native", "Bytes", "Description")

# get additional transports
addtransport <- url %>%
  html() %>%
  html_nodes(xpath='/html/body/div[2]/div/div/table[7]') %>%
  html_table()
addtransport <- data.frame(addtransport[[1]]$Native, addtransport[[1]]$Bytes, 
                           addtransport[[1]]$Description, stringsAsFactors = FALSE)
names(addtransport) <- c("Native", "Bytes", "Description")

# get enclosed emoticons
enclosed <- url %>%
  html() %>%
  html_nodes(xpath='/html/body/div[2]/div/div/table[4]') %>%
  html_table()
enclosed <- data.frame(enclosed[[1]]$Native, enclosed[[1]]$Bytes, 
                       enclosed[[1]]$Description, stringsAsFactors = FALSE)
names(enclosed) <- c("Native", "Bytes", "Description")

# get uncategorized emoticons
uncategorized <- url %>%
  html() %>%
  html_nodes(xpath='/html/body/div[2]/div/div/table[5]') %>%
  html_table()
uncategorized <- data.frame(uncategorized[[1]]$Native, uncategorized[[1]]$Bytes, 
                            uncategorized[[1]]$Description, stringsAsFactors = FALSE)
names(uncategorized) <- c("Native", "Bytes", "Description")

# get additional other emoticons
addothers <- url %>%
  html() %>%
  html_nodes(xpath='/html/body/div[2]/div/div/table[8]') %>%
  html_table()
addothers <- data.frame(addothers[[1]]$Native, addothers[[1]]$Bytes, 
                        addothers[[1]]$Description, stringsAsFactors = FALSE)
names(addothers) <- c("Native", "Bytes", "Description")

# combine all dataframes to overall dataframe
alltogether <- bind_rows(list(emoticons, addemoticons, dingbats, transport, 
                              addtransport, enclosed, uncategorized, addothers))





# Post Emoticons for match-up to database

require(stringi)
library(magrittr)
library(twitteR)
library(tidyr)

# oauth
load("twitterOauthEmoticons.Rdata")
consumer_key <- my_oauth$consumerKey
consumer_secret <- my_oauth$consumerSecret
access_token <- my_oauth$oauthKey
access_secret <- my_oauth$oauthSecret
setup_twitter_oauth(consumer_key = consumer_key, consumer_secret = consumer_secret,
                    access_token = access_token, access_secret = access_secret)

# post emoticons
tweetslist <- paste0(stri_unescape_unicode(alltogether$Native)," ", alltogether$Description)
# don´t post all at once <- rate limit per 15 minutes!
lapply(tweetslist[1:100], tweet)
lapply(tweetslist[101:200], tweet)
lapply(tweetslist[201:300], tweet)
lapply(tweetslist[301:400], tweet)
lapply(tweetslist[401:500], tweet)
lapply(tweetslist[501:600], tweet)
lapply(tweetslist[601:700], tweet)
lapply(tweetslist[701:800], tweet)
lapply(tweetslist[806:length(tweetslist)], tweet)
# I posted two parts per day, 15 minutes appart

# retrieve emoticons
tweetsback <- userTimeline("Remoticons", n = 900) %>%
  twListToDF

# split one column into df with the columns "bytes" and "decription"
tweetsback2 <- data.frame(text = iconv(tweetsback$text, "latin1", "ASCII", "byte"), 
                          stringsAsFactors = FALSE)
# kind of an ugly workaround, but the bytes made it impossible to create a df the proper way
column1 <- separate(tweetsback2, text, into = c("Bytes", "Description"), sep = "\\ ")
column2 <- separate(tweetsback2, text, into = c("Bytes", "Description"), sep = "^[^\\s]*\\s")
df <- data.frame(Bytes = column1$Bytes, Description = column2$Description)

# merge retrieved encoding with original encoding to one df &v write to file
eotw <- merge(alltogether, df1, by = "Description")
names(eotw) <- c("Description", "Native", "Bytes", "R-encoding")
write.csv2(eotw, file = "emDict.csv", row.names = FALSE)



