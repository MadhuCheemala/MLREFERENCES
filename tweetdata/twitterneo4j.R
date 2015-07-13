# library(RNeo4j)
# 
# graph = startGraph("http://localhost:7474/db/data/")
# 
# #alice = createNode(graph, "Person", name="Alice")
# #bob = createNode(graph, "Person", name="Bob")
# 
# #createRel(alice, "KNOWS", bob, since=2001)
# query="match n return n.name limit 25"
# st=cypher(graph,query)
# print (st)
install.packages("twitteR")
library(twitteR)

library(devtools)
library(httr)
install_github("twitteR", username = "geoffjentry")
library(twitteR)
library(ROAuth)


# 
# 
# 
# reqURL <- "https://api.twitter.com/oauth/request_token"
# accessURL <- "https://api.twitter.com/oauth/access_token"
# authURL <- "https://api.twitter.com/oauth/authorize"
# 
# consumerKey <- "CyyTv4drWdZjUeRj21dSFew3y"
# consumerSecret <- "vENuAsPlD2jX14GzNgia6SNSn0skHLOCPLnZKr8PoXfd1Merz0"
# 
twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret,
                             requestURL=reqURL,
                             accessURL=accessURL,
                             authURL=authURL)
# twitCred$handshake()
# registerTwitterOAuth(twitCred)
# ACCESS_TOKEN="1461398263-71ROW7yVzAmsltMJUNlQ4Upbwe3q7zlz35MPdml"
# ACCESS_secret=" PnMQE6jh88cpf3dZKGeoj58fBmUm5tKyyyUaxZGcS3afC"
# CUSTOMER_KEY="CyyTv4drWdZjUeRj21dSFew3y"
# CUSTOMER_SECRET="vENuAsPlD2jX14GzNgia6SNSn0skHLOCPLnZKr8PoXfd1Merz0"

library(twitteR)

api_key = "CyyTv4drWdZjUeRj21dSFew3y"
api_secret = "vENuAsPlD2jX14GzNgia6SNSn0skHLOCPLnZKr8PoXfd1Merz0"
access_token = "1461398263-71ROW7yVzAmsltMJUNlQ4Upbwe3q7zlz35MPdml"
access_token_secret = "PnMQE6jh88cpf3dZKGeoj58fBmUm5tKyyyUaxZGcS3afC"
twitCred<-setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)


save(twitCred, file = "/home/madhu/MadhuR/MLExperiments/twitCred.RData")

getwd()

tweets = searchTwitter("neo4j", n = 100, lang = "en") # Run on 27 May 2014 ~5:00PM CT
more_tweets = searchTwitter("neo4j", n = 100, lang = "en") # Run on 28 May 2014 ~9:00AM CT
even_more_tweets = searchTwitter("neo4j", n = 100, lang = "en") # Run on 29 May 2014 ~ 10:00AM CT

neo4j_tweets = c(tweets, more_tweets, even_more_tweets)


install.packages("stringr")
library(stringr)

getHashtags = function(twit) {
  hashtags = unlist(str_extract_all(twit, perl("(?<=\\s|^)#(.+?)(?=\\b|$)")))
  hashtags = tolower(hashtags)
  
  if(length(hashtags) > 0) {
    return(hashtags)
  } else {
    return(NULL)
  }
}

getRetweetSN = function(twit) {
  retweet = str_extract(twit, perl("(?<=^RT\\s@)(.+?)(?=:)"))
  
  if(!is.na(retweet)) {
    return(retweet)
  } else {
    return(NULL)
  }
}

getMentions = function(twit) {
  mentions = unlist(str_extract_all(twit, perl("(?<!^RT\\s@|^@)(?<=@)(.+?)(?=\\b|$)")))
  
  if(length(mentions) > 0) {
    return(mentions)
  } else{
    return(NULL)
  }
}


library(RNeo4j)
graph = startGraph("http://localhost:7474/db/data/")
clear(graph)

addConstraint(graph, "Tweet", "id")
addConstraint(graph, "User", "username")
addConstraint(graph, "Hashtag", "hashtag")


create_db = function(x) {
  tweet = getOrCreateNode(graph, "Tweet", id = x$id, text = x$text)
  user = getOrCreateNode(graph, "User", username = x$screenName)
  createRel(user, "TWEETED", tweet)
  
  reply_to_sn = x$replyToSN
  
  if(length(reply_to_sn) > 0) {
    reply_user = getOrCreateNode(graph, "User", username = reply_to_sn)
    createRel(tweet, "IN_REPLY_TO", reply_user)
  }
  
  retweet_sn = getRetweetSN(x$text)
  
  if(!is.null(retweet_sn)) {
    retweet_user = getOrCreateNode(graph, "User", username = retweet_sn)
    createRel(tweet, "RETWEET_OF", retweet_user)
  }
  
  hashtags = getHashtags(x$text)
  
  if(!is.null(hashtags)) {
    hashtag_nodes = lapply(hashtags, function(h) getOrCreateNode(graph, "Hashtag", hashtag = h))
    lapply(hashtag_nodes, function(h) createRel(tweet, "HASHTAG", h))
  }
  
  mentions = getMentions(x$text)
  
  if(!is.null(mentions)) {
    mentioned_users = lapply(mentions, function(m) getOrCreateNode(graph, "User", username = m))
    lapply(mentioned_users, function(u) createRel(tweet, "MENTIONED", u))
  }
} 

lapply(neo4j_tweets, create_db)


summary(graph)

query = "MATCH (h:Hashtag)<-[:HASHTAG]-(:Tweet)-[:HASHTAG]->(:Hashtag {hashtag:{hashtag}}) 
         WHERE h.hashtag <> {hashtag}
RETURN h.hashtag AS hashtag, COUNT(*) AS count
ORDER BY count DESC
LIMIT 10"

hashtag_count = cypher(graph, query, hashtag = "#neo4j")

hashtag_coun

install.packages("ggplot2")
library(ggplot2)

ggplot(hashtag_count, aes(x = reorder(hashtag, count), y = count)) + 
  geom_bar(stat = "identity", fill = "darkblue") +
  coord_flip() +
  labs(x = "Hashtag", 
       y = "Count", 
       title = "Count of Hashtag Co-Occurrence with #Neo4j") +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        plot.title = element_text(size = 16, color = "black"))








