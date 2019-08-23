install.packages("twitteR")
install.packages("dplyr")
install.packages("tidyr")

#Cargar librerias
library(twitteR)
library(dplyr)
library(tidyr)

consumerKey = "INSERT KEY HERE"
consumerSecret = "INSERT SECRET KEY HERE"
accessToken = "INSERT TOKEN HERE"
accessSecret = "INSERT SECRET TOKEN HERE"
options(httr_oauth_cache=TRUE)
setup_twitter_oauth(consumer_key = consumerKey, consumer_secret = consumerSecret,
                    access_token = accessToken, access_secret = accessSecret)

#Creamos un objeto que guarde los tweets
luisguitweets<- userTimeline("luisguillermosr", n = 3200)
#Comvertimos el objeto en una base de datos
luisguitweets_df <- tbl_df(map_df(luisguitweets, as.data.frame))
#Lo guardamos como un csv
write.csv(obamatweets_df, "luisguitweets.csv")
Buscar por hashtag
Por ejemplo si buscamos el hashtag #yeswecan
#Ponemos exclude:retweets para que no salgan los retweets
yeswecan <- searchTwitter("#yeswecan exclude:retweets", n=3200)
#convertir en base de datos
yeswecan_df <- tbl_df(map_df(yeswecan, as.data.frame))
#Exportar a csv
write.csv(yeswecan_df, "yeswecan.csv")
