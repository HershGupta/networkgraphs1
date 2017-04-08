library(twitteR)
library(xlsx)
library(stringr)
library(networkD3)
library(graphTweets)
library(dplyr)
library(RKlout, quietly = T)
library(htmlwidgets, quietly = T)


ck <- "UD0KeCr5ZfpMrd8mRbgebuQKY"
cs <- "wsO0tGZ8pQwy4HL3LVI1nIKPXPjteoTsc4sfdOI8O95qRNqxLS"
at <- "835139026432757761-04WPbVYpjw7aUKfUfXb0Whn2PRGJ9GS"
as <- "Xm4A4qpk6oXQNtwuPEaLgLIBdI319BEnqYTHtEH2vhUht"
rk <- "8h3kqwy8c29eqwmgxhsnxeku"

token <- setup_twitter_oauth(ck, cs, at, as)

tweets <- searchTwitter("@EdVotersPA", n = 200)
tweets <- twListToDF(tweets)
edges1 <- getEdges(data = tweets, tweets = "text", source = "screenName")
nodes1 <- getNodes(edges1)
g1 <- simpleNetwork(edges1, linkDistance = 100, charge = -400, zoom = TRUE, opacity = .6, fontSize = 12, fontFamily = "sans-serif")


nodes1$klout <- 0
for(i in 2:length(nodes1$nodes)){
  nodes1$klout[i] <- RKlout(rk,nodes1$nodes[i])
}

edges1$value <- 1
nodes1$group <- 1

#Create Index and Bring to Front
nodes1$name<-0:30
nodes1 <- nodes1 %>% select(name,everything())

#LOOKUP TABLE TO CONVERT MATCHING NAMES TO INDEX NUMBERS
edges1$source <- (nodes1$name[match(edges1$source, nodes1$nodes)])
edges1$target <- (nodes1$name[match(edges1$target, nodes1$nodes)])

forceNetwork(Links = edges1, Nodes = nodes1, Source = "source", Target = "target", Value = "value", NodeID = "nodes", Group = "group", Nodesize = "klout2")

nodes1$klout2 <- 1.07^(nodes1$klout)

saveWidget(g1, file = "graph1.html", selfcontained = F)

tweets2 <- searchTwitter("EITC+OSTC", resultType = "recent", n = 200)
tweets2 <- twListToDF(tweets2)
edges2 <- getEdges(data = tweets2, tweets = "text", source = "screenName")
g2 <- simpleNetwork(edges2, linkDistance = 100, charge = -400, zoom = TRUE, opacity = .6, fontSize = 12, fontFamily = "sans-serif")

saveWidget(g2, file = "graph2.html", selfcontained = F)


