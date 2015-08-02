source('textPred.R')

downloadCourseraSwiftKey()

## no chached: 300
## cached: 8
system.time({
  tus.tokens <-
    getTokens("tus.tokens", 
              "data/final/en_US/en_US.twitter.txt", n=-1L)
})
## 1: 10
## 2: 52
## system.time({
##   nGramsFH("tus", 1, tus.tokens, echo=TRUE)
## })

object.size(tus.tokens)

Ntus <- dim(tus.tokens)[1]
id1 <- tus.tokens[floor(Ntus/3), id]
id2 <- tus.tokens[floor(2*Ntus/3), id]

system.time({
  nGramsFH("tus1h", 4, tus.tokens[id<=id1], echo=TRUE)
  ##preNGramsFH("tus2h", 4, tus.tokens[id>id1 & id<=id2], echo=TRUE)
})

dbList(dbCachePre)
dbList(dbCache)

remove(tus.tokens)
system.time({
  nGramsFH("tus", 3, echo=TRUE)
})


bus.tokens <-
  getTokens("bus.tokens", 
            "data/final/en_US/en_US.blogs.txt", n=-1L)
nus.tokens <-
  getTokens("nus.tokens", 
            "data/final/en_US/en_US.news.txt", n=-1L)
