source('textPred.R')

downloadCourseraSwiftKey()

## no chached: 300
## cached: 8
tus.tokens <-
  getTokens("tus.tokens", 
            "data/final/en_US/en_US.twitter.txt", n=-1L)


## 4: 900 s
system.time({
  addNGramsFH('tus', 'tus', 4, tus.tokens, K=5, echo=TRUE)
  remove('tus.tokens')
})


bus.tokens <-
  getTokens("bus.tokens", 
            "data/final/en_US/en_US.blogs.txt", n=-1L)

# 5: 2167
system.time({
  addNGramsFH('bus', 'bus', 5, bus.tokens, K=5, echo=TRUE)
  remove('bus.tokens')
})

nus.tokens <-
  getTokens("nus.tokens", 
            "data/final/en_US/en_US.news.txt", n=-1L)
