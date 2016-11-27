source('textPred.R')

downloadCourseraSwiftKey()

createTokensCsv("short.tokens", 
                "data/final/en_US/en_US.twitter.txt")
createTokensCsv("bus.tokens", 
                "data/final/en_US/en_US.blogs.txt")
createTokensCsv("nus.tokens", 
                "data/final/en_US/en_US.news.txt")



## 4: 900 s
system.time({
  addNGramsFH('tus', 'tus', 4, tus.tokens, K=5, echo=TRUE)
  remove('tus.tokens')
})


