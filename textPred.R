require("pipeR")
require("data.table")

require('filehash')

cache.fn <- "cache/dbCache"
if (!file.exists('cache')) dir.create('cache')
if (!file.exists(cache.fn)) dbCreate(cache.fn)
dbCache <- dbInit(cache.fn)


treatPunctuation <- function(txts) {
  ## we split, but we can also remove only
  unlist(strsplit(txts, "(?!@|'|-|#)[[:punct:]]| -+ ", perl=TRUE))  
}

trim <- function (txts) gsub("^\\s+|\\s+$", "", txts)

removeExtraSpace <- function(txts) {
  gsub("\\s\\s+", " ", txts)
}

txts.clean <- function(txts) {
  txts %>>%
    tolower %>>%
    treatPunctuation %>>%
    removeExtraSpace %>>%
    trim
}

txt.tokens <- function(txt) unlist(strsplit(txt, ' '))

txts.tokens <- function(txts) {
  txts %>>%
    txt.tokens
}

## txts.count <- function(txts) {
##   sapply(gregexpr(' ', txts), function(x) {if (x[1]==-1) 0 else length(x)})+1
## }

cleanDT <- function(dt) {
  dt[,.(words=txts.clean(words))][words!=""][,id:=.I]
}

tokenizeDT <- function(dt) {
  dt[,.(tokens1=txts.tokens(words)), by=id]
}

basicDT <- function(dt) {
  dt %>>%
    cleanDT %>>%
    tokenizeDT
}

### N grams

ngramDTs <- function(dt, n) {
  N <- dim(dt)[1]
  ngrams.dts <- list()
  ngrams.dts[[1]] <- dt
  if (n>=2) {
    for (k in 2:n) {
      print(k)
      ngrams.dts[[k]] <-  cbind(ngrams.dts[[k-1]][1:(N-k+1),!"newid", with=FALSE] , dt[k:N])
      setnames(ngrams.dts[[k]], c(colnames(ngrams.dts[[k-1]][,!"newid", with=FALSE]),
               "newid", paste0("tokens", k)))
    }
  }
  if (n>=2) {
    for (k in 2:n) {
      ngrams.dts[[k]] <- ngrams.dts[[k]][id==newid][,!"newid", with=FALSE]
    }
  }
  ngrams.dts
}

