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

