source('textPred.R')

require('testthat')


describe('treatPunctuation', {
  expect_that(treatPunctuation("afg@fdsf"), equals("afg@fdsf"))
  expect_that(treatPunctuation("fdsaf'a"), equals("fdsaf'a"))
  expect_that(treatPunctuation("fdsaf'a(aba)"), equals(c("fdsaf'a", "aba")))
  expect_that(treatPunctuation("a-b - c"), equals(c("a-b", "c")))
  expect_that(treatPunctuation("rt #df"), equals("rt #df"))
})

describe("txts.tokens", {
  expect_that(txts.tokens(c('dsfdsfd dfasdfa', 'dsfsda dsfads fdsafsa')),
              equals(c("dsfdsfd", "dfasdfa", "dsfsda",  "dsfads",  "fdsafsa")))
})

describe("basicDT", {
  dt <- data.table(words=c("Ala ma kota.", "Kot ma Alę."))
  expect_that(dim(basicDT(dt)), equals(c(6,2)))
})

describe("preNGramsFH", {
  dt <- data.table(words=c("Ala ma kota.", "Kot ma Alę."))
  expect_that(dim(basicDT(dt)), equals(c(6,2)))
})



describe('preNGramsFH', {
  dt <- data.table(words=c("Ala ma kota.", "Kot ma Alę.")) %>>%
    basicDT 
  name <- "ala"
  test_that("pre1-grams", {
    dbDelete(dbCachePre, "alaPre1grams")
    preNGramsFH(name, 1, dt)
    expect_that(dbExists(dbCachePre, "alaPre1grams"), is_true())
    expect_that(dbFetch(dbCachePre, "alaPre1grams"), equals(dt))
  })

  test_that("pre2grams", {
    dbDelete(dbCachePre, "alaPre2grams")
    preNGramsFH(name, 2, dt)
    expect_that(dbExists(dbCachePre, "alaPre2grams"), is_true())
    pre2 <- dbFetch(dbCachePre, "alaPre2grams")
    expect_that(dim(pre2), equals(c(5,4)))
  })

  test_that("pre3grams", {
    dbDelete(dbCachePre, "alaPre3grams")
    preNGramsFH(name, 3, dt)
    expect_that(dbExists(dbCachePre, "alaPre3grams"), is_true())
    pre3 <- dbFetch(dbCachePre, "alaPre3grams")
    expect_that(dim(pre3), equals(c(4,5)))
  })


  test_that("pre6grams", {
    dbDelete(dbCachePre, "alaPre6grams")
    preNGramsFH(name, 6, dt)
    expect_that(dbExists(dbCachePre, "alaPre6grams"), is_true())
    pre6 <- dbFetch(dbCachePre, "alaPre6grams")
    expect_that(dim(pre6), equals(c(1,8)))
  })

  test_that("pre7grams", {
    dbDelete(dbCachePre, "alaPre7grams")
    preNGramsFH(name, 7, dt)
    expect_that(dbExists(dbCachePre, "alaPre7grams"), is_false())
  })  
})

describe('nGramsFH', {
  dt <- data.table(
    words=c("Ala ma kota.",
            "Kot ma Alę.",
            "Ala ma psa.")) %>>%
    basicDT 
  name <- "ala"
  test_that('1-grams', {
    nGramsFH(name, 1, dt, TRUE)
    a1 <- dbFetch(dbCache, "ala1grams")
    expect_that(a1[tokens1=='ma', counts], equals(3))
  })

  test_that('2-grams', {
    nGramsFH(name, 2, dt, TRUE)
    a2 <- dbFetch(dbCache, "ala2grams")
    expect_that(a2[tokens1=='ala' & tokens2=='ma', counts], equals(2))
  })

  test_that('3-grams', {
    nGramsFH(name, 3, dt, TRUE)
    a3 <- dbFetch(dbCache, "ala3grams")
    expect_that(a3[tokens1=='ala' & tokens2=='ma' & tokens3=='kota',
                   counts],
                equals(1))
  })
  test_that('4-grams', {
    nGramsFH(name, 4, dt, TRUE)
    a4 <- dbFetch(dbCache, "ala4grams")
    expect_that(dim(a4)[1], equals(0))
  })
})

describe("createEmptyTotal", {
  expect_that(dim(createEmptyTotalDT(2)), equals(c(0,3)))
})

describe("createEmptyTotalFH", {
  createEmptyTotalFH("koty", 4)
  expect_that(dim(dbFetch(dbCache, "kotyTotal4grams")), equals(c(0,5)))
})


describe("dbDeletePattern", {
  vs <- c("xyz1", "xyz2")
  for (v in vs) {
    dbInsert(dbCache, v, v)
  }
  pattern <- "xyz[0-9]+"
  vs <- dbList(dbCache)
  expect_that(length(vs[grep(pattern, vs)]), equals(2))
  dbDeletePattern(dbCache, pattern)
  vs <- dbList(dbCache)
  expect_that(length(vs[grep(pattern, vs)]), equals(0))
  
})

describe("createNGramsFH", {
  dt <- data.table(
    words=c("Ala ma kota.",
            "Kot ma Alę.",
            "Ala ma psa.",
            "Ala ma kota i psa.",
            "Ile kosztuje?",
            "Ile kosztuje kot?",
            "Kot czy pies, zależy ile kosztuje."
            )) %>>%
    basicDT 
  name <- "kot"
  nameTotal <- "animals"
  dbDeletePartRecords(name)
  dbDeleteTotalRecords(nameTotal)
  addNGramsFH(name, nameTotal, 4, dt, 3)
  print(dbFetch(dbCache, 'animalsTotal4grams'))
})
