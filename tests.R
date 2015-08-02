## This to be transfor into real tests
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

describe("ngramsFH", {
  dt <- data.table(words=c("Ala ma kota.", "Kot ma Alę."))
  expect_that(dim(basicDT(dt)), equals(c(6,2)))
})



describe('ngramsFH', {
  dt <- data.table(words=c("Ala ma kota.", "Kot ma Alę.")) %>>%
    basicDT 
  name <- "ala"
  test_that("1grams", {
    ngramsFH(dt, name, 1)
    expect_that(dbExists(dbCache, "ala1grams"), is_true())
    expect_that(dbFetch(dbCache, "ala1grams"), equals(dt))
  })
  test_that("1grams", {
    dbDelete(dbCache, "ala1grams")
    ngramsFH(dt, name, 1)
    expect_that(dbExists(dbCache, "ala1grams"), is_true())
    expect_that(dbFetch(dbCache, "ala1grams"), equals(dt))
  })

  test_that("2grams", {
    dbDelete(dbCache, "ala2grams")
    ngramsFH(dt, name, 2)
    expect_that(dbExists(dbCache, "ala2grams"), is_true())
    pre2 <- dbFetch(dbCache, "ala2grams")
    expect_that(dim(pre2), equals(c(5,4)))
  })

  test_that("3grams", {
    dbDelete(dbCache, "ala3grams")
    ngramsFH(dt, name, 3)
    expect_that(dbExists(dbCache, "ala3grams"), is_true())
    pre3 <- dbFetch(dbCache, "ala3grams")
    print('  ')
    print(pre3)
    expect_that(dim(pre3), equals(c(4,5)))
  })

})
