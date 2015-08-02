## This to be transfor into real tests
source('textPred.R')

require('testthat')

test_that('treatPunctuation', {
  expect_that(treatPunctuation("afg@fdsf"), equals("afg@fdsf"))
  expect_that(treatPunctuation("fdsaf'a"), equals("fdsaf'a"))
  expect_that(treatPunctuation("fdsaf'a(aba)"), equals(c("fdsaf'a", "aba")))
  expect_that(treatPunctuation("a-b - c"), equals(c("a-b", "c")))
  expect_that(treatPunctuation("rt #df"), equals("rt #df"))
})

test_that("txts.tokens", {
  expect_that(txts.tokens(c('dsfdsfd dfasdfa', 'dsfsda dsfads fdsafsa')),
              equals(c("dsfdsfd", "dfasdfa", "dsfsda",  "dsfads",  "fdsafsa")))
})

test_that("testujemy", {
  expect_that(TRUE, is_true())
})
