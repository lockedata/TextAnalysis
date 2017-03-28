context("getSentiment")

test_that("getSentiment returns a response with defaults",{
  initialdf<-data.frame(id=1,text="brilliant")
  respo<-getSentiment(initialdf)
  expect_that(respo, is_a("data.frame"))
  expect_equal(colnames(respo),c("id","text","language","score"))
  expect_equal(nrow(initialdf), nrow(respo))
  })

test_that("getSentiment returns a response with language",{
  initialdf<-data.frame(id=1,text="brilliant",language="en")
  respo<-getSentiment(initialdf)
  expect_that(respo, is_a("data.frame"))
  expect_equal(colnames(respo),c("id","text","language","score"))
  expect_equal(nrow(initialdf), nrow(respo))
})

test_that("getSentiment errors with incorrect data.frame",{
  initialdf<-data.frame(ID=1,text="brilliant",language="en")
  expect_error(getSentiment(initialdf))
})
