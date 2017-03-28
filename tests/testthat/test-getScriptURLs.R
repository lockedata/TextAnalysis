context("getScriptURLs")

test_that("getScriptURLs returns expected scripts",{
  result<-getScriptURLs()
  expect_that(result, is_a("tbl_df"))
  expect_equal(colnames(result),c("name","URL"))
  expect_equal(nrow(result), 80)
})
