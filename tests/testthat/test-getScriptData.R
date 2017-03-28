context("getScriptData")

test_that("getScriptData returns scripts in expected format",{
  result<-getScriptData()
  expect_that(result, is_a("tbl_df"))
  expect_equal(nrow(result), 12)
})
