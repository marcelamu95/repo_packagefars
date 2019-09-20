x <- setwd(system.file("extdata", package = "packagefars"))

year <- 2013
test_that("package correct",{
  testthat::expect_is(fars_read("accident_2013.csv.bz2"),
                 c("tbl_df", "tbl", "data.frame"))
  testthat::expect_equal(make_filename(year), paste0("accident_",year,".csv.bz2"))
  testthat::expect_is(make_filename(year), "character")

})

setwd(x)
