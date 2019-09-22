#x <- setwd(system.file("extdata", package = "packagefars"))

test_that("package correct", {
  testthat::expect_equal(list.files(system.file("extdata", package = "packagefars")),
                         c("accident_2013.csv.bz2",
                           "accident_2014.csv.bz2",
                           "accident_2015.csv.bz2"))

  testthat::expect_equal(make_filename(2013), paste0("accident_",2013,".csv.bz2"))

})

#
# year <- 2013
# test_that("package correct",{
#   testthat::expect_is(fars_read("accident_2013.csv.bz2"),
#                  c("tbl_df", "tbl", "data.frame"))
#   testthat::expect_equal(make_filename(year), paste0("accident_",year,".csv.bz2"))
#   testthat::expect_is(make_filename(year), "character")
#
# })
#
# setwd(x)
