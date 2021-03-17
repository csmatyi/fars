library(testthat)
library(fars)

test_check("fars")
expect_equal(make_filename(2000),"accident_2000.csv.bz2")
expect_equal(make_filename(2010),"accident_2010.csv.bz2")
