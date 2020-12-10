test.matrix = make_cross_selection_matrix(number.of.insecticides = 5,
                                          min.cross.selection = 1,
                                          max.cross.selection = 1)

for(i in 1:5){
test_that("zeroes are put in place", {
  expect_equal(test.matrix[i, i], 0)
})
}

test_that("cross selection values are put in place", {
  expect_equal(test.matrix[1, 2], 1)
  
  expect_equal(test.matrix[1, 5], 1)
  
  expect_equal(test.matrix[4, 1], 1)
  
  expect_equal(test.matrix[2, 4], 1)
})