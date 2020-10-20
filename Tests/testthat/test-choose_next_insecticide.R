
test_that("moves to next insecticide in the sequence", {
  expect_equal(choose_next_insecticide(previous.insecticide = 1,
                                       available.insecticides = c(2,3,4),
                                       number.of.insecticides = 4), 2)
})


test_that("returns to the start of the sequence",{
 
  expect_equal(choose_next_insecticide(previous.insecticide = 3,
                                       available.insecticides = c(1,2),
                                       number.of.insecticides = 3), 1) 
  
})