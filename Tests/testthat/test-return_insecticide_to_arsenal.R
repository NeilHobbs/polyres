available.vector = c(1,2)
withdrawn.vector = c(3,4)


test_that("returns adds insecticide to the available vector", {
  expect_equal(return_insecticide_to_arsenal(withdrawn.vector = withdrawn.vector,
                                             available.vector= available.vector,
                                             insecticide.to.return = 3)[[1]], c(1, 2, 3)
  )
})

test_that("returns removes the insecticide from the withdrawn vector", {
  expect_equal(return_insecticide_to_arsenal(withdrawn.vector = withdrawn.vector,
                                             available.vector= available.vector,
                                             insecticide.to.return = 3)[[2]], c(4)
  )
})