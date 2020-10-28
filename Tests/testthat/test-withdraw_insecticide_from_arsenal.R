withdrawn.vector = c(1)
available.vector = c(2,3)

withdraw_insecticide_from_arsenal(withdrawn.vector = withdrawn.vector,
                                  available.vector = available.vector,
                                  insecticide.to.withdraw = 2)

test_that("insecticide is moved to withdrawn", {
  expect_equal(withdraw_insecticide_from_arsenal(withdrawn.vector = withdrawn.vector,
                                                 available.vector = available.vector,
                                                 insecticide.to.withdraw = 2)[[2]], c(1,2)
  )
})


test_that("insecticide is removed from available",{
  expect_equal(withdraw_insecticide_from_arsenal(withdrawn.vector = withdrawn.vector,
                                                 available.vector = available.vector,
                                                 insecticide.to.withdraw = 2)[[1]], c(3)
  )
})