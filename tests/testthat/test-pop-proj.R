test_that("population projection works", {
  loc <- 840
  wpp_dt <- prep_wpp_data()
  loc_dt <- wpp_dt[country_code == loc]
  params <- make_params(loc_dt)
  pop_proj <- project_pop(params)

  expect_true(!any(is.na(pop_proj)))
})
