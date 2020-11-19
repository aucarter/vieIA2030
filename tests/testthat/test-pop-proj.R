testthat::test_that("population projection approximately works", {
    # Testing that the difference in the projected deaths is less than 5%
    is <- "South Africa"
    y0 <- 2000
    y1 <- 2019
    proj <- project_pop(is, y0, y1, test_data$wpp_input) %>%
        add_lt(., is, y0, y1) %>%
        add_obs(., test_data$obs_wpp, is, y0, y1) %>%
        as.data.table()
    estimate <- proj[group == "CCPM"]$deaths_both
    test <- proj[group == "WPP2019"]$deaths_both
    testthat::expect_true(
        sum(abs(estimate - test))  / sum(test) < 0.05
    )
})
