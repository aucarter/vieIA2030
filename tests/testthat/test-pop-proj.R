testthat::test_that("population projection approximately works", {
    # Testing that the difference in the projected deaths is less than 5%
    proj <- project_pop(
        "South Africa", 2000, 2019, test_data$wpp_input, test_data$obs_wpp
    )$out_df %>%
        as.data.table()
    estimate <- proj[group == "CCPM"]$deaths_both
    test <- proj[group == "WPP2019"]$deaths_both
    testthat::expect_true(
        sum(abs(estimate - test))  / sum(test) < 0.05
    )
})
