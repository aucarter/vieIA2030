testthat::test_that("population projection approximately works", {
    # Testing that the difference in the projected pop is less than 10%
    loc <- 840
    wpp_dt <- prep_wpp_data()
    loc_dt <- wpp_dt[country_code == loc]
    params <- make_params(loc_dt)
    pop_proj <- project_pop(params)

    total_pop_proj <- colSums(pop_proj)

    total_pop <- loc_dt[measure %in% c("popM", "popF"),
                        .(t_pop = sum(value)), by = year_start]$t_pop
    testthat::expect_true(
        sum(abs(total_pop_proj - total_pop))  / sum(total_pop) < 0.1
    )
})
