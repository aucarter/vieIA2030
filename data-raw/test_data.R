wpp_input <- db_pull("wpp_input", "ZAF", append_names = T)
obs_wpp <- db_pull("obs_wpp", "ZAF", append_names = T)

test_data <- list(wpp_input = wpp_input, obs_wpp = obs_wpp)

usethis::use_data(test_data, overwrite = T)
