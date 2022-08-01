wuenic_2021_path <- "~/Downloads/coverage--2021.xlsx"
dt <- readxl::read_xlsx(wuenic_2021_path,  sheet = "Data") %>%
    filter(COVERAGE_CATEGORY_DESCRIPTION == "WHO/UNICEF Estimates of National Immunization Coverage") %>%
    select(CODE, YEAR, ANTIGEN, ANTIGEN_DESCRIPTION, COVERAGE) %>%
    as.data.table()

pdf("new_wuenic.pdf", width = 8.5, height = 140)
gg <- ggplot(dt[ANTIGEN == "DTPCV1" & YEAR >= 2015], aes(x = YEAR, y = COVERAGE)) + 
    geom_line() + geom_vline(xintercept = 2019.5, alpha = 0.5, color = "red") + 
    theme_bw() + 
    facet_wrap(.~CODE, ncol = 3)
print(gg)
dev.off()

