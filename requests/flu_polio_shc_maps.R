devtools::load_all()
polio_path <- "/Users/austincarter/Downloads/the-number-of-reported-paralytic-polio-cases-by-world-region.csv"
polio_dt <- fread(polio_path)
loc_list <- unique(polio_dt[Year >= 2020 & `Total (reported) polio cases` > 0]$Code)
map_locations(loc_list, "Any reported polio cases from 2020 to present")

flu_vaccine_path <- "/Users/austincarter/Downloads/DP_LIVE_02032023072431379.csv"
flu_vaccine_dt <- fread(flu_vaccine_path)
loc_list <- unique(flu_vaccine_dt$LOCATION)
map_locations(loc_list, "Influenza vaccine data coverage")