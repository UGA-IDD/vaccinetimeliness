## code to prepare `my_pkg_data` dataset goes here

dhs_country_codes <- read.csv(file.choose(), header = TRUE)





usethis::use_data(dhs_country_codes, internal = TRUE)
