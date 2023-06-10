rm(list = ls())
library(eurostat)

eurostat::get_eurostat_toc() %>%
  filter(code == "prc_hicp_manr") %>%
  pull(4) %>%
  as.Date(format = "%d.%m.%Y")

as.Date(pull(filter(eurostat::get_eurostat_toc(), code == "prc_hicp_manr"), 4), format = "%d.%m.%Y")
