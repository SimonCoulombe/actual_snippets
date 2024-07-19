library(aitoolbox)
library(dplyr)
library(DBI)
library(ggplot2)
pool <- connect_to_azure()
DBI::dbWriteTable(pool, DBI::Id(schema = "wz_pricing",   table = "eg66451_mtcars"),  value = mtcars, overwrite = TRUE)


mtcars_sql <-  tbl(pool,dbplyr::in_schema("WZ_PRICING","eg66451_mtcars"))

z <- mtcars_sql %>%
  mutate(across(everything(), ~count(distinct(.x))))
