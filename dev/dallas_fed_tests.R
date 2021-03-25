library(asbviz)
library(fundManageR)
data <- dallas_fed_international_housing()
data %>%
  select(nameIndex, codeIndex, dateData, nameCountry, value) %>%
  group_by(nameCountry) %>%
  nest() %>%
  ungroup() %>%
  hc_xy_trelliscope(
    data_column_name = "data",
    x = "dateData",
    y = "value",
    group = "nameIndex",
    type = "spline",
    id_columns = "nameCountry",
    glue_title = "{nameCountry} Dallas Fed Indicies"
  )