library(fincalcR)

get_ust_futures_conversion_factor(
  date_delivery = "2008-12-01",
  date_maturity = "2010-10-31",
  bond_coupon = 1.5,
  return_df = T
)

get_ust_futures_conversion_factor(
  date_delivery = "2009-03-01",
  date_maturity = "2012-01-15",
  bond_coupon = 1 + 1/8,
  return_df = T
)


get_ust_futures_conversion_factor(
  date_delivery = "2008-12-01",
  date_maturity = "2018-11-15",
  bond_coupon = 3.75,
  return_df = T
)

get_ust_futures_conversion_factor(
  date_delivery = "2008-12-01",
  date_maturity = "2038-05-15",
  bond_coupon = 4.5,
  return_df = T
)
