testthat::test_that("Correct variation is returned", {

    library(dplyr)

  tooth_data <- force(ToothGrowth) %>%
    filter(supp == "VC") %>% slice(-15:-27) %>%
    mutate(Date = seq.Date(as.Date("2021-01-01"), as.Date("2021-01-17"), by = "days"),
           supp = "Indicator 1",
           polarity = "up",
           greater_than_hundred = FALSE,
           less_than_zero = FALSE,
           unit = "count")

w <- testthat::capture_warnings(
  spc_data <- spc_output(data = tooth_data,
                         time_field = "Date",
                         indicator = "supp",
                         value = "len",
                         output = "data")
)
  
  testthat::expect_match(w, "Outliers will be included in process limit calculations!", all = FALSE)

 
  
  n_static <- spc_narrative(.data = spc_data,
                           .mode = "static",
                           .time_unit = "day")

 # n_interactive <- spc_narrative(.data = spc_data,
 #                                .mode = "interactive",
 #                                .time_unit = "day")


 testthat::expect_true(
   n_static$body$dataset$text[4] == "Improving Special Cause Variation:\n - Latest value above upper control limit \n - Latest 6 data points are above mean "
 )


})
