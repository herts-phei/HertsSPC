testthat::test_that("Test that the number of rows is equal to the number of unique inputted indictors, and the icons shown are correct", {

  library(dplyr)

  tooth_data <- force(ToothGrowth) %>%
    filter(supp == "VC") %>% slice(-15:-27) %>%
    mutate(Date = seq.Date(as.Date("2021-01-01"), as.Date("2021-01-17"), by = "days"),
           supp = "Indicator 1",
           polarity = "up",
           greater_than_hundred = FALSE,
           less_than_zero = FALSE,
           unit = "count")

  tooth_data2 <- force(ToothGrowth) %>%
    filter(supp == "VC") %>% slice(-15:-27) %>%
    mutate(Date = seq.Date(as.Date("2021-01-01"), as.Date("2021-01-17"), by = "days"),
           supp = "Indicator 2",
           polarity = "up",
           greater_than_hundred = FALSE,
           less_than_zero = FALSE,
           unit = "count")

  tooth_data3 <- force(ToothGrowth) %>%
    filter(supp == "VC") %>% slice(-15:-27) %>%
    mutate(Date = seq.Date(as.Date("2021-01-01"), as.Date("2021-01-17"), by = "days"),
           supp = "Indicator 3",
           polarity = "up",
           greater_than_hundred = FALSE,
           less_than_zero = FALSE,
           unit = "count") %>%
    bind_rows(tooth_data, tooth_data2)


  # Retreive SPC data

  w <- testthat::capture_warnings(
  
  spc_data <- spc_output(
         data = tooth_data3,
         time_field = "Date",
         indicator = "supp",
         value = "len",
         output = "data",
         target = 30,
         )
  
  )

  testthat::expect_match(w, "Outliers will be included in process limit calculations!", all = FALSE)
  
  spc_table <- spc_summary_table(
    .data = spc_data,
    .time_field = "Day",
    .value = "Size",
    .indicator = "Tooth Type",
    .nad = FALSE,
    .mode = "static",
    .time_unit = "day"
  )

  testthat::expect_true(
    all(
      basename(tail(spc_table$body$dataset$Variation, 1)) == "Improving_Special_Cause_High_Transparent.png",
      basename(tail(spc_table$body$dataset$Assurance, 1)) == "Consistently_Failing_Target_Transparent.png",
      nrow(spc_table$body$dataset) == length(unique(tooth_data3$supp)),
      inherits(spc_table, "flextable")
    )
  )


})
