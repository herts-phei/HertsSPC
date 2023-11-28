testthat::test_that("Chart theme of correct length", {

  theme <- spc_chart_options()

  testthat::expect_true(

    length(theme) == 9

  )

})



testthat::test_that("Data going into chart has no duplication in date per indicator",{

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
           unit = "count") %>%
    bind_rows(tooth_data)


  # Retreive SPC data
  
  w <- testthat::capture_warnings(
    spc_data <- spc_output(
      data = tooth_data2,
      time_field = "Date",
      indicator = "supp",
      value = "len",
      output = "data")
  )

  testthat::expect_match(w, "Outliers will be included in process limit calculations!", all = FALSE)
  
  
  data_for_chart <- dplyr::filter(spc_data, indicator == "Indicator 1")

  spc_chart(.data = data_for_chart,
            .package = "ggplot",
            .plot_title = "ABCD",
            .base_date_range = NULL)
  
  # w <- testthat::capture_warnings(
  #   chart <- 
  #     spc_chart(
  #       .data = data_for_chart,
  #       .package = "ggplot",
  #       .plot_title = "ABCD",
  #       .base_date_range = NULL
  #     )
  # )
  # 
  # testthat::expect_match(w, "rows containing missing values", all = FALSE)

  testthat::expect_false(

    all(duplicated(data_for_chart$time_field))

  )


})


