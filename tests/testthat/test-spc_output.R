testthat::test_that("Outputs returned as expected", {


tooth_data <- force(ToothGrowth) %>%
  dplyr::filter(supp == "VC") %>%
  dplyr::slice(-15:-27) %>%
  dplyr::mutate(Date = seq.Date(as.Date("2021-01-01"), as.Date("2021-01-17"), by = "days"),
                polarity = "up",
                greater_than_hundred = FALSE,
                less_than_zero = FALSE,
                unit = "count")

# Retreive SPC data


w <- testthat::capture_warnings(
  spc_data <- spc_output(
    data = tooth_data,
    time_field = "Date",
    indicator = "supp",
    value = "len",
    output = "data")
  
  )

testthat::expect_match(w, "Outliers will be included in process limit calculations!", all = FALSE)

# Retrieve static SPC plot

w <- testthat::capture_warnings(
  spc_graph_no_icons <- spc_output(
    data = tooth_data,
    time_field = "Date",
    indicator = "supp",
    value = "len",
    output = "chart",
    package = "ggplot",
    target = NULL,
    plot_title = "Size of teeth (or something along those lines) SPC",
    chart_theme = spc_chart_options(
      x_label = "Day",
      y_label = "Count",
      x_label_format = "%b-%d",
      title_size = 25,
      x_breaks = "1 day"
    )
  )
  
)

testthat::expect_match(w, "Outliers will be included in process limit calculations!", all = FALSE)



w <- testthat::capture_warnings(
  spc_graph_icons_both <- spc_output(
    data = tooth_data,
    time_field = "Date",
    indicator = "supp",
    value = "len",
    output = "chart",
    package = "ggplot",
    target = 30,
    plot_title = "Size of teeth (or something along those lines) SPC",
    chart_theme = spc_chart_options(
      x_label = "Day",
      y_label = "Count",
      x_label_format = "%b-%d",
      title_size = 25,
      x_breaks = "1 day"
    )
  ) %>%
    spc_add_icons()
)

testthat::expect_match(w, "Outliers will be included in process limit calculations!", all = FALSE)
testthat::expect_match(w, "rows containing missing values", all = FALSE)





w <- testthat::capture_warnings(
  spc_graph_icons_no_target <- spc_output(
    data = tooth_data,
    time_field = "Date",
    indicator = "supp",
    value = "len",
    output = "chart",
    package = "ggplot",
    target = NULL,
    plot_title = "Size of teeth (or something along those lines) SPC",
    chart_theme = spc_chart_options(
      x_label = "Day",
      y_label = "Count",
      x_label_format = "%b-%d",
      title_size = 25,
      x_breaks = "1 day"
    )
  ) %>%
    spc_add_icons()
)

testthat::expect_match(w, "Outliers will be included in process limit calculations!", all = FALSE)
testthat::expect_match(w, "rows containing missing values", all = FALSE)




# Retrieve a narrative



w <- testthat::capture_warnings(
  spc_narrative_table <-
    spc_output(
      data = tooth_data,
      time_field = "Date",
      indicator = "supp",
      value = "len",
      output = "narrative")
  )

testthat::expect_match(w, "Outliers will be included in process limit calculations!", all = FALSE)
testthat::expect_match(w, "Table output will be static. Set mode to static or interactive to change.", all = FALSE)



# Retrieve summary table

w <- testthat::capture_warnings(
  spc_summary <-
    spc_output(
      data = tooth_data,
      time_field = "Date",
      indicator = "supp",
      value = "len",
      output = "summary",
      mode = "interactive"
    )
)


testthat::expect_match(w, "Outliers will be included in process limit calculations!", all = FALSE)
testthat::expect_match(w, "Table output will be interactive. Set mode to static or interactive to change.", all = FALSE)


w <- testthat::capture_warnings(
  spc_summary <-
    spc_output(
      data = tooth_data,
      time_field = "Date",
      indicator = "supp",
      value = "len",
      output = "summary",
      mode = "interactive")
)

testthat::expect_match(w, "Outliers will be included in process limit calculations!", all = FALSE)
testthat::expect_match(w, "Table output will be interactive. Set mode to static or interactive to change.", all = FALSE)


testthat::expect_true(
  all(

    inherits(spc_data, "data.frame"),
    ncol(spc_data) == 33,
    inherits(spc_graph_no_icons, "ggplot"),
    length(spc_graph_no_icons$layers) == 8,
    length(spc_graph_icons_both$layers) == 3,
    length(spc_graph_icons_no_target$layers) == 2,
    inherits(spc_narrative_table, "flextable"),
    inherits(spc_summary, "reactable")

  )
)


})


testthat::test_that("Error occurs when columns are of wrong class", {

  tooth_data <- force(ToothGrowth) %>%
    dplyr::filter(supp == "VC") %>%
    dplyr::slice(-15:-27) %>%
    dplyr::mutate(Date = seq(1, 17),
                  polarity = "up",
                  greater_than_hundred = FALSE,
                  less_than_zero = FALSE,
                  unit = "count")


  tooth_data2 <- force(ToothGrowth) %>%
    dplyr::filter(supp == "VC") %>%
    dplyr::slice(-15:-27) %>%
    dplyr::mutate(Date = seq.Date(as.Date("2021-01-01"), as.Date("2021-01-17"), by = "days"),
                  len = as.character(len),
                  polarity = "up",
                  greater_than_hundred = FALSE,
                  less_than_zero = FALSE,
                  unit = "count")


 testthat::expect_error(
   testthat::expect_match(
   testthat::capture_warnings(
     spc_output(
       data = tooth_data,
       time_field = "Date",
       indicator = "supp",
       value = "len",
       output = "data"
     )
   ),
   "Outliers will be included in process limit calculations!"),
   "Specified time_field is not of date class!")


 testthat::expect_error(
   testthat::expect_match(
     testthat::capture_warnings(
       spc_output(
         data = tooth_data2,
         time_field = "Date",
         indicator = "supp",
         value = "len",
         output = "data"
       )
     ),
     "Outliers will be included in process limit calculations!"),
   "Specified value_field is not of numeric class!")




})



testthat::test_that("Warning occurs when columns do not exist and are added", {

  tooth_data <- force(ToothGrowth) %>%
    dplyr::filter(supp == "VC") %>%
    dplyr::slice(-15:-27) %>%
    dplyr::mutate(Date = seq.Date(as.Date("2021-01-01"), as.Date("2021-01-17"), by = "days"))

  w <- testthat::capture_warnings(
    output <- spc_output(
      data = tooth_data,
      time_field = "Date",
      indicator = "supp",
      value = "len",
      output = "data"
    )
  )

  testthat::expect_match(w, "Outliers will be included in process limit calculations!", all = FALSE)
  testthat::expect_match(w, "No greater_than_hundred column detected so it has been set as F for all indicators!", all = FALSE)
  testthat::expect_match(w, "No less_than_zero column detected so it has been set as F for all indicators!", all = FALSE)
  testthat::expect_match(w, paste0("No unit column detected so it has been set as count for all indicators \\(create unit column to specify whether unit should be percent or count for an indicator\\)"), all = FALSE)
  testthat::expect_match(w, paste0("No polarity column detected so it has been set as up for all indicators \\(create polarity column to specify whether polarity should be up or down for an indicator\\)"), all = FALSE)

  testthat::expect_equal(unique(output$greater_than_hundred), FALSE)
  testthat::expect_equal(unique(output$less_than_zero), FALSE)
  testthat::expect_equal(unique(output$unit), "count")
  testthat::expect_equal(unique(output$polarity), "up")


})

