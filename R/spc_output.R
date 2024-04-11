utils::globalVariables(c(".","Target", "value", "time_field", "indicator",
                         "polarity", "unit", "less_than_zero", "unit",
                         "greater_than_hundred", "mean", "average_mR", "mR",
                         "lower_ci", "upper_ci", "rebase_group"))


#' Main function to return desired statistical process output
#'
#' @description A general function used to return a desired statistical process output without needing to call individual output functions.
#'
#' Dataframes going into this SPC function need te following columns (and if they do not exist, they will be defaulted within the function):
#'
#' - unit (either "count" of "percent")
#' - greater_than_hundred (TRUE or FALSE). This implies whether the KPI/metric can theoretically exceed 100 (as a count or as a percentage) for graphical purposes
#' - less_than_zero (TRUE or FALSE). This implies whether the KPI/metric can theoretically go below 0 (as a count or as a percentage) for graphical purposes
#' - polarity ("down" or "up"). Determines whether an improvement is up or down
#' - A time field
#' - A value field
#' - An indicator field
#' - Target (optional)
#'
#' Having columns for these indicators allows producing SPC charts/tables on mass with indicators of varying properties
#'
#'
#' @param data A df with the following columns: 1. A indicator column, e.g. KPI (used to group by if processing more than one indicator); 2. A time field (dates); 3. A value field reflecting performance; 4. A polarity field indicting whether an upward trend indicates improvement ("up", "down", "neutral"); 5. A field called greater_than_hundred indicating whether the value can be legitimately over 100; 6. A field called less_than_zero indicating whether the value can be below zero (T / F); 7. A unit column indicating type of data ("percent" or "count")
#' @param indicator The column in data reflecting the group by column, as a character string. If no grouping column exists, set as NULL.
#' @param time_field The column in data reflecting the time component, as a character string
#' @param value The column in data reflecting the value component to be measured, as a character string
#' @param base_date_range A base date range, structure like c("2021-02-01", "2021-09-01") i.e. c(start, end)
#' @param rebase_dates Manually decided rebasing of dates. Structured like c("2021-09-01", "2022-06-01").
#' @param rebase_data_frame Defaults to NULL. Assign dataframe of interest if you have a dataframe of rebase dates by indicator. Dataframe will join by indicator, meaning unique rebase dates can be applied on mass to multiple indicators during processing. If indicator does not exist in rebasing df, then no rebasing will occur to particular indicator.
#' @param exclude_outliers Set to T if outliers are desired to be excluded from process calculations
#' Reflects periods of rebasing. Can be rebased multiple times. Processing function will fill dates of rebasing downwards. E.g. above example, there will be three groups, start:2021-09-01, 2021-10-01:2022-06-01, 2022-07-01:end
#' @param target Defaults to NULL. Target can either be inputted as a column character string or a known integer, e.g. 80
#' @param output "data", "chart", "summary", "narrative" or "status"
#' @param line_breaks Defaults to F. Determines whether lines will have breaks between rebasing (LCL, UCL and Mean)
#' @param mode "interactive" or "static", depending on output desired. Applied to narratives and summaries
#' @param package In respect to interactive charts output, "ggplot2", "echarts"/"echarts4r" or "plotly"
#' @param plot_title Provides the plot with a title if the output is chart
#' @param yrange Provides a axis range if the output is chart. Argument should be c(min,max). A count of 5 is +/- from values
#' @param chart_theme Theme for chart. Uses spc_chart_options function (see ?HccSPC::spc_chart_options()).
#' @param group_average Compare a metric value against the process of the average of a group of metrics. Defaults to F.
#' @param nad Numerator and denominator. Set to TRUE if you have a Numerator and Denominator column and want to present them figures in the table
#' @param time_unit Defaults to "month". Can set as "day", "month", "quarter" or "week". Only relevant when using summary and narrative outputs. Chart only impacted if "quarter" is selected.
#' @examples
#'
#' library(dplyr)
#'
#' tooth_data <- force(ToothGrowth) %>%
#'   filter(supp == "VC") %>% slice(-15:-27) %>%
#'   mutate(Date = seq.Date(as.Date("2021-01-01"), as.Date("2021-01-17"), by = "days"),
#'   polarity = "up",
#'   greater_than_hundred = FALSE,
#'   less_than_zero = FALSE,
#'   unit = "count")
#'
#'
#' #Retreive SPC data
#
#' spc_output(
#'        data = tooth_data,
#'        time_field = "Date",
#'        indicator = "supp",
#'       value = "len",
#'        output = "data")
#'
#'# Retrieve static SPC plot
#'
#'spc_output(
#'data = tooth_data,
#'        rebase_dates = "2021-01-09", #With a rebase date,
#'time_field = "Date",
#'indicator = "supp",
#'value = "len",
#'output = "chart",
#'package = "ggplot") %>%
#'spc_add_icons() # with icons
#'
#'
#' spc_output(
#' data = tooth_data,
#' rebase_dates = c("2021-01-04", "2021-01-10"),
#' time_field = "Date",
#' indicator = "supp",
#' value = "len",
#' output = "chart",
#' package = "ggplot",
#' target = 30)
#'
#'
#' spc_output(
#'        data = tooth_data,
#'        time_field = "Date",
#'        indicator = "supp",
#'        value = "len",
#'        output = "chart",
#'        package = "ggplot",
#'        target = 30,
#'        plot_title = "Size of teeth (or something along those lines) SPC",
#'        time_unit = "day",
#'        chart_theme = spc_chart_options(x_label = "Day",
#'                                        y_label = "Count",
#'                                        x_label_format = "%b-%d",
#'                                        title_size = 25,
#'                                        x_breaks = "1 day"
#'                                        ))
#'
#'# Retrieve SPC in plotly
#'
#'spc_output(
#'        data = tooth_data,
#'        time_field = "Date",
#'        indicator = "supp",
#'        value = "len",
#'        output = "chart",
#'        package = "plotly")
#'
#'# Retrieve SPC in echarts
#'
#'spc_output(
#'        data = tooth_data,
#'        time_field = "Date",
#'        indicator = "supp",
#'        value = "len",
#'        output = "chart",
#'        package = "echarts") %>%
#'        spc_add_icons()
#'
#'
#'# Retrieve a narrative
#'
#'spc_output(
#'        data = tooth_data,
#'        time_field = "Date",
#'        indicator = "supp",
#'        value = "len",
#'        output = "narrative")
#'
#'spc_output(
#'        data = tooth_data,
#'        time_field = "Date",
#'        indicator = "supp",
#'        value = "len",
#'        output = "narrative",
#'        mode = "interactive")
#'
#'
#'# Retrieve summary table
#'
#' spc_output(
#'        data = tooth_data,
#'        time_field = "Date",
#'        indicator = "supp",
#'        value = "len",
#'        output = "summary",
#'        mode = "static")
#'
#'spc_output(
#'        data = tooth_data,
#'        time_field = "Date",
#'        indicator = "supp",
#'        value = "len",
#'        output = "summary",
#'        mode = "interactive",
#'        nad = FALSE)
#'
#'
#' @export
#' @importMethodsFrom rlang .data
#' @importFrom magrittr %>%

spc_output <- function(data,
                       indicator,
                       time_field,
                       value,
                       exclude_outliers = FALSE,
                       base_date_range = NULL,
                       rebase_dates = NULL,
                       rebase_data_frame = NULL,
                       target = NULL,
                       mode = "static",
                       output = "chart",
                       package = NULL,
                       plot_title = NULL,
                       yrange = NULL,
                       group_average = FALSE,
                       line_breaks = FALSE,
                       chart_theme = NULL,
                       nad = FALSE,
                       time_unit = "month"
){


  if(!(output %in% c("data", "chart", "narrative", "summary", "status"))){
    stop("Output not correctly specified. Needs to be 'data', 'chart', 'narrative', 'status' or 'summary'!")
  }

  if(is.null(indicator)){

    warning("No indicator column provided. Column will be generated assuming there is only one group of data. If not, specifiy an indicator column.")
  }

  if(output == "chart"){

    if(is.null(package)){
      warning("You have requested a chart but you have not specified a package. Defaults to a static ggplot. Set package as either 'ggplot' for static or 'plotly' or 'echarts'/'echarts4r' for an interactive chart!")
      package = "ggplot"
    } else if(!(package %in% c("ggplot", "plotly", "echarts", "echarts4r"))){
      warning("Assigned package is not within options available. Please specify 'ggplot', 'plotly' or 'echarts'/'echarts4r. Package will default to ggplot.")
      package = "ggplot"
    }

  }

  if(output %in% c("summary", "narrative")){
    warning(paste0("Table output will be ", mode, ". Set mode to static or interactive to change."))
  }

  if(exclude_outliers == FALSE){
    warning("Outliers will be included in process limit calculations!")
  }

  if(is.numeric(target)){
    data$Target = target
  } else if(is.null(target)){
    data <- dplyr::mutate(data, Target = NA)
  } else {
    data <- data %>%
      dplyr::mutate(Target = !!dplyr::ensym(target))

    if(!is.numeric(data$Target)) stop("Target column specified is not numeric!")

  }


  if(!("greater_than_hundred" %in% colnames(data))){
    data$greater_than_hundred <- F
    warning("No greater_than_hundred column detected so it has been set as F for all indicators!")
  }
  if(!("less_than_zero" %in% colnames(data))){
    data$less_than_zero <- F
    warning("No less_than_zero column detected so it has been set as F for all indicators!")
  }
  if(!("unit" %in% colnames(data))){
    data$unit <- "count"
    warning("No unit column detected so it has been set as count for all indicators (create unit column to specify whether unit should be percent or count for an indicator)")
  }
  if(!("polarity" %in% colnames(data))){
    data$polarity <- "up"
    warning("No polarity column detected so it has been set as up for all indicators (create polarity column to specify whether polarity should be up or down for an indicator)")
  }

  # Error catching ----------------------------------------------------------


  if(any(is.na(data[[value]]))){ # 
    stop("Value column has NA's values so SPC will break. Look at data. If you are aware of NA's that are due to no denominator for a given month or an unknown count,
         filter out before proceeding with SPC (if there are many NA's, consider whether SPC is appropriate)")
  }

  if(!is.null(base_date_range) & (!is.null(rebase_dates) | !is.null(rebase_data_frame))){
    stop("Cannot have both rebasing and a base date range. One has to be NULL")
  }

  if(!is.null(rebase_dates) & !is.vector(rebase_dates)){
    stop("Rebase dates specified are not in vector form. For example, c('2022-09-01') or c('2021-09-01', '2022-09-01')!")
  }


if(is.null(indicator)){

    data <- data %>%
      dplyr::mutate(indicator = "Indicator 1")

  } else {

    data <- data %>%
      dplyr::mutate(indicator = !!dplyr::ensym(indicator))

  }
  
if(is.null(value)){
  stop("`value` arguement is empty. Assign column of values into order for SPC to run.")
}



  # Processing  -------------------------------------------------------------

  data <- data %>%
    dplyr::mutate(time_field = !!dplyr::ensym(time_field),
                  value = !!dplyr::ensym(value)) %>%
    dplyr::select(time_field, indicator, dplyr::contains("Numerator"), dplyr::contains("Denominator"), value, Target, polarity, unit, greater_than_hundred, less_than_zero)

  if(!inherits(data$time_field, c('Date','POSIXct','POSIXt'))) stop("Specified time_field is not of date class!")
  if(!inherits(data$value, c('numeric','integer'))) stop("Specified value_field is not of numeric class!")


  if(group_average == T){

    group <- data %>%
      dplyr::mutate(time_field = as.Date(time_field),
                    value = as.numeric(value)) %>%
      dplyr::group_by(time_field) %>%
      dplyr::mutate(value = mean(value),
                    indicator = "Average") %>%
      dplyr::ungroup() %>%
      dplyr::distinct() %>%
      dplyr::select(time_field, value, Target, polarity, unit, greater_than_hundred, less_than_zero, dplyr::contains("Numerator"), dplyr::contains("Denominator")) %>%
      dplyr::mutate(mR = abs(value - dplyr::lag(value, default = data.table::first(value))),
                    average_mR = mean(mR[-1], na.rm = TRUE),
                    mR = ifelse(exclude_outliers == T,
                                dplyr::case_when(mR < 3.267 * average_mR ~ mR,
                                                 TRUE ~ as.numeric(NA)
                                ),
                                mR),
                    average_mR = mean(mR[-1], na.rm = TRUE),
                    mean = mean(value, na.rm = TRUE),
                    upper_ci = mean + (2.66*average_mR),
                    lower_ci = mean - (2.66*average_mR),
                    rebase_group = as.character(1)) %>%
      dplyr::select(time_field, mR, average_mR, mean, upper_ci, lower_ci, rebase_group)

    data <- data %>%
      dplyr::left_join(group, by = "time_field") %>%
      dplyr::group_by(indicator) %>%
      dplyr::group_modify(~ spc_processing(.x,
                                           group_average = T,
                                           base_date_range = base_date_range,
                                           rebase_dates = rebase_dates))

  } else {




    data <- data %>%
      dplyr::mutate(time_field = as.Date(time_field),
                    value = as.numeric(value),
                    indicator_ref = indicator) %>%
      dplyr::group_by(indicator) %>%
      dplyr::group_modify(~ spc_processing(.x,
                                           group_average = FALSE,
                                           base_date_range = base_date_range,
                                           rebase_dates = rebase_dates,
                                           rebase_data_frame = rebase_data_frame,
                                           exclude_outliers = exclude_outliers
      )) %>%
      dplyr::ungroup()

  }

  if(output == "data"){

    spc <- data

  } else if(output == "narrative"){

    spc <- spc_narrative(.data = data,
                         .mode = mode,
                         .time_unit = time_unit)

  } else if(output == "summary") {


    spc <- spc_summary_table(.data = data,
                             .mode = mode,
                             .value = value,
                             .time_field = time_field,
                             .indicator = indicator,
                             .nad = nad,
                             .time_unit = time_unit)

  } else if(output == "chart"){

    
    spc <- spc_chart(.data = data,
                     .base_date_range = base_date_range,
                     .package = package,
                     .plot_title = plot_title,
                     .yrange = yrange,
                     .line_breaks = line_breaks,
                     .chart_theme = chart_theme)


  } else if(output == "status"){

    spc <- spc_status(.data = data,
                      .value = value,
                      .time_field = time_field,
                      .indicator = indicator)

  }

  return(spc)

}
