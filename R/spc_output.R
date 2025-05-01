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
#' @param baseline_point_number Defaults to NULL Set to a number if you want your SPC to be calculated using only the first n points of your data (and each distinct rebased period). This sets the baseline to n points across each rebased group. Conflicts with base_date_range, however can be used in conjunction with rebase_dates or rebase_data_frame
#' @param base_date_range A base date range, structure like c("2021-02-01", "2021-09-01") i.e. c(start, end)
#' @param rebase_dates Manually decided rebasing of dates. Structured like c("2021-09-01", "2022-06-01").
#' @param rebase_data_frame Defaults to NULL. Assign dataframe of interest if you have a dataframe of rebase dates by indicator. Dataframe will join by indicator, meaning unique rebase dates can be applied on mass to multiple indicators during processing. If indicator does not exist in rebasing df, then no rebasing will occur to particular indicator.
#' @param exclude_outliers Set to T if outliers are desired to be excluded from process calculations
#' Reflects periods of rebasing. Can be rebased multiple times. Processing function will fill dates of rebasing downwards. E.g. above example, there will be three groups, start:2021-09-01, 2021-10-01:2022-06-01, 2022-07-01:end
#' @param target Defaults to NULL. Target can either be inputted as a column character string or a known integer, e.g. 80
#' @param output "data", "chart", "summary", "narrative" or "status"
#' @param summary_output The desired output type of summary, depending on intentions. Either "table", which produces the final table summary (flextable or reactable depending on mode), or "dataframe", which returns the summary as is before the final table output. Allows for further editing.
#' @param line_breaks Defaults to F. Determines whether lines will have breaks between rebasing (LCL, UCL and Mean)
#' @param mode "interactive" or "static", depending on output desired. Applied to narratives and summaries
#' @param package In respect to interactive charts output, "ggplot2", "echarts"/"echarts4r" or "plotly"
#' @param plot_title Provides the plot with a title if the output is chart
#' @param yrange Provides a axis range if the output is chart. Argument should be c(min,max). A count of 5 is +/- from values
#' @param chart_theme Theme for chart. Uses spc_chart_options function (see ?HccSPC::spc_chart_options()).
#' @param group_average Compare a metric value against the process of the average of a group of metrics. Defaults to F.
#' @param nad Numerator and denominator. Set to TRUE if you have a Numerator and Denominator column and want to present them figures in the table
#' @param time_unit Defaults to "month". Can set as "day", "month", "quarter" or "week". Only relevant when using summary and narrative outputs. Chart only impacted if "quarter" is selected.
#' @param sort_by Determines sorting of table. Can be sorted by assurance, variation, by both, or by alphabetical order of indicator. Defaults to "concern both by assurance". See ?HertsSPC::sort_spc_summary_table() for all available options. To sort by indicator alphabetically, assign "indicator" description
#' @examples
#'
#' library(dplyr)
#'
#' start_date <- as.Date("2025-01-01")
#' end_date <- as.Date("2025-01-20")
#' date_sequence <- seq.Date(from = start_date, to = end_date, by = "day")
#' 
#' 
#' indicator_data_1 <- 
#'   data.frame(Date = date_sequence,
#'              kpi = "Indicator 1",
#'              indicator_value = c(45,48,44,43,45,
#'                                  65,45,46,46,44,
#'                                  43,42,41,40,39,
#'                                  46,47,56,52,50),
#'              target = 60)
#'
#'
#' #Retreive SPC data
#'
#' spc_output(
#'        data = indicator_data_1,
#'        time_field = "Date",
#'        indicator = "kpi",
#'        value = "indicator_value",
#'        output = "data")
#'
#'# Retrieve static SPC plot
#'
#' indicator_data_3 <- 
#'      data.frame(
#'         Date = seq.Date(from = start_date, 
#'                         to = as.Date("2025-01-30"), 
#'                         by = "day"),
#'         kpi = "Indicator 3",
#'         indicator_value = c(45,48,44,43,45,
#'                             45,45,47,46,44,
#'                                  43,44,43,28,44,
#'                                  85,88,88,83,85,
#'                                  85,85,87,86,88,
#'                                  83,88,83,88,88),
#'              target = 45)
#'
#'spc_output(
#'data = indicator_data_3,
#'rebase_dates = "2021-01-15", #With a rebase date,
#'time_field = "Date",
#'indicator = "kpi",
#'value = "indicator_value",
#'output = "chart",
#'package = "ggplot") %>%
#'spc_add_icons() # with icons
#'
#'
#'spc_output(
#' data = indicator_data_3,
#' rebase_dates = c("2021-01-10", "2021-01-20"),
#' time_field = "Date",
#'indicator = "kpi",
#'value = "indicator_value",
#' output = "chart",
#' package = "ggplot",
#' target = 30)
#'
#'
#' spc_output(
#'        data = indicator_data_1,
#'        time_field = "Date",
#'        indicator = "kpi",
#'        value = "indicator_value",
#'        output = "chart",
#'        package = "ggplot",
#'        target = 30,
#'        plot_title = "Performance over time",
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
#'        data = indicator_data_1,
#'        time_field = "Date",
#'        indicator = "kpi",
#'        value = "indicator_value",
#'        output = "chart",
#'        package = "plotly")
#'
#'# Retrieve SPC in echarts
#'
#'spc_output(
#'        data = indicator_data_1,
#'        time_field = "Date",
#'        indicator = "kpi",
#'        value = "indicator_value",
#'        output = "chart",
#'        package = "echarts") %>%
#'        spc_add_icons()
#'
#'
#'# Retrieve a narrative
#'
#'spc_output(
#'        data = indicator_data_1,
#'        time_field = "Date",
#'        indicator = "kpi",
#'        value = "indicator_value",
#'        output = "narrative")
#'
#'spc_output(
#'        data = indicator_data_1,
#'        time_field = "Date",
#'        indicator = "kpi",
#'        value = "indicator_value",
#'        output = "narrative",
#'        mode = "interactive")
#'
#'
#'# Retrieve summary table
#'
#' spc_output(
#'        data = indicator_data_1,
#'        time_field = "Date",
#'        indicator = "kpi",
#'        value = "indicator_value",
#'        output = "summary",
#'        mode = "static")
#'
#'spc_output(
#'        data = dplyr::bind_rows(indicator_data_1,  indicator_data_3),
#'        time_field = "Date",
#'        indicator = "kpi",
#'        value = "indicator_value",
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
                       baseline_point_number = NULL,
                       base_date_range = NULL,
                       rebase_dates = NULL,
                       rebase_data_frame = NULL,
                       target = NULL,
                       mode = "static",
                       output = "chart",
                       summary_output = "table",
                       package = NULL,
                       plot_title = NULL,
                       yrange = NULL,
                       group_average = FALSE,
                       line_breaks = FALSE,
                       chart_theme = NULL,
                       nad = FALSE,
                       sort_by = "concern both by assurance",
                       time_unit = "month"
){


  # Error message to ensure an approriate output type is called upon (case sensitive)
  
  if(!(output %in% c("data", "chart", "narrative", "summary", "status"))){
    stop("Output not correctly specified. Needs to be 'data', 'chart', 'narrative', 'status' or 'summary'!")
  }

  
  # If no indicator column provided it will warn the user. Instead of stopping, it will assume that
  # only one indicator's worth of data (i.e. one group) is provided and the SPC will process as such
  
  if(is.null(indicator)){

    warning("No indicator column provided. Column will be generated assuming there is only one group of data. If not, specifiy an indicator column.")
  }

  # If the output is "chart",  but no chart package is provided, it will default to ggplot
  # Similarly, if there's a spelling error or a package type is not available, it will default to ggplot
  
  if(output == "chart"){

    if(is.null(package)){
      
      warning("You have requested a chart but you have not specified a package. Defaults to a static ggplot. Set package as either 'ggplot' for static or 'plotly' or 'echarts'/'echarts4r' for an interactive chart!")
      
      package <- "ggplot"
      
    } else if(!(package %in% c("ggplot", "plotly", "echarts", "echarts4r"))){
      
      warning("Assigned package is not within options available. Please specify 'ggplot', 'plotly' or 'echarts'/'echarts4r. Package will default to ggplot.")
      
      package <- "ggplot"
      
    }

  }

  
  if(output %in% c("summary", "narrative")){
    warning(paste0("Table output will be ", mode, ". Set mode to static or interactive to change."))
  }

  
  if(exclude_outliers == FALSE){
    warning("Outliers will be included in process limit calculations!")
  }

  # Target can be provided as either a raw number (e.g 80) or in reference to 
  # a column in the data (e.g. "target_column")
  
  # If the target is a number, it will create a column with the target
  # This assumes one group, if you have multiple indicators user will need to
  # provide a target column within the original dataframe
  
  # If target is a column name, it will assign the column to a new one called "Target"
  # It will error if the target column provided is not numeric
  
  if(is.numeric(target)){
    data$Target = target
  } else if(is.null(target)){
    data <- dplyr::mutate(data, Target = NA)
  } else {
    data <- data %>%
      dplyr::mutate(Target = !!dplyr::ensym(target))

    if(!is.numeric(data$Target)) stop("Target column specified is not numeric!")

  }
  

  # If a summary table is desired but the provided sorting method (if any) is not 
  # in the below options, it will error
  
  if(output == "summary" & !sort_by %in% c("indicator",
                                           "assurance concern", "assurance improve", 
                                           "variation concern",  "variation improve", 
                                           "improve both by variation", "improve both by assurance", 
                                           "concern both by assurance", "concern both by variation")){
    
    stop("Output is summary but sort by is assigned as an unavailable options. Use ?HertsSPC::sort_spc_summary_table() to see all options!")
    
  }
  

  # If greater_than_hundred and less_than_zero doesn't exist in the provided dataframe,
  # both will be created below (and assigned to F)
  # This is based on the assumption the SPC is for a proportional % indicator
  
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

  # Some error captures 
  # If any NA's, user will be encouraged to be removed

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
  
  if(!is.null(base_date_range) & !is.null(baseline_point_number)) {
    stop("You have provided a base date range and set baseline_point_number as a number. Either set base_date_range to NULL or set baseline_point_number to NULL")
  }
  
  if(is.character(baseline_point_number)){
    stop("Ensure baseline_point_number is numeric")
  }
  
  # If no indicator column is provided, it will be assigned Indicator 1
  # If provided, indicator column is created
  
  if(is.null(indicator)) {
    data <- data %>%
      dplyr::mutate(indicator = "Indicator 1")
    
  } else {
    data <- data %>%
      dplyr::mutate(indicator = !!dplyr::ensym(indicator))
    
  }
  
  
  # If no value column is provided, it will error
  
  if(is.null(value)) {
    stop("`value` arguement is empty. Assign column of values into order for SPC to run.")
  }



  # Processing  -------------------------------------------------------------

  # Ensures the time field and value field are present
  # Selects all important columns (including Numerator and Denominator if they are present)
  
  data <- data %>%
    dplyr::mutate(time_field = !!dplyr::ensym(time_field),
                  value = !!dplyr::ensym(value)) %>%
    dplyr::select(time_field, indicator, dplyr::contains("Numerator"), dplyr::contains("Denominator"), value, Target, polarity, unit, greater_than_hundred, less_than_zero)

  # Checks the time and value fields are date and numeric in nature
  
  if(!inherits(data$time_field, c('Date','POSIXct','POSIXt'))) stop("Specified time_field is not of date class!")
  if(!inherits(data$value, c('numeric','integer'))) stop("Specified value_field is not of numeric class!")


  # The group_average argument compares each KPI to the average values of them all
  # Not used too often (if at all)
  
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

    # The usual method of processing

    data <- data %>%
      
      # ensures classes are appropriate 
      # creates a reference column for the indicator
      
      dplyr::mutate(time_field = as.Date(time_field),
                    value = as.numeric(value),
                    indicator_ref = indicator) %>%
      
      # Groups by the indicator as the SPC process will be applied to each distinct KPI
      # spc_processing found in the spc_processing.R script
      
      dplyr::group_by(indicator) %>%
      dplyr::group_modify(~ spc_processing(.x,
                                           group_average = FALSE,
                                           baseline_point_number = baseline_point_number,
                                           base_date_range = base_date_range,
                                           rebase_dates = rebase_dates,
                                           rebase_data_frame = rebase_data_frame,
                                           exclude_outliers = exclude_outliers
      )) %>%
      dplyr::ungroup()

  }

  
  # Returns based on output argument 
  # It uses the data created above, this data is then fed into other outputting functions
  # depending on what is called for
  
  if(output == "data"){

    # Returns the SPC data - this will typically be fed into a spc_chart() filtered 
    # for a single indicator. Or, into a spc_chart whilst looping through the unique indicators
    
    spc <- data

  } else if(output == "narrative"){

    
    # See spc_narrative.R for processing
    
    spc <- spc_narrative(.data = data,
                         .mode = mode,
                         .time_unit = time_unit)
    

  } else if(output == "summary") {

    
    # See spc_summary_table.R for processing
    # This is a 'scorecard', so multiple KPI/Indicators can be fed into this function
    
    spc <- spc_summary_table(.data = data,
                             .mode = mode,
                             .value = value,
                             .time_field = time_field,
                             .indicator = indicator,
                             .nad = nad,
                             .time_unit = time_unit,
                             .sort_by = sort_by,
                             .summary_output = summary_output)
    

  } else if(output == "chart"){

    # See spc_chart.R for process
    # Only one indicator at a time can be passed through to this function
    
    spc <- spc_chart(.data = data,
                     .base_date_range = base_date_range,
                     .package = package,
                     .plot_title = plot_title,
                     .yrange = yrange,
                     .line_breaks = line_breaks,
                     .chart_theme = chart_theme,
                     .time_unit = time_unit)


  } else if(output == "status"){

    spc <- spc_status(.data = data,
                      .value = value,
                      .time_field = time_field,
                      .indicator = indicator)

  }

  return(spc)

}
