utils::globalVariables(c(".","Target", "value", "time_field", "indicator",
                         "polarity", "unit", "less_than_zero", "unit",
                         "greater_than_hundred", "mean", "average_mR", "mR",
                         "lower_ci", "upper_ci", "rebase_group", "time_field_hover"))


#' SPC Charting functionality.
#'
#' @description The function used to create an object a SPC datafame to be used as a crosstalk shared object. Creates a dataframe to be used a s a SharedData. Includes less arguements as normal SPC output/chart functionality as crosstalk is relatively restrcitive (e.g. no icons)
#' @param data A df with the following columns: 1. A indicator column, e.g. KPI (used to group by if processing more than one indicator); 2. A time field (dates); 3. A value field reflecting performance; 4. A polarity field indicting whether an upward trend indicates improvement ("up", "down", "neutral"); 5. A field called greater_than_hundred indicating whether the value can be legitimately over 100; 6. A field called less_than_zero indicating whether the value can be below zero (T / F); 7. A unit column indicating type of data ("percent" or "count")
#' @param indicator The column in data reflecting the group by column, as a character string. If no grouping column exists, set as NULL.
#' @param time_field The column in data reflecting the time component, as a character string
#' @param target Defaults to NULL. Target can either be inputted as a column character string or a known integer, e.g. 80
#' @param value The column in data reflecting the value component to be measured, as a character string
#' @param base_date_range A base date range, structure like c("2021-02-01", "2021-09-01") i.e. c(start, end)
#' @param rebase_dates Manually decided rebasing of dates. Structured like c("2021-09-01", "2022-06-01").
#' @param rebase_data_frame Defaults to NULL. Assign dataframe of interest if you have a dataframe of rebase dates by indicator. Dataframe will join by indicator, meaning unique rebase dates can be applied on mass to multiple indicators during processing. If indicator does not exist in rebasing df, then no rebasing will occur to particular indicator.
#' @param exclude_outliers Set to T if outliers are desired to be excluded from process calculations
#' @param group_average Compare a metric value against the process of the average of a group of metrics. Defaults to F.
#' @examples
#'
#'
#'library(dplyr)
#'
#'tooth_data <- force(ToothGrowth) %>%
#'   filter(supp == "VC") %>% slice(-15:-27) %>%
#'   mutate(Date = seq.Date(as.Date("2021-01-01"), as.Date("2021-01-17"), by = "days"),
#'          supp = "Indicator 1",
#'          polarity = "up",
#'          greater_than_hundred = FALSE,
#'          less_than_zero = FALSE,
#'          unit = "count")
#'
#'tooth_data2 <- force(ToothGrowth) %>%
#'   filter(supp == "VC") %>% slice(-15:-27) %>%
#'   mutate(Date = seq.Date(as.Date("2021-01-01"), as.Date("2021-01-17"), by = "days"),
#'          supp = "Indicator 2",
#'          polarity = "up",
#'          greater_than_hundred = FALSE,
#'          less_than_zero = FALSE,
#'          unit = "count") %>%
#'   bind_rows(tooth_data)
#'
#'
#'# Retreive SPC data
#'
#'spc_data_for_crosstalk <- spc_plotly_data(
#'        data = tooth_data2,
#'        time_field = "Date",
#'        indicator = "supp",
#'        value = "len")
#'
#' @export

spc_plotly_data <- function(data,
                            indicator,
                            time_field,
                            value,
                            exclude_outliers = F,
                            base_date_range = NULL,
                            rebase_dates = NULL,
                            rebase_data_frame = NULL,
                            target = NULL,
                            group_average = F){




  if(exclude_outliers == F){
    print("Outliers will be included in process limit calculations!")
  }

  if(is.numeric(target)){
    data$Target = target
  } else if(is.null(target)){
    data$Target = NA
  } else {
    data <- data %>%
      dplyr::mutate(Target = !!dplyr::ensym(target))

    if(!is.numeric(data$Target)) stop("Target column specified is not numeric!")

  }


  if(!("greater_than_hundred" %in% colnames(data))){
    data$greater_than_hundred <- F
    warning("No greater_than_hundred column detected. Set as F for all indicators!")
  }
  if(!("less_than_zero" %in% colnames(data))){
    data$less_than_zero <- F
    warning("No less_than_zero column detected. Set as F for all indicators!")
  }

  # Error catching ----------------------------------------------------------
  #
  # if(nrow(data) < 12){
  #   stop("Data is less than 12 points long. SPC needs to be 12 points or greater.")
  # }

  if(any(is.na(data[[value]]))){ # AY: col name
    stop("value column has NA's values so SPC will break. Look at data")
  }

  if(!is.null(base_date_range) & !is.null(rebase_dates)){
    stop("Cannot have both rebasing and a base date range. One has to be NULL")
  }

  if(!is.null(rebase_dates) & !is.vector(rebase_dates)){
    stop("Rebase dates specified are not in vector form. For example, c('2022-09-01') or c('2021-09-01', '2022-09-01')!")
  }

  # Processing  -------------------------------------------------------------

  data <- data %>%
    dplyr::mutate(time_field = !!dplyr::ensym(time_field),
                  value = !!dplyr::ensym(value),
                  indicator = !!dplyr::ensym(indicator)) %>%
    dplyr::select(time_field, indicator, dplyr::contains("Numerator"), dplyr::contains("Denominator"), value, Target, polarity, unit, greater_than_hundred, less_than_zero)

  if(!inherits(data$time_field, 'Date')) stop("Specified time_field is not of date class!")
  #  if(!inherits(data$value, 'numeric|integer')) stop("Specified value_field is not of numeric class!")


  if(group_average == T){

    group <- data %>%
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
      dplyr::mutate(indicator_ref = indicator) %>%
      dplyr::group_by(indicator) %>%
      dplyr::group_modify(~ spc_processing(.x,
                                           group_average = F,
                                           base_date_range = base_date_range,
                                           rebase_dates = rebase_dates,
                                           rebase_data_frame = rebase_data_frame,
                                           exclude_outliers = exclude_outliers
      )) %>%
      dplyr::ungroup()

  }



  data_list <- list()

  for(i in unique(data$indicator)){



    d <- data %>%
      dplyr::filter(indicator == i)

    if(unique(d$unit) == "percent"){
      unit = "%"
    } else {
      unit = ""
    }

    less_than_zero <- unique(d$less_than_zero)
    greater_than_hundred <- unique(d$greater_than_hundred)
    polarity <- unique(d$polarity)

    spc_pivot <- d %>%
      dplyr::select(
        time_field, value, value_line, Target, Mean = mean, indicator,
        `Upper CI` = upper_ci, `Lower CI` = lower_ci,
        higher_than_mean, lower_than_mean,
        breach_above, breach_below,
        cons_trend6_high, cons_trend6_low,
        two_low_sum, two_high_sum, polarity, greater_than_hundred, less_than_zero
      ) %>%
      tidyr::pivot_longer(cols = c(value, Mean, `Lower CI`, `Upper CI`, value_line, Target,
                                   breach_above, breach_below,
                                   higher_than_mean, lower_than_mean,
                                   cons_trend6_high, cons_trend6_low,
                                   two_low_sum, two_high_sum),
                          names_to = "variable", values_to = "value") %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::mutate(hover = dplyr::case_when(variable == "breach_below" ~ "Breach below lower limit",
                                             variable == "breach_above" ~ "Breach above upper limit",
                                             variable == "higher_than_mean" ~ "Point one of six(+) above mean",
                                             variable == "lower_than_mean" ~ "Point one of six(+) below mean",
                                             variable == "cons_trend6_low" ~ "Point one of six(+) decreasing consecutively",
                                             variable == "cons_trend6_high" ~ "Point one of six(+) increasing consecutively",
                                             variable == "cons_trend6_mean_low" ~ "Point one of six(+) decreasing consecutively below mean",
                                             variable == "cons_trend6_mean_high" ~ "Point one of six(+) increasing consecutively above mean",
                                             variable == "two_low_sum" ~ "2 out of 3 points in zone A (LCL)",
                                             variable == "two_high_sum" ~ "2 out of 3 points in zone A (UCL)",
                                             T ~ "")) %>%
      dplyr::mutate(variable = dplyr::case_when(variable %in% c("breach_below", "cons_trend6_low", "lower_than_mean", "two_low_sum", "four_five_low_sum") &
                                                  polarity == "up" ~ "Special Cause Variation Concerning",
                                                variable %in% c("breach_below", "cons_trend6_low", "lower_than_mean", "two_low_sum", "four_five_low_sum") &
                                                  polarity == "neutral" ~ "Special Cause Variation",
                                                variable %in% c("breach_above", "higher_than_mean", "cons_trend6_high",  "two_high_sum", "four_five_high_sum") &
                                                  polarity == "up" ~ "Special Cause Variation Improving",
                                                variable %in% c("breach_above", "higher_than_mean", "cons_trend6_high",  "two_high_sum", "four_five_high_sum") &
                                                  polarity == "neutral" ~ "Special Cause Variation",
                                                variable %in% c("breach_below",  "cons_trend6_low","lower_than_mean", "two_low_sum", "four_five_low_sum") &
                                                  polarity == "down" ~ "Special Cause Variation Improving",
                                                variable %in% c("breach_above", "higher_than_mean",  "cons_trend6_high", "two_high_sum", "four_five_high_sum") &
                                                  polarity == "down" ~ "Special Cause Variation Concerning",
                                                T ~ variable)) %>%
      dplyr::group_by(time_field, variable) %>%
      dplyr::mutate(hover = paste0(hover, collapse = "<br>")) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(time_field, variable, .keep_all = T)

    table <- list()

    for(t in unique(spc_pivot$time_field)){

      time_point <- dplyr::filter(spc_pivot, time_field == t)

      if(any(time_point$variable == "Special Cause Variation Improving") &
         any(time_point$variable == "Special Cause Variation Concerning")){
        time_point <- dplyr::filter(time_point, variable != "Special Cause Variation Concerning")
      }

      table[[paste(t)]] <- time_point

    }

    spc_pivot <- do.call(rbind, table)

    rownames(spc_pivot) <- NULL

    hover_text <- dplyr::filter(unique(dplyr::select(spc_pivot, time_field, hover)), hover != "")



    pre_plot <- spc_pivot %>%
      dplyr::select(-hover) %>%
      tidyr::pivot_wider(names_from = variable, values_from = value) %>%
      dplyr::left_join(hover_text, by = "time_field") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(time_field_hover = format(time_field, "%b-%Y"),
                    hover = ifelse(is.na(hover),
                                   paste0(time_field_hover, "<br>Common Cause Variation (",value, unit,")"),
                                   paste0(time_field_hover, "<br>Special Cause Variation (", value, unit, ") :<br>", hover)),
                    `Special Cause Variation Improving` = ifelse("Special Cause Variation Improving" %in% names(.), `Special Cause Variation Improving`, NA),
                    `Special Cause Variation Improving` = as.numeric(`Special Cause Variation Improving`),
                    `Special Cause Variation` = as.numeric(ifelse("Special Cause Variation" %in% names(.), `Special Cause Variation`, NA)),
                    `Special Cause Variation Concerning` = ifelse("Special Cause Variation Concerning" %in% names(.), `Special Cause Variation Concerning`, NA),
                    `Special Cause Variation Concerning` = as.numeric(`Special Cause Variation Concerning`),
                    `Target` = ifelse("Target" %in% names(.), `Target`, NA),
                    `Common Cause Variation` = dplyr::case_when(is.na(`Special Cause Variation Improving`) &
                                                                  is.na(`Special Cause Variation Concerning`) ~ value),
                    `Lower CI` = ifelse("Lower CI" %in% names(.), `Lower CI`, NA),
                    `Upper CI` = ifelse("Upper CI" %in% names(.), `Upper CI`, NA),
                    `Upper CI` = ifelse(greater_than_hundred == "F" & `Upper CI` > 100,100,`Upper CI`),
                    `Lower CI` = ifelse(less_than_zero == "F" & `Lower CI` < 0, 0, `Lower CI`)) %>%
      dplyr::ungroup()

    data_list[[paste(i)]] <- pre_plot

  }

  table <- do.call(rbind, data_list)
  rownames(table) <- NULL

  return(table)


}




#' SPC Charting functionality.
#'
#' @description The function used to output data as a plotly chart linked to a crosstalk filter.
#' @param data Data is the output of spc_plotly_data() (a SharedData object)
#' @examples
#'
#' library(dplyr)
#'
#'tooth_data <- force(ToothGrowth) %>%
#'   filter(supp == "VC") %>% slice(-15:-27) %>%
#'   mutate(Date = seq.Date(as.Date("2021-01-01"), as.Date("2021-01-17"), by = "days"),
#'          supp = "Indicator 1",
#'          polarity = "up",
#'          greater_than_hundred = FALSE,
#'          less_than_zero = FALSE,
#'          unit = "count")
#'
#'tooth_data2 <- force(ToothGrowth) %>%
#'   filter(supp == "VC") %>% slice(-15:-27) %>%
#'   mutate(Date = seq.Date(as.Date("2021-01-01"), as.Date("2021-01-17"), by = "days"),
#'          supp = "Indicator 2",
#'          polarity = "up",
#'          greater_than_hundred = FALSE,
#'          less_than_zero = FALSE,
#'          unit = "count") %>%
#'   bind_rows(tooth_data)
#'
#'
#'# Retreive SPC data
#'
#'spc_data_for_crosstalk <- spc_plotly_data(
#'        data = tooth_data2,
#'        time_field = "Date",
#'        indicator = "supp",
#'        value = "len")
#'
#' spc_plotly_crosstalk(spc_data_for_crosstalk)
#'
#' @export




spc_plotly_crosstalk <- function(data){

  spc <- data %>%
    plotly::plot_ly() %>%
    plotly::add_trace(
      x = ~ time_field,
      y = ~ value,
      type = "scatter",
      line = list(color = "#bdbdbd"),
      name = "Performance",
      mode = 'lines'
    ) %>%
    plotly::add_trace(
      x =  ~ time_field,
      y =  ~ Target,
      type = "scatter",
      mode = 'lines',
      name = paste0("Target"),
      line = list(color = '#FF3333', dash = 'dot')
    ) %>%
    plotly::add_trace(
      x =  ~ time_field,
      y =  ~ Mean,
      type = "scatter",
      mode = 'lines',
      name = paste0("Mean"),
      line = list(color = '#000000', dash = 'dot')
    ) %>%
    plotly::add_trace(
      x =  ~ time_field,
      y =  ~ `Upper CI`,
      type = "scatter",
      mode = 'lines',
      name = paste0("UCL"),
      line = list(color = '#000000', dash = 'dash')
    ) %>%
    plotly::add_trace(
      x =  ~ time_field,
      y =  ~ `Lower CI`,
      type = "scatter",
      mode = 'lines',
      name = paste0("LCL"),
      line = list(color = '#000000', dash = 'dash')
    ) %>%
    plotly::add_trace(
      x =  ~ time_field,
      y =  ~ `Special Cause Variation Concerning`,
      type = "scatter",
      mode = 'markers',
      name = paste0("Concern"),
      hoverinfo = "text",
      hovertemplate =  ~ hover,
      marker = list(color = '#f48601',
                    size = 11)
    ) %>%
    plotly::add_trace(
      x =  ~ time_field,
      y =  ~ `Special Cause Variation Improving`,
      type = "scatter",
      mode = 'markers',
      name = paste0("Improvement"),
      hoverinfo = "text",
      hovertemplate =  ~ hover,
      marker = list(color = '#2b8cf9',
                    size = 11)
    ) %>%
    plotly::add_trace(
      x =  ~ time_field,
      y =  ~ `Common Cause Variation`,
      type = "scatter",
      mode = 'markers',
      name = paste0("Commom Cause"),
      hoverinfo = "text",
      hovertemplate =  ~ hover,
      marker = list(color = '#bdbdbd')
    ) %>%
    plotly::add_trace(
      x =  ~ time_field,
      y =  ~ `Special Cause Variation`,
      type = "scatter",
      mode = 'markers',
      name = paste0("Special Cause"),
      hoverinfo = "text",
      hovertemplate =  ~ hover,
      marker = list(color = '#a300f2',
                    size = 11)
    ) %>%
    plotly::layout(xaxis=list(title=list(text="Date",
                                         font=list(family="Lato",
                                                   size=20),
                                         standoff=35)),
                   yaxis = list(title = list(text = "Performance",
                                             font = list(family = "Lato",
                                                         size = 20),
                                             standoff = 30))
    ) %>%
    plotly::config(modeBarButtonsToRemove = c(
      "zoom2d",
      "pan2d",
      "select2d",
      "lasso2d",
      "zoomIn2d",
      "zoomOut2d",
      "autoScale2d",
      "resetScale2d",
      "hoverClosestCartesian",
      "hoverCompareCartesian",
      "zoom3d",
      "pan3d",
      "resetCameraDefault3d",
      "resetCameraLastSave3d",
      "hoverClosest3d",
      "orbitRotation",
      "tableRotation",
      "zoomInGeo",
      "zoomOutGeo",
      "resetGeo",
      "hoverClosestGeo",
      "sendDataToCloud",
      "hoverClosestGl2d",
      "hoverClosestPie",
      "toggleHover",
      "resetViews",
      "toggleSpikelines",
      "resetViewMapbox"
    ),
    displayLogo = F)



  return(spc)

}

