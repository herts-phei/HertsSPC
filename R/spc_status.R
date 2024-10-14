utils::globalVariables(c(".","time_field", "indicator", "value", "Mean", "upper_ci", "lower_ci",
                         "unit", "mean", "polarity", "Target", "cons_trend6_icon",
                         "value_breach_icon", "higher_than_mean",
                         "lower_than_mean", "two_low_sum", "two_high_sum",
                         "two_low", "two_high",
                         "value_breach",
                         "variation_sort", "assurance_sort",
                         "variation", "assurance",
                         "UCL", "LCL", "Mean"))


#' SPC status table functionality.
#'
#' @description The function used to output data as a metric level status dataframe. Includes both assurance and variation
#' @param .data Data cleaned and processed within spc_output(), or data returned from spc_ouput(output = "data")
#' @param .indicator column reflecting the indicator, which is grouped. Needed for column header
#' @param .time_field column reflecting the time. Needed for column header
#' @param .value colunm reflecting value to be reported. Needed for column header
#' @examples
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
#'
#' spc_data <- spc_output(data = tooth_data,
#'                        time_field = "Date",
#'                        indicator = "supp",
#'                        value = "len",
#'                        output = "data")
#'
#' spc_status(.data = spc_data,
#'                   .indicator = "Tooth Type",
#'                   .value = "Size",
#'                   .time_field = "Day")
#'
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%

spc_status <- function(.data,
                       .indicator,
                       .time_field,
                       .value
                       # .status = F
){


  data <- .data

  spc_table <- data %>%
    dplyr::group_by(indicator) %>%
    dplyr::mutate(assurance = dplyr::case_when(unique(polarity) == "up" & utils::tail(lower_ci,1) > utils::tail(Target,1) ~ "on target",
                                               unique(polarity) == "up" & utils::tail(upper_ci, 1) <= utils::tail(Target,1)  ~ "failing target",
                                               unique(polarity) == "up" & utils::tail(upper_ci,1) >= utils::tail(Target, 1) & utils::tail(Target, 1) > utils::tail(lower_ci, 1) ~ "variable target",
                                               unique(polarity) == "down" & utils::tail(upper_ci, 1) < utils::tail(Target, 1) ~ "on target",
                                               unique(polarity) == "down" & utils::tail(lower_ci, 1) >= utils::tail(Target, 1) ~ "failing target",
                                               unique(polarity) == "down" & utils::tail(upper_ci, 1) >= utils::tail(Target, 1) & utils::tail(Target, 1) > utils::tail(lower_ci, 1) ~ "variable target")
    ) %>%
    dplyr::mutate(indicator = as.character(as.factor(indicator)),
                  upper_ci = round(upper_ci, 2),
                  lower_ci = round(lower_ci, 2),
                  variation = dplyr::case_when(
                    unique(polarity) == "up" & (value_breach_icon == "Lower" |
                                                  cons_trend6_icon == "Lower" |
                                                  !is.na(utils::tail(lower_than_mean, 1)) |
                                                  two_low == 1
                    ) ~ "concern special low",
                    unique(polarity) == "up" & (value_breach_icon == "Higher" |
                                                  cons_trend6_icon == "Higher" |
                                                  !is.na(utils::tail(higher_than_mean, 1)) |
                                                  two_high == 1
                    ) ~ "improve special high",
                    unique(polarity) == "neutral" & (value_breach_icon == "Higher" |
                                                       cons_trend6_icon == "Higher" |
                                                       !is.na(utils::tail(higher_than_mean, 1)) |
                                                       two_high == 1
                    ) ~ "neutral special high",
                    unique(polarity) == "neutral" & (value_breach_icon == "Lower" |
                                                       cons_trend6_icon == "Lower" |
                                                       !is.na(utils::tail(lower_than_mean, 1)) |
                                                       two_low == 1
                    ) ~ "neutral special low",
                    unique(polarity) == "down" & (value_breach_icon == "Lower" |
                                                    cons_trend6_icon == "Lower" |
                                                    !is.na(utils::tail(lower_than_mean, 1)) |
                                                    two_low == 1
                    ) ~ "improve special low",
                    unique(polarity) == "down" & (value_breach_icon == "Higher" |
                                                    cons_trend6_icon == "Higher" |
                                                    !is.na(utils::tail(higher_than_mean, 1)) |
                                                    two_high == 1
                    ) ~ "concern special high",
                    T ~ "common cause")
    )  %>%
    dplyr::group_by(indicator) %>%
    dplyr::filter(time_field == max(time_field)) %>%
    dplyr::ungroup() %>%
    dplyr::select(indicator, time_field, `Variation Type` = variation, `Assurance Type` = assurance)



}



#' SPC assruance functionality.
#'
#' @description Return the state of assurance for a selected metric that has been processed into an SPC dataframe.
#' @param data Dataframe with SPC processed metric.
#' @param metric The metric in the indicator column you want to return assurance for
#' @examples
#'
#' #NOT TO BE CALLED INDEPENDENTLY
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data



spc_assurance <- function(data,
                          metric){



  data_filtered <- data %>%
    dplyr::filter(indicator == metric)

  values <- utils::tail(data_filtered$value[!is.na(data_filtered$value)], 6)

  polarity <- utils::tail(data_filtered$polarity, 1)

  if(polarity == "up" & is.na(utils::tail(data_filtered$Target, 1)) == F) {

    if(polarity == "up" & utils::tail(data_filtered$Target, 1) <= utils::tail(data_filtered$lower_ci,1)){

      assurance <- "Process can be expected to consistently meet target"

    } else if(polarity == "up" & utils::tail(data_filtered$Target, 1) > utils::tail(data_filtered$upper_ci,1)){

      assurance <- "Process consistently not meeting performance target"

    } else if(polarity == "up" & utils::tail(data_filtered$upper_ci, 1) >= utils::tail(data_filtered$Target, 1) & utils::tail(data_filtered$Target, 1) > utils::tail(data_filtered$lower_ci, 1)){

      assurance <- "Process will meet target inconsistently"

    }

  } else if(polarity == "down" & is.na(utils::tail(data_filtered$Target, 1)) == F) {

    if(polarity & utils::tail(data_filtered$Target, 1) >= utils::tail(data_filtered$upper_ci, 1)){

      assurance <- "Process can be expected to consistetnly meet target"

    } else if(polarity == "down" & utils::tail(data_filtered$Target, 1) <= utils::tail(data_filtered$lower_ci, 1)){

      assurance <- "Process consistently not meeting performance target"

    } else if(polarity == "down" & utils::tail(data_filtered$upper_ci, 1) >= utils::tail(data_filtered$Target, 1) & utils::tail(data_filtered$Target, 1) > utils::tail(data_filtered$lower_ci, 1)){

      assurance <- "Process will meet target inconsistently"

    }

  } else if(is.na(utils::tail(data_filtered$Target, 1)) == T){

    assurance <- "Metric does not have target"

  }




  return(assurance)

}



#' SPC variation functionality.
#'
#' @description Return the state of variation for a selected metric that has been processed into an SPC dataframe.
#' @param data Dataframe with SPC processed metric.
#' @param metric The metric in the indicator column you want to return variation for.
#' @examples
#'
#' #NOT TO BE CALLED INDEPENDENTLY
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data



spc_variation <- function(data,
                          metric){



  data_filtered <- dplyr::filter(data, indicator == metric)

  values <- utils::tail(data_filtered$value[!is.na(data_filtered$value)], 6)

  polarity <- utils::tail(data_filtered$polarity, 1)


  if(!is.na(utils::tail(data_filtered$value_breach, 1))){

    breached_value <- paste0(
      if(utils::tail(data_filtered$value_breach_icon, 1) == "Lower" & polarity == "down") "Latest value below lower control limit (improvement)"
      else if(utils::tail(data_filtered$value_breach_icon, 1) == "Lower" & polarity == "up") "Latest value below lower control limit (concern)"
      else if(utils::tail(data_filtered$value_breach_icon, 1) == "Higher" & polarity == "up") "Latest value above upper control limit (improvement)"
      else "Latest value above upper control limit (concern)")

  } else {


    breached_value <- ""

  }


  if(!is.na(utils::tail(data_filtered$cons_trend6_icon, 1)) & utils::tail(data_filtered$cons_trend6_icon, 1) != "common"){


    breached_consec <- paste0(
      if(utils::tail(data_filtered$cons_trend6_icon, 1) == "Lower" & polarity == "down") "Measure has fallen 6 consecutive data points (improvement)"
      else if(utils::tail(data_filtered$cons_trend6_icon, 1) == "Lower" & polarity == "up ") "Measure has fallen 6 consecutive data points (concern)"
      else if(utils::tail(data_filtered$cons_trend6_icon, 1) == "Higher" & polarity == "down") "Measure has rissen 6 consecutive data points (conern)"
      else "Measure has risen 6 consecutive data points (improvement)")

  } else {

    breached_consec <- ""


  }



  if(utils::tail(!is.na(data_filtered$higher_than_mean), 1) | utils::tail(!is.na(data_filtered$lower_than_mean), 1)){

    breached_side_of_mean <- paste0(
      if(utils::tail(!is.na(data_filtered$lower_than_mean), 1) & polarity == "down") "Latest 6 data points are below mean (improvement)"
      else if (utils::tail(!is.na(data_filtered$lower_than_mean), 1) & polarity == "up") "Latest 6 data points are below mean (concern)"
      else if (utils::tail(!is.na(data_filtered$higher_than_mean), 1) & polarity == "up") "Latest 6 data points are above mean (improvement)"
      else "Latest 6 data points are above mean (concern)")

  } else {

    breached_side_of_mean <- ""

  }


  if(!is.na(utils::tail(data_filtered$two_low_sum, 1))){

    two_of_three_low <- paste0(
      if(polarity == "up")
        "Two of three data points within lower zone A (improvement)"
      else
        "Two of three data points within lower zone A (concern)")

  } else {

    two_of_three_low <- ""

  }


  if(!is.na(utils::tail(data_filtered$two_high_sum, 1))){

    two_of_three_high <- paste0(
      if(polarity == "Y")
        "Two of three data points within upper zone A (concern)"
      else
        "Two of three data points within upper zone A (improvement)")

  } else {

    two_of_three_high <- ""

  }


  variation_text <- c(breached_value,
                      breached_consec,
                      breached_side_of_mean,
                      two_of_three_high, two_of_three_low)




}
