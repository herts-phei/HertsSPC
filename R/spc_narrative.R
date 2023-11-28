
utils::globalVariables(c(".","time_field", "value", "Mean", "upper_ci", "lower_ci",
                         "unit", "mean", "polarity", "Target", "cons_trend6_icon",
                         "value_breach_icon", "higher_than_mean",
                         "lower_than_mean", "two_low_sum", "two_high_sum",
                         "value_breach"))

# narrative table ---------------------------------------------------------

#' SPC Narrative functionality.
#'
#' @description The function used to output data as an SPC chart. Can be ran independently from spc_output()
#' @param .data Data cleaned and processed within spc_output(), or data returned from spc_ouput(output = "data"). Can only be run with one indicator unless function is in loop.
#' @param .mode "static" or "interactive".
#' @param .time_unit Defaults to "month". Can be "day", "month", "quarter" or "week".
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
#'
#' spc_data <- spc_output(data = tooth_data,
#'                        time_field = "Date",
#'                        indicator = "supp",
#'                        value = "len",
#'                        output = "data")
#'
#' spc_narrative(.data = spc_data,
#'               .mode = "static")
#'
#' spc_narrative(.data = spc_data,
#'               .mode = "interactive")
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data


spc_narrative <- function(.data,
                          .mode,
                          .time_unit = "month"){


  data <- .data
  mode <- .mode
  time_unit <- if(.time_unit == "month") "%b-%Y" else if(.time_unit == "day") "%d %b %Y" else if(.time_unit == "week") "%d %b %Y"


  if(.time_unit != "quarter"){

     date <-  paste0(format(max(data$time_field), time_unit))

     previous_date <- paste0(format(utils::tail(data$time_field, 2)[1], time_unit))

    } else {

    quarter_date <- max(.data$time_field)

    date <- gsub("Q0", "Q4",
                 paste0(
                   "Q",
                   lubridate::quarter(quarter_date) - 1,
                   " ",
                   ifelse(
                     lubridate::month(quarter_date) %in% c(1, 2, 3),
                     paste0(
                       stringr::str_sub(lubridate::year(quarter_date) - 1, -2, -1),
                       "/",
                       stringr::str_sub(lubridate::year(quarter_date), -2, -1)
                     ),
                     paste0(
                       stringr::str_sub(lubridate::year(quarter_date), -2, -1),
                       "/",
                       stringr::str_sub(lubridate::year(quarter_date) + 1, -2, -1)
                     )
                   )
                 ))

    previous_quarter_date <- utils::tail(.data$time_field, 2)[1]

    previous_date <- gsub("Q0", "Q4",
                 paste0(
                   "Q",
                   lubridate::quarter(previous_quarter_date) - 1,
                   " ",
                   ifelse(
                     lubridate::month(previous_quarter_date) %in% c(1, 2, 3),
                     paste0(
                       stringr::str_sub(lubridate::year(previous_quarter_date) - 1, -2, -1),
                       "/",
                       stringr::str_sub(lubridate::year(previous_quarter_date), -2, -1)
                     ),
                     paste0(
                       stringr::str_sub(lubridate::year(previous_quarter_date), -2, -1),
                       "/",
                       stringr::str_sub(lubridate::year(previous_quarter_date) + 1, -2, -1)
                     )
                   )
                 ))


    }


  data <- dplyr::filter(data, !is.na(value))

  if(unique(data$unit) == "percent"){
    unit = "%"
  } else {
    unit = ""
  }

  polarity = unique(data$polarity)

  values <- utils::tail(data$value[!is.na(data$value)], 6)


  if(polarity == "up" & is.na(utils::tail(data$Target, 1)) == F) {

    if(polarity == "up"  & utils::tail(data$Target, 1) <= utils::tail(data$lower_ci,1)){

      assurance <- "Process consistently meeting performance target"

    } else if(polarity == "up"  & utils::tail(data$Target, 1) > utils::tail(data$upper_ci,1)){

      assurance <- "Process consistently not meeting performance target"

    } else if(polarity == "up"  & utils::tail(data$upper_ci, 1) >= utils::tail(data$Target, 1) & utils::tail(data$Target, 1) > utils::tail(data$lower_ci, 1)){

      assurance <- "Process inconsistently meeting performance target"

    }

  } else if(polarity == "down"  & is.na(utils::tail(data$Target, 1)) == F) {

    if(polarity == "down" & utils::tail(data$Target, 1) >= utils::tail(data$upper_ci, 1)){

      assurance <- "Process consistently meeting performance target"

    } else if(polarity == "down" & utils::tail(data$Target, 1) <= utils::tail(data$lower_ci, 1)){

      assurance <- "Process consistently not meeting performance target"

    } else if(polarity == "down" & utils::tail(data$upper_ci, 1) >= utils::tail(data$Target, 1) & utils::tail(data$Target, 1) > utils::tail(data$lower_ci, 1)){

      assurance <- "Process inconsistently meeting performance target"

    }

  } else if(is.na(utils::tail(data$Target, 1)) == T){

    if(utils::tail(data$value, 1) > utils::tail(data$value, 2)[1]){
      assurance <- paste0("Value has increased since previous data point (", previous_date, ", ", round(utils::tail(data$value, 2)[1], 1), unit,")")
    } else if(utils::tail(data$value, 1) < utils::tail(data$value, 2)[1]){
      assurance <- paste0("Value has decreased since previous data point (", previous_date, ", ", round(utils::tail(data$value, 2)[1], 1), unit,")")
    } else if(utils::tail(data$value, 1) == utils::tail(data$value, 2)[1]){
      assurance <- paste0("Value has remianed the same as previous data point (", previous_date, ", ", round(utils::tail(data$value, 2)[1], 1), unit,")")

    }

  }



  if(!is.na(utils::tail(data$value_breach, 1))){

    breached_value <- paste0(
      if(utils::tail(data$value_breach_icon, 1) == "Lower" & polarity == "down") "Latest value below lower control limit (improvement)"
      else if(utils::tail(data$value_breach_icon, 1) == "Lower" & polarity == "up") "Latest value below lower control limit (concern)"
      else if(utils::tail(data$value_breach_icon, 1) == "Lower" & polarity == "neutral") "Latest value below lower control limit"
      else if(utils::tail(data$value_breach_icon, 1) == "Higher" & polarity == "up") "Latest value above upper control limit (improvement)"
      else if(utils::tail(data$value_breach_icon, 1) == "Higher" & polarity == "neutral") "Latest value above upper control limit"
      else "Latest value above upper control limit (concern)")

  } else {


    breached_value <- ""

  }


  if(utils::tail(data$cons_trend6_icon, 1) != "common"){


    breached_consec <- paste0(
      if(utils::tail(data$cons_trend6_icon, 1) == "Lower" & polarity == "down") "Measure has fallen 6 consecutive data points (improvement)"
      else if(utils::tail(data$cons_trend6_icon, 1) == "Lower" & polarity == "up") "Measure has fallen 6 consecutive data points (concern)"
      else if(utils::tail(data$cons_trend6_icon, 1) == "Lower" & polarity == "neutral") "Measure has fallen 6 consecutive data points"
      else if(utils::tail(data$cons_trend6_icon, 1) == "Higher" & polarity == "down") "Measure has risen 6 consecutive data points (conern)"
      else if(utils::tail(data$cons_trend6_icon, 1) == "Higher" & polarity == "neutral") "Measure has risen 6 consecutive data points"
      else "Measure has risen 6 consecutive data points (improvement)")

  } else {

    breached_consec <- ""


  }



  if(utils::tail(!is.na(data$higher_than_mean), 1) | utils::tail(!is.na(data$lower_than_mean), 1)){

    breached_side_of_mean <- paste0(
      if(utils::tail(!is.na(data$lower_than_mean), 1) & polarity == "down") "Latest 6 data points are below mean (improvement)"
      else if (utils::tail(!is.na(data$lower_than_mean), 1) & polarity == "up") "Latest 6 data points are below mean (concern)"
      else if (utils::tail(!is.na(data$lower_than_mean), 1) & polarity == "neutral") "Latest 6 data points are below mean"
      else if (utils::tail(!is.na(data$higher_than_mean), 1) & polarity == "up") "Latest 6 data points are above mean (improvement)"
      else if (utils::tail(!is.na(data$higher_than_mean), 1) & polarity == "neutral") "Latest 6 data points are above mean"
      else "Latest 6 data points are above mean (concern)")

  } else {

    breached_side_of_mean <- ""

  }


  if(!is.na(utils::tail(data$two_low_sum, 1)) == 1){

    two_of_three_low <- paste0(
      if(polarity == "down")
        "Two of three data points within lower zone A (improvement)"
      else if(polarity == "up")
        "Two of three data points within lower zone A (concern)"
      else if(polarity == "neutral")
        "Two of three data points within lower zone A")

  } else {

    two_of_three_low <- ""

  }


  if(!is.na(utils::tail(data$two_high_sum, 1)) == 1){

    two_of_three_high <- paste0(
      if(polarity == "down")
        "Two of three data points within upper zone A (concern)"
      else if(polarity == "up")
        "Two of three data points within upper zone A (improvement)"
      else if(polarity == "neutral")
        "Two of three data points within upper zone A")

  } else {

    two_of_three_high <- ""

  }


  variation <- c(breached_value,
                 breached_consec,
                 breached_side_of_mean,
                 two_of_three_high, two_of_three_low)




  if(all(variation == "")){

    variation = "The KPI is currently undergoing common cause variation"

  } else {

    variation <- variation[variation != ""]

    cause <- paste0(
      if(grepl("Concerning", paste(variation, collapse = ""))) "Concerning "
      else if(grepl("improvement", paste(variation, collapse = ""))) "Improving "
      else ""
    )

    if(mode == "interactive"){

      variation <- htmltools::HTML(paste0(cause, "Special Cause Variation:<br> - ",
                                          paste(gsub("[()]|improvement|concern", "", variation), collapse = "<br> - ")))

    } else {

      variation <- htmltools::HTML(paste0(cause, "Special Cause Variation:\n - ",
                                          paste(gsub("[()]|improvement|concern", "", variation), collapse = "\n - ")))

    }

  }



  if(mode == "static"){

    table <- data.frame(text = c(htmltools::HTML(paste0(date)),
                                 htmltools::HTML(paste0(round(utils::tail(data$value, 1), 1), unit)),
                                 htmltools::HTML(paste0("Variance Type")),
                                 htmltools::HTML(paste0(variation)),
                                 htmltools::HTML(paste0("Assurance")),
                                 htmltools::HTML(paste0(assurance)))) %>%
      flextable::flextable() %>%
      flextable::align(align = "center", part = "all") %>%
      flextable::bg(i = c(1,3, 5),
                    bg="#c9cacb") %>%
      flextable::bold(i = c(1,3,5), part = "body") %>%
      flextable::color(i = c(1,3,5), color = "#000000") %>%
      flextable::width(j = 1, width = 10) %>%
      flextable::delete_part(part = "header") %>%
      flextable::border_outer(part="all") %>%
      flextable::border_outer(border = officer::fp_border("#000000")) %>%
      flextable::vline( border = officer::fp_border("#000000"), part = "all") %>%
      flextable::hline(border = officer::fp_border("#000000") , part = "all") %>%
      flextable::font(fontname = "Trebuchet MS")


  } else {

    table <- data.frame(text = c(htmltools::HTML(paste0(date)),
                                 htmltools::HTML(paste0(utils::tail(data$value, 1))),
                                 htmltools::HTML(paste0("Variance Type")),
                                 htmltools::HTML(paste0(variation)),
                                 htmltools::HTML(paste0("Assurance")),
                                 htmltools::HTML(paste0(assurance))))

    table <- reactable::reactable(table, sortable = F,
                                  columns = list(
                                    text = reactable::colDef(
                                      align = "center",
                                      html = T,
                                      header = "",
                                      style = function(value) {

                                        if(value %in% c("Variance Type", "Assurance", date)){
                                          colour = "#fecb78"
                                        } else {

                                          colour = "white"
                                        }

                                        list(background = colour)
                                      }
                                    )))

  }

  return(table)

}
