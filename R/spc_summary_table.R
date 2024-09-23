
utils::globalVariables(c(".","time_field", "indicator", "value", "Mean", "upper_ci", "lower_ci",
                         "unit", "mean", "polarity", "Target", "cons_trend6_icon",
                         "value_breach_icon", "higher_than_mean",
                         "lower_than_mean", "two_low_sum", "two_high_sum",
                         "value_breach",
                         "variation_sort", "assurance_sort",
                         "Variation", "Assurance",
                         "UCL", "LCL", "Mean", "Numerator",
                         "Denominator", "tooltip_variation",
                         "tooltip_assurance"))

#' SPC Summary Table functionality.
#'
#' @description The function used to output data as an SPC summary table. Can be ran independently from spc_output()
#' @param .data Data cleaned and processed within spc_output(), or data returned from spc_ouput(output = "data").
#' @param .mode mode entered into spc_output(mode) or can be entered into spc_summary_table() the same way ("static" or "interactive")
#' @param .indicator column reflecting the indicator, which is grouped. Needed for column header
#' @param .time_field column reflecting the time. Needed for column header
#' @param .value column reflecting value to be reported. Needed for column header
#' @param .nad Determines whether the numerator and denominator appear in the final table (alongside the value). Only set to T if a numerator and denominator column is present in your input dataframe. Defaults to F.
#' @param .time_unit Can be set to "month", "day", "quarter" or "week". Determines the date column format in the summary table.
#' @param .sort_by Determines sorting of table. Can be sorted by assurance, variation, by both, or by alphabetical order of indicator. Defaults to "concern both by assurance". See ?HertsSPC::sort_spc_summary_table() for all available options. To sort by indicator alphabetically, assign "indicator"
#' @param .summary_output summary_output The desired output type of summary, depending on intentions. Either "table", which produces the final table summary (flextable or reactable depending on mode), or "dataframe", which returns the summary as is before the final table output. Allows for further editing.
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
#' spc_summary_table(.data = spc_data,
#'                   .mode = "static",
#'                   .time_field = "Day",
#'                   .indicator = "Tooth Type",
#'                   .value = "Size",
#'                   .nad = FALSE)
#'
#' spc_summary_table(.data = spc_data,
#'                   .mode = "interactive",
#'                   .time_field = "Day",
#'                   .indicator = "Tooth Type",
#'                   .value = "Size",
#'                   .nad = FALSE)
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data



spc_summary_table <- function(.data,
                              .mode = "static",
                              .indicator,
                              .time_field,
                              .value,
                              .nad = FALSE,
                              .time_unit = "month",
                              .summary_output = "table",
                              .sort_by = "concern both by assurance"
){


  if(!.time_unit %in% c("month", "day", "week", "quarter")){
    stop("Time unit is not set to month, day, quarter or week. Correct before continuing!")
  }

  mode <- .mode
  data <- .data
  time_unit <- if(.time_unit == "month") "%b-%Y" else if(.time_unit == "day") "%d %b %Y" else if(.time_unit == "week") "%d %b %Y" else if(.time_unit == "quarter") NA
  summary_output <- .summary_output


  if(.nad == T & ("Numerator" %in% colnames(data)  == F | "Denominator" %in% colnames(data)== F)){
    stop(".nad is set to true however Numerator and/or Denominator columns to not exist in the original data input. Set .nad to FALSE.")
  }


  spc_table <- data %>%
    dplyr::group_by(indicator) %>%
    dplyr::mutate(assurance_sort = dplyr::case_when(unique(polarity) == "up" & utils::tail(lower_ci,1) > utils::tail(Target,1) ~ "on target",
                                                    unique(polarity) == "up" & utils::tail(upper_ci, 1) <= utils::tail(Target,1)  ~ "failing target",
                                                    unique(polarity) == "up" & utils::tail(upper_ci,1) >= utils::tail(Target, 1) & utils::tail(Target, 1) > utils::tail(lower_ci, 1) ~ "variable target",
                                                    unique(polarity) == "down" & utils::tail(upper_ci, 1) < utils::tail(Target, 1) ~ "on target",
                                                    unique(polarity) == "down" & utils::tail(lower_ci, 1) >= utils::tail(Target, 1) ~ "failing target",
                                                    unique(polarity) == "down" & utils::tail(upper_ci, 1) >= utils::tail(Target, 1) & utils::tail(Target, 1) > utils::tail(lower_ci, 1) ~ "variable target",
                                                    T ~ "NO ASSURANCE"),
                  Assurance = dplyr::case_when(
                    assurance_sort == "on target" ~ paste0("icons/" ,"Consistently_Hitting_Target_Transparent.png"),
                    assurance_sort == "failing target" ~ paste0("icons/", "Consistently_Failing_Target_Transparent.png"),
                    assurance_sort == "variable target"  ~ paste0("icons/", "Hit_and_Miss_Transparent.png"),
                    T ~ " "
                  )
    ) %>%

    dplyr::mutate(indicator = as.character(as.factor(indicator)),
                  upper_ci = round(upper_ci, 2),
                  lower_ci = round(lower_ci, 2),
                  variation_sort = dplyr::case_when(
                    unique(polarity) == "up" & (value_breach_icon == "Lower" |
                                                        cons_trend6_icon == "Lower" |
                                                        !is.na(utils::tail(lower_than_mean, 1)) |
                                                        two_low_sum == 1
                    ) ~ "concern special low",
                    unique(polarity) == "up" & (value_breach_icon == "Higher" |
                                                        cons_trend6_icon == "Higher" |
                                                  !is.na(utils::tail(higher_than_mean, 1)) |
                                                    two_high_sum == 1
                    ) ~ "improve special high",
                    unique(polarity) == "neutral" & (value_breach_icon == "Higher" |
                                                             cons_trend6_icon == "Higher" |
                                                       !is.na(utils::tail(higher_than_mean, 1)) |
                                                         two_high_sum == 1
                    ) ~ "neutral special high",
                    unique(polarity) == "neutral" & (value_breach_icon == "Lower" |
                                                             cons_trend6_icon == "Lower" |
                                                       !is.na(utils::tail(lower_than_mean, 1)) |
                                                         two_low_sum == 1
                    ) ~ "neutral special low",
                    unique(polarity) == "down" & (value_breach_icon == "Lower" |
                                                          cons_trend6_icon == "Lower" |
                                                    !is.na(utils::tail(lower_than_mean, 1)) |
                                                      two_low_sum == 1
                    ) ~ "improve special low",
                    unique(polarity) == "down" & (value_breach_icon == "Higher" |
                                                          cons_trend6_icon == "Higher" |
                                                    !is.na(utils::tail(higher_than_mean, 1)) |
                                                      two_high_sum == 1
                    ) ~ "concern special high",
                    T ~ "common cause"),
                  Variation =  dplyr::case_when(
                    variation_sort == "concern special low" ~ paste0("icons/", "Concerning_Special_Cause_Low_Transparent.png"),
                    variation_sort == "improve special high" ~ paste0("icons/", "Improving_Special_Cause_High_Transparent.png"),
                    variation_sort == "improve special low" ~ paste0("icons/", "Improving_Special_Cause_Low_Transparent.png"),
                    variation_sort == "concern special high" ~ paste0("icons/", "Concerning_Special_Cause_High_Transparent.png"),
                    variation_sort == "neutral special high" ~ paste0("icons/", "Neutral_Special_Cause_High_Transparent.png"),
                    variation_sort == "neutral special low" ~ paste0("icons/", "Neutral_Special_Cause_Low_Transparent.png"),
                    T ~ paste0("icons/", "Common_Cause_Transparent.png")),

                  value = paste0(round(value, 1), ifelse(unit == "percent", "%", "")),
                  LCL = dplyr::case_when(less_than_zero == F & lower_ci < 0 ~ 0,
                                         T ~ lower_ci),
                  UCL = dplyr::case_when(greater_than_hundred == F & upper_ci > 100 ~ 100,
                                         T ~ upper_ci),
                  UCL = paste0(round(UCL, 1), ifelse(unit == "percent", "%", "")),
                  LCL = paste0(round(LCL, 1), ifelse(unit == "percent", "%", "")),
                  Target = paste0(Target, ifelse(unit == "percent", "%", "")),
                  Mean = paste0(round(mean, 1), ifelse(unit == "percent", "%", ""))
    )  %>%
    dplyr::group_by(indicator) %>%
    dplyr::filter(time_field == max(time_field)) %>%
    dplyr::ungroup() %>%
    dplyr::select(indicator, time_field, value, Target,
                  Variation, Assurance, dplyr::contains("Status"),
                  dplyr::contains("Numerator"), dplyr::contains("Denominator"),
                  unit, Mean, LCL, UCL,
                  assurance_sort, variation_sort)


  spc_table <- sort_spc_summary_table(data = spc_table,     # Find function in helpers.R
                                      sort_by = .sort_by)
  
  
  if(time_unit != "quarter"){
    spc_table <- spc_table %>%
      dplyr::mutate(time_field = format(time_field, time_unit))
  } else if(time_unit == "quarter"){
    spc_table <- spc_table %>%
      dplyr::mutate(time_field = gsub("Q0", "Q4",
                                      paste0("Q",
                                             lubridate::quarter(time_field)-1,
                                             " ",
                                             ifelse(lubridate::month(time_field) %in% c(1,2,3),
                                                    paste0(stringr::str_sub(lubridate::year(time_field) - 1,-2,-1), "/", stringr::str_sub(lubridate::year(time_field),-2,-1)),
                                                    paste0(stringr::str_sub(lubridate::year(time_field),-2,-1), "/", stringr::str_sub(lubridate::year(time_field) + 1,-2,-1))
                                             ))
      ))
  }

  

  if(mode == "interactive" & summary_output != "dataframe"){


    spc_table <- spc_table %>%
      dplyr::group_by(indicator) %>%
      dplyr::mutate(tooltip_variation = paste0(indicator, " - ",
                                               paste(if(variation_sort == "concern special low") "currently undergoing concerning special cause variation (low)."
                                                     else if(variation_sort == "concern special high") "currently undergoing concerning special cause variation (high)."
                                                     else if(variation_sort == "improve special low") "currently undergoing improving special cause variation (low)."
                                                     else if(variation_sort == "improve special high") "currently undergoing improving special cause variation (high)."
                                                     else if(variation_sort == "neutral special high") "currently undergoing neutral special cause variation (high)."
                                                     else if(variation_sort == "neutral special low") "currently undergoing neutral special cause variation (low)."
                                                     else if(variation_sort == "common cause") "currently undergoing common cause variation."
                                                     else if(variation_sort == "straight up blue") "increased since last data point in positive direction"
                                                     else if(variation_sort == "straight down blue") "decreased since last data point in a positive direction"
                                                     else if(variation_sort == "straight up yellow") "increased since last data point in a concerning direction"
                                                     else if(variation_sort == "straight down yellow") "decreased since last data point in a concerning direction"
                                                     else if(variation_sort == "straight forward green") "no change since last data point")),
                    tooltip_assurance = paste0(indicator, " - ",
                                               if(assurance_sort == "failing target") "process cannot be expected to meet target. "
                                               else if(assurance_sort == "on target") "Process can be expected to consistently meet target. "
                                               else if(assurance_sort == "variable target") "Process can be expected to inconsistently meet target. "
                                               else "NO ASSURANCE "
                    )) %>%
      dplyr::ungroup() %>%
      dplyr::select(-assurance_sort, -variation_sort)


    for(i in 1:nrow(spc_table)){

      if(spc_table[i, "Variation"] != " "){

        spc_table[i, "Variation"] <- spc_img_uri(system.file(paste0(spc_table[i, "Variation"]), package = "HertsSPC"))



      }

      if(spc_table[i, "Assurance"] != " "){

        spc_table[i, "Assurance"] <- spc_img_uri(system.file(paste0(spc_table[i, "Assurance"]), package = "HertsSPC"))

      }

    }


    spc_table <-  spc_table %>%
      dplyr::select(indicator, time_field, value, Target,
                    Variation, Assurance, Mean, LCL,
                    UCL, tooltip_variation, tooltip_assurance)


    colnames(spc_table) <- c(.indicator, .time_field,
                             .value, "Target", "Variation",
                             "Assurance", "Mean", "LCL", "UCL", "tooltip_variation",
                             "tooltip_assurance")


    spc_table <- spc_table %>%
      reactable::reactable(pagination=F,
                           bordered = T,
                           striped = T,
                           sortable = F,
                           defaultColDef = reactable::colDef(vAlign = "center"),
                           columns = list(
                             tooltip_variation = reactable::colDef(show = F),
                             tooltip_assurance = reactable::colDef(show = F),
                             Target = reactable::colDef(minWidth = 70),
                             Variation = reactable::colDef(html = T,
                                                           cell =  function(value, index, name) {
                                                             spc_reactable_tippy(text = value,
                                                                             tooltip = spc_table[index, "tooltip_variation"],
                                                                             placement = "left"
                                                             )}),
                             Assurance = reactable::colDef(html = T,
                                                           cell =  function(value, index, name) {
                                                             spc_reactable_tippy(text = value,
                                                                             tooltip = spc_table[index, "tooltip_assurance"],
                                                                             placement = "left"
                                                             )}))

      )








  } else if(mode == "static"  & summary_output != "dataframe"){



    spc_table <- spc_table %>%
      dplyr::rowwise() %>%
      dplyr::mutate(Assurance = ifelse(Assurance == " " | is.na(Assurance),
                                       paste0("icons/", "white_space.png"),
                                       Assurance),
                    Variation = ifelse(Variation == " ",
                                       paste0("icons/","white_space.png"),
                                       Variation),
                    value = ifelse(.nad == T & unit == "percent",
                                   paste0(value, " (", Numerator, "/",Denominator,")"),
                                   value)
      )


    for(i in 1:nrow(spc_table)){

      if(spc_table[i, "Variation"] != " "){

        spc_table[i, "Variation"] <- system.file(paste0(spc_table[i, "Variation"]), package = "HertsSPC")

      }


      if(spc_table[i, "Assurance"] != " "){

        spc_table[i, "Assurance"] <- system.file(paste0(spc_table[i, "Assurance"]), package = "HertsSPC")

      }


    }


    fontname <- "Arial"

    spc_table <-  spc_table %>%
      dplyr::select(indicator, time_field, value,  Target,
                    Variation, Assurance, Mean)



    colnames(spc_table) <- c(.indicator, .time_field,
                             .value, "Target", "Variation",
                             "Assurance",
                             "Mean")

    spc_table <- spc_table %>%
      flextable::flextable() %>%
      flextable::style(pr_t= officer::fp_text(font.family='Arial'), part = "all") %>%
      flextable::align(part = "body", align = "center") %>%
      flextable::fontsize(size = 9) %>%
      flextable::border_outer(border = officer::fp_border("#000000")) %>%
      flextable::vline( border = officer::fp_border("#000000"), part = "all") %>%
      flextable::hline(border = officer::fp_border("#000000") , part = "all") %>%
      flextable::bg(j = c("Variation", "Assurance"), bg = "#ffffff") %>%
      flextable::colformat_image(
        j = c("Variation", "Assurance"), width = 30/60, height = 30/60) %>%
      flextable::set_table_properties(
        opts_html = list(
          scroll = NULL))


  } else if(summary_output == "dataframe"){
    
    
    spc_table <- spc_table %>%
      dplyr::rowwise() %>%
      dplyr::mutate(Assurance = ifelse(Assurance == " " | is.na(Assurance),
                                       paste0("icons/", "white_space.png"),
                                       Assurance),
                    Variation = ifelse(Variation == " ",
                                       paste0("icons/","white_space.png"),
                                       Variation),
                    value = ifelse(.nad == T & unit == "percent",
                                   paste0(value, " (", Numerator, "/",Denominator,")"),
                                   value)
      )
    
    
    for(i in 1:nrow(spc_table)){
      
      if(spc_table[i, "Variation"] != " "){
        
        spc_table[i, "Variation"] <- system.file(paste0(spc_table[i, "Variation"]), package = "HertsSPC")
        
      }
      
      
      if(spc_table[i, "Assurance"] != " "){
        
        spc_table[i, "Assurance"] <- system.file(paste0(spc_table[i, "Assurance"]), package = "HertsSPC")
        
      }
      
      
    }
    
    
    fontname <- "Arial"
    
    spc_table <-  spc_table %>%
      dplyr::select(indicator, time_field, value,  Target,
                    Variation, Assurance, Mean)
    
  }
    


  return(spc_table)

}
