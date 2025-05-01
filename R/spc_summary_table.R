
utils::globalVariables(c(".","time_field", "indicator", "value", "Mean", "upper_ci", "lower_ci",
                         "unit", "mean", "polarity", "Target", "cons_trend6_icon",
                         "value_breach_icon", "higher_than_mean",
                         "lower_than_mean", "two_low_sum", "two_high_sum",
                         "two_low", "two_high",
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
#' start_date <- as.Date("2025-01-01")
#' end_date <- as.Date("2025-01-20")
#' date_sequence <- seq.Date(from = start_date, to = end_date, by = "day")
#' 
#' 
#' indicator_data_1 <- 
#'   data.frame(Date = date_sequence,
#'              kpi = "Indicator 1",
#'              Numerator = c(45,48,44,43,45,
#'                                  65,45,46,46,44,
#'                                  43,42,41,40,39,
#'                                  46,47,56,52,50),
#'              Denominator = c(95,99,94,101,96,
#'                                  95,96,94,97,100,
#'                                  88,95,93,96,92,
#'                                  95,95,96,97,96),                   
#'              target = 60,
#'              unit = "percent",
#'              polarity = "up",
#'              greater_than_hundred = FALSE,
#'              less_than_zero = FALSE) %>% 
#'  dplyr::mutate( indicator_value = round((Numerator/Denominator)*100, 2))
#' 
#' 
#' indicator_data_2 <- 
#'   data.frame(Date = date_sequence,
#'              kpi = "Indicator 2",
#'              indicator_value = c(45,48,44,43,45,
#'                                  45,45,47,46,44,
#'                                  43,44,43,28,44,
#'                                  45,47,45,43,46),
#'              target = 45,
#'              greater_than_hundred = TRUE,
#'              unit = "count",
#'              polarity = "up",
#'              less_than_zero = FALSE)
#' 
#' 
#' all_indicator_data <- bind_rows(indicator_data_1, indicator_data_2)
#' 
#' spc_data <- HertsSPC::spc_output(
#'   data = all_indicator_data,
#'   time_field = "Date",
#'   indicator = "kpi",
#'   value = "indicator_value",
#'   output = "data",
#'   target = "target"
#' )
#'
#'
#' spc_data <- spc_output(data = all_indicator_data,
#'                        time_field = "Date", 
#'                         indicator = "kpi",
#'                         value = "indicator_value",
#'                         output = "data",
#'                         target = "target"
#'                         )
#'
#' spc_summary_table(.data = spc_data,
#'                   .mode = "static",
#'                   .time_field = "Day",
#'                   .time_unit = "day",
#'                   .indicator = "Metric",
#'                   .value = "Value",
#'                   .nad = TRUE)
#'
#' spc_summary_table(.data = spc_data,
#'                   .mode = "interactive",
#'                   .time_field = "Day",
#'                   .time_unit = "day",
#'                   .indicator = "Metric",
#'                   .value = "Value",
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


  # Set to make sure time unit is set appropriately (defaults to month)
  
  if(!.time_unit %in% c("month", "day", "week", "quarter")){
    stop("Time unit is not set to month, day, quarter or week. Correct before continuing!")
  }

  mode <- .mode
  data <- .data
  time_unit <- if(.time_unit == "month") "%b-%Y" else if(.time_unit == "day") "%d %b %Y" else if(.time_unit == "week") "%d %b %Y" else if(.time_unit == "quarter") NA
  summary_output <- .summary_output

  
  # Force error if nad (Numerator And Denominator) is set to T but a Numerator or Denominator column doesn't exist in the data

  if(.nad == T & ("Numerator" %in% colnames(data)  == F | "Denominator" %in% colnames(data)== F)){
    stop(".nad is set to true however Numerator and/or Denominator columns to not exist in the original data input. Set .nad to FALSE.")
  }


  
  spc_table <- data %>%
    
    # Group by indicator and test for assurance / variation for each point
    
    dplyr::group_by(indicator) %>%
    
    # Two assurance columns 
    # 1) used for sorting - on target, failing target, variable target
    # 2) Assurance column with pathway to image determined by assurance_sort
    
    dplyr::mutate(assurance_sort = dplyr::case_when(unique(polarity) == "up" & utils::tail(lower_ci,1) > utils::tail(Target,1) ~ "on target",
                                                    unique(polarity) == "up" & utils::tail(upper_ci, 1) < utils::tail(Target,1)  ~ "failing target",
                                                    unique(polarity) == "up" & utils::tail(upper_ci,1) >= utils::tail(Target, 1) & utils::tail(Target, 1) >= utils::tail(lower_ci, 1) ~ "variable target",
                                                    unique(polarity) == "down" & utils::tail(upper_ci, 1) < utils::tail(Target, 1) ~ "on target",
                                                    unique(polarity) == "down" & utils::tail(lower_ci, 1) > utils::tail(Target, 1) ~ "failing target",
                                                    unique(polarity) == "down" & utils::tail(upper_ci, 1) >= utils::tail(Target, 1) & utils::tail(Target, 1) >= utils::tail(lower_ci, 1) ~ "variable target",
                                                    T ~ "NO ASSURANCE"),
                  Assurance = dplyr::case_when(
                    assurance_sort == "on target" ~ paste0("icons/" ,"Consistently_Hitting_Target_Transparent.png"),
                    assurance_sort == "failing target" ~ paste0("icons/", "Consistently_Failing_Target_Transparent.png"),
                    assurance_sort == "variable target"  ~ paste0("icons/", "Hit_and_Miss_Transparent.png"),
                    T ~ " "
                  )
    ) %>%

    # Determines the variation and if any triggers have been hit
    # 1) variation_sort - similar to assurance
    # 2) Variation - a pathway to the appropriate icon
    
    dplyr::mutate(indicator = as.character(as.factor(indicator)),
                  upper_ci = round(upper_ci, 2),
                  lower_ci = round(lower_ci, 2),
                  variation_sort = dplyr::case_when(
                    unique(polarity) == "up" & (value_breach_icon == "Lower" |
                                                        cons_trend6_icon == "Lower" |
                                                        !is.na(utils::tail(lower_than_mean, 1)) |
                                                        !is.na(tail(two_low_sum,1))
                    ) ~ "concern special low",
                    unique(polarity) == "up" & (value_breach_icon == "Higher" |
                                                        cons_trend6_icon == "Higher" |
                                                  !is.na(utils::tail(higher_than_mean, 1)) |
                                                  !is.na(tail(two_high_sum,1))
                    ) ~ "improve special high",
                    unique(polarity) == "neutral" & (value_breach_icon == "Higher" |
                                                             cons_trend6_icon == "Higher" |
                                                       !is.na(utils::tail(higher_than_mean, 1)) |
                                                       !is.na(tail(two_high_sum,1))
                    ) ~ "neutral special high",
                    unique(polarity) == "neutral" & (value_breach_icon == "Lower" |
                                                             cons_trend6_icon == "Lower" |
                                                       !is.na(utils::tail(lower_than_mean, 1)) |
                                                       !is.na(tail(two_low_sum,1))
                    ) ~ "neutral special low",
                    unique(polarity) == "down" & (value_breach_icon == "Lower" |
                                                          cons_trend6_icon == "Lower" |
                                                    !is.na(utils::tail(lower_than_mean, 1)) |
                                                    !is.na(tail(two_low_sum,1))
                    ) ~ "improve special low",
                    unique(polarity) == "down" & (value_breach_icon == "Higher" |
                                                          cons_trend6_icon == "Higher" |
                                                    !is.na(utils::tail(higher_than_mean, 1)) |
                                                    !is.na(tail(two_high_sum,1))
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
    
    # Filters latest data point to be presented on the score card
    
    dplyr::filter(time_field == max(time_field)) %>%
    dplyr::ungroup() %>%
    
    # Select the columns of interest
    # assurance_sort and variation_sort are included for sorting purposes
    # and are omitted from the final output
    
    dplyr::select(indicator, time_field, value, Target,
                  Variation, Assurance, dplyr::contains("Status"),
                  dplyr::contains("Numerator"), dplyr::contains("Denominator"),
                  unit, Mean, LCL, UCL,
                  assurance_sort, variation_sort)


  # Sorts the above table
  
  spc_table <- sort_spc_summary_table(data = spc_table,     # Find function in helpers.R
                                      sort_by = .sort_by)
  
  
  # Assigns time field format 
  # Quarters are processed in a different way - see spc_narrative.R for commentated example
  
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

  
  # The following section of ifelse's produce a table depending on arguments 
  # passed to function
  # The processing for static and interactive tables differ
  
  if(mode == "interactive" & summary_output != "dataframe"){

    # The interactive table includes a tooltip with the 
    
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
                    ),
                    value = ifelse(.nad == T & unit == "percent",
                                   paste0(value, " (", Numerator, "/",Denominator,")"),
                                   value)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-assurance_sort, -variation_sort)

    
    # For each row in the table (each unique KPI), the variation and 
    # assurance icons are identified on the in individuals system (these are included in the package)
    # see spc_img_uri in helpers.R

    for(i in 1:nrow(spc_table)){

      if(spc_table[i, "Variation"] != " "){

        spc_table[i, "Variation"] <- spc_img_uri(system.file(paste0(spc_table[i, "Variation"]), package = "HertsSPC"))



      }

      if(spc_table[i, "Assurance"] != " "){

        spc_table[i, "Assurance"] <- spc_img_uri(system.file(paste0(spc_table[i, "Assurance"]), package = "HertsSPC"))

      }

    }

    
    # Select columns and columns names

    spc_table <-  spc_table %>%
      dplyr::select(indicator, time_field, value, Target,
                    Variation, Assurance, Mean, LCL,
                    UCL, tooltip_variation, tooltip_assurance)


    colnames(spc_table) <- c(.indicator, .time_field,
                             .value, "Target", "Variation",
                             "Assurance", "Mean", "LCL", "UCL", "tooltip_variation",
                             "tooltip_assurance")


    # Feed processed data into a reactable
    # see helpers.R for reference to spc_reactable_tippy(), which creates the 
    # tooltip for the icons
    
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


    # For the static table, there's no tooltip

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

    
    # For each row in the table (each unique KPI), the variation and 
    # assurance icons are identified on the in individuals system (these are included in the package)

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
    
    
    # Returns the table in df format - this may be used if further processing
    # is needed
    # For example, adding non-SPC indicators to a scorecard
    # If that's the case, the code from the static / interactive chart can be copied
    # into a normal script, or the user has freedom to design the table as they see fit
    
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
    
    
    spc_table <-  spc_table %>%
      dplyr::select(indicator, time_field, value,  Target,
                    Variation, Assurance, Mean)
    
  }
    


  return(spc_table)

}
