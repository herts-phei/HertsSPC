
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
#' indicator_data_2 <- 
#'   data.frame(Date = date_sequence,
#'              kpi = "Indicator 2",
#'              indicator_value = c(45,48,44,43,45,
#'                                  45,45,47,46,44,
#'                                  43,44,43,28,44,
#'                                  45,47,45,43,60),
#'              target = 45)
#' 
#' 
#' all_indicator_data <- bind_rows(indicator_data_1, indicator_data_2) %>% 
#'   dplyr::mutate(polarity = "up",
#'                 greater_than_hundred = FALSE,
#'                 less_than_zero = FALSE,
#'                 unit = "count")
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
#' spc_narrative(.data = filter(spc_data, indicator == "Indicator 1"),
#'               .mode = "static",
#'               .time_unit = "day")
#'
#' spc_narrative(.data = filter(spc_data, indicator == "Indicator 2"),
#'               .mode = "interactive",
#'               .time_unit = "day")
#'               
#' HertsSPC::spc_output(
#'   data = filter(all_indicator_data, kpi == "Indicator 1"),
#'   time_field = "Date",
#'   indicator = "kpi",
#'   value = "indicator_value",
#'   output = "narrative",
#'   mode = "interactive",
#'   target = "target"
#' )                     
#'               
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data


spc_narrative <- function(.data,
                          .mode,
                          .time_unit = "month"){


  data <- .data
  mode <- .mode
  
  # Set date format depending on for table depending on time_unit argument
  
  time_unit <- if(.time_unit == "month") "%b-%Y" else if(.time_unit == "day") "%d %b %Y" else if(.time_unit == "week") "%d %b %Y"


  if (.time_unit != "quarter") {
    
    # If day, week, month, it returns the latest date vale and the previous date value
    
    date <-  paste0(format(max(data$time_field), time_unit))
    
    previous_date <- paste0(format(utils::tail(data$time_field, 2)[1], time_unit))
    
    
  } else {
    
    # If time_unit is quarter, it will return the latest date value
    # This is used to calculate the quarter the date falls under
    # (May be a better way to do this, but the issue is the Quarters run financially, April-March,
    # as opposed to Jan - December, the way are deals sets quarters)
    
    quarter_date <- max(.data$time_field)
      
    date <-
      gsub("Q0", 
           "Q4",
           
           # The below forms the Quarter the month falls in
           # We combine the Q and the Year determined by the latest date
           # in the time field
           
           # The following commentary will use a date of 2024-03-01 (1st March 2024) as an example
           # The final output for this should be Q4 23/24 (the final quarter of the 2023 2024 financial year)
           
           paste0(
             # starts with a Q
             "Q",
             
             # Obtain the quarter from the date - lubridates quarter() function
             # would return Q1 (going by calendar year), so we minus 1 from the number, to obtain Q0 (we convert this to Q4 at the top of this chunk, as is gsub())
             lubridate::quarter(quarter_date) - 1,
             
             # Add a space ready to put the year
             " ",
             
             # Our year, in this example, will be 23/24
             # We feed the year calculation into an ifelse
             
             ifelse(
              
               # If the month is January, February or *March*...
               lubridate::month(quarter_date) %in% c(1, 2, 3),
               
               
               paste0(
                 
                 # This code returns the first part, 23
                 # year("2024-03-01") returns 2024
                 # So the code minuses 1, returning 2023
                 # The last 2 digits are extracted, returning 23
                 stringr::str_sub(lubridate::year(quarter_date) - 1, -2, -1),
                 
                 # separates the above and below
                 "/",
                 
                 # Returns the year 2024, and the last two digits 24
                 stringr::str_sub(lubridate::year(quarter_date), -2, -1)
                 ),
               
               
               # If it's any other month, one does not need to be subtracted from the first component,
               # rather 1 needs to be added to the second
               # For example, 2024-06-01 would need to be 24/25
               
               paste0(
                 stringr::str_sub(lubridate::year(quarter_date), -2, -1),
                 "/",
                 stringr::str_sub(lubridate::year(quarter_date) + 1, -2, -1)
                 )
               )
             )
           )


    # Same as the above but for the quarter previous
        
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


  # Shouldn't be any at this point, but filters out NA's
  
  data <- dplyr::filter(data, !is.na(value))

  # If the unit is a percent, set to %
  
  if(unique(data$unit) == "percent"){
    unit = "%"
  } else {
    unit = ""
  }

  
  # Returns the polarity for the KPI
  
  polarity = unique(data$polarity)

  # Takes last 6 values
  
  values <- utils::tail(data$value[!is.na(data$value)], 6)

  # This section tests against the assurance metric
  # Assurance is only tested against is the latest time point has a target value
  # The sentence produced to used in the final narrative table
  
  # If the KPI polarity is up (i.e. up is good)...
  
  if(polarity == "up" & is.na(utils::tail(data$Target, 1)) == F) {

    
    if(polarity == "up"  & utils::tail(data$Target, 1) < utils::tail(data$lower_ci,1)){

      # If the target falls below the lower_ci of the SPC, it's on target / meeting target
      
      assurance <- "Process consistently meeting performance target"
      
    } else if(polarity == "up"  & utils::tail(data$Target, 1) > utils::tail(data$upper_ci,1)){
      
      # If the target sits above upper_ci of the SPC, it's not going to hit target

      assurance <- "Process consistently not meeting performance target"

    } else if(polarity == "up"  & dplyr::between(utils::tail(data$Target, 1),
                                                 utils::tail(data$lower_ci, 1),
                                                 utils::tail(data$upper_ci, 1)
                                          )
              ){
      
      # If the target falls between the lower_ci and upper_ci, it will not consistently hit target of the SPC, it's on target / meeting target

      assurance <- "Process inconsistently meeting performance target"

    }

    
    # The same as above, but the opposite way around - assumes the KPI direction 
    # is down and there is a target for the latest data point
    
  } else if(polarity == "down"  & is.na(utils::tail(data$Target, 1)) == F) {

    if(polarity == "down" & utils::tail(data$Target, 1) > utils::tail(data$upper_ci, 1)){

      assurance <- "Process consistently meeting performance target"

    } else if(polarity == "down" & utils::tail(data$Target, 1) < utils::tail(data$lower_ci, 1)){

      assurance <- "Process consistently not meeting performance target"

    } else if(polarity == "down" & dplyr::between(utils::tail(data$Target, 1),
                                           utils::tail(data$lower_ci, 1),
                                           utils::tail(data$upper_ci, 1)
    )){

      assurance <- "Process inconsistently meeting performance target"

    }

    
    # If there is no target, change in value from the previous point is returned instead
    
  } else if(is.na(utils::tail(data$Target, 1)) == T){

    if(utils::tail(data$value, 1) > utils::tail(data$value, 2)[1]){
      assurance <- paste0("Value has increased since previous data point (", previous_date, ", ", round(utils::tail(data$value, 2)[1], 1), unit,")")
    } else if(utils::tail(data$value, 1) < utils::tail(data$value, 2)[1]){
      assurance <- paste0("Value has decreased since previous data point (", previous_date, ", ", round(utils::tail(data$value, 2)[1], 1), unit,")")
    } else if(utils::tail(data$value, 1) == utils::tail(data$value, 2)[1]){
      assurance <- paste0("Value has remianed the same as previous data point (", previous_date, ", ", round(utils::tail(data$value, 2)[1], 1), unit,")")

    }

  }


  # The following section runs through each trigger to produce a relevant sentence
  # These sentences are combined to be presented in the final table, under the 
  # variance section 
  # Some KPI's may be undergoing the multiple causes of improvement / concern
  # Each section uses a column in the spc data output for reference
  # If the trigger for a concern/improvement isn't met, the sentence returns as ""
  
  variation <- spc_variation(data = .data,
                             metric = unique(.data$indicator))

  


  # If the list is empty (i.e. no triggers), the following sentence is returned
  
  if(all(variation == "")){

    variation = "The KPI is currently undergoing common cause variation"

  } else {
    
    # If there are some triggers met, the following is done..
    
    # Empty triggers removed
    variation <- variation[variation != ""]

    # If the triggers are improving, Improving is pasted at the start,
    # alternatively Concerning is pasted at the start
    # If an SPC is being used appropriately, there's shouldn't be a case of
    # concerning and improving triggers for the same point
    
    cause <- paste0(
      if(grepl("Concerning", paste(variation, collapse = ""))) "Concerning "
      else if(grepl("improvement", paste(variation, collapse = ""))) "Improving "
      else ""
    )

    # If the final table is interactive (reactable), the sentences are pasted together with 
    # <br> separators
    
    # If the final table is static (flextable), the sentences are pasted together with 
    # \n separators
    
    if(mode == "interactive"){

      variation <- htmltools::HTML(paste0(cause, "Special Cause Variation:<br> - ",
                                          paste(gsub("[()]|improvement|concern", "", variation), collapse = "<br> - ")))

    } else {

      variation <- htmltools::HTML(paste0(cause, "Special Cause Variation:\n - ",
                                          paste(gsub("[()]|improvement|concern", "", variation), collapse = "\n - ")))

    }

  }

  
  # The tables are built below
  # They are layered in their build, with hard coded titles and 
  # then values put in based on the above
  
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
