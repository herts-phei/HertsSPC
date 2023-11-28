utils::globalVariables(c(".","Target", "value", "time_field", "indicator",
                         "polarity", "unit", "less_than_zero", "unit",
                         "greater_than_hundred", "mean", "average_mR", "mR",
                         "lower_ci", "upper_ci", "rebase_group", "lag_trend",
                         "side_of_mean", "two_low", "two_high",
                         "two_low_sum", "two_high_sum", "cons_trend6_group",
                         "cons_trend6_group_icon", "value_breach_icon"))



#' SPC Processing
#'
#' @description The function used to process the data inputted into spc_output(). The function does not work independently
#' @param data Data cleaned within the first half of spc_output(). This data is a df grouped by indicator
#' @param time_field The column reflecting the time field
#' @param value The column reflecting the value field
#' @param base_date_range Base date range entered into spc_output(base_date_range)
#' @param rebase_dates Rebase dates entered into spc_output(rebase_dates)
#' @param rebase_data_frame Whether rebase dates are coming from a dataframe
#' @param target The column reflecting the target column
#' @param group_average Compare a metric value against the process of the average of a group of metrics. Defaults to F. Provided in spc_output().
#' @param exclude_outliers Set to T if outliers are desired to be excluded from process calculations. Provided in spc_output().
#' @examples
#'
#'#NOT TO BE CALLED INDEPENDENTLY
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data




spc_processing <- function(data,
                           time_field, ## The field used for time. Month, weeks, years etc.
                           value, ## value field of the dataset
                           base_date_range = NULL, ## is base date range is wanted, structure like c("2021-02-01", "2021-09-01") i.e. c(start, end)
                           rebase_dates = NULL,
                           rebase_data_frame = NULL,
                           group_average = F,
                           exclude_outliers = F,
                           target = NULL ## If the target is a column in the dataset, name the column name in quotations (e.g. "Target_col"). If its a fixed known value but not a column, put the value (e.g 90)
){



  if(!is.null(base_date_range)){

    base_date_range <- as.Date(base_date_range)

    base_date_df <- data %>%
      dplyr::filter(dplyr::between(time_field, base_date_range[1], base_date_range[2])) %>%
      dplyr::mutate(mR = abs(value - dplyr::lag(value, default = data.table::first(value))),
                    average_mR = mean(mR[-1], na.rm = TRUE),
                    mR = dplyr::case_when(exclude_outliers == F ~ mR,
                                          mR < 3.267 * average_mR ~ mR,
                                          TRUE ~ as.numeric(NA)),
                    average_mR = mean(mR[-1], na.rm = TRUE),
                    mean = mean(value, na.rm = TRUE),
                    upper_ci = mean + (2.66*average_mR),
                    lower_ci = mean - (2.66*average_mR))

    df <- data %>%
      dplyr::mutate(mean = unique(base_date_df$mean),
                    upper_ci = unique(base_date_df$upper_ci),
                    lower_ci = unique(base_date_df$lower_ci),
                    average_mR = unique(base_date_df$average_mR),
                    rebase_group = as.character(1))


  }  else if(!is.null(rebase_dates) | !is.null(rebase_data_frame)) {


    if(is.null(rebase_data_frame)){

      rebase_x <- c(as.Date(rebase_dates),
                    utils::tail(data$time_field, 1))

      
    } else if(!is.null(rebase_data_frame)){

      rebase_x <- sort(c(unique(rebase_data_frame$Rebase.Dates[rebase_data_frame$Indicator == unique(data$indicator_ref)]),
                         utils::tail(data$time_field, 1)))

    }


    df <- data %>%

      # join datframe with the above vector, rebase group assigned for each date

      dplyr::left_join(data.frame(time_field =  rebase_x,
                                  rebase_group = seq(1, length(rebase_x))),
                       by = "time_field") %>%

      # Fill rebase group upwards

      tidyr::fill(rebase_group, .direction = "up") %>%
      dplyr::mutate(time_field = as.Date(time_field)) %>%
      dplyr::group_by(rebase_group) %>%
      dplyr::mutate(mR = abs(value - dplyr::lag(value, default = data.table::first(value))),
                    average_mR = mean(mR[-1], na.rm = TRUE),
                    mR = dplyr::case_when(exclude_outliers == F ~ mR,
                                          mR < 3.267 * average_mR ~ mR,
                                          TRUE ~ as.numeric(NA)),
                    average_mR = mean(mR[-1], na.rm = TRUE),
                    mean = mean(value, na.rm = TRUE),
                    upper_ci = mean + (2.66*average_mR),
                    lower_ci = mean - (2.66*average_mR)
      ) %>%
      dplyr::ungroup()


  }  else if(is.null(base_date_range) & is.null(rebase_dates)) {

    if(group_average == F){

      df <- data %>%
        dplyr::select(time_field, value, Target, polarity,
                      unit, greater_than_hundred, less_than_zero,
                      dplyr::contains("Numerator"), dplyr::contains("Denominator")) %>%
        dplyr::mutate(mean = mean(value, na.rm = TRUE),
                      mR = abs(value - dplyr::lag(value, default = data.table::first(value))),
                      average_mR = mean(mR[-1], na.rm = TRUE),
                      mR = ifelse(exclude_outliers == T &  (mR > 3.267 * average_mR), NA, mR),
                      average_mR = mean(mR[-1], na.rm = TRUE),
                      upper_ci = mean + (2.66*average_mR),
                      lower_ci = mean - (2.66*average_mR),
                      rebase_group = as.character(1))





    } else {

      df <- data

    }
  }


  spc_data <- df


  looped_data <- list()

  for(i in unique(spc_data$rebase_group)){

    df <- spc_data %>% dplyr::filter(rebase_group == i)

    df <- df %>%
      dplyr::arrange(time_field) %>%
      dplyr::mutate(

        # to find trend, compare previous value to current on a rolling basis to see if it increased/decreased
        lag_trend = dplyr::case_when(is.na(dplyr::lag(value)) ~ 0,
                                     dplyr::lag(value) > value ~ -1,
                                     dplyr::lag(value) < value ~ 1,
                                     dplyr::lag(value) == value ~ 0),

        # then do a rolling sum for last 6 values
        roll_sum_trend = zoo::rollapply(lag_trend, 6, sum, fill = NA, align = "right", partial = F),

        # C1: if any of the above are -6 or +6, add the value to highlight in plot.
        cons_trend6 = dplyr::case_when(roll_sum_trend >= 6 | roll_sum_trend <= -6 ~ value),
        cons_trend6_group = dplyr::case_when(!is.na(cons_trend6) ~ cons_trend6),
        cons_trend6_high = dplyr::case_when(roll_sum_trend >= 6 ~ value),
        cons_trend6_low = dplyr::case_when(roll_sum_trend <= -6 ~ value),

        # C2: if any value is above the upper cl, or if any value is below the lower cl, highlight in plot.
        value_breach = ifelse(value < lower_ci | value > upper_ci, value, NA),
        value_breach_icon = dplyr::case_when(value < lower_ci ~ "Lower", value > upper_ci ~ "Higher"),

        # C3: if any values are consecutively above or below the mean, highlight in plot.
        side_of_mean = dplyr::case_when(value > mean ~ 1, # AY: was >=
                                        value < mean ~ -1),
        side_of_mean = zoo::rollapply(side_of_mean, 6, sum, fill = NA, align = "right", partial = F),
        higher_than_mean = dplyr::case_when(side_of_mean == 6 ~ value),
        lower_than_mean = dplyr::case_when(side_of_mean == -6 ~ value),

        # C4: if any 2/3 consecutive values are within Zone A (between roughly the second and third deviations from the mean)
        two_low = ifelse(value < mean - 2*(average_mR/1.128) & value > lower_ci, 1, 0),
        two_low_sum = ifelse(
          two_low == 1 & #AY: can the below code be made cleaner using rollapply instead? Similar to lines 126?
            #    zoo::rollapply(two_low, 2, sum, fill = NA, align = "center", partial = F),
            (dplyr::lag(two_low, n = 2, default = 0) +  dplyr::lag(two_low, n = 1, default = 0) + two_low >= 2 |
               dplyr::lag(two_low, n = 1, default = 0) + two_low +  dplyr::lead(two_low, n = 1, default = 0) >= 2 |
               two_low + dplyr::lead(two_low, n = 1, default = 0) +  dplyr::lead(two_low, n = 2, default = 0) >= 2),
          1,
          0),
        two_high = ifelse(value > mean + 2*(average_mR/1.128) & value < upper_ci, 1, 0),
        two_high_sum = ifelse(
          two_high == 1 &
            # zoo::rollapply(two_high, 2, sum, fill = NA, align = "center", partial = F),
            (dplyr::lag(two_high, n = 2, default = 0) +  dplyr::lag(two_high, n = 1, default = 0) + two_high >= 2 |
               dplyr::lag(two_high, n = 1, default = 0) + two_high +  dplyr::lead(two_high, n = 1, default = 0) >= 2 |
               two_high + dplyr::lead(two_high, n = 1, default = 0) +  dplyr::lead(two_high, n = 2, default = 0) >= 2),
          1,
          0)

      )

    # Fill the consecutive values for C3
    if (length(stats::na.omit(df$higher_than_mean)) != 0) {

      fill_6 <- as.list(which(!is.na(df$higher_than_mean))) # row numbers where value isnt NA
      fill_6 <- (lapply(fill_6, function(x) { (x - 5):x } )) #for each row, find vector of last 6

      fill <- unique(unlist(fill_6))
      df$higher_than_mean[fill] <- df$value[fill] #fill those rows with values

    }

    if (length(stats::na.omit(df$lower_than_mean)) != 0) {

      fill_6 <- as.list(which(!is.na(df$lower_than_mean))) # row numbers where value isnt NA
      fill_6 <- (lapply(fill_6, function(x) { (x - 5):x } )) #for each row, find vector of last 6

      fill <- unique(unlist(fill_6))
      df$lower_than_mean[fill] <- df$value[fill] #fill those rows with values

    }

    # Fill the consecutive values for C1
    if (length(stats::na.omit(df$cons_trend6_high)) != 0) {

      fill_6 <- as.list(which(!is.na(df$cons_trend6_high))) # row numbers where value isnt NA
      fill_6 <- (lapply(fill_6, function(x) { (x - 5):x } )) #for each row, find vector of last 6

      fill <- unique(unlist(fill_6))
      df$cons_trend6_high[fill] <- df$value[fill] #fill those rows with values

    }

    if (length(stats::na.omit(df$cons_trend6_low)) != 0) {

      fill_6 <- as.list(which(!is.na(df$cons_trend6_low))) # row numbers where value isnt NA
      fill_6 <- (lapply(fill_6, function(x) { (x - 5):x } )) #for each row, find vector of last 6

      fill <- unique(unlist(fill_6))
      df$cons_trend6_low[fill] <- df$value[fill] #fill those rows with values

    }

    data <- df %>%
      dplyr::group_by(cons_trend6_group) %>%
      dplyr::mutate(cons_trend6_icon = dplyr::case_when(!is.na(cons_trend6_group) & roll_sum_trend > 0 ~ "Higher",
                                                        !is.na(cons_trend6_group) & roll_sum_trend < 0 ~ "Lower")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        value_breach_icon = ifelse(is.na(value_breach_icon), "common", value_breach_icon),
        cons_trend6_icon = ifelse(is.na(cons_trend6_icon), "common", cons_trend6_icon),
        value_line = value,
        breach_above = ifelse(value > upper_ci, value, NA),
        breach_below = ifelse(value < lower_ci, value, NA),
        two_low_sum = ifelse(two_low_sum == 1, value, NA),
        two_high_sum = ifelse(two_high_sum == 1, value, NA)
      )


    looped_data[[paste(i)]] <- data

  }

  data <- do.call(rbind, looped_data)
  rownames(data) <- NULL


  return(data)


}
