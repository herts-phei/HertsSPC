utils::globalVariables(c(".","time_field", "value", "Mean", "Upper CI", "Lower CI",
                         "variable", "mean",
                         "two_high_sum", "two_low_sum", "breach_below", "breach_above",
                         "less_than_zero", "greater_than_hundred", "polarity", "unit",
                         "target", "cons_trend6_low", "cons_trend6_high",
                         "lower_than_mean", "higher_than_mean", "value_line",
                         "Common Cause Variation", "Special Cause Variation Improving",
                         "Special Cause Variation Concerning", "Special Cause Warning",
                         "hover", "Special Cause Variation", "Numerator", "Denominator",
                         "x1", "x2", "y1", "y2"))

#' SPC Chart options
#'
#' @description Chart options for ggplot. Pipe spc_add_icons() onto end of function to apply icons (sizes can be ammended to suit the plot if needed, see ?spc_add_icons.
#' @param x_title_size Size of x axis title
#' @param x_label_format e.g. "%b %d"
#' @param y_title_size Size of y axis title
#' @param x_text_size Size of x axis tick labels
#' @param y_text_size Size of y axis tick labels
#' @param x_label x axis title
#' @param y_label y axis title
#' @param title_size Size of title
#' @param x_breaks Breaks in x axis. Defaults to 3 months
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
#'spc_output(
#'        data = tooth_data,
#'        time_field = "Date",
#'        indicator = "supp",
#'        value = "len",
#'        output = "chart",
#'        package = "ggplot",
#'        chart_theme = spc_chart_options(x_title_size = 20,
#'                                         x_label = "Date",
#'                                         y_label = "Measure",
#'                                         x_label_format = "%d %b"))
#'
#' @export
#'
#' @importFrom magrittr %>%

spc_chart_options <- function(x_title_size = NULL,
                              x_text_size = NULL,
                              x_label_format = "%b-%Y",
                              y_title_size = NULL,
                              y_text_size = NULL,
                              y_label = NULL,
                              x_label = NULL,
                              title_size = NULL,
                              x_breaks = "1 month"){

  return(list(x_title_size = x_title_size,
              x_text_size = x_text_size,
              x_label_format = x_label_format,
              y_title_size = y_title_size,
              y_text_size = y_text_size,
              y_label = y_label,
              x_label = x_label,
              title_size = title_size,
              x_breaks = x_breaks))

}



#' SPC Charting functionality.
#'
#' Use this function on a spc processed data frame. Can only provide graph for one metric upon each call (doesn't support faceted charts as of yet)
#'
#' append %>% spc_add_icons to end of chart object to add icons.
#'
#' @description The function used to output data as an SPC chart. Can be ran independently from spc_output()
#' @param .data Data cleaned and processed within spc_output(), or data returned from spc_ouput(output = "chart").
#' @param .base_date_range Base date range entered into spc_output(base_date_range) or can be entered into spc_chart() the same way
#' @param .package Package as entered into spc_output(package) or can be entered into spc_chart() the same way ("echarts" or "plotly", only applies if mode = "interactive")
#' @param .plot_title Provides the plot with a title if the output is chart
#' @param .yrange Provides a axis range if the output is chart. Argument should be c(min,max). A count of 5 is +/- from values
#' @param .chart_theme Takes a list of arguements that would appear in theme arguement of
#' @param .line_breaks Determines whether mean and process limit lines break after a rebase. Defaults to F description
#' @examples
#' #An SPC Chart
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
#'spc_data <- spc_output(
#'        data = tooth_data2,
#'        time_field = "Date",
#'        indicator = "supp",
#'        value = "len",
#'        output = "data")
#'
#'spc_chart(.data = filter(spc_data, indicator == "Indicator 1"),
#'           .package = "ggplot",
#'           .plot_title = "ABCD",
#'           .base_date_range = NULL)
#'
#'spc_chart(.data = filter(spc_data, indicator == "Indicator 2"),
#'           .plot_title = "VC Plotly (equal axis)",
#'           .base_date_range = NULL,
#'           .package = "plotly",
#'           .yrange = c(min(c(spc_data$value, spc_data$Target,
#'                             spc_data$upper_ci, spc_data$lower_ci),
#'                             na.rm = TRUE),
#'                       max(c(spc_data$value, spc_data$Target,
#'                             spc_data$upper_ci, spc_data$lower_ci),
#'                             na.rm = TRUE)))
#'
#'
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data




spc_chart <- function(.data,
                      .base_date_range,
                      .package = NULL,
                      .plot_title = NULL,
                      .yrange = NULL,
                      .line_breaks = F,
                      .chart_theme = NULL){



  data <- .data
  base_date_range <- .base_date_range
  package <- .package
  line_breaks <- .line_breaks


  if(is.null(package)){
    warning("You have requested a chart but you have not specified a package. Defaults to a static ggplot. Set package as either 'ggplot' for static or 'plotly' or 'echarts' for an interactive chart!")
    package = "ggplot"
  } else if(!(package %in% c("ggplot", "plotly", "echarts"))){
    warning("Assigned package is not within options available. Please specify 'ggplot', 'plotly' or 'echarts'. Package will default to ggplot.")
    package = "ggplot"
  }

  if(length(unique(.data$indicator)) > 1 | any(duplicated(.data$time_field)) == T){
    stop("Your calling this chart function with more than one indicator. Filter input data for one indicator. If mass processing, putting function in a loop is recommeneded.")
  }



  if(is.null(.chart_theme)){
    chart_theme <- spc_chart_options()
  } else {
    chart_theme <- .chart_theme
  }



  icons_list <- spc_status(.data = data)

  # Formatting table for input into graphs  --------


  if(unique(data$unit) == "percent"){
    unit = "%"
  } else {
    unit = ""
  }


  spc_pivot <- data %>%
    dplyr::select(
      time_field, value, value_line, Target, Mean = mean,
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
    dplyr::mutate(hover = paste0(hover, collapse = "<br>"),
                  time_field = as.Date(time_field)) %>%
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
                  `Special Cause Warning` = ifelse("Special Cause Warning" %in% names(.), `Special Cause Warning`, NA),
                  `Special Cause Warning` = as.numeric(`Special Cause Warning`),
                  `Target` = ifelse("Target" %in% names(.), Target, NA),
                  `Common Cause Variation` = dplyr::case_when(is.na(`Special Cause Variation Improving`) &
                                                                is.na(`Special Cause Variation`) &
                                                                is.na(`Special Cause Variation Concerning`) &
                                                                is.na(`Special Cause Warning`)~ value),
                  `Upper CI` = ifelse(greater_than_hundred == FALSE & `Upper CI` > 100,100,`Upper CI`),
                  `Lower CI` = ifelse(less_than_zero == FALSE & `Lower CI` < 0, 0, `Lower CI`),
                  `Variation Icon` = icons_list$`Variation Type`,
                  `Assurance Icon` = icons_list$`Assurance Type`
    )



  # Graphs (depending on mode) ---------------------------------

  if(is.null(.yrange)){

    ymax <- max(stats::na.omit(c(pre_plot$value, pre_plot$Target, pre_plot$`Upper CI`)))
    ymin <- min(stats::na.omit(c(pre_plot$value, pre_plot$Target, pre_plot$`Lower CI`)))

  } else {

    ymin <- .yrange[1]
    ymax <- .yrange[2]

  }


  ymin <- if(ymin - 5 < 0  & as.character(unique(pre_plot$less_than_zero)) == "FALSE") 0 else round(ymin-5, 0)
  ymax <- if(ymax + 5 > 100 & as.character(unique(pre_plot$greater_than_hundred))== "FALSE") 100 else round(ymax+5, 0)


  if(is.null(chart_theme$`y_label`)) {

    ylabel <- paste0("Performance ",unit)

  } else {

    ylabel <- chart_theme$`y_label`

  }

  if(is.null(chart_theme$`x_label`)) {

    xlabel <- paste0("Date")

  } else {

    xlabel <- chart_theme$`x_label`

  }


  if(is.null(chart_theme$`x_label_format`)) {

    x_label_format <- "%b-%d"

  } else {

    x_label_format <- chart_theme$`x_label_format`

  }

  if(package == "echarts"){


    spc <- pre_plot %>%
      echarts4r::e_charts(time_field) %>%
      echarts4r::e_title(textStyle = list(color = "#d4d4d4")) %>%
      echarts4r::e_x_axis(min = min(pre_plot$time_field)-5,
                          max = max(pre_plot$time_field)+5) %>%
      echarts4r::e_y_axis(min = ymin, max = ymax,
                          axisLabel = list(color = "#aaa"),
                          nameGap = 40) %>%
      echarts4r::e_line(value, symbol = "none", color = "#bdbdbd") %>%
      echarts4r::e_line(Mean, symbol = "none", color = "black") %>%
      echarts4r::e_line(`Upper CI`, symbol = "none", color = "black", lineStyle = list(type = "dashed")) %>%
      echarts4r::e_line(`Lower CI`, symbol = "none", color = "black", lineStyle = list(type = "dashed")) %>%
      echarts4r::e_scatter(`Common Cause Variation`, symbol = "circle", color = "#bdbdbd", symbolSize = 13, bind = hover) %>%
      echarts4r::e_effect_scatter(`Special Cause Variation Improving`, symbol = "circle", color = "#2b8cf9", symbolSize = 13, bind = hover) %>%
      echarts4r::e_effect_scatter(`Special Cause Variation Concerning`, symbol = "circle", color = "#f48601", symbolSize = 13, bind = hover) %>%
      echarts4r::e_effect_scatter(`Special Cause Variation`, symbol = "circle", color = "#a300f2", symbolSize = 13, bind = hover)  %>%
      echarts4r::e_grid(right = "12%", left = "5%") %>%
      echarts4r::e_legend(type = "scroll", orient = 'horizontal',
                          bottom = "0%") %>%
      echarts4r::e_tooltip(formatter = htmlwidgets::JS("
        function(params){
          return(params.name)
                  }
      ")) %>%
      echarts4r::e_axis_labels(x = "Date", y = ylabel) %>%
      echarts4r::e_theme("wonderland") %>%
      echarts4r::e_hide_grid_lines(which = c("y","x"))


  } else if(package == "plotly"){

    spc <-
      pre_plot %>%
      plotly::plot_ly()

    if(line_breaks == T){

      for (i in unique(pre_plot$Mean)) {



        mean <- data.frame(x1 = utils::head(pre_plot$time_field[pre_plot$Mean == i], 1),
                           x2 = utils::tail(pre_plot$time_field[pre_plot$Mean == i ], 1),
                           y1 = i,
                           y2 = i)
        ucl <- data.frame(x1 = utils::head(pre_plot$time_field[pre_plot$Mean == i], 1),
                          x2 = utils::tail(pre_plot$time_field[pre_plot$Mean == i ], 1),
                          y1 = unique(pre_plot$`Upper CI`[pre_plot$Mean == i]),
                          y2 = unique(pre_plot$`Upper CI`[pre_plot$Mean == i]))
        lcl <- data.frame(x1 = utils::head(pre_plot$time_field[pre_plot$Mean == i], 1),
                          x2 = utils::tail(pre_plot$time_field[pre_plot$Mean == i ], 1),
                          y1 = unique(pre_plot$`Lower CI`[pre_plot$Mean == i]),
                          y2 = unique(pre_plot$`Lower CI`[pre_plot$Mean == i]))


        spc <- spc  %>%
          plotly::add_segments(x = mean$x1, xend = mean$x2, y = mean$y1, yend = mean$y2, line = list(color = "black", dash = "dot"),
                               showlegend = F,
                               name = "Mean") %>%
          plotly::add_segments(x = lcl$x1, xend = lcl$x2, y = lcl$y1, yend = lcl$y2, line = list(dash = "dash", color = "black"),
                               showlegend = F,
                               name = "LCL") %>%
          plotly::add_segments(x = ucl$x1, xend = ucl$x2, y = ucl$y1, yend = ucl$y2, line = list(dash = "dash", color = "black"),
                               showlegend = F,
                               name = "UCL")

      }

    } else {

      spc <- spc %>%
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
        )

    }


    spc <-
      spc %>%
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
        y =  ~ `Special Cause Variation`,
        type = "scatter",
        mode = 'markers',
        name = paste0("Special Cause"),
        hoverinfo = "text",
        hovertemplate =  ~ hover,
        marker = list(color = '#a300f2',
                      size = 11)
      ) %>%
      plotly::add_trace(
        x =  ~ time_field,
        y =  ~ `Common Cause Variation`,
        type = "scatter",
        mode = 'markers',
        name = paste0("Common Cause"),
        hoverinfo = "text",
        hovertemplate =  ~ hover,
        marker = list(color = '#bdbdbd',
                      size = 11)
      ) %>%
      plotly::layout(
        title = .plot_title,
        xaxis= list(title=list(text="Date",
                               font=list(family="Lato", size=20),
                               standoff=35)),
        yaxis = list(title = list(text = ylabel,
                                  font = list(family = "Lato", size = 20),
                                  standoff = 30),
                     range = c(ymin,ymax))
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


  } else {

    plot <- ggplot2::ggplot(data = pre_plot, ggplot2::aes(x = time_field))

    if(line_breaks == T){

      for (i in unique(pre_plot$Mean)) {



        mean <- data.frame(x1 = utils::head(pre_plot$time_field[pre_plot$Mean == i], 1),
                           x2 = utils::tail(pre_plot$time_field[pre_plot$Mean == i ], 1),
                           y1 = i,
                           y2 = i)
        ucl <- data.frame(x1 = utils::head(pre_plot$time_field[pre_plot$Mean == i], 1),
                          x2 = utils::tail(pre_plot$time_field[pre_plot$Mean == i ], 1),
                          y1 = unique(pre_plot$`Upper CI`[pre_plot$Mean == i]),
                          y2 = unique(pre_plot$`Upper CI`[pre_plot$Mean == i]))
        lcl <- data.frame(x1 = utils::head(pre_plot$time_field[pre_plot$Mean == i], 1),
                          x2 = utils::tail(pre_plot$time_field[pre_plot$Mean == i ], 1),
                          y1 = unique(pre_plot$`Lower CI`[pre_plot$Mean == i]),
                          y2 = unique(pre_plot$`Lower CI`[pre_plot$Mean == i]))


        plot <-  plot +
          ggplot2::geom_segment(ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2), data = mean) +
          ggplot2::geom_segment(ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2), data = ucl, linetype = "dashed") +
          ggplot2::geom_segment(ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2), data = lcl, linetype = "dashed") +
          ggplot2::geom_line(ggplot2::aes(y = value), color = "#47525a")

      }

    } else {

      plot <- plot +
        ggplot2::geom_line(ggplot2::aes(y = value), color = "#47525a") +
        ggplot2::geom_line(ggplot2::aes(y = `Upper CI`), color = "black", linetype = "dashed") +
        ggplot2::geom_line(ggplot2::aes(y = `Lower CI`), color = "black", linetype = "dashed") +
        ggplot2::geom_line(ggplot2::aes(y = Mean), color = "black", linetype = "solid")


    }


    spc <- plot +
      ggplot2::geom_point(ggplot2::aes(y = `Common Cause Variation`), color = "#b0b0b0", size = 3) +
      ggplot2::geom_point(ggplot2::aes(y = `Special Cause Variation Improving`),
                          color = "#258ad6",
                          size = 4) +
      ggplot2::geom_point(ggplot2::aes(y = `Special Cause Variation Concerning`),
                          color = "#c37300",
                          size = 4)  +
      ggplot2::geom_point(ggplot2::aes(y = `Special Cause Variation`),
                          color = "#a300f2",
                          size = 4) +
      ggplot2::ylim(ymin, ymax) +
      ggplot2::labs(x = xlabel,
                    y = ylabel,
                    title = .plot_title) +
       ggplot2::scale_x_date(labels = scales::date_format(format = x_label_format),
                             breaks = chart_theme$x_breaks
                             ) +
      ggplot2::theme(
        axis.line = ggplot2::element_line(color = 'black'),
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.grid.major.y = ggplot2::element_line(colour = "grey"),
        panel.grid.minor.y = ggplot2::element_line(colour = "grey"),
        legend.key.size = ggplot2::unit(0.2, 'cm'),
        legend.key.height = ggplot2::unit(0.2, 'cm'),
        legend.key.width =ggplot2:: unit(0.2, 'cm'),
        legend.title = ggplot2::element_text(size = 8),
        legend.text = ggplot2::element_text(size = 5),
        axis.title.y  = ggplot2::element_text(size = chart_theme$y_title_size),
        axis.title.x  = ggplot2::element_text(size = chart_theme$x_title_size, vjust = 0.1),
        plot.title = ggplot2::element_text(size = ifelse(is.null(chart_theme$title_size), 20, chart_theme$title_size)),
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 50, vjust = 0.5, size = chart_theme$x_text_size),
        axis.text.y = ggplot2::element_text(size = chart_theme$y_text_size)
      )



    if(!is.null(base_date_range)){

      spc <- spc +
        ggplot2::geom_rect(mapping=ggplot2::aes(xmin = as.Date(base_date_range[1], "%Y-%m-%d"), xmax = as.Date(base_date_range[2], "%Y-%m-%d"),
                                       ymin = ymin, ymax = ymax), color = "#dcf0ff", fill = "#dcf0ff")

    }


    if(utils::tail(!is.na(pre_plot$Target),1)){

      spc <- spc +
        ggplot2::geom_line(ggplot2::aes(y = Target), color = "red", linetype = "solid")

    }

  }

  return(spc)

}
