# SPC Icons ---------------------------------------------------------------

#' SPC icons functionality.
#'
#' @description Icons generation used for SPC charts. Need www/ folder in project directory (found in Reference and Guidance in Data Science Secondment). Cannot be run independently from spc_ouput()/spc_chart().
#' Returns a list of icons, element 1 being Assurance and element 2 being Variation.
#' @param .data Data cleaned and processed within spc_output(), or data returned from spc_ouput(output = "chart").
#' @param .polarity The polarity of the indicator in question, obtained from column in dataframe. "up" or "down"
#' @examples
#' # NOT TO BE CALLED INDEPENDENTLY
#' @export
#'
#' @importFrom magrittr %>%



spc_icons <- function(.data,
                      .polarity){



  data <- .data
  polarity <- .polarity


  if(is.na(utils::tail(data$Target, 1)) == T){


    assurance <- "white_space.png"

  } else if(polarity == "up"  & utils::tail(data$Target, 1) <= utils::tail(data$lower_ci,1)){
    assurance = "Consistently_Hitting_Target_Transparent.png"
  } else if(polarity == "up" & utils::tail(data$Target, 1) > utils::tail(data$upper_ci, 1)){
    assurance = "Consistently_Failing_Target_Transparent.png"
  } else if(polarity == "up" & utils::tail(data$upper_ci, 1) >= utils::tail(data$Target, 1) & utils::tail(data$Target, 1) > utils::tail(data$lower_ci, 1)){
    assurance = "Hit_and_Miss_Transparent.png"
  } else if(polarity == "down" & utils::tail(data$Target, 1) >= utils::tail(data$upper_ci, 1)){
    assurance = "Consistently_Hitting_Target_Transparent.png"
  } else if(polarity == "down" & utils::tail(data$Target, 1) < utils::tail(data$lower_ci, 1)){
    assurance = "Consistently_Failing_Target_Transparent.png"
  } else if(polarity == "down" & utils::tail(data$upper_ci, 1) >= utils::tail(data$Target, 1) & utils::tail(data$Target, 1) > utils::tail(data$lower_ci, 1)){
    assurance = "Hit_and_Miss_Transparent.png"
  }




  if(polarity == "up" & (utils::tail(data$value_breach_icon, 1) == "Lower" |
                         utils::tail(data$cons_trend6_icon,1) == "Lower" |
                         sum(utils::tail(stats::na.omit(data$value, 6)) < utils::tail(stats::na.omit(data$mean, 6))) == 6 |
                         !is.na(utils::tail(data$two_low_sum, 1))
                         #utils::tail(data$four_five_low_sum,1) == 1
  )){

    variation = "Concerning_Special_Cause_Low_Transparent.png"

  } else if(polarity == "up" & (utils::tail(data$value_breach_icon, 1) == "Higher" |
                                utils::tail(data$cons_trend6_icon,1) == "Higher" |
                                sum(utils::tail(stats::na.omit(data$value, 6)) > utils::tail(stats::na.omit(data$mean, 6))) == 6 |
                                !is.na(utils::tail(data$two_high_sum,1))
  )){

    variation = "Improving_Special_Cause_High_Transparent.png"

  } else if(polarity == "neutral" & (utils::tail(data$value_breach_icon, 1) == "Higher" |
                                     utils::tail(data$cons_trend6_icon,1) == "Higher" |
                                     sum(utils::tail(stats::na.omit(data$value, 6)) > utils::tail(stats::na.omit(data$mean, 6))) == 6 |
                                     !is.na(utils::tail(data$two_high_sum,1))
  )){

    variation = "Neutral_Special_Cause_High_Transparent.png"

  } else if(polarity == "neutral" & (utils::tail(data$value_breach_icon, 1) == "Lower" |
                                     utils::tail(data$cons_trend6_icon,1) == "Lower" |
                                     sum(utils::tail(stats::na.omit(data$value, 6)) < utils::tail(stats::na.omit(data$mean, 6))) == 6 |
                                     !is.na(utils::tail(data$two_low_sum,1))
                                     #utils::tail(data$four_five_low_sum,1) == 1
  )){

    variation = "Neutral_Special_Cause_Low_Transparent.png"

  } else if(polarity == "down" & (utils::tail(data$value_breach_icon, 1) == "Lower" |
                                  utils::tail(data$cons_trend6_icon,1) == "Lower" |
                                  sum(utils::tail(stats::na.omit(data$value, 6)) < utils::tail(stats::na.omit(data$mean, 6))) == 6 |
                                  !is.na(utils::tail(data$two_low_sum,1))
                                  #utils::tail(data$four_five_low_sum,1) == 1
  )){

    variation = "Improving_Special_Cause_Low_Transparent.png"

  } else if(polarity == "down" & (utils::tail(data$value_breach_icon, 1) == "Higher" |
                                  utils::tail(data$cons_trend6_icon,1) == "Higher" |
                                  sum(utils::tail(stats::na.omit(data$value, 6)) > utils::tail(stats::na.omit(data$mean, 6))) == 6 |
                                  !is.na(utils::tail(data$two_high_sum,1))
                                  # utils::tail(data$four_five_high_sum, 1) == 1
  )){

    variation = "Concerning_Special_Cause_High_Transparent.png"

  } else if((utils::tail(data$value_breach_icon, 1) == "common" |
             utils::tail(data$cons_trend6_icon,1) == "common" |
             sum(utils::tail(stats::na.omit(data$value, 6)) < utils::tail(stats::na.omit(data$mean, 6)))!= 6)) {

    variation = "Common_Cause_Transparent.png"

  }


  icons <- c(paste0("icons/", assurance),
             paste0("icons/", variation))


}















# SPC Icons ---------------------------------------------------------------

#' SPC icons functionality.
#'
#' @description Add icons after your ggplot is created and edited.
#' @param .spc SPC chart ggplot (spc_chart()).
#' @param echarts_variation c(right, top, z, width, height)
#' @param echarts_assurance c(right, top, z, width, height)
#' @param ggplot_variation c(x,y,scale)
#' @param ggplot_assurance c(x,y,scale)
#' @param plotly_variation c(x,y,sizex,sizey)
#' @param plotly_assurance c(x,y,sizex,sizey)
#' @examples
#' # NOT TO BE CALLED INDEPENDENTLY
#' @export
#'
#' @importFrom magrittr %>%



spc_add_icons <- function(.spc,
                          echarts_variation = c(245, 0, -999, 50, 50),
                          echarts_assurance = c(200, 0, -999, 50, 50),
                          ggplot_variation = c(0.32, 0.4, 0.1),
                          ggplot_assurance = c(0.4, 0.4, 0.1),
                          plotly_variation = c(0.8, 0.95, 0.1, 0.1),
                          plotly_assurance = c(0.8, 0.95, 0.1, 0.1)){

  graph_type <- class(.spc)

  if (inherits(.spc, c('echarts4r'))) {

    icons <- .spc[["x"]][["data"]]

    icons <- icons[[1]]

    icon_variation <- unique(icons$`Variation Icon`)
    icon_assurance <- unique(icons$`Assurance Icon`)

  } else if (inherits(.spc, c('plotly'))) {

    icons <- environment(.spc[["x"]][["visdat"]][[1]])[["data"]]

    icon_variation <- unique(icons$`Variation Icon`)
    icon_assurance <- unique(icons$`Assurance Icon`)

  } else {

    icon_variation <- unique(.spc[["data"]][["Variation Icon"]])
    icon_assurance <- unique(.spc[["data"]][["Assurance Icon"]])

  }



  if(is.na(icon_assurance)){


    icon_assurance <- "white_space.png"

  } else if(icon_assurance == "on target"){
    icon_assurance = "Consistently_Hitting_Target_Transparent.png"
  } else if(icon_assurance == "failing target"){
    icon_assurance = "Consistently_Failing_Target_Transparent.png"
  } else {
    icon_assurance = "Hit_and_Miss_Transparent.png"
  }


  if(icon_variation == "concern special low"){

    icon_variation = "Concerning_Special_Cause_Low_Transparent.png"

  } else if(icon_variation == "improve special high"){

    icon_variation = "Improving_Special_Cause_High_Transparent.png"

  } else if(icon_variation == "neutral special high"){

    icon_variation = "Neutral_Special_Cause_High_Transparent.png"

  } else if(icon_variation == "neutral special low"){

    icon_variation = "Neutral_Special_Cause_Low_Transparent.png"

  } else if(icon_variation == "improve special low"){

    icon_variation = "Improving_Special_Cause_Low_Transparent.png"

  } else if(icon_variation == "concern special high"){

    icon_variation = "Concerning_Special_Cause_High_Transparent.png"

  } else if(icon_variation == "common cause") {

    icon_variation = "Common_Cause_Transparent.png"

  }



  if(any(graph_type %in% "plotly")){

    #https://linking.plotly-r.com/embedding-images.html
    
    spc <- .spc %>%
      plotly::layout(images = list(
        list(
          source = base64enc::dataURI(file = system.file(paste0("icons/", icon_variation), package = "HertsSPC")),
          x = plotly_variation[1], y = plotly_variation[2], sizex = plotly_variation[3], sizey = plotly_variation[4],
          xref = "paper", yref = "paper", xanchor = "left", yanchor = "bottom"
        ),
        if(icon_assurance != "white_space.png") list(
          source = base64enc::dataURI(file = system.file(paste0("icons/", icon_assurance), package = "HertsSPC")),
          x = plotly_assurance[1], y = plotly_assurance[2], sizex = plotly_assurance[3], sizey = plotly_assurance[4],
          xref = "paper", yref = "paper", xanchor = "left", yanchor = "bottom"
        )
      ))

  } else if(any(graph_type %in% c("echarts4r"))){

    spc <- .spc %>%
      echarts4r::e_image_g(
        elements = list(
          list(type = "image",
               right = echarts_variation[1],
               top = echarts_variation[2],
               z = echarts_variation[3],
               style = list(
                 image = system.file(paste0("icons/", icon_variation), package = "HertsSPC"),
                 width = echarts_variation[4],
                 height = echarts_variation[5],
                 opacity = 1
               )))
      )


      if(icon_assurance != "white_space.png"){
        spc <- spc %>%
          echarts4r::e_image_g(
            elements = list(
          list(type = "image",
               right = echarts_assurance[1],
               top = echarts_assurance[2],
               z = echarts_assurance[3],
               style = list(
                 image = system.file(paste0("icons/", icon_assurance), package = "HertsSPC"),
                 width = echarts_assurance[4],
                 height = echarts_assurance[5],
                 opacity = 1
               )))
          )
      }


  } else {


    spc <- cowplot::ggdraw() +
      cowplot::draw_plot(.spc) +
      cowplot::draw_image(magick::image_read(system.file(paste0("icons/", icon_variation), package = "HertsSPC")),  x = ggplot_variation[1], y = ggplot_variation[2], scale = ggplot_variation[3])


    if(!grepl("white_space.png", icon_assurance)){
      spc <- spc +
        cowplot::draw_image(magick::image_read(system.file(paste0("icons/", icon_assurance), package = "HertsSPC")),  x = ggplot_assurance[1], y = ggplot_assurance[2], scale = ggplot_assurance[3])

    } else {spc <- spc}

  }


  return(spc)

}
