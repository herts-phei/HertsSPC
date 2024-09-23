
#' Attaining SPC icons in summary reactable table.
#'
#' @description The function used to read in SPC icons for reactable summary table
#' @param icon_string the icons locations
#' @examples
#'
#' #Not to be used independently
#'
#' @export


spc_img_uri <- function(icon_string
                    ) {
  sprintf('<img src="%s"/>', knitr::image_uri(icon_string))

}


#' Sorting SPC summary tab.
#'
#' @description The function used to sort the SPC summary table by desired columns and desired order. Function used within the processing of the SPC summary table as columns cannot be custom sorted once the flextable or reactable is prodcued
#' @param data The data is the SPC summary table dataframe once icons have been determined
#' @param sort_by The desired sorting mechanism. Can be any of the following: "indicator", "assurance concern", "assurance improve", "variation concern", "variation improve", "improve both by variation", "improve both by assurance", "concern both by assurance", "concern both by variation"
#' @examples
#'
#' #Not to be used independently
#'
#' @export

sort_spc_summary_table <- function(data,
                                   sort_by){
  
  
  if(sort_by == "indicator"){   # Sort descending alphabetically
    data <- dplyr::arrange(data, indicator)
  } else if(sort_by == "assurance concern"){
    data <- dplyr::arrange(data, match(assurance_sort, c("failing target", "variable target", "on target")))
  } else if(sort_by == "assurance improve"){
    data <- dplyr::arrange(data, match(assurance_sort, c("on target", "variable target", "failing target")))
  } else if(sort_by == "variation concern"){
    data <- dplyr::arrange(data, match(variation_sort, c("concern special high", "concern special low",
                                                            "common cause", "improve special low",
                                                            "improve special high","neutral special high",
                                                            "neutral special low")
    )
    )
  } else if(sort_by == "variation improve"){
    data <- dplyr::arrange(data, match(variation_sort, c("improve special low",
                                                            "improve special high", 
                                                            "common cause",
                                                            "concern special high", "concern special low",
                                                            "neutral special high","neutral special low")
    )
    )
  } else if(sort_by == "improve both by assurance"){
    data <- dplyr::arrange(data,
                         match(
                           assurance_sort, c("on target", "variable target", "failing target")
                         ),
                         match(
                           variation_sort,  c("improve special low","improve special high",
                                              "common cause",
                                              "concern special high","concern special low",
                                              "neutral special high","neutral special low"
                           )
                         )
    )
  } else if(sort_by == "improve both by variation"){
    data <- dplyr::arrange(data,
                         match(
                           variation_sort,  c("improve special low","improve special high",
                                              "common cause",
                                              "concern special high","concern special low",
                                              "neutral special high","neutral special low"
                           )
                         ),
                         match(
                           assurance_sort, c("on target", "variable target", "failing target")
                         )
    )
  } else if(sort_by == "concern both by variation"){
    data <- dplyr::arrange(data,
                         match(
                           variation_sort,  c("concern special high", "concern special low",
                                              "common cause", "improve special low",
                                              "improve special high","neutral special high",
                                              "neutral special low")
                         ),
                         match(assurance_sort, c("failing target", "variable target", "on target"))
    )
  } else if(sort_by == "concern both by assurance"){
    data <- dplyr::arrange(data,
                         match(assurance_sort, c("failing target", "variable target", "on target")),
                         match(
                           variation_sort,  c("concern special high", "concern special low",
                                              "common cause", "improve special low",
                                              "improve special high","neutral special high",
                                              "neutral special low"))
    )
  }
  
  return(data)
}


#' Creating hover-over for reactable summary icons. Returns the variation/assurance for a given indicator when hovering cursor over icons
#'
#' @description The function used to read in SPC icons for reactable summary table
#' @param text value in the reactable, in this case the icons
#' @param tooltip Text to be displayed in tippy (gathered from data in SPC summary function)
#' @param placement placement of hover-over
#' @examples
#'
#' #Not to be used independently
#'
#' @export


spc_reactable_tippy <- function(text,
                                tooltip,
                                placement){
  htmltools::div(
    style = "text-decoration: underline; text-decoration-style: dotted;
                  text-decoration-color: #000000;
                  cursor: auto;
                  white-space: nowrap;
                  overflow: hidden;
                  text-overflow: ellipsis;",
    tippy::tippy(text = text,
                 tooltip = paste0("<span style='font-size:20px;'>",paste(tooltip), "<span>"),
                 placement = placement,
                 theme = "light",
                 arrowType = "round",
                 arrow = T,
                 interactiveBorder = 5,
                 allowHTML = T))

  #https://kabbouchi.github.io/tippyjs-v4-docs/all-options/

}
