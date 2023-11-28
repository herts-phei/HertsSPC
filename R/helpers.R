
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
