#' Visualise a \code{ssmTMB} fit to data as a map and as 1-d time-series
#'
#' @importFrom ggplot2 ggplot geom_point geom_map theme geom_path ggtitle
#' @importFrom ggplot2 geom_line geom_rug ylim xlim coord_fixed map_data
#' @importFrom gridExtra grid.arrange
#' @importFrom dplyr %>% do filter
#' @importFrom tibble tibble
#' @export

plots <- function(ssm, d, res) {
  
  ssm %>% do(., plotf(., res))
  
  
}