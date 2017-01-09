#' Trims deployment start and end periods of spurious data by
#'   specifying a buffer distance around a deployment location
#'
#' Use graphical interface to identify proper start and end of animal tracks
#'
#' @param d track data that has already been \code{strip}'ped
#' @param buffer distance in m from deployment location (defaults to first recorded
#' location) within which observed locations are to be trimmed
#'
#' @author Ian Jonsen
#' @examples
#' \dontrun{
#' }
#' @importFrom lubridate as_datetime
#' @importFrom dplyr %>% tbl_df group_by do mutate
#' @importFrom geosphere distGeo
#' @export
trim <- function(d, b = 4000) {
  
  if (class(d)[1] != "grouped_df")
    stop("Input data must be a tbl_df grouped by individual id")
  
  d.fn <-
    function(x)
      distGeo(c(x$lon[1], x$lat[1]), cbind(x$lon, x$lat))
  
  y <- do(d, mutate(., dist = d.fn(.), home = dist > b)) %>%
    do(., mutate(., newtrip = abs(c(0, diff(
      !home
    ))))) %>%
    do(., mutate(., newtrip = ifelse(home == 0, 0, newtrip))) %>%
    do(., mutate(., trip = cumsum(newtrip) + 1))
    
    
    y
}
