#' Trims deployment start and end periods of spurious data
#' 
#' Use graphical interface to identify proper start and end of animal tracks
#' 
#' @param dat track data that has already been \code{strip}'ped
#' 
#' @author Ian Jonsen
#' @examples
#' \dontrun{
#' }
#' @importFrom lubridate as_datetime
#' @importFrom tibble data_frame
#' @export 
trim <- function(dat) {

  tr <- function(k) {
    layout(matrix(c(1, 2, 2, 3, 3), 5, 1))
    par(mar = c(0, 0, 0, 0))
    
    plot(lat ~ date,
         k,
         type = 'b',
         pch = 3,
         cex = 0.4)

    s.idx <- max(which(k$date < as_datetime(locator(n = 1)$x, tz = "GMT")))
    e.idx <- min(which(k$date > as_datetime(locator(n = 1)$x, tz = "GMT")))

    plot(
      lat ~ date,
      k,
      subset = 1:s.idx,
      type = 'b',
      pch = 3,
      cex = 0.4
    )
    st <- as_datetime(locator(n = 1)$x, tz = "GMT")
    
    plot(
      lat ~ date,
      k,
      subset = e.idx:nrow(k),
      type = 'b',
      pch = 3,
      cex = 0.4
    )
    e <- as_datetime(locator(n = 1)$x, tz = "GMT")
    
    idx <- rep(TRUE, nrow(k))
    idx[k$date < st] <- FALSE
    idx[k$date > e] <- FALSE
    with(k, data_frame(id, date, lc, lon, lat, keep = idx))
  }
  
  lapply(dat, tr)

}
