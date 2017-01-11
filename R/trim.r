#' Trims deployment start and end periods of spurious data via
#'    changepoint estimation
#'
#'
#' @param d track data that has already been \code{strip}'ped
#' @param n window size to be used to obtain moving average of distance
#' from deployment (first) location
#'
#' @author Ian Jonsen
#' @examples
#' \dontrun{
#' }
#' @importFrom dplyr %>% tbl_df group_by filter do mutate select
#' @importFrom geosphere distGeo
#' @importFrom changepoint cpt.mean
#' @export
trim <- function(d, n = 11) {
  if (!n %% 2)
    stop("window size, n, must be an odd number")
  if (class(d)[1] != "grouped_df")
    d <- tbl_df(d) %>% grouped_by(id)
  if ("keep" %in% names(d)) {
    d1 <- filter(d, keep)
  }
  
  ## calculate moving average to smooth variability in distances from deployment location
  calc_dist <-
    function(x) {
      d <- distGeo(c(x$lon[1], x$lat[1]), cbind(x$lon, x$lat)) / 1000
      d.ma <- stats::filter(d, rep(1 / n, n), sides = 2)
      ## set NA distances to first (or last) ma value
      d.ma[1:(n %/% 2)]  <- d.ma[!is.na(d.ma)][1]
      d.ma[(length(d.ma) - (n %/% 2) - 1):length(d.ma)] <-
        d.ma[!is.na(d.ma)][length(d.ma[!is.na(d.ma)])]
      d.ma
    }
  ## estimate start distance changepoint
  t_start <- function(x) {
    cpt <- cpt.mean(log(x$dist[x$dist > 0]))@cpts[1]
    x$date[x$dist > 0][cpt]
  }
  ## estimate end distance changepoint
  t_end <- function(x) {
    cpt <- cpt.mean(log(x$dist[x$dist > 0]), method = "PELT")@cpts
    if (length(cpt) == 2)
      cpt <- cpt[2]
    else if (length(cpt) > 2 &&
             x$date[x$dist == max(x$dist)] > x$date[cpt[length(cpt) - 1]]) {
      cpt <- cpt[length(cpt)]
    }
    else {
      cpt <- cpt[length(cpt) - 1]
    }
    x$date[x$dist > 0][cpt]
  }
  
  ## use changepoints to define track segment(s) to flag for discard
  y <- d1 %>% do(., mutate(., dist = calc_dist(.))) %>%
    do(., mutate(
      .,
      start = t_start(.),
      end = ifelse(t_start(.) != t_end(.), t_end(.), max(date))
    )) %>%
    do(., mutate(., keep = ifelse(date >= start &
                                    date <= end, keep, FALSE)))
  
  ## remove hanging locations after large data gaps at end of track
  y <-
    y %>% do(., mutate(., ddiff = c(0, as.numeric(diff(
      date
    ))))) %>%
    do(., mutate(., keep =
                   ifelse(
                     date > quantile(date, 0.95) &
                       ddiff > quantile(ddiff, 0.98),
                     FALSE,
                     keep
                   ))) 

  ##return original data_frame with keep flag updated
  d$keep[d$keep] <- y$keep
  
  d
}
