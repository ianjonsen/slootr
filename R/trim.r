#' Trims deployment start and end periods of spurious data via
#'    changepoint estimation
#'
#'
#' @param d track data that has already been \code{strip}'ped
#' @param hang quantile of date range above which data gaps should be examined 
#' for potential discarding
#' @param gap quantile of gap size (time difference) above which data beyond the hang-th
#' quantile should be flagged for discard.
#' 
#' @return A tbl_df grouped by individual id is returned.
#' 
#' @author Ian Jonsen
#' @examples
#' \dontrun{
#' }
#' @importFrom dplyr %>% tbl_df group_by filter do mutate select
#' @importFrom geosphere distGeo
#' @importFrom changepoint cpt.mean
#' @export
trim <- function(d, hang = 0.95, gap = 0.98) {

  if (class(d)[1] != "grouped_df")
    d <- tbl_df(d) %>% grouped_by(id)
  if ("keep" %in% names(d)) {
    d1 <- filter(d, keep)
  }
  else {
    d1 <- mutate(d, keep = TRUE)
  }
  
  ## calculate moving average to smooth variability in distances from deployment location
  calc_dist <-
    function(x) {
      n <- 11  ## ma window 
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
  
  ## identify hanging locations after large data gaps at end of track
  flag_all <- function(x) {
    st <- which(!x$keep.end & x$keep)
    if(length(st) > 0) {    
    idx <- st[1]:nrow(x)
    x$keep[idx] <- FALSE
    }
    x$keep
  }
  
  y <-
    y %>% do(., mutate(., ddiff = c(0, as.numeric(diff(
      date
    ))))) %>%
    do(., mutate(., keep.end =
                   ifelse(
                     date > quantile(date, hang) &
                       ddiff > quantile(ddiff, gap),
                     FALSE,
                     keep
                   ))) %>%
    do(., mutate(., keep = flag_all(.)))
 
  ##return original data_frame with keep flag updated
  d$keep[d$keep] <- y$keep
  
  d
}
