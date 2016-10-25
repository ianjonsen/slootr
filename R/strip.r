#' Strip Argos track data of unwanted records
#'
#' Strip does the following:  1) removes duplicated date records; 2) removes start locations in
#' N Hemisphere, eg. Seattle, BAS, SMRU (will be generalised later); 3) removes deployments with
#' less than min.obs records; 4) removes deployments that last less than min.days; 5) removes
#' records with NA lat and/or lon; 6) shifts 0, 360 longitudes to -180, 180. Each of these steps
#' can be optionally turned off.
#'
#' @param dat A data.frame containing the following columns:
#' "id", "date", "lc", "lon", "lat". "id" is a unique identifier for the tracking dataset.
#' "date" is the GMT date-time of each observation with the following format
#' "2001-11-13 07:59:59". "lc" is the Argos location quality class of each
#' observation, values in ascending order of quality are "Z", "B", "A", "0", "1",
#' "2", "3". "lon" is the observed longitude in decimal degrees. "lat" is the
#' observed latitude in decimal degress.
#'
#' @return A list is returned with each outer list element corresponding to each unique
#' individual id in the input data. 
#'
#' @author Ian Jonsen
#'
#' @examples
#' \dontrun{
#' }
#' @export

strip <- function(dat,
                  what = rep(1, 6),
                  min.obs = 30,
                  min.days = 10
                  ) {
  
  x <- split(dat, dat$id)
  
  ## remove duplicated date/time entries within each individual dataset
  x1 <- lapply(x, function(k) {
    k[!duplicated(k$date), ]
  })
  
  ## remove start locations in N hemisphere (e.g., Seattle, BAS, SMRU)
  x2 <- lapply(x1, function(k) {
    subset(k, lat < 10)
  })
  
  ## remove deployments with less than min.obs records
  deplen.log <- sapply(x2, nrow) < min.obs
  x3 <- x2[!deplen.log]
  
  ## remove deployments that last less than min.days days
  depdur.log <-
    sapply(x3, function(k)
      difftime(max(k$date), min(k$date), unit = "days") < min.days)
  x4 <- x3[!depdur.log]
  
  ## remove records with NA lat and/or lon
  x5 <- lapply(x4, function(k)
    subset(k, !is.na(lat) & !is.na(lon)))
  
  ## shift 0,360 longitudes to -180,180
  x6 <- lapply(x5, function(k) {
    k$lon <- with(k, ifelse(lon > 180, lon - 360, lon))
    k
  })
  
}