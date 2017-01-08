#' Strip Argos track data of unwanted records
#'
#' Strip does the following:  1) removes duplicated date records; 2) removes start locations in
#' N Hemisphere, eg. Seattle, BAS, SMRU (will be generalised later); 3) removes deployments with
#' less than min.obs records; 4) removes deployments that last less than min.days; 5) removes
#' records with NA lat and/or lon; 6) removes Z-class locations.
#' Each of these steps can be optionally turned off via the \code{what} logical vector.
#'
#' @param d A data.frame containing the following columns:
#' "id", "date", "lc", "lon", "lat". "id" is a unique identifier for the tracking dataset.
#' "date" is the GMT date-time of each observation with the following format
#' "2001-11-13 07:59:59". "lc" is the Argos location quality class of each
#' observation, values in ascending order of quality are "Z", "B", "A", "0", "1",
#' "2", "3". "lon" is the observed longitude in decimal degrees. "lat" is the
#' observed latitude in decimal degress.
#' @param what A logical vecotr of length 6 that turns on or off each of the 6 data stripping
#' steps.
#' @param min_obs The minimum number of observations an individual track requires to be retained.
#' @param min_days The minimum number of deployment days an individual tracks requires to be retained. 
#'
#' @return A tbl_df grouped by individual id is returned. 
#'
#' @author Ian Jonsen
#'
#' @examples
#' \dontrun{
#' }
#' @importFrom dplyr %>% tbl_df group_by do distinct filter summarise
#' @export

strip <- function(d,
                  what = rep(TRUE, 6),
                  min_obs = 30,
                  min_days = 10
                  ) {

  if(class(d)[1] != "tbl_df") d <- tbl_df(d)
  
  x <- d %>% group_by(id)
  
  ## remove records with duplicate date/time
  if (what[1]) x1 <- do(x, distinct(., date, .keep_all = TRUE))
  
  ## remove start locations in N hemisphere (e.g., Seattle, BAS, SMRU)
  if (what[2]) x2 <- do(x1, filter(., lat < 10))
  
  ## remove deployments with less than min_obs records
  if (what[3]) x3 <- do(x2, filter(., n() >= min_obs))
    
  ## remove deployments that last less than min_days days
  if (what[4]) {
    dt <- function(x) difftime(max(x$date), min(x$date), unit = "days")
    x4 <- do(x3, filter(., dt(.) >=  min_days))
  }

  ## remove records with NA lat or lon
  if (what[5]) x5 <- do(x4, filter(., !is.na(lat), !is.na(lon)))
  
  ## remove Z-class location records
  if (what[6]) x6 <- do(x5, filter(., lc != "Z"))
  
  cat("\nResults:\n")
  r_st <- summarise(x) %>% dim() %>% .[1]
  r_obs <- summarise(x3) %>% dim() %>% .[1]
  r_days <- summarise(x4) %>% dim() %>% .[1]
  
  cat(sprintf("%d tracks input\n", r_st))
  cat(sprintf("%d tracks with < min_obs records removed\n", r_st - r_obs))
  cat(sprintf("%d tracks lasting < min_days removed\n", r_st - (r_days - r_obs))
  cat(sprintf("%d tracks retained\n\n", r_st - r_days))
   
 x6 
}