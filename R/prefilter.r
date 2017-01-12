#' Prefilters extreme outlier locations in Argos data
#' 
#' Use \code{diveMove::grpSpeedFilter} to identify extreme 
#' outlier locations prior to \code{feed}-ing to \code{ssmTMB::fit_ssm}. Outliers are not 
#' removed but flagged with a logical vector so they can be ignored when \code{feed}-ing 
#' to \code{ssmTMB::fit_ssm}. \code{prefilter} also ensures data are temporally ordered.
#' 
#' @param d A data_frame containing the following columns: 
#' "id", "date", "lc", "lon", "lat". "id" is a unique identifier for the tracking dataset.
#' "date" is the GMT date-time of each observation with the following format
#' "2001-11-13 07:59:59". "lc" is the Argos location quality class of each
#' observation, values in ascending order of quality are "Z", "B", "A", "0", "1",
#' "2", "3". "lon" is the observed longitude in decimal degrees. "lat" is the
#' observed latitude in decimal degress. 
#' 
#' @return A tbl_df grouped by individual id is returned.
#' 
#' @author Ian Jonsen
#' 
#' @examples
#' \dontrun{
#' }
#' @importFrom diveMove grpSpeedFilter
#' @importFrom dplyr %>% tbl_df group_by do mutate arrange
#' @export 
prefilter <-
  function(d,
           vmax = 10,
           w = 5
           ) {

    if (class(d)[1] != "grouped_df") d <- tbl_df(d) %>% grouped_by(id)
    
    ## flag extreme travel rate locations for removal at ssm filter stage
    y <- d %>% do(., mutate(., keep = grpSpeedFilter(cbind(date, lon, lat), speed.thr = vmax, window = w))) %>%
      do(., arrange(., date))
    
  }