#' strip out extreme outlier locations from Argos data
#' 
#' Use \code{diveMove::grpSpeedFilter} & \code{geosphere::distGeo} to identify extreme 
#' outlier locations prior to \code{feed}-ing to \code{ssmTMB::fit_ssm}. Outliers are not 
#' removed but flagged with a logical vector so they can be ignored when \code{feed}-ing 
#' to \code{ssmTMB::fit_ssm}. \code{geosphere::distGeo} is only used for the first and
#' last locations, which aren't handled by \code{diveMove::grpSpeedFilter}.
#' 
#' @param dat A data_frame containing the following columns: 
#' "id", "date", "lc", "lon", "lat". "id" is a unique identifier for the tracking dataset.
#' "date" is the GMT date-time of each observation with the following format
#' "2001-11-13 07:59:59". "lc" is the Argos location quality class of each
#' observation, values in ascending order of quality are "Z", "B", "A", "0", "1",
#' "2", "3". "lon" is the observed longitude in decimal degrees. "lat" is the
#' observed latitude in decimal degress. 
#' 
#' @return A list is returned with each outer list element corresponding to each unique 
#' individual id in the input data. 
#' @author Ian Jonsen
#' 
#' @examples
#' \dontrun{
#' }
#' @importFrom ssmTMB fit_ssm
#' @importFrom tibble data_frame as_data_frame
#' @importFrom diveMove grpSpeedFilter
#' @importFrom geosphere distGeo
#' @importFrom pbapply pblapply
#' @export 
strip <-
  function(dat,
           min.obs = 20,
           min.days = 1,
           vmax = 10,
           dmax = 500
           ) {
    
    ## flag extreme travel rate locations for removal at ssm filter stage
    options("pbapply" = "txt")
    x <- pblapply(dat, function(k) {
      k$filt <-
        try(with(k, grpSpeedFilter(cbind(date, lon, lat), speed.thr = vmax)), silent =
              TRUE)
      k
    })
    
    ## speed filter doesn't flag first or last locations so use a distance threshold of dmax km
    y <- lapply(x, function(k) {
      d1 <-
        with(k, distGeo(cbind(lon, lat)[1, ], cbind(lon, lat)[2, ], a = 6378.137))
      d2 <-
        with(k, distGeo(cbind(lon, lat)[nrow(k) - 1, ], cbind(lon, lat)[nrow(k), ], a =
                          6378.137))
      if (d1 > dmax)
        k$filt[1] <- FALSE
      if (d2 > dmax)
        k$filt[nrow(k)] <- FALSE
      k
    })
  y
  }