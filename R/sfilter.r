#' State-space Filter Argos track(s) via \code{ssmTMB::fit_ssm}
#' 
#' This is a wrapper function that feeds single or multiple Argos datasets for state-space filtering via
#' \code{ssmTMB::fit_ssm}
#' 
#' @param d data output from prefilter() or trim(), ie. a tibble data_frame, grouped by individual id
#' with a 'keep' logical variable indicating which observations are to be ignored
#' @param ts time step in hours
#' @param ... other arguments to \code{ssmTMB::fit_ssm}
#' 
#' @return A list is returned with each outer list elements corresponding to each unique 
#' individual id in the input data. 
#' @author Ian Jonsen
#' 
#' @examples
#' \dontrun{
#' }
#' @importFrom ssmTMB fit_ssm
#' @export 

sfilter <- function(d,
                 ts = 4,
                 ...) {
  if (class(d)[1] != "grouped_df") stop("Data must be a tibble grouped by individual id")
  if (!"keep" %in% names(d)) stop("Data must include a logical variable named 'keep'")
  
  d %>% do(ssm = try(fit_ssm(., subset = .$keep, tstep = ts / 24, ...), silent = TRUE))
}



