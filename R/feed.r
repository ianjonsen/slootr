#' Feed Argos track(s) to ssmTMB
#' 
#' This is a wrapper function that feeds single or multiple Argos datasets for filtering via
#' \code{ssmTMB::fit_ssm}
#' 
#' @param dat A data frame containing the following columns, "id","date",
#' "lc", "lon", "lat". "id" is a unique identifier for the tracking dataset.
#' "date" is the GMT date-time of each observation with the following format
#' "2001-11-13 07:59:59". "lc" is the Argos location quality class of each
#' observation, values in ascending order of quality are "Z", "B", "A", "0", "1",
#' "2", "3". "lon" is the observed longitude in decimal degrees. "lat" is the
#' observed latitude in decimal degress. The Z-class locations are assumed to 
#' have the same error distributions as B-class locations.
#' @param dlen index to trim input data down to dlen tracks for testing
#' @param ts time step in h
#' @param pass2 logical, try a second optimisation using optim (much slower than nlminb)
#' @param plot logical, generate plot of fits to data. File name is generated from name of
##          the input data object and appended with "_ssm.pdf"
#' @param span parameter that controls the degree of smoothing by \code{stats::loess},
#' used to obtain initial values for the location states. Smaller values = less
#' smoothing. Values > 0.3 may be required for sparse datasets
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
#' @importFrom pbapply pblapply
#' @export 

feed <- function(dat,
                  dlen = NULL,
                  ts = 2,
                  pass2 = FALSE,
                  plot = FALSE,
                  ...
                ) {
  
  options("pbapply" = "txt", warn = -1)
  
  if (is.null(dlen))
    dlen = 1:length(dat)
  
  cat("Starting optimisation using nlminb...\n")
  etime <- system.time(ssm <- pblapply(dlen, function(i) {
    d <- dat[[i]]
    try(fit_ssm(d, 
                subset = d$keep, 
                tstep = ts / 24, 
                ...), 
        silent = TRUE)
  }))
  cat(sprintf("\nTotal Elapsed time: %.2f min\n\n", etime[3] / 60))
  
  ## try second pass optimisation on any failed or false convergence results
  if (pass2) {
    fc <- which(sapply(ssm, function(x)
      length(x) == 1 || x$opt$conv != 0))
    cat(sprintf("%d tracks failed to converge\n", length(fc)))
    cat("Starting 2nd pass optimisation using optim...\n")
    etime <- system.time(foo <- pblapply(fc, function(i) {
      d <- dat[[i]]
      try(fit_ssm(d, 
                  subset = d$keep, 
                  tstep = ts / 24, 
                  optim = "optim", 
                  ...), 
          silent = TRUE)
    }))
    cat(sprintf("\nTotal Elapsed time (2nd pass): %.2f min\n", etime[3] /
                  60))
    ssm[fc] <- foo
  }
  options(warn = 0)
  
  if (plot)
    plot_fit(ssm, dat, deparse(substitute(dat)), dlen)
  
  ssm
}

