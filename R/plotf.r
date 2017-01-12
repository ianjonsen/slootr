#' Visualise a \code{ssmTMB} fit to data as a map and as 1-d time-series
#'
#' @importFrom ggplot2 ggplot geom_point geom_map theme geom_path ggtitle
#' @importFrom ggplot2 geom_line geom_rug ylim xlim coord_fixed map_data
#' @importFrom gridExtra grid.arrange
#' @importFrom dplyr filter
#' @importFrom tibble tibble
#' @export

plotf <- function(ssm, res) {
  require(mapdata)
  
  wrapLon <- function(lon, lmin = -180)
    (lon - lmin) %% 360 + lmin
  
  unwrapLon <- function(lon, lmin = -180)
    cumsum(c(wrapLon(lon[1], lmin), wrapLon(diff(lon))))
  
  switch(res,
         low = {
           world_map <- map_data("world")
         },
         high = {
           world_map <- map_data("worldHires")
         })
  
  
  x.rng <- extendrange(c(wrapLon(ssm$predicted$lon),
                         filter(ssm$d, keep)$lon), f = 0.05)
  y.rng <- extendrange(c(ssm$predicted$lat,
                         filter(ssm$d, keep)$lat), f = 0.05)
  
  p1 <- ggplot() + geom_map(data = world_map,
                            map = world_map,
                            aes(map_id = region),
                            fill = "grey70") +
    theme(legend.position = "none") + ylim(y.rng) + xlim(x.rng) +
    geom_point(
      aes(x = lon, y = lat),
      data = filter(ssm$d, keep),
      colour = "dodgerblue",
      alpha = 0.25
    ) +
    geom_point(
      data = filter(ssm$d,!keep),
      aes(x = lon, y = lat),
      colour = "yellow3",
      size = 0.75,
      alpha = 0.8
    ) +
    geom_path(
      data = ssm$predicted,
      aes(x = wrapLon(lon), y = lat),
      colour = "firebrick1",
      size = 0.2
    ) +
    geom_point(
      data = ssm$predicted,
      aes(x = wrapLon(lon), y = lat),
      colour = "firebrick1",
      size = 0.6
    ) +
    ggtitle(
      paste(
        "ID = ",
        unique(ssm$d$id),
        "; time step = ",
        ssm$tstep * 24,
        " h\n",
        "optimisation result: ",
        ssm$opt$convergence,
        "; ",
        ssm$opt$message,
        sep = ""
      )
    )
  lon_resid <- tibble(
    date = ssm$fitted$date,
    resid = ssm$fitted$lon - filter(ssm$d, keep)$lon
  )
  lat_resid <- tibble(
    date = ssm$fitted$date,
    resid = ssm$fitted$lat - filter(ssm$d, keep)$lat
  )
  p2 <-
    ggplot(data = lon_resid) +
    geom_point(aes(y = resid, x = date), colour = "dodgerblue") +
    geom_rug(col = "dodgerblue", sides = "b")
  
  
  p3 <-
    ggplot(data = lat_resid) +
    geom_point(aes(y = resid, x = date), colour = "dodgerblue") +
    geom_rug(col = "dodgerblue", sides = "b")
  
  grid.arrange(p1, p2, p3, heights = c(2, 1, 1))
  
}