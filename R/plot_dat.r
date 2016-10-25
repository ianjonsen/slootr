dat.plt <- function(dat) {
  ## generate a .pdf containing pre-filtered QC plots for all individual datasets
  require(ggplot2, quietly = TRUE)
  require(gridExtra, quietly = TRUE)
  require(pbapply, quietly = TRUE)
  require(mapdata, quietly = TRUE)
  require(Bremerhaven, quietly = TRUE)
  options("pbapply" = "txt")
  map.world <- map_data(map = "worldHires")
  
  d <- dat$pfdat
  sp <- deparse(substitute(dat))
  
  pdf(
    paste(sp, ".pdf", sep = ""),
    width = 8,
    height = 10,
    pointsize = 16
  )
  pblapply(d, function(k) {
    dt <-
      as.numeric(with(k, difftime(date[-1], date[-nrow(k)], units = "hours")))
    x.rng <- extendrange(k$lon, f = 0.1)
    y.rng <- extendrange(k$lat, f = 0.1)
    z <- with(k, data.frame(
      deploy = c("year", "deploy"),
      xmin = c(ISOdate(format(date[1], "%Y"), 1, 1), date[1]),
      xmax = c(ISOdate(format(date[nrow(k)], "%Y"), 12, 31), date[nrow(k)])
    ))
    p1 <-
      ggplot(z) + geom_rect(aes(
        fill = deploy,
        ymin = 0,
        ymax = 1,
        xmin = xmin,
        xmax = xmax
      )) +
      theme(legend.position = "none") + theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()
      ) +
      scale_fill_manual(values = c("dodgerblue", "grey80")) +
      ggtitle(paste(
        "ID = ",
        unique(k$id),
        "; Device = ",
        unique(k$device_type),
        sep = ""
      ))
    p2 <-
      ggplot() + geom_map(
        data = map.world,
        map = map.world,
        aes(map_id = "Antarctica"),
        fill = "snow"
      ) +
      theme(legend.position = "none") +
      geom_point(
        aes(x = lon, y = lat),
        data = k,
        col = "firebrick",
        pch = 16,
        cex = 0.9
      ) +
      geom_path(data = k,
                aes(x = lon, y = lat),
                col = "firebrick",
                lwd = 0.5) +
      geom_point(
        data = k[1, ],
        aes(x = lon, y = lat),
        pch = 17,
        cex = 2,
        col = "dodgerblue"
      ) +
      ggtitle(paste("Mean time difference:", round(mean(dt), 1), "h"))
    p3 <- ggplot(k, aes(y = unwrapLon(lon), x = date)) +
      geom_point(col = "firebrick", pch = 16, cex = 0.9) +
      geom_rug(col = "dodgerblue", sides = "b")
    p4 <- ggplot(k, aes(y = lat, x = date)) +
      geom_point(col = "firebrick", pch = 16, cex = 0.9) +
      geom_rug(col = "dodgerblue", sides = "b")
    grid.arrange(p1, p2, p3, p4, heights = c(0.4, 2, 1, 1))
  })
  dev.off()
}