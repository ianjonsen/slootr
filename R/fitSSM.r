fitSSM = function(dat,
                  dlen = NULL,
                  ts = list(gps = 1, gls = 12, ptt = 2),
                  pass2 = FALSE,
                  plot = TRUE,
                  out = NULL,
                  ...) {
  ## wrapper function for calling dcrw() from Bremerhaven package
  ##
  ## dat - input cleaned data as a list of individual track data.frames in the expected format
  ## dlen - used to trim input data for testing
  ## ts - specify a list of time steps (in h) for gps, gls, & ptt datasets
  ## pass2 - try a second optimisation using optim for pass1 crashes & convergence failures
  ## plot - TRUE if .pdf of fit plots is to be generated. File name is generated from name of
  ##          the input data object and appended with "_ssm.pdf"
  ## out - name of output object. If not provided then the name of the input data object with
  ##          ".ssm" appended will be used
  ## ... - additional arguments passed to dcrw
  
  require(Bremerhaven, quiet = TRUE)
  require(pbapply, quiet = TRUE)
  options("pbapply" = "txt", warn = -1)
  
  if (is.null(dlen))
    dlen = 1:length(dat$pfdat)
  
  cat("Starting optimisation using nlminb...\n")
  etime <- system.time(ssm <- pblapply(dlen, function(i) {
    d <- dat$pfdat[[i]]
    switch(
      unique(d$device_type),
      GPS = {
        if (is.null(dat$ts))
          ts = ts$gps
        else{
          ts = dat$ts[i]
        }
      },
      GLS = {
        if (is.null(dat$ts))
          ts = ts$gls
        else {
          ts = dat$ts[i]
        }
      },
      PTT = {
        if (is.null(dat$ts))
          ts = ts$ptt
        else {
          ts = dat$ts[i]
        }
      }
    )
    try(dcrw(d, subset = d$filt, tstep = ts / 24, ...), silent = TRUE)
  }))
  cat(sprintf("\nTotal Elapsed time: %.2f min\n\n", etime[3] / 60))
  
  if (pass2) {
    ## try second pass optimisation on any failed or false convergence results
    fc = which(sapply(ssm, function(x)
      length(x) == 1 || x$opt$conv != 0))
    cat(sprintf("%d tracks failed to converge\n", length(fc)))
    cat("Starting 2nd pass optimisation using optim...\n")
    etime <- system.time(foo <- pblapply(fc, function(i) {
      d <- dat$pfdat[[i]]
      switch(
        unique(d$device_type),
        GPS = {
          if (is.null(dat$ts))
            ts = ts$gps
          else{
            ts = dat$ts[i]
          }
        },
        GLS = {
          if (is.null(dat$ts))
            ts = ts$gls
          else {
            ts = dat$ts[i]
          }
        },
        PTT = {
          if (is.null(dat$ts))
            ts = ts$ptt
          else {
            ts = dat$ts[i]
          }
        }
      )
      try(dcrw(d,
               subset = d$filt,
               tstep = ts / 24,
               optim = "optim"),
          silent = TRUE)
    }))
    cat(sprintf("\nTotal Elapsed time (2nd pass): %.2f min\n", etime[3] /
                  60))
    ssm[fc] <- foo
  }
  options(warn = 0)
  
  if (is.null(out))
    out <- deparse(substitute(dat))
  assign(paste(out, ".ssm", sep = ""),
         ssm,
         pos = 1,
         immediate = TRUE)
  
  if (plot)
    plotSSM(ssm, dat, out, dlen)
  
}

plotSSM <- function(ssm,
                    dat = NULL,
                    sp,
                    dlen = NULL,
                    res = "low") {
  ## generate a .pdf of SSM fits to data
  require(Bremerhaven, quiet = TRUE)
  require(ggplot2, quietly = TRUE)
  require(gridExtra, quietly = TRUE)
  require(pbapply, quietly = TRUE)
  require(mapdata, quietly = TRUE)
  options("pbapply" = "txt")
  switch(res,
         low = {
           map.world <- map_data(map = "world")
         },
         high = {
           map.world <- map_data(map = "worldHires")
         })
  
  if (!is.null(dat)) {
    d <- dat$pfdat
    if (is.null(dlen))
      dlen = 1:length(d)
  }
  else {
    d <- lapply(ssm, function(x) {
      if (length(x) > 1)
        x$data
      else {
        NA
      }
    })
  }
  
  cat("\n generating pdf plots...\n")
  pdf(
    paste(sp, "_par.pdf", sep = ""),
    width = 10,
    height = 8,
    pointsize = 16
  )
  
  params <- data.frame(do.call(rbind, lapply(ssm, function(x) {
    if (length(x) > 2)
      x$par[, 1]
    else{
      rep(NA, 7)
    }
  })))
  pairs(
    params,
    panel = text,
    cex.labels = 0.9,
    cex = 0.6,
    gap = 0.25
  )
  dev.off()
  
  pdf(
    paste(sp, "_ssm.pdf", sep = ""),
    width = 8,
    height = 10,
    pointsize = 16
  )
  
  pblapply(1:length(dlen), function(i) {
    if (length(ssm[[i]]) >= 6) {
      x.rng <-
        extendrange(c(wrapLon(ssm[[i]]$predicted$lon), subset(d[[dlen[i]]], filt)$lon), f =
                      0.05)
      y.rng <-
        extendrange(c(ssm[[i]]$predicted$lat, subset(d[[dlen[i]]], filt)$lat), f = 0.05)
      
      p1 <-
        ggplot() + geom_map(
          data = map.world,
          map = map.world,
          aes(map_id = "Antarctica"),
          fill = "snow"
        ) +
        theme(legend.position = "none") + ylim(y.rng) + xlim(x.rng) +
        geom_point(
          aes(x = lon, y = lat),
          data = subset(d[[dlen[i]]], filt),
          col = "firebrick",
          pch = 16,
          size = 1
        )
      
      if (sum(!d[[i]]$filt) > 0)
        p1 <-
        p1 + geom_point(
          aes(x = lon, y = lat),
          data = subset(d[[dlen[i]]], !filt),
          pch = 3,
          size = 1
        )
      
      p1 <-
        p1 + geom_path(
          data = ssm[[i]]$predicted,
          aes(x = wrapLon(lon), y = lat),
          col = "dodgerblue",
          lwd = 0.25
        ) +
        geom_point(
          data = ssm[[i]]$predicted,
          aes(x = wrapLon(lon), y = lat),
          pch = 16,
          col = "dodgerblue",
          size = 1
        ) +
        ggtitle(
          paste(
            "ID = ",
            unique(ssm[[i]]$data$id),
            "\n device type = ",
            unique(ssm[[i]]$data$device_type),
            "; time step = ",
            ssm[[i]]$tstep * 24,
            " h\n",
            "optimisation result: ",
            ssm[[i]]$opt$convergence,
            "; ",
            ssm[[i]]$opt$message,
            "\n gamma: ",
            round(ssm[[i]]$par[2, 1], 3)
            ,
            sep = ""
          )
        )
      
      y.rng <-
        extendrange(c(ssm[[i]]$predicted$lon, unwrapLon(subset(d[[dlen[i]]], filt)$lon)), f =
                      0.05)
      p2 <-
        ggplot(data = subset(d[[dlen[i]]], filt), aes(y = unwrapLon(lon), x = date)) + ylim(x.rng) +
        geom_point(col = "firebrick",
                   pch = 16,
                   size = 1)
      
      p2 <- p2 + geom_rug(col = "firebrick", sides = "b") +
        geom_line(
          data = ssm[[i]]$predicted,
          aes(x = date, y = lon + lon.se * 1.96),
          lwd = 0.25,
          col = "dodgerblue"
        ) +
        geom_line(
          data = ssm[[i]]$predicted,
          aes(x = date, y = lon - lon.se * 1.96),
          lwd = 0.25,
          col = "dodgerblue"
        ) +
        geom_point(
          data = ssm[[i]]$predicted,
          aes(x = date, y = lon),
          pch = 16,
          col = "dodgerblue",
          size = 1
        ) +
        geom_rug(
          data = ssm[[i]]$predicted,
          col = "dodgerblue",
          sides = "b",
          alpha = 0.5
        )
      
      y.rng <-
        extendrange(c(ssm[[i]]$predicted$lat, subset(d[[dlen[i]]], filt)$lat), f = 0.05)
      p3 <-
        ggplot(subset(d[[dlen[i]]], filt), aes(y = lat, x = date)) + ylim(y.rng) +
        geom_point(col = "firebrick",
                   pch = 16,
                   size = 1)
      
      p3 <- p3 + geom_rug(col = "firebrick", sides = "b") +
        geom_line(
          data = ssm[[i]]$predicted,
          aes(x = date, y = lat + lat.se * 1.96),
          lwd = 0.25,
          col = "dodgerblue"
        ) +
        geom_line(
          data = ssm[[i]]$predicted,
          aes(x = date, y = lat - lat.se * 1.96),
          lwd = 0.25,
          col = "dodgerblue"
        ) +
        geom_point(
          data = ssm[[i]]$predicted,
          aes(x = date, y = lat),
          pch = 16,
          col = "dodgerblue",
          size = 1
        ) +
        geom_rug(
          data = ssm[[i]]$predicted,
          col = "dodgerblue",
          sides = "b",
          alpha = 0.5
        )
      
      grid.arrange(p1, p2, p3, heights = c(2, 1, 1))
    }
    else if (length(ssm[[i]]) < 6) {
      if (!is.null(dat)) {
        x.rng <- extendrange(d[[dlen[i]]]$lon, f = 0.1)
        y.rng <- extendrange(d[[dlen[i]]]$lat, f = 0.1)
        p <-
          ggplot(d[[dlen[i]]], aes(x = lon, y = lat)) + 
          geom_point(col = "firebrick", size = 0.75) +
          theme(legend.position = "none") + 
          ggtitle(paste("Error fitting SSM to", as.character(dat$pfdat[[dlen[i]]]$id[1]))) + 
          coord_fixed(1) + 
          xlim(x.rng) +
          ylim(y.rng)
        print(p)
      }
      else if (is.null(dat)) {
        plot(
          0,
          0,
          type = 'n',
          main = "Error fitting SSM",
          axes = F,
          xlab = "",
          ylab = ""
        )
      }
    }
  })
  dev.off()
  
}