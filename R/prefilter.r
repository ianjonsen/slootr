prefilter <-
  function(sp = "ADPE",
           fp = NULL,
           min.obs = 20,
           min.days = 1,
           vmax = 10,
           dmax = 500,
           path2dropbox = "~/Dropbox") {
    require(pbapply, quietly = TRUE)
    require(diveMove, quietly = TRUE)
    require(geosphere, quietly = TRUE)
    require(tibble, quietly = TRUE)
    
    if (is.null(fp)) {
      ## load canonical metadata & tracking data files
      fp.meta.sb <-
        file.path(
          path2dropbox,
          "RAATD_Data_2016",
          "WS1_Delmenhorst",
          "metadata",
          "v0.15",
          "SCAR_Metadata_Seabirds_2016.csv"
        )
      fp.meta.mm <-
        file.path(
          path2dropbox,
          "RAATD_Data_2016",
          "WS1_Delmenhorst",
          "metadata",
          "v0.15",
          "SCAR_Metadata_MM_2016.csv"
        )
      fp.raatd <-
        file.path(
          path2dropbox,
          "RAATD_Data_2016",
          "WS1_Delmenhorst",
          "trimmed_data",
          paste("RAATD2016_", sp, ".csv", sep = "")
        )
    }
    else {
      fp.meta.sb <- fp[[1]]
      fp.meta.mm <- fp[[2]]
      fp.raatd <- fp[[3]]
    }
    ## merge metadata & subset to species==sp
    ## only using the first 33 columns of both files (mm metadata has more)
    
    meta.sb <-
      read.csv(
        fp.meta.sb,
        stringsAsFactors = FALSE,
        header = TRUE,
        sep = ",",
        comment.char = ""
      )
    
    meta.mm <-
      read.csv(
        fp.meta.mm,
        stringsAsFactors = FALSE,
        header = TRUE,
        sep = ",",
        comment.char = ""
      )
    
    meta.mm <-
      meta.mm[, intersect(colnames(meta.mm), colnames(meta.sb))]
    meta.sb <-
      meta.sb[, intersect(colnames(meta.mm), colnames(meta.sb))]
    
    meta <- rbind(meta.sb, meta.mm)
    
    meta.sub <- subset(meta, abbreviated_name == sp)
    
    dev.dat <-
      with(meta.sub,
           data_frame(individual_id = individual_id, device_type))
    
    ## find individual tracks to discard
    discard.ids <-
      with(meta.sub, individual_id[which(keepornot == "Discard")])
    
    ## read in tracking dataset & merge with dev.dat to add correct Unit (tag/data) type
    dat <-
      read.csv(
        fp.raatd,
        stringsAsFactors = FALSE,
        comment.char = "",
        fileEncoding = "UTF-8"
      )
    dat <-
      merge(
        dat,
        dev.dat,
        by = "individual_id",
        all.x = TRUE,
        suffixes = c(".x", "")
      )
    
    ## remove 'hanging' locations at track starts & ends using 'location_to_keep' flag in canonical data
    d <-
      with(
        subset(dat, location_to_keep == 1 | is.na(location_to_keep)),
        data_frame(
          id = as.character(individual_id),
          date = as.POSIXct(paste(paste(
            year, month, day, sep = "-"
          ), time), "%Y-%m-%d %H:%M:%S", tz = "GMT"),
          lc = location_quality,
          lon = as.numeric(decimal_longitude),
          lat = as.numeric(decimal_latitude),
          device_type = device_type,
          trip = trip,
          season = Season
        )
      )
        
    d$lc <- as.character(d$lc)
    d <- subset(d,!is.na(date))
    
    ## remove individual datasets ID'd as "Discard" by Data Management Group
    d <- subset(d,!id %in% discard.ids)
    
    ## assign location class 3 to all records for GLS & GPS data
    dev.typ <- unique(as.character(d$device_type))
    d$device_type <-
      as.character(d$device_type) # force device_type to be character NOT factor

    tmp <- split(d, d$device_type)
    tmp <- lapply(tmp, function(x) {
      switch(
        unique(as.character(x$device_type)),
        GLS = {
          x$lc = "3"
        },
        GPS = {
          x$lc = "3"
        },
        PTT = {
          x$lc[x$lc == "-9"] = "Z"
          x$lc[x$lc == "-3"] = "Z"
          x$lc[x$lc == "-2"] = "B"
          x$lc[x$lc == "-1"] = "A"
        }
      )
      x
    })
    
    d <- do.call(rbind, tmp)
    ## for WAAL, fill in missing lc values in Marion Island PTT data with "3" & coerce lc to an ordered factor
    if (sp == "WAAL")
      d$lc[d$device_type == "PTT" & is.na(d$lc)] <- "3"
    d$lc <-
      factor(as.character(d$lc),
             levels = c(3, 2, 1, 0, "A", "B", "Z"),
             ordered = TRUE)
    
    ## coerce to list of individual track data.frames
    x <- split(d, d$id)
    
    ## order records by date/time & remove duplicated date/time entries within each individual dataset
    x1 <- lapply(x, function(z) {
      z1 = z[order(z$date), ]
      z1[!duplicated(z1$date), ]
    })
    
    ## remove start locations in N hemisphere (e.g., Seattle, BAS, SMRU)
    x2 <- lapply(x1, function(k) {
      subset(k, lat < 10)
    })
    
    ## remove deployments with less than min.obs (originally set to < 40)
    deplen.log <- sapply(x2, nrow) < min.obs
    x3 <- x2[!deplen.log]
    
    ## remove deployments that last less than min.days days (originally set to < 10)
    depdur.log <-
      sapply(x3, function(k)
        difftime(max(k$date), min(k$date), unit = "days") < min.days)
    x4 <- x3[!depdur.log]
    
    ## remove Z-class locations
    x5 <- lapply(x4, function(k)
      subset(k, lc != "Z"))
    
    ## remove records with NA lat and/or lon
    x6 <- lapply(x5, function(k)
      subset(k,!is.na(lat) & !is.na(lon)))
    
    ## shift 0,360 longitudes to -180,180
    x7 <- lapply(x6, function(k) {
      k$lon <- with(k, ifelse(lon > 180, lon - 360, lon))
      k
    })
    
    ## flag extreme travel rate locations for removal at ssm filter stage
    options("pbapply" = "txt")
    x8 <- pblapply(x7, function(k) {
      k$filt <-
        try(with(k, grpSpeedFilter(cbind(date, lon, lat), speed.thr = vmax)), silent =
              TRUE)
      k
    })
    
    ## speed filter doesn't flag first or last locations so use a distance threshold of dmax km
    y <- lapply(x8, function(k) {
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
 
    final_ids <- sapply(y, function(x) x$id[1])
    meta <- meta.sub[meta.sub$individual_id %in% final_ids, ]
       
    ## return list of original data (minus id's flagged to discard - from metadata) & filtered data
    ##  numfilteredout - is the number of datasets dropped because they were below the min.obs &/or
    ##  min.days thresholds
    list(
      pfdat = y,
      meta = meta,
      min.obs = min.obs,
      min.days = min.days,
      vmax = vmax,
      dmax = dmax,
      numfilteredout = length(unique(dat$individual_id)) - length(y)
    )
  }

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