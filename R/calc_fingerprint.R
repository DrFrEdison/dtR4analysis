median_spc <- function(spc, numcol = NA){

  if( is.na(numcol[1])) suppressWarnings( median_spc <- as.numeric(apply(spc, 2, function(x) median(x, na.rm = T))))

  if(!is.na(numcol)[1]) suppressWarnings( median_spc <- as.numeric( apply( spc[ , numcol$numcol, with = F], 2, function(x) median(x, na.rm = T))))

  return(median_spc)
}

median_daily_spc <- function(spc, date, tz = "UTC", numcol = NA) {
  date <- as.Date(date, tz = tz)
  date.u <- unique(as.Date(date, tz = tz))
  date.us <- format(as.Date(date.u), "%y%m%d")
  date.usx <- paste0("X", date.us)
  names(date.u) <- date.usx
  date.v <- lapply(date.u, function(x) which(x == date))

  if( is.na(numcol[1])) suppressWarnings( daily_median <- lapply(date.v, function(x) median_spc(spc[ x , ])))

  if( !is.na(numcol[1])) suppressWarnings( daily_median <- lapply(date.v, function(x) median_spc(spc = spc[ x , numcol, with = F])))

  daily_median <- do.call(rbind, daily_median)
  ppp <- transfer_csv.num.col( spc )
  colnames( daily_median ) <- ppp$wl

  daily_median <- data.table(Date = as.POSIXct( gsub("X", "", rownames(daily_median)), format = "%y%m%d")
                             , daily_median)


  return(daily_median)
}

median_hourly_spc <- function(spc, datetime, tz = "UTC", numcol = NA) {

  hour_date <- paste0( as.Date( datetime), "_",   strftime( datetime, format = "%H"))

  hour_date.u <- unique(hour_date)

  hour_date.ux <- paste0("X", hour_date.u)
  names(hour_date.u) <- hour_date.ux

  hour_date.v <- lapply(hour_date.u, function(x) which(x == hour_date))

  if (is.na(numcol[1]))
    suppressWarnings(hourly_median <- lapply(hour_date.v, function(x) median_spc(spc[x,
    ])))
  if (!is.na(numcol[1]))
    suppressWarnings(hourly_median <- lapply(hour_date.v, function(x) median_spc(spc = spc[x,numcol, with = F])))

  hourly_median <- do.call(rbind, hourly_median)
  ppp <- transfer_csv.num.col( spc )
  colnames( hourly_median ) <- ppp$wl

  hourly_median <- data.table(Date_Hour = as.POSIXct( gsub("X", "", rownames(hourly_median)), format = "%Y-%m-%d_%H", tz = "UTC"), hourly_median)

  return(hourly_median)
}

median_weekly_spc <- function (spc, date, tz = "UTC", numcol = NA){
  cw_year <- paste0( strftime( date, format = "%y"), "_",   strftime( date, format = "%V"))

  cw_year.u <- unique(cw_year)

  cw_year.ux <- paste0("X", cw_year.u)
  names(cw_year.u) <- cw_year.ux

  cw_year.v <- lapply(cw_year.u, function(x) which(x == cw_year))

  if (is.na(numcol[1]))
    suppressWarnings(weekly_median <- lapply(cw_year.v, function(x) median_spc(spc[x,
    ])))
  if (!is.na(numcol[1]))
    suppressWarnings(weekly_median <- lapply(cw_year.v, function(x) median_spc(spc = spc[x,numcol, with = F])))

  weekly_median <- do.call(rbind, weekly_median)
  ppp <- transfer_csv.num.col( spc )
  colnames( weekly_median ) <- ppp$wl

  weekly_median <- data.table(KW = gsub("X", "", rownames(weekly_median)), weekly_median)

  # as.POSIXct( paste0(gsub("X", "", rownames(weekly_median)), "_1"), format = "%y_%U_%u")

  return(weekly_median)
}

fingerprint <- function(spc_0, spc_1, numcol = NA){

  if( is.na(numcol[1])) suppressWarnings( fp <- data.frame(t(apply(spc_1, 1, function(x) as.numeric(log10( spc_0 / x))))))

  if( !is.na(numcol[1])) suppressWarnings( fp <- data.frame(t(apply(spc_1[ , numcol$numcol, with = F], 1, function(x) as.numeric(log10( spc_0 / x))))) )

  if( is.na(numcol[1])) colnames(fp) <- colnames(spc_1)
  if( !is.na(numcol[1])) colnames(fp) <- colnames(spc_1)[ numcol$numcol ]
  return(fp)
}


