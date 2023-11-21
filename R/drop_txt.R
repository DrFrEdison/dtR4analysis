drop_txt <- function( path, recursive = F, wl = 190:598){

  dt <- list()
  dt$wd <- path
  setwd( dt$wd )

  dt$files <- list.files( pattern = "\\.txt$", recursive = recursive)

  dt$raw <- lapply( dt$files, function( x ) fread( x, header = F, sep = ";"))

  dt$raw <- lapply( dt$raw, function( x ) gsub("\\{", "", x))
  dt$raw <- lapply( dt$raw, function( x ) gsub("\\}", "", x))
  dt$raw <- lapply( dt$raw, function( x ) gsub("\\,", ".", x))
  dt$raw <- lapply( dt$raw, as.numeric)

  dt$trs <- do.call( rbind, dt$raw)
  colnames( dt$trs ) <- wl

  dt$name$date <- substr(dt$files, 3, 8)
  dt$name$time <- substr(dt$files, 10, 15)
  dt$name$type <- substr( dt$files
                          , lapply(gregexpr("_", dt$files), function( x ) x[[ 3 ]] + 1)
                          , lapply(gregexpr("\\.", dt$files), function( x ) x[[ 1 ]] - 1))

  dt$export <- data.frame( ID = dt$files
                           , datetime = paste(dt$name$date, dt$name$time)
                           , date = dt$name$date
                           , time = dt$name$time
                           , dt$trs)

  write.csv2( dt$export, paste0( substr(gsub("-", "", Sys.Date()), 3, 8), "_Spektren.csv"), row.names = F)
}
