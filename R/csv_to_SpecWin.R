csv_to_SpecWin <- function( csv_file
                            , datetime = NA
                            , location = NA
                            , line = NA
                            , pl = NA
                            , info.text = NA
                            , merge.csv = T){

  csv_to_spc <- list()

  csv_to_spc$raw <- csv_file
  csv_to_spc$ppp <- transfer_csv.num.col( csv_file )

  if( nrow( csv_to_spc$raw) > 1 & length( datetime ) == 1) datetime <- rep(datetime, nrow( csv_to_spc$raw))
  if( nrow( csv_to_spc$raw) > 1 & length( location ) == 1) location <- rep(location, nrow( csv_to_spc$raw))
  if( nrow( csv_to_spc$raw) > 1 & length( line ) == 1) line <- rep(line, nrow( csv_to_spc$raw))
  if( nrow( csv_to_spc$raw) > 1 & length( pl ) == 1) pl <- rep(pl, nrow( csv_to_spc$raw))
  if( nrow( csv_to_spc$raw) > 1 & length( info.text ) == 1) info.text <- rep(info.text, nrow( csv_to_spc$raw))

  for(i in 1 : nrow(csv_file)){

    if( is.na(datetime[ i ]) &
        is.na(location[ i ]) &
        is.na(line[ i ]) &
        is.na(pl[ i ]) &
        is.na(info.text[ i ])){ csv_to_spc$name[[ i ]] <- paste0(gsub(":", ""
                                                                      , gsub(" ", "_"
                                                                             , gsub("-", "", substr( as.character(  Sys.time())
                                                                                                     , 3
                                                                                                     , nchar( as.character(  Sys.time())))))), ".csv") } else{
                                                                                                       csv_to_spc$name[[ i ]] <- paste0(gsub(":", ""
                                                                                                                                             , gsub(" ", "_"
                                                                                                                                                    , gsub("-", "", substr( as.character( datetime[ i ])
                                                                                                                                                                            , 3
                                                                                                                                                                            , nchar( as.character( datetime[ i ]))))))
                                                                                                                                        , "_"
                                                                                                                                        , location[ i ], "_"
                                                                                                                                        , line[ i ], "_"
                                                                                                                                        , pl[ i ], "_"
                                                                                                                                        , info.text[ i ]
                                                                                                                                        , ".csv")
                                                                                                     }

    csv_to_spc$name[[ i ]] <- gsub("NA_", "", csv_to_spc$name[[ i ]])
    csv_to_spc$name[[ i ]] <- gsub("_NA", "", csv_to_spc$name[[ i ]])

    csv_to_spc$export[[ i ]] <- data.frame( X1 = c("Wellenlänge [nm]", csv_to_spc$ppp$wl)
                                            , X2 = c("Intensität", as.numeric(csv_to_spc$raw[i , csv_to_spc$ppp$numcol ,with = F])))


    colnames( csv_to_spc$export[[ i ]] ) <- c( csv_to_spc$name[[ i ]], "X")

    csv_to_spc$export[[ i ]][ , 1] <- as.character( csv_to_spc$export[[ i ]][ , 1])
    csv_to_spc$export[[ i ]][ , 2] <- as.character( csv_to_spc$export[[ i ]][ , 2])
    csv_to_spc$export[[ i ]][ , 2] <- gsub( "\\.", "\\,", csv_to_spc$export[[ i ]][ , 2])

    csv_to_spc$export[[ i ]] <- data.frame( csv_to_spc$export[[ i ]], X.1 = NA)
  }

  if( !merge.csv)
    for(i in 1 : nrow(csv_file))
      write.table(csv_to_spc$export[[ i ]], csv_to_spc$name[[ i ]], row.names = F, quote = F, na = "", sep = ";")

  if( merge.csv){
    for(i in 1 : nrow(csv_file)){ csv_to_spc$export[[ i ]][ , 3] <- NULL

    csv_to_spc$export.merge <- do.call(cbind, csv_to_spc$export)
    colnames( csv_to_spc$export.merge ) <- gsub("\\.csv", "", gsub("X", "", colnames( csv_to_spc$export.merge )) )
    csv_to_spc$export.merge  <- cbind(csv_to_spc$export.merge , NA)
    }
    write.table(csv_to_spc$export.merge, csv_to_spc$name[[ 1 ]], row.names = F, quote = F, na = "", sep = ";")
  }
}
