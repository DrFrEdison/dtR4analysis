read_olup <- function(olup_raw, ncomp = NA, show_else = F, write = F){

  olup <- list()
  olup$raw <- olup_raw

  if( length( grep("Y-Predicted", olup_raw) ) == 0 ) stop("Copy and Paste from OLUP did not work")

  if( length( grep("Plane", olup$raw) ) == 0) olup$Plane <- 0
  if( length( grep("Plane", olup$raw) ) > 0) olup$Plane <- max( as.numeric( unique( substr( olup$raw[ unlist( grep("Plane", olup$raw) ) ] , 7, 7))))

  if( !is.na( ncomp )) olup$ncomp <- ncomp
  if( !is.na( ncomp )) olup$Plane <- olup$ncomp - 1
  if( is.na( ncomp )) olup$ncomp <- olup$Plane + 1

  if( olup$Plane != 0)  olup$ypred <- olup$raw[ grep( paste0( "Plane ", olup$Plane), olup$raw) ]

  if( olup$Plane == 0)  olup$ypred <- olup$raw[ grep( "Y-Predicted ", olup$raw) + 1 ]

  if( length( olup$ypred) == 0) stop("Wrong Principal Components chosen")

  olup$ypred <- gsub( paste0("Plane ", olup$Plane, ":\t"), "", olup$ypred)

  olup$tothepower <- 10 ^ as.numeric( substr(olup$ypred, lapply( gregexpr( "e", olup$ypred), function( x ) min( x ) + 2), nchar(olup$ypred)) )

  olup$ypred <- as.numeric( substr(olup$ypred, 1, lapply( gregexpr( "e", olup$ypred), function( x ) min( x ) - 1)))

  olup$ypred <- olup$ypred * olup$tothepower

  if(show_else){

    olup$scores <- olup$raw[ grep( "Scores", olup$raw) + olup$ncomp ]

    olup$tothepower <- 10 * as.numeric( substr(olup$scores, lapply( gregexpr( "e", olup$scores), function( x ) min( x ) + 2), nchar(olup$scores)) )

    olup$scores <- as.numeric( substr(olup$scores, 1, lapply( gregexpr( "e", olup$scores), function( x ) min( x ) - 1)))

    olup$scores <- olup$scores * olup$tothepower


    olup$leverages <- olup$raw[ grep( "Leverages ", olup$raw) + olup$ncomp ]

    olup$tothepower <- 10 * as.numeric( substr(olup$leverages, lapply( gregexpr( "e", olup$leverages), function( x ) min( x ) + 2), nchar(olup$leverages)) )

    olup$leverages <- as.numeric( substr(olup$leverages, 1, lapply( gregexpr( "e", olup$leverages), function( x ) min( x ) - 1)))

    olup$leverages <- olup$leverages * olup$tothepower
  }

  if(!show_else) olup$return <- data.frame(Software = "OLUP", ncomp = olup$ncomp, Y_Pred = olup$ypred )
  if(show_else) olup$return <- data.frame(Software = "OLUP", ncomp = olup$ncomp, Y_Pred = olup$ypred, Scores = olup$scores, Leverages = olup$leverages )

  if( write ) write.csv2(olup$return, paste0(date.dt(), "_Olup_Prediction.csv"), row.names = F)
  return(  olup$return )
}
