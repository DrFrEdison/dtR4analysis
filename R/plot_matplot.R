matplot.r4dt <- function(spc
                         , ppp = NA
                         , derivative = "spc"
                         , xlab = lambda
                         , ylab = NA
                         , seqp = ifelse(nrow(spc) > 1000, round(nrow(spc) / 800, 0), 1)
                         , lty = 1
                         , type = "l"
                         , main = ""
                         , colp = NA){

  if( length(ppp) == 1) x <- spc$wl
  if( length(ppp) > 1) x <- ppp$wl

  if( length(ppp) == 1) y <- spc[[ grep(derivative, names( spc ))[ 1 ] ]]
  if( length(ppp) > 1) y <- spc[ , ppp$numcol, with = F]

  if( is.na( ylab)) ylab <- ifelse(sum(y) / nrow(y) > 5000, "Counts", "AU")

  if( derivative == "1st") ylab = ylab_1st
  if( derivative == "2nd") ylab = ylab_2nd

  rowp <- seq( 1, nrow(y), seqp)

  if( all(is.na(colp))) colp = colorRamps::blue2red( length( rowp ))

  matplot(x, t(y[rowp , ])
          , lty = lty
          , type = type
          , xlab = xlab
          , ylab = ylab
          , main = main
          , col = colp)

}
