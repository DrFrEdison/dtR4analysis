# Create validation set with extreme spectra ####
validation.set <- function( spc
                            , lambda.choose
                            , lambda.min.max = 10){

  spc.val.set <- list()

  spc.val.set$sub <- list()
  spc.val.set$sub$lambda.choose <- list()
  spc.val.set$sub$lambda.choose.min <- list()
  spc.val.set$sub$lambda.choose.max <- list()
  spc.val.set$sub$lambda.choose.middle <- list()

  for(l in lambda.choose){

    x <- as.numeric(unlist( spc[ , grep(paste0("X", l), names( spc )), with = F] ))
    spc.val.set$sub$lambda.choose[[ which(lambda.choose %in% l) ]] <- order( x )
    spc.val.set$sub$lambda.choose[[ which(lambda.choose %in% l) ]] <- spc.val.set$sub$lambda.choose[[ which(lambda.choose %in% l) ]][ which( !is.na(x)) ]

    spc.val.set$sub$lambda.choose.min[[ which(lambda.choose %in% l) ]] <- spc.val.set$sub$lambda.choose[[ which(lambda.choose %in% l) ]][ 1 : lambda.min.max ]

    spc.val.set$sub$lambda.choose.middle[[ which(lambda.choose %in% l) ]] <- spc.val.set$sub$lambda.choose[[ which(lambda.choose %in% l) ]][ (round(length(spc.val.set$sub$lambda.choose[[ which(lambda.choose %in% l) ]]) / 2, 0) - lambda.min.max / 3 + 1) :
                                                                                                                                               (round(length(spc.val.set$sub$lambda.choose[[ which(lambda.choose %in% l) ]]) / 2, 0) + lambda.min.max / 3 - 1)
    ]

    spc.val.set$sub$lambda.choose.max[[ which(lambda.choose %in% l) ]] <- spc.val.set$sub$lambda.choose[[ which(lambda.choose %in% l) ]][ (length( spc.val.set$sub$lambda.choose[[ which(lambda.choose %in% l) ]] ) - (lambda.min.max - 1)) : length( spc.val.set$sub$lambda.choose[[ which(lambda.choose %in% l) ]] )]

  }

  spc.val.set$validation.vector <- unique( c(unique( unlist( spc.val.set$sub$lambda.choose.min))
                                             , unique( unlist( spc.val.set$sub$lambda.choose.max))
                                             , unique( unlist( spc.val.set$sub$lambda.choose.middle))))

  # spc.val.set$spc <- data.table(validation = "valid"
  #                               , datetime = datetime[ spc.val.set$validation.vector ]
  #                               , spc[ spc.val.set$validation.vector , ])

  # spc.val.set$spc <- spc[ spc.val.set$validation.vector , ]
  return( spc.val.set$validation.vector )
}

# Read up to date validation set ####
validation.csv <- function(pattern = "_SDS_validation_set.csv"
                           , beverage = sds$info$beverage
                           , sep = ";", dec = ","
                           , dir_wd = getwd()){

  setwd( dir_wd )
  csv.val <- list()

  csv.val$csv <- dir( pattern = paste0( beverage, pattern))
  csv.val$csv <- sort(csv.val$csv)
  csv.val$csv <- csv.val$csv[ length( csv.val$csv)]
  csv.val$csv <- fread(csv.val$csv, sep = sep, dec = dec)
  csv.val$ppp <- transfer_csv.num.col(csv.val$csv)

  for(d in 1 : length( unique( as.Date( csv.val$csv$datetime ) ))){

    csv.val$spc.list[[ d ]] <- csv.val$csv[ which(as.Date( csv.val$csv$datetime ) == unique( as.Date( csv.val$csv$datetime ) )[[ d ]]) ]

  }

  returnlist <- list(csv.val$csv, csv.val$ppp, csv.val$spc.list)
  names(returnlist) <- c("csv", "ppp", "spc.list")
  return(returnlist)
}

# Do statistics for validation set ####
validation.spc <- function( spc = spc.val$pca$spc.lambda1
                            , ncomp = 2
                            , limT2 = sds$para$limT2
                            , limQ = sds$para$limQ
                            , lim.type = sds$para$lim.type
                            # , limLOOP = .8
                            # , k.LOOP = 5
                            # , lambda.LOOP = 7
                            # , limiF = .8
                            ){

  spc.val <- list()
  spc.val$pca <- mdatools::pca(spc
                               , ncomp = ncomp
                               , lim.type = lim.type)

  # spc.val$isolationForest <- isotree::isolation.forest( spc )
  # spc.val$anomaly_scores <- predict(spc.val$isolationForest, newdata = spc)
  # spc.val$LOOP <- LOOP(spc, k = k.LOOP, lambda = lambda.LOOP)

  returnlist <- list(spc.val$pca) #, spc.val$LOOP, spc.val$anomaly_scores)

  names(returnlist) <- c("PCA") #, "LOOP", "anomaly")
  return(returnlist)
}

validation.spc.val <- function(spc
                               , ppp
                               , lambda
                               , n){
  lambda.choose <- unique(c(min(lambda), max(lambda), sample(lambda, 3)))

  pca.sub <- mdatools::pca(spc[, ppp$numcol[ ppp$wl %in% lambda ], with = F], ncomp = 2)
  pca.sub.order.Q.1 <- order(pca.sub$calres$Q[ , 1])
  pca.sub.order.Q.2 <- order(pca.sub$calres$Q[ , 2])
  pca.sub.order.T2.1 <- order(pca.sub$calres$T2[ , 1])
  pca.sub.order.T2.2 <- order(pca.sub$calres$T2[ , 2])

  spc.length <- nrow(spc)

  choose.low <- 1:n
  choose.high <- spc.length : (spc.length - (n - 1))
  choose.middle <- c(spc.length / 2 - round(n / 2, 0), spc.length / 2, spc.length / 2 + round(n / 2, 0))

  choose.c <- unique(
    c(
      c(which(pca.sub.order.Q.1 %in% choose.low)
        , which(pca.sub.order.Q.2 %in% choose.low)
        , which(pca.sub.order.T2.1 %in% choose.low)
        , which(pca.sub.order.T2.2 %in% choose.low))
      ,
      c(which(pca.sub.order.Q.1 %in% choose.high)
        , which(pca.sub.order.Q.2 %in% choose.high)
        , which(pca.sub.order.T2.1 %in% choose.high)
        , which(pca.sub.order.T2.2 %in% choose.high))
      ,
      c(which(pca.sub.order.Q.1 %in% choose.middle)
        , which(pca.sub.order.Q.2 %in% choose.middle)
        , which(pca.sub.order.T2.1 %in% choose.middle)
        , which(pca.sub.order.T2.2 %in% choose.middle))
    )
  )

  return(choose.c)
}

# Return statistical results for new sample ####
validation.test <- function(validation.spc = sds$sds$val.lambda1[[ z ]][[ i ]]
                            , ncomp = 2
                            , Qlim = 1:2
                            , T2lim = 1:2
                            , LOOPlim = .8
                            , iFlim = .7){

  spc.val <- list()
  spc.val$spc <- validation.spc

  spc.val$Qc <- max(spc.val$spc$PCA$calres$Q[ , ncomp][ - length(spc.val$spc$PCA$calres$Q[ , ncomp])])
  spc.val$Qx <- spc.val$spc$PCA$calres$Q[ , ncomp][ length(spc.val$spc$PCA$calres$Q[ , ncomp])]
  # spc.val$Qlim <- ifelse(spc.val$Qc > spc.val$spc$PCA$Qlim[Qlim,ncomp], spc.val$Qc, spc.val$spc$PCA$Qlim[Qlim,ncomp])
  spc.val$Qlim <- spc.val$spc$PCA$Qlim[Qlim,ncomp]

  spc.val$T2c <- max(spc.val$spc$PCA$calres$T2[ , ncomp][ - length(spc.val$spc$PCA$calres$T2[ , ncomp])])
  spc.val$T2x <- spc.val$spc$PCA$calres$T2[ , ncomp][ length(spc.val$spc$PCA$calres$T2[ , ncomp])]
  # spc.val$T2lim <- ifelse(spc.val$T2c > spc.val$spc$PCA$T2lim[T2lim,ncomp], spc.val$T2c, spc.val$spc$PCA$T2lim[T2lim,ncomp])
  spc.val$T2lim <- spc.val$spc$PCA$T2lim[T2lim,ncomp]

  # spc.val$LOOPc <- max(spc.val$spc$LOOP[ - length(spc.val$spc$LOOP)])
  # spc.val$LOOPx <- spc.val$spc$LOOP[ length(spc.val$spc$LOOP)]
  # spc.val$LOOPlim <- ifelse(spc.val$LOOPc > LOOPlim, spc.val$LOOPc, LOOPlim)

  # spc.val$iFc <- max(spc.val$spc$anomaly[ - length(spc.val$spc$anomaly)])
  # spc.val$iFx <- spc.val$spc$anomaly[ length(spc.val$spc$anomaly)]
  # spc.val$iFlim <- ifelse(spc.val$iFc > iFlim, spc.val$iFc, iFlim)

  Q.test <- "valid"
  T2.test <- "valid"
  Q.T2.test <- "valid"
  LOOP.test <- "valid"
  iF.test <- "valid"

  spc.val$result <- "valid"

  if( spc.val$Qx >= spc.val$Qlim ) Q.test <- "warning"
  if( spc.val$T2x >= spc.val$T2lim ) T2.test <- "warning"
  if( spc.val$Qx >= spc.val$Qlim & spc.val$T2x >= spc.val$T2lim ) Q.T2.test <- "alarm"
  # if(!is.na(spc.val$LOOPlim)) if( spc.val$LOOPx >= spc.val$LOOPlim ) LOOP.test <- "warning"
  # if( spc.val$iFx >= spc.val$iFlim ) iF.test <- "warning"

  spc.val$test <- factor(c(Q.test, T2.test, Q.T2.test
                           # , LOOP.test, iF.test
                           ), levels = c("valid", "warning", "alarm"))

  if(table(spc.val$test)[ 2 ] >= 1) spc.val$result <- "warning"
  if(table(spc.val$test)[ 3 ] > 0) spc.val$result <- "warning"
  if(table(spc.val$test)[ 1 ] < 2) spc.val$result <- "warning"
  if(table(spc.val$test)[ 1 ] < 2 & table(spc.val$test)[ 3 ] > 0) spc.val$result <- "alarm"

  return(spc.val$result)
}
