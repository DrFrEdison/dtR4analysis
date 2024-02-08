validation.set <- function( validation.vector
                            , z = NA
                            , i = NA
                            , pattern = c("valid", "warning")
                            , filter.size = 100
                            , filter.length = round( log( filter.size), 0)
                            , spc = sds$sds$spc.list
                            , ppp = sds$sds$ppp
                            , lambda = sds$para$lambda
                            , lambda.choose = sample(lambda, 3)
                            , lambda.min.max = 3){

  pca.val <- list()

  if( z > 1)  validation.vector <- validation.vector[ 1 : z]
  if( z > 1)  validation.vector[[ z ]] <- validation.vector[[ z ]][ 1 : i]

  pca.val$validation.vector <- lapply(validation.vector, function( x ) which( x %in% pattern))
  # if( length( pca.val$validation.vector ) > filter.size){
  #
  #   pca.val$validation.vector <- sample(pca.val$validation.vector, size = filter.size)
  #
  # }

  pca.val$sub <- list()
  for(v in 1 : length( pca.val$validation.vector )){

    if( length( pca.val$validation.vector[[ v ]]) < 10) next

    pca.val$sub[[ v ]] <- list()
    pca.val$sub[[ v ]]$lambda.choose <- list()
    pca.val$sub[[ v ]]$lambda.choose.min <- list()
    pca.val$sub[[ v ]]$lambda.choose.max <- list()

    for(l in lambda.choose){

      pca.val$sub[[ v ]]$lambda.choose[[ which(lambda.choose %in% l) ]] <- order( unlist( spc[[ v ]][ pca.val$validation.vector[[ v ]] , grep(paste0("X", l), names( spc[[ v ]] )), with = F] ))

      pca.val$sub[[ v ]]$lambda.choose.min[[ which(lambda.choose %in% l) ]] <- pca.val$sub[[ v ]]$lambda.choose[[ which(lambda.choose %in% l) ]][ 1 : lambda.min.max ]
      pca.val$sub[[ v ]]$lambda.choose.max[[ which(lambda.choose %in% l) ]] <- pca.val$sub[[ v ]]$lambda.choose[[ which(lambda.choose %in% l) ]][ (length( pca.val$sub[[ v ]]$lambda.choose[[ which(lambda.choose %in% l) ]] ) - (lambda.min.max - 1)) : length( pca.val$sub[[ v ]]$lambda.choose[[ which(lambda.choose %in% l) ]] )]

    }
  }

  pca.val$validation.vector <- lapply(pca.val$sub, function( x ) c(unique(unlist( x$lambda.choose.min)), unique(unlist( x$lambda.choose.max))))

  pca.val$validation.vector <- mapply( function(spc, pca) as.character(spc$datetime)[ pca ]
          , pca = pca.val$validation.vector[ 1:length(validation.vector)]
          , spc = spc[ 1:length(validation.vector)]
          , SIMPLIFY = F)

  if( length( unlist(lapply(pca.val$validation.vector, as.character)) ) > filter.size ){
    pca.val$validation.vector <- sample(unlist(lapply(pca.val$validation.vector, as.character)) , filter.size)
  } else{ pca.val$validation.vector <- unlist(lapply(pca.val$validation.vector, as.character)) }

  return( pca.val$validation.vector )
}
