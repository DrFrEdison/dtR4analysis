plot_plotly_spc <- function(transfer_csv
                            , derivative = "spc"
                            , colp_dat = NA
                            , col_type = "diff"
                            , colp = r4dt::colp
                            , range_x = NA
                            , range_y = NA
                            , plot_name = NA
                            , immediately = T
                            , export_html = F
                            , legend_name = colp_dat){

  library(hyperSpec)
  if(is.na(plot_name)) plot_name <- paste0( date.dt(), "_", derivative, "_plot")

  data_to_plot <- transfer_csv$data
  if( !is.data.table(data_to_plot)) data.table( data_to_plot )

  if(!is.na(colp_dat)){
    factor_v <- factor( as.character( unlist( data_to_plot[ , which( names( data_to_plot) == colp_dat), with = F] )))

    if(col_type == "gradient") factor_v <- factor( as.character( unlist( data_to_plot[ , which( names( data_to_plot) == colp_dat), with = F] ))
                                                   , levels = unique( as.character( unlist( data_to_plot[ , which( names( data_to_plot) == colp_dat), with = F] ))), ordered = T)
    factor_l <- levels(factor_v)

    if(col_type == "diff") colp <- colp[1:length(factor_l)]
    if(col_type == "gradient"){
      colfun <- colorRampPalette(c("red", "green", "blue"))
      colp <- colfun(length(factor_l))
    }

    colp2 <- colp[factor_v]
  }

  if(is.na(colp_dat)) colp <- "blue"
  if(is.na(colp_dat)) colp2 <- rep("blue",nrow(data_to_plot))

  if(derivative == "spc")  spc_to_plot <- transfer_csv$spc
  if(derivative == "1st")  spc_to_plot <- transfer_csv$spc1st
  if(derivative == "2nd")  spc_to_plot <- transfer_csv$spc2nd

  if(any(!is.na(range_y))) range = c(range_y[1], range_y[2])
  if(any(is.na(range_y))) range = c(round( ifelse( range( unlist( spc_to_plot))[ 1 ] < 0
                                                   , range( unlist( spc_to_plot))[ 1 ] * 1.025
                                                   , range( unlist( spc_to_plot))[ 1 ] * 0.975), 1)
                                    , round( ifelse( range( unlist( spc_to_plot))[ 2 ] < 0
                                                     , range( unlist( spc_to_plot))[ 2 ] * 0.975
                                                     , range( unlist( spc_to_plot))[ 2 ] * 1.025), 1))

  round( ifelse( range( unlist( spc_to_plot))[ 1 ] < 0, range( unlist( spc_to_plot))[ 1 ] * 1.025, range( unlist( spc_to_plot))[ 1 ] * 0.975), 1)

  range(unlist(spc_to_plot))[1] * 1.025

  if(any(!is.na(range_x))) range_xx = c(range_x[1], range_x[2])
  if(any(is.na(range_x))) range_xx = c(range(transfer_csv$wl)[1], range(transfer_csv$wl)[2])

  data_to_hover <- list()
  for(j in 1 : nrow( data_to_plot)){

    data_to_hover[[ j ]] <- list()

    for( i in 1 : ncol( data_to_plot)){
      data_to_hover[[ j ]][[ i ]] <- paste0( "<br>", names( data_to_plot)[ i ], " = ", as.character( data_to_plot[ j , i, with = F]))
    }
  }

  for( j in 1 : nrow( data_to_plot)) data_to_hover[[ j ]] <- paste( lapply(data_to_hover[[ j ]], paste ), collapse = "")
  for( j in 1 : nrow( data_to_plot)) data_to_hover[[ j ]] <- substr( data_to_hover[[ j ]], 5, nchar( data_to_hover[[ j ]]))

  plotlydat <- list()
  plotlydat$au <- list(yp = list( title = "AU"
                                  , showline = T
                                  , showgrid = F
                                  , mirror = T
                                  , range = range
                                  , ticks="outside"),
                       xp = list( title = "lambda/nm"
                                  , showline = T
                                  , showgrid = F
                                  , mirror = T
                                  , ticks = "outside"
                                  , range = range_xx))
  plotly_au <- plot_ly( type = "scatter"
                        , mode = "lines") %>% layout( yaxis = plotlydat$au$yp
                                                      , xaxis = plotlydat$au$xp
                                                      , font = list(size=plotlydat$sizep))

  for(i in 1:nrow(data_to_plot)){ plotly_au <- plotly_au %>% add_trace(x = transfer_csv$wl
                                                                       , y = as.numeric(spc_to_plot[i,])
                                                                       , line = list(color=colp2[i])
                                                                       , name = ifelse( !is.na( legend_name )
                                                                                        , as.character( unlist(data_to_plot[i , which( names( data_to_plot) == colp_dat), with = F]))
                                                                                        , data_to_plot[i , 1, with = F])
                                                                       , text= data_to_hover[[ i ]]
  )
  }
  if(export_html==T) htmlwidgets::saveWidget(as_widget(plotly_au),paste0(plot_name,".html"))
  if(immediately==T) return(plotly_au)
}

