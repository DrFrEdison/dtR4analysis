drop_spc <- function(path = dirname(rstudioapi::getSourceEditorContext()$path), recursive = T, plotlyplot = F){
  writespc <- list()

  writespc$export <- c("ref", "spc", "trans", "drk")
  writespc$spc_daten <-  read_spc_files(directory = path
                                        , baseline = writespc$baseline <- NA
                                        , pngplot = F
                                        , plotlyplot = plotlyplot
                                        , recursive = recursive
                                        , filestext = NA
                                        , colp = NA)

  names(writespc$spc_daten)[ grep("au", names(writespc$spc_daten)) ] <- "spc"

  setwd(path)
  for(i in 1:length(writespc$export)){
    if(length(writespc$spc_daten[[grep(writespc$export[i], names(writespc$spc_daten))]]) == 0) writespc$export[i] <- NA
    if(is.na(writespc$export[i])) next

    namep <- paste0(date.dt(), "_", writespc$export[i])
    if(all(writespc$export[i] == "spc", !is.na(writespc$baseline))) namep <- paste0(date.dt(), "_", writespc$export[i], "_bl")

    write_spc_files(spc_file = writespc$spc_daten[[grep(writespc$export[i], names(writespc$spc_daten))]]
                    , baseline = all(writespc$export[i] == "spc", !is.na(writespc$baseline))
                    , write = T
                    , filename = namep
                    , return_R = F)
  }
}
