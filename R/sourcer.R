#==========================================================================================#
#==========================================================================================#
#' Sourcer is a function to source the necessary files in the main plottin script
#' @rdname sourcer
#' @usage sourcer(source.dir)
#' @description Sourcer is a function to source the necessary files in the main plottin script
#' @param source.dir directory to source
#' @export
#'
sourcer = function (source.dir){
  #------------------------------------------------------------------------------------------#
  #     Organise the files so we load them in the right order.                               #
  #------------------------------------------------------------------------------------------#
  at.first      = c("rconstants.r","globdims.r","unitlist.r")
  at.end        = "pft.coms.r"
  myself        = c("plot.all.manfredo.R", "sourcer.R")
  others        = "Bci_histogram.R"
  all.scripts   = sort(list.files(path=source.dir,pattern="\\.[Rr]$"))
  back.up       = sort(list.files(path=source.dir,pattern="^[~]"))
  keep          = ! ( all.scripts %in% at.first
                      | all.scripts %in% at.end
                      | all.scripts %in% back.up
                      | all.scripts %in% myself
                      | all.scripts %in% others
  )#end
  middle        = all.scripts[keep]
  order.scripts = c(at.first,middle,at.end)
  nscripts      = length(order.scripts)
  #------------------------------------------------------------------------------------------#


  #------------------------------------------------------------------------------------------#
  #     Load all files, in order.  Here we replace the warnings by errors, just to make      #
  #     sure that all the functions are clean.                                               #
  #------------------------------------------------------------------------------------------#
  warn.orig = getOption("warn")
  options(warn=2)
  cat(" + Loading scripts from ",source.dir,"...","\n")
  for (iscript in sequence(nscripts)){
    script.now  = order.scripts[iscript]
    full        = file.path(source.dir,script.now)
    isok        = try(source(full),silent=TRUE)
    if ("try-error" %in% is(isok)){
      options(warn=warn.orig)
      cat("   - Script ",script.now," has bugs!  Check the errors/warnings: ","\n")
      source(full)
      stop("Source code problem")
    }#end if
    else{cat(" Script", script.now, "sourced", "\n")}
  }#end for
  options(warn=warn.orig)
  #------------------------------------------------------------------------------------------#
}
