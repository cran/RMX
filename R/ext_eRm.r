ext_erm = function(resobj, pp=NULL, debug=TRUE){

  if (debug) message("Entering eRm-extractor...")

       if (length(grep("RM" ,resobj$call))>0) model="RM"
  else if (length(grep("RSM",resobj$call))>0) model="RSM"
  else if (length(grep("PCM",resobj$call))>0) model="PCM"
  else stop("Only RM/PCM/RSM supported for the eRm package.")

  if (is.character(pp) && pp=="auto") {
         pp = eRm::person.parameter(resobj)             # get person parameter from eRm
  } else if (!is.null(attr(pp,"pp"))) {
         pp = attr(pp,"pp")
         stopifnot("Please use the result object from eRm::person.parameter()." = inherits(pp,"ppar"))
  } else if (is.character(pp) && pp != "skip") {                       # 'skip' for RMX::RMD!
         stopifnot("Please use the result object from eRm::person.parameter()." = inherits(pp,"ppar"))
  }

  if (model == "RM") {
      thrsh = rbind(-resobj$betapar)
  } else {
      thrsh = t(eRm::thresholds(resobj)$threshtable[[1]][,-1])
  }


  rownames(thrsh) = paste0("T", 1:nrow(thrsh))
  dims_list = vector(mode="list", length=1)        # final list
  names(dims_list) <- "F1"
  model = rep(model, ncol(thrsh))                  # special distinction for PCM/RSM
  dims_list[[1]]$thresholds =      thrsh
  dims_list[[1]]$discrimination =  rbind(rep(1, ncol(thrsh)))
  dims_list[[1]]$guessing =        rep(0, ncol(thrsh))
  dims_list[[1]]$laziness =        rep(1, ncol(thrsh))
  dims_list[[1]]$model =           model
  if (is.list(pp)) {
      px = pp$theta.table[,1]                        # 2024-08-27 RWA
      names(px) = rownames(pp$theta.table)
      dims_list[[1]]$person_par =  px     # was: eRm:::coef.ppar(pp,extrapolated=TRUE)   # 2023-06-25
  } else {
      dims_list[[1]]$person_par =  "skip" # required for RMD
  }

  data = resobj$X
  dims_list$freq = apply(data,2,table,simplify=FALSE) # requires R > 4.1.0 (*)

  colnames(dims_list[[1]]$thresholds) = names(dims_list$freq) # check erm adds 'beta'
  
  attr(dims_list,"source") = paste0("eRm/",model[1])
  attr(dims_list,"pp")     = pp
  attr(dims_list,"data")   = data

  if (debug) message("Leaving eRm-extractor...")

  return(dims_list)
} # end of ext_erm
  # (*) apply received the simplify option as of 4.1.0

  #  eRm:::coef.ppar:
  #  x <- object$theta.table[, 1]
  #  if (!extrapolated) x[object$theta.table[, 3]] <- NA
  #  names(x) <- rownames(object$theta.table)