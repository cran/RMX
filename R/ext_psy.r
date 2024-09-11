ext_psy = function(resobj, pp=NULL, debug=TRUE){
  
  if (debug) message("Entering psychotools-extractor...")

  tt = psychotools::threshpar(resobj)
  inum = length(tt)
  ncat = sapply(tt, length)
  mseq = seq_len(max(ncat))
  
  if (is.character(pp) && pp=="auto") {
     pp = psychotools::personpar(resobj, personwise=TRUE)
  } else if (!is.null(attr(pp,"pp"))) {
         pp = attr(pp,"pp")
         stopifnot("Please use the result object from psychotools::personpar()." = inherits(pp,"personpar"))
  } else if (is.character(pp) && pp != "skip") {                       # 'skip' for RMX::RMD!
         stopifnot("Please use the result object from psychotools::personpar()." = inherits(pp,"personpar"))
  }

  dims_list = vector(mode="list", length=1)
  names(dims_list) = "F1"
  dims_list[[1]]$thresholds     = rbind(sapply(tt, "[", i = mseq))
  dims_list[[1]]$discrimination = rbind(psychotools::discrpar(resobj))
  dims_list[[1]]$guessing       =       psychotools::guesspar(resobj)
  dims_list[[1]]$laziness       =       psychotools::upperpar(resobj)
  dims_list[[1]]$model          = rep(attributes(tt)$model, inum)
  dims_list[[1]]$person_par     = pp[!is.na(pp)]
  
  colnames(dims_list[[1]]$thresholds) = colnames(dims_list[[1]]$discrimination)
  
  dims_list$freq = apply(resobj$data,2,table,simplify=FALSE)
  data = resobj$data

  attr(dims_list,"source") = paste0("psy/",attributes(tt)$model)
  attr(dims_list,"pp")     = pp
  attr(dims_list,"data")   = data
  
  if (debug) message("Leaving psychotools-extractor...")

  return(dims_list)
} # end of ext_psy