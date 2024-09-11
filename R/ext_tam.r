ext_tam = function(resobj, pp=NULL, debug=TRUE){

  if (debug) message("Entering TAM-extractor...")

  D = dim(resobj$B)[3]
  dims_list = vector(mode="list", length=D) 
  names(dims_list) = paste0("F",1:D)

  if (is.character(pp) && pp=="auto") {
     pp = cbind(resobj$person[,seq(6,ncol(resobj$person),2)]) # TODO specify global pp
  } else {
     #stopifnot(inherits(pp,"..."))
  }


  for(d in 1:D){
    all_alphas = resobj$B[,2,d]
    valid_itm  = all_alphas != 0
    alpha      = all_alphas[valid_itm]
    dd         = rbind(apply(resobj$AXsi_[valid_itm,], 1, diff))
    TT         = sweep(dd, 2, alpha, FUN="/")

#   colnames(TT) = rownames(resobj$item) # upd 2023-07-26: no rownames with 3PL
    colnames(TT) = resobj$item$item[resobj$item[,4+resobj$maxK-1+d] != 0] # extract names for the valid dimension.  2024-04-06

    if(is.null(resobj$guess)) gg = rep(0, ncol(TT))
    else                      gg = resobj$guess
    
    dims_list[[d]]$thresholds     = TT
    dims_list[[d]]$discrimination = rbind(alpha)        # check for NRM
    dims_list[[d]]$guessing       = gg
    dims_list[[d]]$laziness       = rep(1, ncol(TT))
    dims_list[[d]]$model          = rep(resobj$irtmodel, ncol(TT))
    dims_list[[d]]$person_par     = pp[,d]

  } # end for d in D

  dims_list$freq = apply(resobj$resp,2,table,simplify=FALSE)
  data = resobj$resp

  attr(dims_list,"source") = paste0("TAM/",resobj$irtmodel)
  attr(dims_list,"data")   = data

  if (debug) message("Leaving TAM-extractor...")

  return(dims_list)
}# end of ext_tam


