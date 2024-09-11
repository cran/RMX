ext_ltm = function(resobj, pp=NULL, debug=TRUE){

  if (debug) message("Entering ltm-extractor...")

  D=1
  parmat = coef(resobj)

  if (is.list(parmat)) {                                   # if unequal cats/item (PCM/GPCM)
      cmax = max(sapply(parmat,length))                    # max no. of pars/item
      atmp = sapply(parmat,function(x) x[length(x)])       # last is alpha
      ctmp = lapply(parmat,function(x) c(x[1:(length(x)-1)],rep(NA,cmax-length(x)))) # others are thresholds
      parmat = t(rbind(as.data.frame(ctmp),alpha=atmp))
  }

  guessing = rep(0, nrow(parmat))
  laziness = rep(1, nrow(parmat))
  
  if(inherits(resobj,"rasch")){
     if(all(parmat[,2] == 1)) model = rep("Rasch", nrow(parmat))
     else                     model = rep("2PL",   nrow(parmat))  # 1PL explained in the BA docu
     tt = rbind(parmat[,1])   # thresholds
     dd = rbind(parmat[,2])   # discriminations
  }else if(inherits(resobj,"ltm")){
     if(ncol(parmat)==3) D=2
     model = rep("2PL", nrow(parmat))
     tt = rbind(parmat[,1])
     dd = rbind(parmat[,2:ncol(parmat)])
  }else if(inherits(resobj,"gpcm")){
     dd = rbind(parmat[,ncol(parmat)])
     if(all(dd == 1)) model = rep("PCM",  nrow(parmat))
     else             model = rep("gpcm", nrow(parmat))
     tt = t(parmat[,-ncol(parmat)])      # 23-04-12: changed from -parmat
  }else if(inherits(resobj,"grm")){
     model = rep("graded",nrow(parmat))
     dd = rbind(parmat[, ncol(parmat)])
     tt =     t(parmat[,-ncol(parmat)])
  }else  if(inherits(resobj,"tpm")){
     model = rep("3PL", nrow(parmat))
     guessing = parmat[,1]
     tt = rbind(parmat[,2])
     dd = rbind(parmat[,ncol(parmat)])
  }else stop(paste("Unknown class of input object:",class(resobj)))
  
  dims_list = vector(mode="list", length=D)
  names(dims_list) = paste0("F", 1:D)

  if (is.character(pp) && pp=="auto") {
         pp=ltm::factor.scores(resobj, resp.patterns=resobj$X)
  } else if (!is.null(attr(pp,"pp"))) {
         pp = attr(pp,"pp")
         stopifnot("Please use the result object from ltm::factor.scores()." = inherits(pp,"fscores"))
  } else if (is.character(pp) && pp != "skip") {                       # 'skip' for RMX::RMD!
         stopifnot("Please use the result object from ltm::factor.scores()." = inherits(pp,"fscores"))
  }



  pnam = paste0("z", 1:D)
  if (is.list(pp)) {
      pp_only = pp$score.dat[,pnam,drop=FALSE]  # not 'skip'! 2024-04-17
  } else {
      pp_only = rbind(rep("skip",D))
  }
  
  if(resobj$IRT.param) TT = tt
  else{
    for (d in 1:D) TT = -t(t(tt)/dd[,d])     # 23-04-12: -tt
    if(inherits(resobj,"grm")) TT = -TT    # GRM is special...
  }

  for(d in 1:D){
      dims_list[[d]]$thresholds       = TT
      dims_list[[d]]$discrimination   = dd[d,,drop=FALSE]
      dims_list[[d]]$guessing         = guessing
      dims_list[[d]]$laziness         = laziness
      dims_list[[d]]$model            = model
      dims_list[[d]]$person_par       = pp_only[,d]
  } # end for d

  data = resobj$X
  attr(dims_list,"source") = paste0("ltm/",model[1])
  attr(dims_list,"pp")     = pp
  attr(dims_list,"data")   = data

  dims_list$freq = apply(resobj$X,2,table,simplify=FALSE)

  if (debug) message("Leaving ltm-extractor...")

  return(dims_list)
  
}

