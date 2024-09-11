decider = function(resobj,pp=NULL,debug=TRUE){

  if (debug) message("Entering decider...")

  if (inherits(resobj,c("dRm", "Rm", "eRm"))){
    
    if (!requireNamespace("eRm")) stop("Please install the eRm package.")
    extracted_obj = ext_erm(resobj, pp=pp, debug=debug)
    
  } else if(inherits(resobj,c("rasch", "ltm", "tpm", "gpcm", "grm"))){
    
    if (!requireNamespace("ltm")) stop("Please install the ltm package.")
    extracted_obj = ext_ltm(resobj, pp=pp, debug=debug)
    
  } else if(inherits(resobj,c("raschmodel", "nplmodel", "pcmodel", "rsmodel", "gpcmodel"))){
    
    if (!requireNamespace("psychotools")) stop("Please install the psychotools package.")
    extracted_obj = ext_psy(resobj, pp=pp, debug=debug)
    
  } else if(!is.null(attributes(class(resobj))$package)){
    
    if(attributes(class(resobj))$package == "mirt"){
      if (!requireNamespace("mirt")) stop("Please install the mirt package.")
      extracted_obj = ext_mirt(resobj, pp=pp, debug=debug)
    }
    
  } else if(inherits(resobj,c("tam.mml", "tam.mml.2pl", "tam.mml.3pl"))){

    if (!requireNamespace("TAM")) stop("Please install the TAM package.")
    extracted_obj = ext_tam(resobj, pp=pp, debug=debug)
    
  } else stop("Given result object is not supported by PIccc!")
  
  if (debug) message("Leaving decider...")

  return(extracted_obj)
  
} # end of decider fun
