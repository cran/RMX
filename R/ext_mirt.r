ext_mirt = function(resobj, pp=NULL, defined=TRUE, debug=TRUE){
 
  if (debug) message("Entering mirt-extractor...")


  if(!inherits(resobj,"SingleGroupClass")) stop(paste("Class",class(resobj),"is incompatible with PIccc!"))
  
  itmodel = mirt::extract.mirt(resobj, what="itemtype")                         # which model?
  allcoef = mirt::coef(resobj)                                                        # list of length items+1

  inum = length(allcoef)-1
  inam = mirt::extract.mirt(resobj, what="itemnames")

  dnum = mirt::extract.mirt(resobj, what="nfact")                               # get dimensions number
  dnam = mirt::extract.mirt(resobj, what="factorNames")                         # get dimensions names

  K    = mirt::extract.mirt(resobj, what="K")                                   # get max categories
  tnum = ifelse(any(itmodel == "nominal"), max(K), max(K)-1)                    # num. of thresholds
  arow = ifelse(any(itmodel == "nominal"), max(K), 1  )                         # row num for alpha array

  dims_list = vector(mode="list", length=dnum)                                  # final list
  names(dims_list) <- dnam
 


  if (is.character(pp) && pp=="auto") {
     #if (dnum==1) pp = mirt::fscores(resobj) else pp = fscores(resobj,QMC=TRUE)
         pp = mirt::fscores(resobj,QMC=dnum>1)
  } else if (!is.null(attr(pp,"pp"))) {
         pp = attr(pp,"pp")
         stopifnot("Please use the result object from mirt::fscores()." = is.matrix(pp))
  } else if (is.character(pp) && pp == "skip") {                       # 'skip' for RMX::RMD!
         pp = rbind(rep("skip",dnum))
  } else {
         stopifnot("Please use the result object from mirt::fscores()." = is.matrix(pp))
  }

  modset1 = c("Rasch", "2PL", "3PL", "3PLu", "4PL")
  modset2 = c("PCM", "gpcm", "nominal")
  modset3 = c("gpcmIRT", "rsm", "grsmIRT")
  modset4 =   "graded"
  
  # create container matrices for parameters

  alpha = array(NA, dim=c(arow, inum, dnum))
  thrsh = matrix(NA, tnum, inum)
  guess = rep(NA, inum)
  upper = rep(NA, inum)

  colnames(thrsh) = inam
  
  for(d in 1:dnum){
    for(i in 1:inum){

      tmp_i = rbind(allcoef[[i]])[1,]  # 2023-07-03: ignore SE; thx CG
      alpha[,i,d] = tmp_i[d]

          if(itmodel[i] == "Rasch" & length(tmp_i) == dnum+3){  # simple Rasch
            
             tt = -tmp_i[dnum+1]
 
          } else if(itmodel[i] %in% modset1[-1]){               # 2/3/4PL
        
             tt =  -tmp_i[dnum+1]/alpha[,i,d]
       
          } else if( (itmodel[i] == "Rasch" & length(tmp_i) > dnum+3) | itmodel[i] == "gpcm" ){ # PCM, GPCM
        
             tt = -diff(tmp_i[(dnum+K[i]+1):(dnum+2*K[i])])/alpha[,i,d]
    
          } else if(itmodel[[i]] == "nominal"){
       
             ak = tmp_i[(dnum+1):(dnum+K[i])]
             aa = ak*tmp_i[d] - mean(ak*tmp_i[d])
             if(dim(alpha)[1] > length(aa)) aa[dim(alpha)[1]] = NA # fill with NA

             dd = tmp_i[(dnum+1+K[i]) : (dnum+2*K[i])]
             tt = dd-mean(dd, na.rm = TRUE)
             alpha[,i,d] = aa

          } else if(itmodel[i] == "grsmIRT"){ # !!! grsm
            
             b  = tmp_i[(dnum+1):(dnum+max(K)-1)]  
             c  = tmp_i[length(tmp_i)]
             tt = -(b+c)
         
          } else if(itmodel[i] == "rsm"){
           
             b  = tmp_i[(dnum+1):(dnum+max(K)-1)]  
             c  = tmp_i[length(tmp_i)]
             tt = b-c
       
          } else if(itmodel[i] == "gpcmIRT"){
         
             tt = tmp_i[(dnum+1):(dnum+max(K)-1)]    # check: max(K) ???

          } else if(itmodel[i] == "graded"){
          
             dd = tmp_i[(dnum+1):(length(tmp_i))]  
             tt = -dd/alpha[,i,d]
          
          } else {
             warning(paste("Model: ", itmodel[i], "not yet implemented and will be ignored in the output!"))
          }
      if(itmodel[i] %in% c(modset1, modset2, modset3, modset4)){
      
        if(itmodel[i] %in% c("2PL", "3PL", "3PLu", "4PL")){
          guess[ i] =  tmp_i[dnum+2]
          upper[ i] =  tmp_i[dnum+3]
        }else{
          guess[ i] = 0
          upper[ i] = 1
        }
        
        if(length(tt) < nrow(thrsh)) tt = c(tt,rep(NA, nrow(thrsh)-length(tt)))
        thrsh[,i] = tt
        
      } # do not save previous calculations for undefined model
    } # end of iteration through items
    
  # remove undefined Items
    
  rm_check = rep(NA, inum)
  for(i in 1:inum) rm_check[i] = (sum(is.na(thrsh[,i])) != nrow(thrsh)) # thrsh and alpha have same dimensions
  if(sum(rm_check)==0) stop("No valid items were left for analysis!", call. = FALSE)
  
  dims_list[[d]]$thresholds     = thrsh         [,rm_check   ,drop=FALSE]
  dims_list[[d]]$discrimination = rbind(alpha   [,rm_check, d])
  dims_list[[d]]$guessing       = guess         [ rm_check]
  dims_list[[d]]$laziness       = upper         [ rm_check]
  dims_list[[d]]$model          = itmodel       [ rm_check]
  dims_list[[d]]$person_par     = pp[ ,d]
  
  if(defined){
    
    dd = dims_list[[d]]$discrimination != 0       # identify confirmatory models: a=0!
    td = apply(dd,2,all,na.rm=TRUE)
    
    dims_list[[d]]$thresholds     = dims_list[[d]]$thresholds    [,td, drop=FALSE]
    dims_list[[d]]$discrimination = dims_list[[d]]$discrimination[,td, drop=FALSE]
    dims_list[[d]]$guessing       = guess  [td]
    dims_list[[d]]$laziness       = upper  [td]
    dims_list[[d]]$model          = itmodel[td]
    dims_list[[d]]$person_par     = pp[ ,d]
    
  } # end of defined check
  
 } # end of iteration through dimensions
  
  dims_list$freq = apply(resobj@Data$data,2,table,simplify=FALSE)
  data = resobj@Data$data

  attr(dims_list,"source") = paste0("mirt/",unique(itmodel[ rm_check]))
  attr(dims_list,"pp")     = pp
  attr(dims_list,"data")   = data

  if (debug) message("Leaving mirt-extractor...")

  return(dims_list)
  
} # end of mirt_Extractor
