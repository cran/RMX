cleaner = function(dlist, isel=NULL, ilab=NULL, dsel=NULL, dlab=NULL, csel=NULL,
                   how_sort="none", tot_sort=FALSE, theta=seq(-6,6,0.1),highlight=NULL,
                   #ngroups=NULL,
                   debug=TRUE){
  
  drange = function(x,na.rm=TRUE) diff(range(x,na.rm=na.rm))

  if (debug) message("Entering cleaner...")
  
  data = attributes(dlist)$data


  if (is.null(ilab)) {
         ilab = names(dlist$freq)
         attr(ilab, "was.null") = TRUE
  } else {
        attr(ilab, "was.null") = FALSE
        if (length(ilab) > length(dlist$freq)) {
          stop(paste("Too many item labels:",length(ilab),"labels for",length(dlist$freq),"items."))
        } else if (length(ilab) < length(dlist$freq)) {
          stop(paste("Too few item labels:",length(ilab),"labels for",length(dlist$freq),"items."))
        } else if (length(unique(ilab)) < length(ilab)) {
          print(sort(table(ilab)))
          stop(paste("Non-unique item labels."))
        } # end else
  } # end else (not is.null)

  if (!attributes(ilab)$was.null) {
      if ((length(dlist)-1) > 1) {
        stop("Item names cannot be modified for multi-dimensional models.\nYou might want to consider renaming of original data.")
      } else {
        colnames(dlist[[1]]$thresholds)     = ilab
        colnames(dlist[[1]]$discrimination) = ilab
           names(dlist[[1]]$guessing)       = ilab
           names(dlist[[1]]$laziness)       = ilab
           names(dlist[[1]]$model)          = ilab
           names(dlist$freq)                = ilab
        colnames(data)                      = ilab
      } # end else = rename
  } # end rename required

  freq = dlist$freq  # temporarily "outsourced"
  dlist$freq = NULL  # so that length of dlist = no. of dims.!

  if (all(is.null(isel),is.null(dsel),is.null(dlab)))  {  # no user changes

    dlist_sel_itm = dlist
    isel = unlist(lapply(sapply(dlist,"[","thresholds"),colnames))

  } else {     # any changes (isel, dsel, dlab)
    
    # --- 0. Dimension and item labels

    if     (is.null(dlab))                   dlab = names(dlist)
    else if(length(dlab) != length(dlist))   stop(paste0("Invalid length of dlab. \nNumber of labels must equal number of dimensions.\nLabels: ", length(dlab), ", dimensions:", length(dlist), "."),call.=FALSE)
    else if(!is.character(dlab))             dlab = as.character(dlab) 
    
    names(dlist) = dlab
    
    # --- 1. Select dimensions

    if (is.null(dsel)) {

      dlist_sel_dim = dlist

    }else{        # execute dsels
      
      if(anyNA(dsel))           stop("Invalid Input for dsel: Missing values (NA) are not accepted." )
      if(any(duplicated(dsel))) stop("Invalid Input for dsel: Same dimension was selected multiple times.")
      
      if(is.character(dsel)){
        if(all(dsel %in% dlab)){

#         dlist_sel_dim = dlist[dlab]
          dlist_sel_dim = dlist[dsel]  # RWA 23-06-19
          
        }else stop(paste0("Dimension selection did not work: Non-matching dsel values \ndsel: ", dsel, "\ndimension labels: ", dlab, "\n"))
        
      }else if(is.numeric(dsel)){
        
        if(!all(is.int(dsel)))        stop("Invalid Input for dsel: All values need to be integers.", call. = FALSE)
        if(any(dsel <= 0))            stop("Invalid Input for dsel: Only positive integers allowed.", call. = FALSE)
        if(any(dsel > length(dlist))) stop(paste0("Invalid Input: dsel ",dsel," exceeds total number of dimensions (",length(dlist),")"))
        
        dlist_sel_dim = dlist[dsel]
#        isel = unlist(lapply(sapply(dlist_sel_dim,"[","thresholds"),colnames))
        
      } # end of if input was numeric
      
    } # end if/else dimension selection
    
    # --- 2. Select items (selection string character)

    if(is.null(isel)){

       dlist_sel_itm = dlist_sel_dim  # no item selection: is.null(isel)
       isel = unlist(lapply(sapply(dlist_sel_dim,"[","thresholds"),colnames))

    } else {  # execute isels

      dlist_sel_itm = vector(mode="list", length=length(dlist_sel_dim)) 
      names(dlist_sel_itm) = names(dlist_sel_dim)
      
      if(anyNA(isel) != 0)        stop("Invalid Input for isel! Missing values (NA) are not accepted." )
      if(anyDuplicated(isel)!= 0) stop("Invalid Input for isel! Same Items were selected multiple times.")
      if(is.numeric(isel)) isel = ilab[isel]    # convert numeric indices to label strings

      for(d in seq_along(dlist_sel_dim)){

          i_lab = colnames(dlist_sel_dim[[d]]$thresholds)
          isitm = i_lab %in% isel

          if (sum(isitm)==0) {
               warning(paste0("\nItem '", isel[!(isel %in% i_lab)], "' not found in dimension ", d, " (", dlab[d], ")." ), call. = FALSE)
               dlist_sel_itm[[d]] = NA  # not NULL here -- loop counter d!
          }else{
               dlist_sel_itm[[d]]$thresholds     = dlist_sel_dim[[d]]$thresholds    [,isitm, drop=FALSE]
               dlist_sel_itm[[d]]$discrimination = dlist_sel_dim[[d]]$discrimination[,isitm, drop=FALSE]
               dlist_sel_itm[[d]]$guessing       = dlist_sel_dim[[d]]$guessing      [ isitm ]
               dlist_sel_itm[[d]]$laziness       = dlist_sel_dim[[d]]$laziness      [ isitm ]
               dlist_sel_itm[[d]]$model          = dlist_sel_dim[[d]]$model         [ isitm ]
               dlist_sel_itm[[d]]$person_par     = dlist_sel_dim[[d]]$person_par
          }# end of if/else isitm valid
          
          

      } # for d

    } # end if/else isel=NULL

  } # end of if/else all _sel=NULL
  

  dlist_sel_itm[is.na(dlist_sel_itm)] = NULL # eliminate unused dims
  freq_sel = freq[isel]
  data = data[,isel,drop=FALSE]

  # --- sorting
  ks = unlist(lapply(lapply(dlist_sel_itm,"[[","thresholds"),ncol)) # items/dim
  sortvec = 1:sum(ks)
  sortopt = c("mean","median","var","min","max","range","disc","guess","lazy", "none")
  sorterr = paste0("Invalid sorting option '",how_sort,"'.\n  Use one out of: ",paste(sortopt,collapse="/"),".\n  No sorting will be performed.")

  if (length(how_sort) == 1 & how_sort != "none") { # any sorting wishes?

      if (how_sort %in% sortopt) {
           D0 = length(dlist_sel_itm)

           if(tot_sort){ # over all dimensions (total sort) ?

                tx = NULL
                dx = NULL
                gx = NULL
                lx = NULL
                for(d in 1:D0) {
                    tx = cbind(tx,dlist_sel_itm[[d]]$thresholds)
                    dx = cbind(dx,dlist_sel_itm[[d]]$discrimination)
                    gx =     c(gx,dlist_sel_itm[[d]]$guessing)
                    lx =     c(lx,dlist_sel_itm[[d]]$laziness)
                } # end for d
                sortvec = switch(how_sort,
                                 "mean"   = rank(colMeans(tx         ,na.rm=TRUE),ties.method="first"),
                                 "median" = rank(   apply(tx,2,median,na.rm=TRUE),ties.method="first"),
                                 "var"    = rank(   apply(tx,2,var   ,na.rm=TRUE),ties.method="first"),
                                 "min"    = rank(   apply(tx,2,min   ,na.rm=TRUE),ties.method="first"),
                                 "max"    = rank(   apply(tx,2,max   ,na.rm=TRUE),ties.method="first"),
                                 "range"  = rank(   apply(tx,2,drange,na.rm=TRUE),ties.method="first"),
                                 "disc"   = rank(colMeans(dx         ,na.rm=TRUE),ties.method="first"),
                                 "guess"  = rank(         gx                     ,ties.method="first"),
                                 "lazy"   = rank(         lx                     ,ties.method="first")
                                 ) # end switch
           }else {  # sort per dimension

                sortvec = NULL
                kk = c(0,cumsum(ks))
                for (d in 1:D0) {
                     sortmodel = unique(dlist_sel_itm[[d]]$model)
                     if (length(sortmodel) > 1) {

                       warning(paste0("Mixed model types currently not supported (",sortmodel,"). No sorting will be performed for dimension",d,".\n"))

                     } else { # only items of the same type present

                       if (how_sort=="disc"  & length(unique(as.vector(dlist_sel_itm[[d]]$discrimination)))==1)             message(       "Note: Sorting by discrimination parameter has no effect if all items have equal discrimination parameter values.\n")
                       if (how_sort=="guess" & length(unique(as.vector(dlist_sel_itm[[d]]$guessing)))==1)                   message(       "Note: Sorting by guessing parameter has no effect if all items have equal guessing parameter values.\n")
                       if (how_sort=="lazy"  & length(unique(as.vector(dlist_sel_itm[[d]]$laziness)))==1)                   message(       "Note: Sorting by laziness parameter has no effect if all items have equal laziness parameter values.\n")
                       if (sortmodel %in% c("Rasch","2PL","3PL","3PLu","4PL") & how_sort %in% c("var","min","max","range")) message(paste0("Note: Sorting by variance, min, max, and range will have no effect for model ",sortmodel,".\n"))

                       # TODO: adapt model names for eRm, ltm, psychotools, TAM

                       if (how_sort=="mean" )  sortvec = c(sortvec, kk[d] + rank(colMeans(dlist_sel_itm[[d]]$thresholds         ,na.rm=TRUE),ties.method="first"))
                       if (how_sort=="median") sortvec = c(sortvec, kk[d] + rank(   apply(dlist_sel_itm[[d]]$thresholds,2,median,na.rm=TRUE),ties.method="first"))
                       if (how_sort=="var"  )  sortvec = c(sortvec, kk[d] + rank(   apply(dlist_sel_itm[[d]]$thresholds,2,var   ,na.rm=TRUE),ties.method="first"))
                       if (how_sort=="min"  )  sortvec = c(sortvec, kk[d] + rank(   apply(dlist_sel_itm[[d]]$thresholds,2,min   ,na.rm=TRUE),ties.method="first"))
                       if (how_sort=="max"  )  sortvec = c(sortvec, kk[d] + rank(   apply(dlist_sel_itm[[d]]$thresholds,2,max   ,na.rm=TRUE),ties.method="first"))
                       if (how_sort=="range")  sortvec = c(sortvec, kk[d] + rank(   apply(dlist_sel_itm[[d]]$thresholds,2,drange,na.rm=TRUE),ties.method="first"))
                       if (how_sort=="disc" )  sortvec = c(sortvec, kk[d] + rank(colMeans(dlist_sel_itm[[d]]$discrimination     ,na.rm=TRUE),ties.method="first"))
                       if (how_sort=="guess")  sortvec = c(sortvec, kk[d] + rank(         dlist_sel_itm[[d]]$guessing                       ,ties.method="first"))
                       if (how_sort=="lazy" )  sortvec = c(sortvec, kk[d] + rank(         dlist_sel_itm[[d]]$laziness                       ,ties.method="first"))

                     } # end else (items of same type)

                       # TODO: GRM/dicho --> error
                       # TODO: NRM/dicho --> no sorting (poly?)
                       # TODO: variance  --> only polytomous!

                       # warning: sort result will not be visible

                } # end for d
           } # ende else (= sort per dim
      }else warning(sorterr) # end valid sort option
  } # end any sorting wishes

  dlist_final = dlist_sel_itm

  # --- calculate iif and prob

  dlist_final[is.na(dlist_final)] = NULL
  D = length(dlist_final)
  
  # --- calculate TIF for all (also not selected) items
  for(d in 1:D){

    # k = dim(dlist[[d]]$thresholds)[2]      MK 14.04.2024: use dlist_final (not dlist without selection)
    k  = dim(dlist_final[[d]]$thresholds)[2]  # final list with selected items
    kf = dim(dlist[[d]]$thresholds)[2]        # original
    
    test_info = matrix(NA,nrow=length(theta),ncol=k)
    test_info_all = matrix(NA,nrow=length(theta),ncol=kf)
    check = rep(NA, k) # for csel
    


#    dlist_final[[d]]$EMP = calc_obs(data=data, perspar=na.omit(dlist_final[[d]]$person_par), ngroups = ngroups) # defunct 2024-04-06
    
    # --- selected list for sTIF & sSE
    
    for(j in 1:k){
      if      (       dlist_final[[d]]$model[j]        == "nominal")        test_info[,j] = with(dlist_final[[d]], info_nom(diff =thresholds[,j], alpha=discrimination[,j], theta=theta))$iif
      else if (       dlist_final[[d]]$model[j] %in% c("graded","grsmIRT")) test_info[,j] = with(dlist_final[[d]], info_sam(thres=thresholds[,j], alpha=discrimination[,j], theta=theta))$iif
      else if (length(dlist_final[[d]]$thresholds[,j]) == 1)                test_info[,j] = with(dlist_final[[d]], info_4pl(thres=thresholds[,j], alpha=discrimination[,j], theta=theta, gamma=guessing[j], upper=laziness[j]))$iif
      else                                                                  test_info[,j] = with(dlist_final[[d]], info_mas (thres=thresholds[,j], alpha=discrimination[,j], theta=theta, gamma=guessing[j], graded=FALSE))$iif
    } # end of j
    
    # --- original list for TIF & SE
    
    for(j in 1:kf){
      if      (       dlist[[d]]$model[j]        == "nominal")              test_info_all[,j] = with(dlist[[d]], info_nom(diff =thresholds[,j], alpha=discrimination[,j], theta=theta))$iif
      else if (       dlist[[d]]$model[j] %in% c("graded","grsmIRT"))       test_info_all[,j] = with(dlist[[d]], info_sam(thres=thresholds[,j], alpha=discrimination[,j], theta=theta))$iif
      else if (length(dlist[[d]]$thresholds[,j]) == 1)                      test_info_all[,j] = with(dlist[[d]], info_4pl(thres=thresholds[,j], alpha=discrimination[,j], theta=theta, gamma=guessing[j], upper=laziness[j]))$iif
      else                                                                  test_info_all[,j] = with(dlist[[d]], info_mas(thres=thresholds[,j], alpha=discrimination[,j], theta=theta, gamma=guessing[j], graded=FALSE))$iif
    } # end of 

    dlist_final[[d]]$TIF = rowSums(test_info_all)
    dlist_final[[d]]$SE  = sqrt(1/rowSums(test_info_all))
    
    dlist_final[[d]]$sTIF = rowSums(test_info)
    dlist_final[[d]]$sSE  = sqrt(1/rowSums(test_info))
  } # end of d for all TIF

  for (d in 1:D) {
    
     k = dim(dlist_final[[d]]$thresholds)[2]
     test_info = matrix(NA,nrow=length(theta),ncol=k)
     
      
     for (j in 1:k) {
        if (dlist_final[[d]]$model[j] == "nominal") {

          prob = with(dlist_final[[d]], calc_nom(diff=thresholds[,j], alpha=discrimination[,j], theta=theta))
          dlist_final[[d]]$CCC[[j]] = prob$prob_ccc
          dlist_final[[d]]$TCC[[j]] = prob$prob_tcc

          temp = with(dlist_final[[d]], info_nom(diff=thresholds[,j], alpha=discrimination[,j], theta=theta))
          dlist_final[[d]]$CIF[[j]] = temp$cif
          dlist_final[[d]]$IIF[[j]] = matrix(temp$iif,length(theta),1)
          test_info[,j]             = temp$iif

        } else if (dlist_final[[d]]$model[j]%in% c("graded","grsmIRT")) {
          
          prob = with(dlist_final[[d]], calc_ccc(delta=thresholds[,j], alpha=discrimination[,j], gamma=guessing[j], graded=TRUE, theta=theta))
          dlist_final[[d]]$CCC[[j]] = prob$prob_ccc
          dlist_final[[d]]$TCC[[j]] = prob$prob_tcc
          
          temp = with(dlist_final[[d]], info_sam(thres=thresholds[,j], alpha=discrimination[,j], theta=theta))
          dlist_final[[d]]$CIF[[j]] = temp$cif
          dlist_final[[d]]$IIF[[j]] = matrix(temp$iif,length(theta),1)
          test_info[,j]             = temp$iif
          
        } else if(length(dlist_final[[d]]$thresholds[,j])==1){  # dichotom (for upper and gamma)
          
          prob = with(dlist_final[[d]], calc_ccc(delta=thresholds[,j], alpha=discrimination[,j], gamma=guessing[j], graded=FALSE, theta=theta))
          dlist_final[[d]]$CCC[[j]] = prob$prob_ccc
          dlist_final[[d]]$TCC[[j]] = prob$prob_tcc
          
          temp = with(dlist_final[[d]], info_4pl(thres=thresholds[,j], alpha=discrimination[,j], gamma=guessing[j], upper=laziness[j], theta=theta))
          dlist_final[[d]]$CIF[[j]] = temp$cif
          dlist_final[[d]]$IIF[[j]] = matrix(temp$iif,length(theta),1)
          test_info[,j]             = temp$iif
          
        }else{   # PCM, GPCM
          
          prob = with(dlist_final[[d]], calc_ccc(delta=thresholds[,j], alpha=discrimination[,j], gamma=guessing[j], graded=FALSE, theta=theta))
          dlist_final[[d]]$CCC[[j]] = prob$prob_ccc
          dlist_final[[d]]$TCC[[j]] = prob$prob_tcc
          
          temp = with(dlist_final[[d]], info_mas(thres=thresholds[,j], alpha=discrimination[,j], gamma=guessing[j], graded=FALSE, theta=theta))
          dlist_final[[d]]$CIF[[j]] = temp$cif
          dlist_final[[d]]$IIF[[j]] = matrix(temp$iif,length(theta),1)
          test_info[,j]             = temp$iif
          
        } # end else
    
       
       if(!is.null(csel)){
        
         stopifnot(is.numeric(csel))
         if(any(csel<=0)) stop("Invalid csel argument! Please use values >= 1.")
         
         
         iscat = csel > ncol(dlist_final[[d]]$CCC[[j]])   # has the user selected invalid categories (itemwise)
         check[j] = all(iscat)
         
         
         if (length(iscat) == 1){
           if(iscat){
             catsel=1:ncol(dlist_final[[d]]$CCC[[j]])
             warning(paste0("'csel' value is larger than the total numer of categories for item ", j, " on dimension ", d, ". The 'csel' argument will be ignored.\n"), call. = FALSE)
            }else catsel=csel
         } else if(length(iscat)  > 1) {
           catsel = csel[iscat==FALSE]  # if many take only the valid ones
           csel=catsel
         }
         if(length(catsel) == 0){
           catsel = 1:ncol(dlist_final[[d]]$CCC[[j]])
           csel   = 1:ncol(dlist_final[[d]]$CCC[[j]])
           warning(paste0("'csel' values are larger than the total numer of categories for item ", j, " on dimension ", d, ". The 'csel' argument will be ignored.\n"), call. = FALSE)
         } 
         
        if(j==k & all(check)) stop(paste0("Invalid 'csel' argument was defined for all items from dimension ", d, ". PIccc cannot be plotted. You may use 'dsel' to select only valid dimensions. \n"), call. = FALSE)
         
         dlist_final[[d]]$CCC[[j]] = dlist_final[[d]]$CCC[[j]][,catsel, drop=FALSE]
         dlist_final[[d]]$CIF[[j]] = dlist_final[[d]]$CIF[[j]][,catsel, drop=FALSE]
#         dlist_final[[d]]$EMP[[j]] = dlist_final[[d]]$EMP[[j]][,catsel, drop=FALSE]  # defunct 2024-04-06
  
       } # end of csel
  
     } # end for j
      
     nx = colnames(dlist_final[[d]]$thresholds) # paste0("I",1:k)
     names(dlist_final[[d]]$CCC) = nx
     names(dlist_final[[d]]$TCC) = nx
     names(dlist_final[[d]]$CIF) = nx
     names(dlist_final[[d]]$IIF) = nx
#     names(dlist_final[[d]]$EMP) = nx  # defunct 2024-04-06
     
     

  } # end for d
  
  dlist_final$freq = freq_sel  # mod 23-04-12: freq$freq; 23-06-27
  dlist_final$sortvec = sortvec

  attr(dlist_final,"source") = attr(dlist,"source")
  attr(dlist_final,"pp")     = attr(dlist,"pp")
  attr(dlist_final,"theta")  = theta

  if (debug) message("Leaving cleaner...")

  return(dlist_final)
  
  
} # end cleaner