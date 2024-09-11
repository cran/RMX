# Calculate CCC and TCC ---------------------------------------------------

  calc_ccc = function(delta, theta=seq(-6,6,0.01), alpha=1, gamma=0, upper=1, graded=FALSE){
    
    dlt = rmNA(delta)
    
    if(length(alpha) == 1) current_alpha = rmNA(rep(alpha, each=length(dlt)))  # Ueberlegung: a,g,u skalar lassen
    else                   current_alpha = alpha
    
    current_gamma = rmNA(rep(gamma, each=length(dlt)))
    current_upper = rmNA(rep(upper, each=length(dlt)))
    
    # --- TCC
    
    prob_tcc = matrix(NA ,nrow=length(theta), ncol=length(dlt))
    for(i in seq_along(dlt)) prob_tcc[,i] = current_gamma[i]+((current_upper[i]-current_gamma[i])*exp( current_alpha[i] * (theta - dlt[i])  )/(1+exp(current_alpha[i] * (theta - dlt[i])  )) )
    
    # --- CCC
    
    if(!graded){
      
      num = matrix(NA ,byrow=TRUE, nrow=length(theta), ncol=length(dlt))                              # numerator calculations
      par_sum = cumsum(dlt)
      
      for(i in 1:length(dlt)) num[ ,i] = exp( current_alpha[i]  * (i * theta - par_sum[i]))
      
      denom = 1 + rowSums(num)
      pp = (num[, 1:length(dlt)]/denom)                                                               # add 0 category
      prob_out = cbind( (1/denom), pp)                                                                # result
      
    }else{
      
      if(length(dlt)==1) prob_out = cbind(1-prob_tcc[,1],prob_tcc[,ncol(prob_tcc)])                   # cbind generates a matrix! :)
      else               {
        
        fst_cat = 1-prob_tcc[,1]
        lst_cat = prob_tcc[,ncol(prob_tcc)]
        if(is.vector(-apply(prob_tcc, 1, diff))) mdd_cts = cbind(-apply(prob_tcc, 1, diff))
        else                                     mdd_cts = t(-apply(prob_tcc, 1, diff))
        
        prob_out = cbind(fst_cat, mdd_cts,lst_cat) # first, middle,last categories
      }
      
    } # end of CCC calculations
    if(any(current_gamma != 0)) prob_out = cbind(1-prob_tcc, prob_tcc)  # CCC perspective P(0) and P(1), 3PL correction 
    return(list(prob_ccc=prob_out, prob_tcc=prob_tcc))
    
  } # end of calc_ccc
  
  
# Probability for NRM -----------------------------------------------------

calc_nom = function(diff, alpha, theta=seq(-6,6,0.1)){ # Bock 1972
  
  d = rmNA(diff)     # difficulty
  a = rmNA(alpha)    # alpha/discrimination
  
  if(length(d) != length(a)) stop("alpha and difficulty lengths differ!")

  z_mat      = matrix(NA, nrow=length(theta), ncol=length(d))
  for(i in 1:ncol(z_mat)) z_mat[,i] = d[i] + a[i]*theta
  z_mat_e    = exp(z_mat)
  denom      = rowSums(z_mat_e)
  cat_mat    = apply(z_mat_e, 2, function(x){x/denom})
  prob_tcc   = calc_ccc(get_intersection(gamma = d, alpha = a), theta = theta, alpha = diff(a))$prob_tcc
  return_obj = list(prob_ccc=cat_mat, prob_tcc=prob_tcc)
  
  return(return_obj)
  
}

# Get Intersection --------------------------------------------------------

get_intersection = function(gamma, alpha){ # de Ayala 1993 (2022)
  
  if(length(gamma) != length(alpha)) stop("Invalid vector length!")
  delta = -diff(gamma)/diff(alpha)
  return(delta)
  
}# end of get_intersection


# informations for 1-4pl --------------------------------------------------

info_4pl = function(thres, theta=seq(-6,6,0.1), alpha=1, gamma=0, upper=1){
  
  prob = calc_ccc(delta=thres, theta=theta, alpha=alpha, gamma=gamma, upper=upper)$prob_tcc
  cats = calc_ccc(delta=thres, theta=theta, alpha=alpha, gamma=gamma, upper=upper)$prob_ccc
  
  iif  = (alpha^2*(prob-gamma)^2*(upper-prob)^2)/((upper-gamma)^2*prob*(1-prob))
  cif  = apply(cats, 2, function(x) x*iif)
  
  return(list(cif=cif, iif=iif))
} # end of fun

# IIF Masters Formulation -------------------------------------------------

info_mas = function(thres, theta=seq(-6,6,0.1), alpha=1, gamma=0, graded=FALSE){

  cat_prob = calc_ccc(delta=thres, theta=theta, alpha=alpha, gamma=gamma, graded=graded)$prob_ccc

  k = 1:ncol(cat_prob)
  
  info = (alpha)^2 * (rowSums(sweep(cat_prob, 2, k^2, '*')) - rowSums(sweep(cat_prob, 2, k, '*'))^2)
  cif_mat = cat_prob*info
  
  return(list(cif=cif_mat, iif=rowSums(cif_mat)))
  
} # end of info_Masters

# IIF Samejimas Formulation -----------------------------------------------

info_sam = function(thres, theta=seq(-6,6,0.1), alpha=1){
  
  t = rmNA(thres)
  a = rmNA(alpha)
  
  tcc_prob = calc_ccc(delta=t, theta=theta, alpha=alpha)$prob_tcc                 # prob
  first_deriv=matrix(NA, nrow=length(theta), ncol=ncol(tcc_prob))                 # prepare matrix for derivatives
  for(i in 1:ncol(first_deriv)) first_deriv[,i]=tcc_prob[,i]*(1-tcc_prob)[,i]     # calc derivative (conditional variance)
  
  num_cat0 = (-first_deriv[,1])                                                   # create numerator
  num      = matrix(NA, nrow=length(theta), ncol=(ncol(first_deriv)-1))
  for(i in 1:ncol(num)) num[,i] = (first_deriv[,i]-first_deriv[,i+1])
  num_last = first_deriv[,ncol(first_deriv)]
  num_mat  = cbind(num_cat0, num, num_last)^2
  
  ccc_prob = calc_ccc(delta=t, theta=theta, alpha=alpha, graded=TRUE)$prob_ccc
  
  cif_share = matrix(NA, nrow=length(theta), ncol=ncol(ccc_prob))
  for(i in 1:ncol(ccc_prob))cif_share[,i] = (alpha)^2*(num_mat[,i]/(ccc_prob[,i]))
  
  return(list(cif=cif_share, iif=rowSums(cif_share)))
  
} # end of info_Samejima

# IIF for NRM -------------------------------------------------------------

info_nom = function(diff, alpha, theta=seq(-6,6,0.1)){
 
  d = rmNA(diff)     # difficulty
  a = rmNA(alpha)    # alpha/discrimination
  
  iif  = rep(NA, length(theta))
  
  for(i in seq_along(theta)){
    
      P       =  calc_nom(diff = d, alpha = a, theta = theta[i])  # get prob only for one theta
      W       =  outer(-P$prob_ccc, P$prob_ccc)                   # calculate W matrix
      diag(W) =  P$prob_ccc*(1-P$prob_ccc)
      iif[i]  =  t(a) %*% W %*% a
   
  } # end of for loop
  
  prob = calc_nom(diff=d, alpha=a, theta=theta)$prob_ccc
  return(list(cif=prob*iif, iif=iif))

  
} # end of info_nom


# empirical CCCs Davide ---------------------------------------------------

  calc_obs = function (data,perspar, ngroups = NULL) {

    cats = range(data, na.rm = TRUE)[2]+1   # why range()[2], not max()?
    n_cat = aggregate(data+1, by = list(perspar), tabulate, nbins = cats)  # data must be zero based
    n_cat = n_cat[-1]
    prop_list = list()

    for (isl in 1:NCOL(data)) {
         prop_cat = n_cat[isl]/rowSums(n_cat[isl])
         prop_list[[isl]] = prop_cat
    } # end for

    sort_perspar = sort(unique(perspar))

    if (!is.null(ngroups)) {
      quantiles = quantile(perspar, seq(0, 1, length = ngroups+1))
      cuts = cut(perspar, unique(quantiles),  include.lowest = TRUE)
      n_cat = aggregate(data+1, by = list(cuts), tabulate, nbins = cats)
      n_cat = n_cat[-1]
      prop_list = list()
      attr(prop_list, "cuts") = cuts

      for (isl in 1:NCOL(data)) {
           prop_cat = n_cat[isl]/rowSums(n_cat[isl])
           prop_list[[isl]] = as.matrix(prop_cat)
      }
      sort_perspar =  aggregate(perspar, list(cuts), mean, na.rm = TRUE)[, 2]

      if (sum(duplicated(quantiles)) > 0) {
        warning("Some Quantiles were not unique and were therefore combined.", call. = FALSE)
      } # end if

    } # not is.null(ngroups)
    attr(prop_list, "sort_perspar") = sort_perspar
    return(prop_list)
  }  
# Helper Functions --------------------------------------------------------

is.int = function(x) floor(x) == x

# ---

rmNA = function(x){
  if(sum(is.na(x)) == 0) return(x)
  else                   return(x[-which(is.na(x))])
}



