drawperson = function(cl_obj,     # person parameters as vector
                 #    pp_how,     # barplot, hist, boxplot, violinplot
                      pp_offy,    # y offset
                      pp_offx,    # x offset
                      pp_col,
                      pp_labs = c("none","abs","rel","den"),
                      pp_den  = TRUE,
                      pp_nam  = "",
                      dd_col  = "black",
                      do_tif  = TRUE,
                      do_se   = TRUE,
                      do_stif = TRUE,
                      do_sse  = TRUE,
                      do_lab  = 4:6,
                      th_min,
                      th_max,
                      th_vec  = seq(th_min,th_max,length.out=101),
                      tifcol  = "seagreen4",    # tifcolNA for omission
                      se_col  = "firebrick4",    # --"--
                      tifmax  = "auto",
                      se_max  = "auto",
                      dw,
                      dh,
                      d0,          # left margin for scale
                      wd      = dw-d0,
                      breaks,
                      debug=TRUE
){
  
  if (debug) message("Entering drawperson...")

  pp_vec = cl_obj$person_par

#  stopifnot(pp_how %in% c("barplot", "hist", "boxplot", "violinplot"))

  n   = length(pp_vec)
  sel = which(pp_vec >= th_min & pp_vec <= th_max)

  pp_hist = hist(pp_vec[sel], plot=FALSE, breaks=breaks)
  pp_labs = match.arg(pp_labs)

  hist_den = pp_hist$density
  hist_abs = pp_hist$counts
  hist_rel = 100*hist_abs/n
  hist_mid = pp_hist$mids

  hist_use = switch(pp_labs,
                    "abs"  = hist_abs, #*0.95*dh/max(hist_abs),
                    "rel"  = hist_rel, #*0.95*dh/max(hist_rel),
                    "den"  = hist_den, #*0.95*dh/max(hist_den),
                    "none" = hist_abs) #*0.95*dh/max(hist_abs))

  hist_fac = 0.95*dh/max(hist_use)
  hist_val = hist_use*hist_fac

  dens_fac = 0.95*dh/max(hist_den)
  dens_val = density(pp_vec,from=th_min,to=th_max,na.rm=TRUE)
  dens_thx = dens_val$x
  dens_thy = dens_val$y * dens_fac

#  f_scale  = dh/max(hist_den)*0.95
#  dens_rel = hist_den * f_scale

#  if(pp_how == "hist"){

     K = length(pp_hist$breaks)
     rect( xleft   = pp_hist$breaks[-K],
           ybottom = rep(pp_offy, K-1),
           xright  = pp_hist$breaks[-1],
           ytop    = pp_offy + hist_val, #dens_rel,
           col     = pp_col)

     if (pp_den) lines(dens_thx,pp_offy+dens_thy,col=dd_col,lwd=2)

     if (pp_labs == "rel") hist_shw=paste(round(hist_use,2),"%") else hist_shw=round(hist_use,2)
     if (pp_labs != "none") {
         text(pp_hist$mid, pp_offy + hist_val, hist_shw,cex=0.6,adj=c(-0.2,0.5),srt=90)
         text(th_min+0.1*(th_max-th_min)     , pp_offy + dh, paste0(pp_nam,"\nn=",n),cex=0.8,adj=c(0,1))
     } # end if labels

#  }else if(pp_how == "boxplot"){
#         # to come...
#  } # end if/else


# TODO: legend for TIF / SE if axes omitted

    if (4 %in% do_lab) {
        xt = round(seq(0,max(hist_use),length.out=5))
        segments(d0        , pp_offy+xt*hist_fac,
                 d0+0.01*dw, pp_offy+xt*hist_fac)
        text(    d0+0.02*dw, pp_offy+xt*hist_fac, xt,cex=0.4,adj=c(-0.3,0.5))
    } # end if do_lab 4

    # --- TIF
      tifo  = cl_obj$TIF                      # original TIF

      if (tifmax=="auto") {                   # 2024-07-08 [RWA]
          tifm = max(tifo[is.finite(tifo)])
      } else {
          tifm = tifmax
      }

      tifs  = pp_offy + dh*tifo/tifm          # scaled

    # --- sTIF
      stifo = cl_obj$sTIF                     # original sTIF
      stifs = pp_offy + dh*stifo/tifm         # scaled

    # --- SE
      se_o  = cl_obj$SE                       # original SE

      if (se_max=="auto") {                   # 2024-07-08 [RWA]
          se_m = max(se_o[is.finite(se_o)])
      } else {
          se_m = se_max
      }

      se_s  = pp_offy + dh*se_o/se_m          # scaled

    # --- sSE
      sse_o = cl_obj$sSE                      # original sSE
      sse_s = pp_offy + dh*sse_o/se_m         # scaled
      sse_s[sse_s > max(se_s[is.finite(se_s)])] = NA # cut at upper border

    # --- draw lines
      if (do_tif)  lines(th_vec, tifs,  lwd=2, col=tifcol)
      if (do_stif) lines(th_vec, stifs, lwd=2, col=tifcol, lty=2)
      if (do_se)   lines(th_vec, se_s,  lwd=2, col=se_col)
      if (do_sse)  lines(th_vec, sse_s, lwd=2, col=se_col, lty=2)

    # --- prep ticks
      ticw = wd*0.01                          # width of tics
      ticn = 5                                # number of tics
      tics = seq(0,dh,length.out=ticn)        # seqence of tics

    # --- SE & sSE axis, ticks, & labels
      if ((do_se | do_sse) & (5 %in% do_lab)) {
          ticx = th_min
          ticl = round(seq(0,se_m,length.out=ticn),1)                                    # tick pos
          segments(ticx     ,pp_offy     ,ticx     ,pp_offy+dh              ,col=se_col) # axis
         #segments(ticx     ,pp_offy+tics,ticx+ticw,pp_offy+tics            ,col=se_col) # ticks
          segments(ticx     ,pp_offy+tics,ticx-ticw,pp_offy+tics            ,col=se_col) # ticks
          text(    ticx+ticw,pp_offy+tics,ticl      ,cex=0.5,adj=c(-0.2,0.5),col=se_col) # labels
          text(    ticx+ticw,pp_offy+tics[ticn],"SE",cex=0.5,adj=c( 0.5,-1 ),col=se_col) # header
      } # end (s)SE axis

    # --- TIF & sTIF axis, ticks, & labels
      if ((do_tif | do_stif) & (6 %in% do_lab)) {
          ticx = th_max
          ticl = round(seq(0,tifm,length.out=ticn),1)                                    # tick pos
          segments(ticx+ticw,pp_offy     ,ticx+ticw,pp_offy+dh              ,col=tifcol) # axis
          segments(ticx     ,pp_offy+tics,ticx+ticw,pp_offy+tics            ,col=tifcol) # ticks
          text(    ticx     ,pp_offy+tics,ticl       ,cex=0.5,adj=c(1.2,0.5),col=tifcol) # labels
          text(    ticx     ,pp_offy+tics[ticn],"TIF",cex=0.5,adj=c(0.5,-1 ),col=tifcol) # header
      } # end (s)TIF axis





  if (debug) message("Leaving drawperson...")

}# end fun draw_person    



