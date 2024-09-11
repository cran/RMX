#  source("drawitem.r")

drawer = function(cleanobj, 
                  theta=seq(-6,6,0.1), 
                  type="CCC",
                  height=8,
                  width=height*(sqrt(5)-1)/2,
                  funhprop=0.7,
                  funwprop=(sqrt(5)-1)/2,
                  funcol=2:20,
                  funlty=1, 
                  funlwd=2, 
                  infcol="black",
                  infoequal=TRUE,
                  infomax="auto",
                  bg=grey(0.9),
                  pplab="abs", 
                  dcol=grey(.9),
                  classical=FALSE,
                  thrsup=TRUE,
                  thrpch=16,
                  legend=TRUE, 
                  legcex=0.7,
                  leglsp=NULL,
                  legoff=0.1,
                  src=TRUE,
                  disind="*",
                  discol="red",
                  discex=1.5,
                  lmar=2,
                  tmar=0,
                  main=NULL,
                  ylas=ylas,
                  usedimcol=TRUE,
                  dencol="black",
                  tifcol="seagreen4",
                  se_col="firebrick4",
                  gridcol="white",

                  grid=TRUE,
                  dens=TRUE,
                  TIF=TRUE,
                  sTIF=TRUE,
                  SE=TRUE,
                  sSE =TRUE,
                  TIFmax="auto",
                  SEmax="auto",

                  showlab=1:3,
                  extwin=TRUE,
                  debug=TRUE,
                  csel=NULL,
                  highlight=NULL,
                 # empirical=FALSE,
                 # ngroups=NULL,
                 # show.size=FALSE,
                  resetpar=TRUE,
                  breaks=NULL
  ){

  if (debug) message("Entering drawer...")

  D = length(cleanobj)-2                                        # no. of dims = all - freq - sort
  I = rep(NA,D)                                                 # no. of items/dim

# Axis  ------------------------------------------------------------------------

  dimnames = names(cleanobj)[-length(names(cleanobj))]
  itmnames = NULL
  itmlabel = NULL
  
  for(d in 1:D) {
      I[d] = ncol(cleanobj[[d]]$thresholds)                    # cumulative count of items per dim
      itmnames = c(itmnames,                    colnames(cleanobj[[d]]$thresholds))
      itmlabel = c(itmlabel, paste(dimnames[d], colnames(cleanobj[[d]]$thresholds), sep="_")) 
  } # end for
  
# disordered thresholds --------------------------------------------------------
  
  isdis = NULL
  for(d in 1:D){
      for(i in 1:ncol(cleanobj[[d]]$thresholds)){
         if(cleanobj[[d]]$model[i] == "nominal") isdis = c(isdis,is.unsorted(cleanobj[[d]]$discrimination[,i],na.rm=TRUE))
         else                                    isdis = c(isdis,is.unsorted(cleanobj[[d]]$thresholds[,i],na.rm=TRUE))
      } # end for i
  } # end for d
  
  dislab = ifelse(isdis, disind, " ")


# diagram coordinates ----------------------------------------------------------

  if(length(type) > 1) P = length(type) else P = I  # no. of plots
  K  = sum(P)                                       # no. items across all dims
  ppzero    =  K*funhprop                           # baseline for pp diagrams

  baselines = (K-1):0*funhprop                      # baselines = seq((K)*funhprop, 0, -funhprop)
  if (length(type)==1) baselines = baselines[cleanobj$sortvec]           # totalsort for 1 diag/many items

  t0 = min(theta)
  t1 = max(theta)
  td = t1-t0
  ml = 0.10*td                                      # left margin
  mr = 0.05*td                                      # right margin
  w0 = t0-ml                                        # left-most point in x-direction
  w1 = (w0+ml+td+mr)                                # vertical splitter fun/freq
  wd = w1-w0
  w2 = w1-wd+wd/funwprop

  xx = ifelse(funwprop==0, 1, funwprop)             # for the purpose of drawing baselines, pretend funwprop=1
  w2 = w1-wd+wd/xx

  ph = (K+D-1) - ppzero                             # person diagrams total hight
  pf = ph/D                                         # moving factor for pp diagrams
  ps = 0.85                                         # scaling factor for pp diagrams
  op = rev(ppzero + 0:(D-1)*pf)                     # baselines of pp-diagrams

  imax = max(unlist(lapply(cleanobj[1:D],"[[","IIF")), na.rm=TRUE)  # max value IIF (for y-axis scaling)
  cmax = max(unlist(lapply(cleanobj[1:D],"[[","CIF")), na.rm=TRUE)  # max value CIF (for y-axis scaling)
  cnum = max(unlist(lapply(cleanobj$freq,length)))                  # max no. cats (PCM+NRM!), corr. 2024-04-05

  while (length(funcol) < cnum) funcol = rep(funcol,2) # adapt color vector length

# ---------------------------------------------------------- === NEW WINDOW HERE
  if (extwin) {
      dev.new(width=width,height=height,noRStudioGD=TRUE)
      orig.par = par(no.readonly = TRUE)
      orig.pal = palette()

      if (resetpar) {
          on.exit(par(orig.par))
          on.exit(palette(orig.pal),add=TRUE)
      } # end if reset
  } # end if extwin
# ------------------------------------------------------------------------------

  par(mar=c(2,lmar,tmar,0)+0.2)
  
  if(D > 1 & funhprop==1) y1=ppzero
  else                    y1=K+D-1
  
  plot(0:1, 0:1,
       type = "n",
       yaxt = "n",
       xaxs = "i",
       yaxs = "i",
       xlim = c(w0,w2), 
       ylim = c(-0.05,y1),
       ylab = "",
       main=main,
       axes = FALSE)

  if(D==1) ax2lab = itmnames
  else     ax2lab = itmlabel

  if(1 %in% showlab) axis(1,at=round(theta,0),labels=round(theta,0), cex.axis=0.7)
  
 # --- draw person area

  dimcol = dcol

  while (length(dimcol) < D) dimcol = rep(dimcol,2)
  while (length(tifcol) < D) tifcol = rep(tifcol,2)
  while (length(se_col) < D) se_col = rep(se_col,2)

  if (funhprop < 1  & funwprop > 0) {

        dimnam = names(cleanobj)[1:D]


        for(d in 1:D) {
          
            drawperson(cleanobj[[d]],
                       pp_offx = t0,  #theta[5], TODO: check!
                       pp_offy = op[d]+pf*ps*0.05,
                       pp_labs = pplab,
                       pp_col  = dimcol[d],
                       pp_nam  = dimnam[d],
                       pp_den  = dens,
                       dd_col  = dencol[d],
                       do_tif  = TIF,
                       do_se   = SE,
                       do_stif = sTIF,
                       do_sse  = sSE,
                       do_lab  = showlab,
                       th_min  = t0,
                       th_max  = t1,
                       th_vec  = theta,
                       tifcol  = tifcol[d],
                       se_col  = se_col[d],
                       tifmax  = TIFmax,
                       se_max  = SEmax,
                       dw      = td,
                       dh      = pf*ps, # (K+D-1)*(1-funhprop)/D
                       d0      = w0,
                       wd      = wd,
                       breaks  = breaks,
                       debug   = debug
                       ) # end call drawperson

        } # end for d

  } # end funhprop < 1 & funwprop > 0

  if (funhprop > 0 & funwprop > 0) {

   # --- draw background
    if(bg != "none" & !classical){
      
      rect(rep(t0,length(baselines)),       # xleft
           baselines,                       # ybottom
           rep(t1,length(baselines)),       # xright
           baselines+(funhprop*0.9),        # ytop
           col=bg,
           border = NA)   #### <-- dim color ...................................
      
      # draw baselines (adjusted for bg)
      segments(rep(t0, length(baselines)),  # x0
               baselines,                   # y0
               w2,                          # x1 
               baselines,                   # y1
               col=grey(0.6)
      )
      
    } #  end of bg
    

# ========================================================= 0. classical diagram ================================
  if(classical){
    
      II = c(0, cumsum(I))                  # offset for drawing across dims (see below)
      new_base = baselines+funhprop/2
      
      for(d in 1:D){
          for(i in 1:I[d]){
            if(cleanobj[[d]]$model[i] == "nominal") {
              ttmp = with(cleanobj[[d]], get_intersection(gamma = thresholds[,i], alpha = discrimination[,i]))
            } else {
              ttmp = cleanobj[[d]]$thresholds[,i]
            } # end if/else nominal
            sel = which(ttmp >= t0 & ttmp <= t1)
            ii = i + II[d]
            if (length(sel) > 0) {
                if (usedimcol) colx=dimcol[d] else colx=funcol[sel]
                segments(min(ttmp),new_base[ii],max(ttmp),new_base[ii],lty=ifelse(isdis[ii],3,1))
                points(ttmp[sel],rep(new_base[ii],length(ttmp[sel])),col=colx, pch=thrpch)
                text(  ttmp[sel],rep(new_base[ii],length(ttmp[sel])),sel,adj=c(0.5,-0.5),cex=0.5)
            } # end if length sel > 0
          } # end of i
      } # end of d 

      if(2 %in% showlab) axis(2,at=baselines+funhprop/2,labels=ax2lab, las=ylas, cex.axis=0.7)  # (0.3*funhprop):((K-1)*funhprop)

  }else{
    
    # --- draw item curves
    
    CC = 0
    II = c(0, cumsum(I))          # offset for drawing across dims
    
    if(!is.null(highlight)){
       graycol = rep("gray36", cnum)
       graycol[highlight] = funcol[highlight]
       funcol = graycol
    } # end of if highlight defined

    if(is.null(csel)) funcol_c = funcol
    else              funcol_c = funcol[csel]
    
# ========================================== 1. multiple items / single function ================================
    if (length(type)==1) {

        for (d in 1:D) {
             for (i in 1:I[d]) {
                 
                  ii = i + II[d]

                # --- extract reqested data
                  xmat = NULL
                  if (type == "BIF") {
                    xmat = cbind(
                           cleanobj[[d]] [["CIF"]] [[i]],
                           cleanobj[[d]] [["IIF"]] [[i]]
                    ) # end cbind
                  } else {
                    xmat = cleanobj[[d]] [[type]] [[i]]
           
                  } # end else BIF

                # --- rescale item information functions
                  if (type %in% c("CIF","IIF","BIF")) {
                      if (infomax=="auto")  {
                                                     xmax = max(xmat, na.rm=TRUE)
                      } else if (infomax=="equal") {
                        if (type %in% c("IIF","BIF")) {
                                                     xmax = imax
                        } else if (type=="CIF") {
                                                     xmax = cmax
                        } # end type=IIF/BIF vs. CIF
                      } else {
                                                     xmax = infomax
                      } # end if/else equal
                  } else {
                                                     xmax = 1
                  } # end if/else info/prob

                  xmat[xmat>xmax] = NA
                  xmat = 0.9*funhprop * xmat/xmax
                  tics = seq(0,xmax,xmax/5)
                  tpos = 0.9*funhprop * tics/xmax

                # --- draw grid
                  if(grid){
                      vx = seq(ceiling(t0),floor(t1),1)
                      vy = baselines[ii]
                      hy = baselines[ii] + tpos
                      segments(vx,vy+0.01,vx,vy+0.9*funhprop,col=gridcol)
                      segments(t0,hy     ,t1,hy             ,col=gridcol)
                   }  # end if grid


                # --- draw the requested diagram
                  C = ncol(xmat)
                

               # defunct 2024-01-28, now covered by cnum [RWA]
               #   if (C > CC) {
               #       CC = C    # seek maximum no. of cats for legend
               #     #  while (CC > length(funcol_c)) funcol_c = rep(funcol_c,2)
               #   } # end check max no. of cats

               #   if(type=="CCC" | type=="CIF" |type =="BIF"){
               #     if(type=="BIF") catnum = C-1
               #     else            catnum = C
               #     if(catnum != length(funcol_c)) funcol_c=funcol
               #   }
                  
                  for(c in 1:C) {
                      if (type=="IIF" | (type=="BIF" & c==C)) {
                                             colx=infcol 
                      }else if (type=="TCC") colx=funcol[c+1]
                       else                  colx=funcol_c[c] 
                       
                      lines(theta, baselines[ii]+xmat[,c], col=colx, lty=funlty, lwd=funlwd)
                      
                   #   if(empirical){                                                    # defunct 2024-04-06, eRm only
                   #      temp = cleanobj[[d]]$EMP[[i]]  # temporary empirical matrix
                   #      xemp = attributes(cleanobj[[d]]$EMP)$sort_perspar
                   #      if (show.size & is.null(ngroups)) {
                   #            size = table(cleanobj[[d]]$person_par)/max(table(cleanobj[[d]]$person_par))
                   #            ecex = exp(size)*0.5
                   #      }else if(show.size & !is.null(ngroups)) {
                   #            size = table(attributes(cleanobj[[d]]$EMP)$cuts)/max(table(attributes(cleanobj[[d]]$EMP)$cuts))
                   #            ecex = exp(size)*0.5
                   #      }else ecex=1
                   #
                   #      points(xemp, baselines[ii]+temp[,c]*0.9*funhprop, col=colx, pch=16, cex=ecex)
                   #
                   #   } # end of if empirical

                  } # end for

                # --- threshold indicators & support lines for CCC
                  if (type=="CCC") {

                      ttmp = cleanobj[[d]]$thresholds[,i]


                      if (cleanobj[[d]]$model[i] == "nominal"){ # calculate intersection points

                          itmp = get_intersection(gamma = ttmp, alpha = cleanobj[[d]]$discrimination[,i])    # intersection temp.
                          thry = calc_nom(diff = ttmp, alpha = cleanobj[[d]]$discrimination[,i], theta=itmp)$prob_ccc

                          if (is.vector(thry)) {                # corr 2024-04-04, thx to Davide
                              thry = thry[1]                    # dichotomous case
                          } else {
                              thry = diag(thry[,-ncol(thry)])   # polytomous case: ncol(thry) = nrow(thry)+1
                          } # end else

                          tsel = which(itmp > t0 & itmp < t1)

                          if (length(tsel) > 0) {
                            if(thrsup) segments(itmp[tsel],    baselines[ii],       itmp[tsel],baselines[ii]+0.9*thry[tsel]*funhprop,lty=5,col=funcol[tsel])
                                       points(  itmp[tsel],rep(baselines[ii],length(itmp[tsel]))                               ,pch=thrpch,col=funcol[tsel],xpd=TRUE)
                          }

                      }else{  # all others (not nominal)

                          thry = calc_ccc(delta=ttmp,theta=ttmp, alpha=cleanobj[[d]]$discrimination[i], graded=cleanobj[[d]]$model[i]=="graded")$prob_ccc

                          if (ncol(thry) > 2) {                 # corrected 2024-04-04, thx to Davide
                              thry = diag(thry[,-ncol(thry)])   # polytomous case: ncol(thry) = nrow(thry)+1
                          } else {
                              thry = thry[1,1]                  # dichotomous case (results in 0.5)
                          } # end else

                          tsel = which(ttmp > t0 & ttmp < t1)

                          if (length(tsel) > 0) {
                              if(thrsup) segments(ttmp[tsel],    baselines[ii],       ttmp[tsel],baselines[ii]+0.9*thry[tsel]*funhprop,lty=5,col=funcol[tsel])
                                         points(  ttmp[tsel],rep(baselines[ii],length(ttmp[tsel]))                               ,pch=thrpch,col=funcol[tsel], xpd=TRUE)
                          } # end if length tsel

                      } # end else (not nominal)

                 } # end if ccc
                 
                # --- threshold indicators & 0.5-line for TCC
                  if (type=="TCC"){
                   
                   ttmp = cleanobj[[d]]$thresholds[,i]
                   
                   if(cleanobj[[d]]$model[i] == "nominal"){
                     
                     itmp = get_intersection(gamma = ttmp, alpha = cleanobj[[d]]$discrimination[,i])
                     tsel = which(itmp > t0 & itmp < t1)
                     
                     if (length(tsel) > 0) {
                       if(thrsup) segments(itmp,    baselines[ii],       itmp,baselines[ii]+0.9*0.5*funhprop,lty=5,col=funcol)
                                  points(  itmp,rep(baselines[ii],length(itmp))                                   ,col=funcol,pch=thrpch,xpd=TRUE)
                     }
                   }else{
                     
                     tsel = which(ttmp > t0 & ttmp < t1)
                     if (length(tsel) > 0) {
                       if(thrsup) segments(ttmp[tsel],    baselines[ii],       ttmp[tsel]  ,baselines[ii]+0.9*0.5*funhprop,lty=5,col=funcol[tsel])
                                  points(  ttmp[tsel],rep(baselines[ii],length(ttmp[tsel])),                        ,pch=thrpch, col=funcol[tsel],xpd=TRUE)
                     }
                   } # end of else
                   
                   if(thrsup) segments(t0,baselines[ii]+0.9*0.5*funhprop,t1,baselines[ii]+0.9*0.5*funhprop,lty=5,col=grey(.7)) # horiz. middle line 0.5

                 } # end if tcc

                # --- tickmarks/scale on y-axis per diagram and disorder marks
                  if(3 %in% showlab){
                     segments(w0        , baselines[ii]+tpos,
                              w0+0.01*td, baselines[ii]+tpos)
                     text(    w0+0.02*td, baselines[ii]+tpos,
                              formatC(tics,format="f",width=4,digits=2),cex=0.4,adj=c(-0.3,0.5))
                  }

                # --- disorder indicator
                  text(t1+0.03*td,baselines[ii]+0.9*funhprop,dislab[ii],col=discol,cex=discex)
                   
             } # end for i
       } # end for d
      
     # --- legend (top right area)
       if(funhprop < 1 & funwprop < 1){

         if (legend) {
              legx0 = w1              # max(theta)
              legx1 = w2              # legx0+legw
              legy0 = ppzero          # 2.8
              legy1 = K+D-1           # top margin
              legh  = legy1-legy0     # 1.2
              legw  = legx1-legx0     # 7.4
              linh  = legh/15 * leglsp

              legtxt = switch(type,
                              "CCC" = " Category\n Characteristic\n Curves",
                              "TCC" = " Threshold\n Characteristic\n Curves",
                              "IIF" = " Item Information\n Function",
                              "CIF" = " Category Information\n  Function",
                              "BIF" = " Category + Item\n  Information Function"
              ) # end switch type

              text(legx0,legy1,legtxt,cex=0.7,adj=c(-0.02,1.2),font=2)

              legcat = switch(type,
                              "CCC" = "Category",
                              "TCC" = "Threshold",
                              "IIF" = "Item", # unnoetig
                              "CIF" = "Category",
                              "BIF" = "Category"
              ) # end switch type


              if(type=="IIF"){

                     points(legx0+legw*0.15              ,legy1-legoff-(1+4)*linh                   ,col=infcol,pch=15,) # c+4
                     text(  legx0+legw*0.25              ,legy1-legoff-(1+4)*linh,"Item Information",cex=legcex,adj=c(0,0.4))

              } else if(type=="BIF"){

                   # Item Information
                     points(legx0+legw*0.15              ,legy1-legoff-(0+4)*linh                   ,col=infcol,pch=15,) # c+4
                     text(  legx0+legw*0.25              ,legy1-legoff-(0+4)*linh,"Item Information",cex=legcex,adj=c(0,0.4))

                     if(is.null(csel)) nshowcat = cnum
                     else              nshowcat = length(funcol_c)

                     if (is.null(csel)) {
                         label = paste(legcat, 1:nshowcat)
                     } else {
                         label = paste(legcat,   csel)
                     }

                   # Categories
                     points(rep(legx0+legw*0.15, nshowcat) ,legy1-legoff-(1:nshowcat+4)*linh, col=funcol_c,pch=15)
                     text(  rep(legx0+legw*0.25, nshowcat) ,legy1-legoff-(1:nshowcat+4)*linh, label,cex=legcex,adj=c(0,0.4))

              } else if(type %in% c("CCC","CIF")){

                     if (is.null(csel)) {
                         label    = paste(legcat, 1:cnum)
                         nshowcat = cnum
                     } else {
                         label = paste(legcat,   csel)
                         nshowcat  = length(csel)
                     }

                     points(rep(legx0+legw*0.15, nshowcat)   ,legy1-legoff-(1:nshowcat+4)*linh ,col=funcol_c[1:cnum],pch=15)
                     text(  rep(legx0+legw*0.25, nshowcat)   ,legy1-legoff-(1:nshowcat+4)*linh, label, cex=legcex,adj=c(0,0.4))

              } else if(type=="TCC"){ # type=="TCC"

                     points(rep(legx0+legw*0.15, cnum)   ,legy1-legoff-(1:cnum+4)*linh ,col=funcol[-1],pch=15)
                     text(  rep(legx0+legw*0.25, cnum)   ,legy1-legoff-(1:cnum+4)*linh, paste(legcat,1:cnum),cex=legcex,adj=c(0,0.4))

              } else warning(paste("Unknown type:",type),call.=FALSE)


         } # end if legend

       } # end if funhprop

       if(2 %in% showlab) axis(2,at=baselines+funhprop/2,labels=ax2lab, las=ylas, cex.axis=0.7)  # (0.3*funhprop):((K-1)*funhprop)

    } else {

# =========================================== 2. single item / multiple diagrams ================================

      for (t in 1:length(type)) {
           xmat = NULL
           if (type[t] == "BIF") {
             xmat = cbind(
               cleanobj[[d]] [["CIF"]] [[1]],
               cleanobj[[d]] [["IIF"]] [[1]]
             ) # end cbind
           } else {
             xmat = cleanobj[[d]] [[type[t]]] [[1]]
           } # end else BIF


          # --- empirical CCCs (MK & DA) --------------------------------------- # temporarily defunct 2024-04-06

       #    if(empirical){
       #      temp = cleanobj[[d]]$EMP[[1]]  # temporary empirical matrix
       #      xemp = attributes(cleanobj[[1]]$EMP)$sort_perspar
       #
       #      if (show.size & is.null(ngroups)) {
       #        size = table(cleanobj[[d]]$person_par)/max(table(cleanobj[[d]]$person_par))
       #        ecex = exp(size)*0.5
       #      }else if(show.size & !is.null(ngroups)) {
       #        size = table(attributes(cleanobj[[d]]$EMP)$cuts)/max(table(attributes(cleanobj[[d]]$EMP)$cuts))
       #        ecex = exp(size)*0.5
       #      }else ecex=1
       #    }


         # --- rescale item and cat information functions
           if (type[t] %in% c("CIF","IIF","BIF")) {

               if (infomax=="auto")  {
                                              xmax = max(xmat, na.rm=TRUE)
               } else if (infomax=="equal") {

                 if (type[t]=="CIF") {

                     if (any(type %in% c("IIF","BIF"))) {
                                              xmax = imax         # cif & iif
                     } else {
                                              xmax = cmax         # cif only
                     } # end if/else CIF only

                 } else if (type[t] %in% c("IIF","BIF")) {
                                              xmax = imax         # iif only
                 } # end iif

               } else { # end if equal
                                              xmax = infomax      # value
               }
           } else {
                                              xmax = 1            # ccc & tcc
           } # end if/else info

           xmat[xmat>xmax] = NA
           xmat = 0.9*funhprop * xmat/xmax
           tics = seq(0,xmax,xmax/5)
           tpos = 0.9*funhprop * tics/xmax

         # --- draw grid
           if(grid){
              vx = seq(ceiling(t0),floor(t1),1)  # todo: adapt step to width...
              vy = baselines[t]
              hy = baselines[t] + tpos
              segments(vx,vy+0.01,vx,vy+0.9*funhprop,col=gridcol,lty=1)
              segments(t0,hy     ,t1,hy             ,col=gridcol,lty=1)
           }  # end if grid

         # --- draw requested lines
           C = ncol(xmat)
           for(c in 1:C) {
               if (type[t]=="IIF" | (type[t]=="BIF" & c==C)) ccc=infcol else ccc=funcol_c[c]
               if (type[t]=="TCC") ccc=funcol[c+1]
               lines(theta, baselines[t]+xmat[,c], col=ccc, lty=funlty, lwd=funlwd)
            #   if(empirical & type[t] == "CCC") points(xemp, baselines[t]+temp[,c]*0.9*funhprop, col=ccc, pch=16, cex=ecex) # defunct 2024-04-06
               
           } # end for c

         # --- disordered indicator
           if (type[t]=="CCC") {

              ttmp = cleanobj[[d]]$thresholds[,1]

              if(cleanobj[[d]]$model[1] == "nominal"){ # intersection points of NRM

                 itmp = get_intersection(gamma = ttmp, alpha = cleanobj[[d]]$discrimination[,1])    # intersection temp.
                 thry = calc_nom(diff = ttmp, alpha = cleanobj[[d]]$discrimination[,1], theta=itmp)$prob_ccc

                 if (is.vector(thry)) {                # corr 2024-04-04, thx to Davide
                     thry = thry[1]                    # dichotomous case
                 } else {
                     thry = diag(thry[,-ncol(thry)])   # polytomous case: ncol(thry) = nrow(thry)+1
                 } # end else

                 tsel = which(itmp > t0 & itmp < t1)

                 if (length(tsel) > 0) {
                     if(thrsup) segments(itmp[tsel],    baselines[t],
                                         itmp[tsel],    baselines[t]+0.9*thry[tsel]*funhprop,lty=5,col=funcol_c[tsel])
                                  points(itmp[tsel],rep(baselines[t],length(itmp[tsel])),xpd=TRUE, col=funcol_c[tsel], pch=thrpch)
                 }
             } else {                                   # intersection points of all other models

               thry = calc_ccc(delta=ttmp,theta=ttmp, alpha=cleanobj[[d]]$discrimination[1], graded=cleanobj[[d]]$model[1]=="graded")$prob_ccc

               if (ncol(thry) > 2) {
                   thry = diag(thry[,-ncol(thry)])   # polytomous case: ncol(thry) = nrow(thry)+1
               } else {
                   thry = thry[1,1]                  # dichotomous case (results in 0.5)
               } # end else

               tsel = which(ttmp > t0 & ttmp < t1)

               if (length(tsel) > 0) {
                   if(thrsup) segments(ttmp[tsel],baselines[t],
                                       ttmp[tsel],baselines[t]+0.9*thry[tsel]*funhprop,lty=5   , col=funcol_c[tsel])
                              points(  ttmp[tsel],rep(baselines[t],length(ttmp[tsel])),xpd=TRUE, col=funcol_c[tsel], pch=thrpch)
               } # end thresholds & thrsup-lines
             } # end if/else nominal


           } # end if ccc

           if (type[t]=="TCC"){
             
              
              ttmp = cleanobj[[d]]$thresholds[,1]

              if(cleanobj[[d]]$model[1] == "nominal"){

                 itmp = get_intersection(gamma = ttmp, alpha = cleanobj[[d]]$discrimination[,1])
                 tsel = which(ttmp > t0 & ttmp < t1)
                 segments(itmp,baselines[t],itmp,baselines[t]+0.9*0.5*funhprop,lty=5,col=funcol_c)
                 points(itmp,rep(baselines[t],length(itmp)),col=funcol_c,xpd=TRUE, pch=thrpch)

              }else{

                 tsel = which(ttmp > t0 & ttmp < t1)
                 if (length(tsel) > 0) {
                     if(thrsup) segments(ttmp[tsel],baselines[t]    ,ttmp[tsel],baselines[t]+0.9*0.5*funhprop,lty=5,col=funcol[tsel])
                                points(  ttmp[tsel],rep(baselines[t],length(ttmp[tsel])),col=funcol[tsel],xpd=TRUE, pch=thrpch)
                 } # end if
             } # end else

             if(thrsup) segments(t0,baselines[t]+0.9*0.5*funhprop,t1,baselines[t]+0.9*0.5*funhprop,lty=5,col=grey(.7)) # horiz. middle line 0.5

           }# end of if TCC

         # --- disorder indicator for TCC and CCC
           if (type[t] %in% c("TCC","CCC")) text(t1+0.03*td,baselines[t]+0.9*funhprop,dislab,col=discol,cex=1.5)



         # --- tickmarks/scale on y-axis per diagram
           if(3 %in% showlab){
              segments(w0    ,baselines[t]+tpos,
                       w0+0.1,baselines[t]+tpos)
              text(    w0+0.2,baselines[t]+tpos,#round(tic,1),cex=0.4,adj=c(-0.3,0.5))
                       formatC(tics,format="f",width=4,digits=2),cex=0.4,adj=c(-0.3,0.5))
           }# end of showlab 3

         # --- legends
           if(funwprop < 1){

             if (legend) {
                 legx0 = w1
                 legx1 = w2              # legx0+legw
                 legy0 = baselines[t]    # 2.8
                 legy1 = legy0+0.95*funhprop     # legy0+legh
                 legh  = legy1-legy0     # 1.2
                 legw  = legx1-legx0     # 7.4
                 linh  = legh/8 * leglsp
                 legtxt = switch(type[t],
                                 "CCC" = " Category Characteristic Curves",
                                 "TCC" = " Threshold Characteristic Curves",
                                 "IIF" = " Item Information\n Function",
                                 "TIF" = " Test Information Function",
                                 "CIF" = " Category Information Function",
                                 "BIF" = " Category + Item Information Function"
                 ) # end switch type

                 legcat = switch(type[t],
                                 "CCC" = "Category",
                                 "TCC" = "Threshold",
                                 "IIF" = "Item",
                                 "TIF" = "Test\nInformation\nFunction",
                                 "CIF" = "Category",
                                 "BIF" = "Category"
                 ) # end switch type

                 if (type[t] != "IIF") {
                     for (c in 1:C) {
                          ytmp = legy1-c*linh
                          if (ytmp < baselines[t]) message("Legend labels exceed vertical legend space. Reducing legend line spacing (leglsp=) may help.\n")
                          if (type[t] == "BIF" & c==C) {
                              points(legx0+legw*0.15,ytmp,pch=15,col=infcol)
                              text(  legx0+legw*0.25,ytmp,"Item Information",cex=legcex,adj=c(0,0.4))
                          } else if (type[t] == "TCC") {
                              points(legx0+legw*0.15,ytmp,pch=15,col=funcol[c+1])
                              if(cleanobj[[d]]$model == "nominal") thrann = itmp # threshold annotation
                              else                                 thrann = ttmp
                              tlab = paste0(legcat," ",c," (",formatC(thrann[c],format="f",width=4,digits=2),")")
                              text(  legx0+legw*0.25,ytmp,tlab              ,cex=legcex,adj=c(0,0.4))
                          } else {
                            
                            if(length(csel)==1) label = paste(legcat,csel)
                            else                label = paste(legcat,c)
                            
                              points(legx0+legw*0.15,ytmp,pch=15,col=funcol_c[c])
                              text(  legx0+legw*0.25,ytmp,label  ,cex=legcex,adj=c(0,0.4))
                          } # end if/else bif/last cat
                     } # end for c
                 } else {
                     ytmp = legy1-linh
                     points(legx0+legw*0.15,ytmp,pch=15,col=infcol)
                     text(  legx0+legw*0.25,ytmp,legtxt,cex=legcex,adj=c(0,0.4))

                 } # end if/else not IIF
             } # end if legend
          } # end of funwprop check < 1
      } # end for t

      flexbar(cleanobj$freq[[1]],
              ox=w1*1.05,
              oy=ppzero,
              dw=(w2-w1)*0.9,
              dh=pf*0.8, #0.95*funhprop,
              bg="white",
              im=0.05,
              bo=NA,
              col=funcol,
              debug=debug)

      iname = colnames(cleanobj$F1$thresholds)[1]
      text(w1,ppzero+pf*0.9,iname,adj=c(-0.5,0),cex=0.8,font=2)
      if(2 %in% showlab) axis(2,at=baselines+funhprop/2,labels=type, las=ylas, cex.axis=0.8)  # (0.3*funhprop):((K-1)*funhprop)



    } # end else (= mult items/one type)

 } # end of if/else classical

  segments(t0,op,w1,op,col=grey(.8))
  abline(v=w1, col=grey(.6))
  abline(h=ppzero, col=grey(.6))

 } # end funhprop < 1 
  
  
  # flexbars getrennt wegen funwprop
  if(funwprop < 1 & funhprop > 0 & length(type)==1){
    
    # flexbar start and width
    
    if(funwprop==0){
       f0 = w0
       fw = (w1-w0)*0.98
    }else {
       f0 = w1
       fw = (w2-w1)*0.98
    }
    II = c(0, cumsum(I))
     
    for(d in 1:D){
      for(i in 1:I[d]){
        ii = i + II[d]
        flexbar(cleanobj$freq[[itmnames[ii]]],
                ox=f0,
                oy=baselines[ii],
                dw=fw,
                dh=0.9*funhprop,
                # bg="white",
                bg="white",
                im=0.05,
                bo=NA, # no borders
                col=funcol,
                debug=debug)
      }# end for i
    }# end for d
    
    abline(v=w1, col=grey(0.7))
    abline(h=ppzero, col=grey(0.7))
  }# end of funwprop check

# --- add source indicator
  if (src) {
      xxx = par("usr")
      text(xxx[2],xxx[3],paste("RMX:",attr(cleanobj,"source")),cex=0.7,adj=c(1.1,1.5),xpd=TRUE)
  } # end if src
  
  box()

  if (debug) message("Leaving drawer...")

} # end fun drawer