
  flexbar = function(dat,
                     col,
                     type=c("freq","raw","indiv"),   # dat is freq count or raw
                     ox = 0.5,                       # offset x (diagram left)
                     oy = 0.5,                       # offset y (diagram bottom)
                     dw = 4,                         # diagram width
                     dh = 3,                         # diagram height
                     im = 0.1,                       # inner margin (%)
                     bd = 0.05,                      # distance between bars
                     bo = 2,                         # bar borders width (NA=no border)
                     w0 = dw*(1-2*im),               # effective witdh of baseline
                     bg = rgb(0.7,0.7,0.8),          # background color
                     pf = TRUE,                      # print abs+rel freq & cats?
                     rf = FALSE,                     # relative frequencies?
                     na = c("no","ifany", "always"), # NA handling (table!)
                     debug=TRUE,
                     ...) {

            if (debug) message("Entering flexbar...")

            count = function(x,useNA,bins=20) {
                    xmin = min(x,na.rm=TRUE)
                    xmax = max(x,na.rm=TRUE)
                    xseq = seq(xmin,xmax,length.out=bins)
                    xfrq = table(cut(x,xseq))
            } # end fun count

            if (!is.table(dat)) {
                str(dat)
                stop("Frequency table required.")
            } # end if not table


            if (sum(c(TRUE,"yes") %in% na) > 0) na = "ifany"

                 if (type[1]=="freq")  t=dat
            else if (type[1]=="raw")   t=table(dat,useNA=na)
            else if (type[1]=="indiv") t=count(dat,useNA=na)
            else stop("Invalid type for flexbar!")

            f = as.numeric(t)
            n = sum(f)
            c = dimnames(t)$dat
            k = length(t)

            if (NA %in% c) c[which(is.na(c))] = "NA"
            if (rf) fx=paste(round(100*f/n,1),"%")
            if (missing(col)) col = 1:k+1

            bw = (w0 - bd*(k+1)) / k                 # bar width
            x0 = ox + im*dw + bd + 0:(k-1)*(bw+bd)   # bars left limits
            x1 = x0 + bw                             # bars right limits
            y0 = oy + dh*im                          # bars baseline

            unit = (dh/max(f))*(1-3*im)              # height unit
            bh = unit*f                              # bar heights
            bm = x0 + bw/2                           # bar midpoints

            rect(ox,oy,ox+dw,oy+dh,col=bg,border=NA) # background box
            segments(ox+im*dw,y0,ox+(1-im)*dw,y0,...)# bottom axis (x)
            rect(x0,y0,x1,y0+bh,lwd=bo,col=col,...)  # bars
            if (pf) {
#               text(bm,y0   ,c ,adj=c(0.5, 1.5),font=2,cex=0.8,...)
                text(bm,y0+bh,f ,adj=c(0.5,-0.5),font=2,cex=0.7,...)
        if (rf) text(bm,y0+bh,fx,adj=c(0.5, 1.5),font=3,cex=0.6,...)
#               text(ox+dw*0.05,ox+dh*(1-0.05),paste("n =",n),font=2,cex=1.1,adj=c(0,1))
            }

            res = list(midpoints=bm,x0=x0,y0=y0,x1=x1,y1=y0+bh,freq=f,relfreq=f/n)

            if (debug) message("Leaving flexbar...")

            return(invisible(res))

  } # end fun flexbar

  