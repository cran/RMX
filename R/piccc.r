#' RMX Rasch Modeling Extended: A Visualisation Tool for IRT Models
#'
#' Provides a customizable function \code{plotPIccc()}, which draws classical
#' PImaps (Wright-Maps) with various graphical extensions, enabling the user
#' to get quick and differentiated overview for psychometric analyses.
#'
#' @param resobj     Result object of originating package. Compatible packages
#'                   are \code{eRm}, \code{psychotools}, \code{ltm}, \code{mirt},
#'                   and \code{TAM}. Supported models are 1-4PL Models, RM,
#'                   PCM, RSM, GPCM, GRM, GRSM, and NRM.
#' @param pp         Provide previously calculated person parameter estimates
#'                   (practical if the estimation process is slow). In the
#'                   multidimensional case person parameters need to be provided
#'                   column wise in matrix form, i.e., first column = PP of
#'                   first dimension, ...).
#'                   If "auto" (default) the person parameters are determined
#'                   using the default function of the originating package.
#'                   The special option pp="skip" will omit the entire plot
#'                   and prevent the call to person parameter estimation to
#'                   speed up.
#' @param breaks     Adopted from the base R hist function. Used in the 
#'                   person parameter histogram.
#' @param type       The function type to plot. Possible options are:
#'                   \itemize{
#'                   \item CCC, the Category Characteristic Curve (default),
#'                   \item TCC, the Threshold Characteristic Curve,
#'                   \item IIF, the Item Information Function,
#'                   \item CIF, the Category Information Function, and
#'                   \item BIF, both the Category and Item information curves.
#'                   }
#'                   Multiple types may be plotted simultaneously
#'                   if only one item is selected. (\code{length(isel)==1}).
#' @param tmin       Minimum of theta range to plot.
#' @param tmax       Maximum of theta range to plot.
#' @param isel       Item selection. Select items either by their index on each
#'                   dimension or by their names. If an item is defined on
#'                   multiple dimensions, all instances will be used.
#' @param ilab       Provide item labels (names) for plotting. Has priority over
#'                   \code{isel}.
#' @param csel       Category selection. Select categories to draw by category
#'                   indices. Only numerical values are allowed.
#'                   This is merely a graphical option and has no effect on
#'                   the calculation of related constructs (IIF or TIF).
#'                   Do not combinde \code{csel} with \code{highlight}.
#' @param plab       Labels of person parameter diagramm:
#'                   \itemize{
#'                   \item \code{abs}: absolute frequencies,
#'                   \item \code{rel}: relative frequencies (in %),
#'                   \item \code{den}: density estimate values,
#'                   \item \code{none}: omit labels (default).
#'                   }
#' @param dsel       Dimension selection. Same principle as for \code{isel}.
#' @param dlab       Provide dimension labels for plotting. See \code{ilab} for
#'                   more information.
#' @param lmar       Left margin (in lines).
#' @param ylas       Specify alignment of the y-axis labels (e.g. \code{ylas=2.}
#'                   for horizontal labels).
#' @param showlab    Vector of options in the range are 0:6:
#'                   0 = show no axis labels,
#'                   1 = show outer x-axis (theta range) labels,
#'                   2 = show outer y-axis (item labels),
#'                   3 = show inner y-axis labels of item function diagrams,
#'                   4 = show y-axis labels of person parameter distribution,
#'                   5 = show y-axis labels of SE and sSE line,
#'                   6 = show y-axis labels of TIF and sTIF line,
#'                   (default: 1:3).
#'
#' @param isort      Sorting way may be specified. Possible options are
#'                   \code{mean}, \code{var}, \code{min},
#'                   \code{max}, \code{range}, \code{disc},
#'                   \code{lazy}, \code{guess}, and \code{none} (default).
#' @param gsort      Global sort. If \code{TRUE} all items will be sorted
#'                   across dimensions. If \code{FALSE} items will be sorted
#'                   within dimensions.
#' @param infomax    Vertical scaling of information curves (i.e., CIF, IIF, and
#'                   BIF). Takes either one of the keywords
#'                   \itemize{
#'                   \item \code{auto} (maximize each plot) or
#'                   \item \code{equal} (same scaling for all visible
#'                   information functions), or
#'                   \item a number indicating the maximum to be shown.
#'                   }
#' @param height     Specify the hight of the diagram window.
#' @param width      Width of the diagram (default: the golden ratio to the height).
#' @param funhprop   Proportions of the horizontal axis to be used for the
#'                   function plots. The value of 1 will show only the
#'                   functions, a value of 0 only the person parameters + legend(s).
#' @param funwprop   Proportion of the vertical axis to be used for the functions.
#'                   A value of 1 will only draw the functions and
#'                   person parameter histograms, and a value of 0 frequency
#'                   barplots.
#' @param main       Title string (e.g., for question wording or scale name).
#'                   If not NULL, the upper margin is set to 2, otherwise 0.

#' @param dimcol     Vector of dimension color for the person parameter histograms.
#' @param dencol     Vector of color for the person parameter density lines.
#' @param funcol     The color for functions and thresholds. Provide color vector
#'                   or one of the predefined color schemes
#'                   \itemize{
#'                   \item \code{Rstandard} (default),
#'                   \item \code{rainbow10} (up to 10 response categories),
#'                   \item \code{warm1} (up to 5 response categories),
#'                   \item \code{cool1} (up to 5 response categories), 
#'                   \item \code{contrast1} (up to 5 response categories) or
#'                   \item \code{colorful1} (up to 5 response categories).
#'                   }
#' @param infcol     Specify a separate color for the item information function
#'                   curve (default: \code{black}).
#' @param tifcol     Specify color of the test information function (TIF).
#' @param se_col     Specify color of the standard error function.
#' @param bgcol      Background color of function plot areas.
#' @param gridcol    Grid color in function plots.
#' @param usedimcol  Use dimension colors for threshold points (classical only).
#' @param highlight  Numerical vector containing the category numbers to be
#'                   highlighted (the others will be drawn in grey).
#'                   Do not combine \code{highlight} with \code{csel}.

#' @param dens       Draw kernel density estimate of person parameter distribution
#'                   (default: TRUE).
#' @param TIF        Draw test information function (default: TRUE).
#' @param sTIF       Draw test information function of selected items (default: TRUE).
#' @param SE         Draw standard error function (default: TRUE).
#' @param sSE        Draw standard error function of selected items (default: TRUE).
#' @param TIFmax     Maximum of test information function (default: "auto").
#' @param SEmax      Maximum of standard error function (default: "auto").

#' @param funlty     Line type of the functions, see \code{type}.
#' @param funlwd     Line width of the functions.

#' @param disind     Disordering indicator (default: \code{"*"}).
#' @param discol     Color of the threshold disorder indicator (default: "red")
#' @param discex     Size of the threshold disorder indicator (default: 1.5)

#' @param legend     If \code{TRUE} (default) insert legend.
#' @param legoff     Legend offset (vertical) from top margin (default: 0.1)
#' @param legcex     Legend text size.
#' @param leglsp     Legend line spacing.

#' @param thrpch     Threshold point character mark may be set.
#' @param thrsup     If \code{TRUE} (default) plot threshold supporting lines.

#' @param src        If \code{TRUE} write the originating package and the
#'                   estimated model in the bottom right corner.
#' @param grid       If \code{TRUE} (default) draw a grid in the function area.
#' @param classical  If \code{TRUE} plot the classical person-item map.
#' @param plot       Plot the diagram (default: \code{TRUE})
#' @param extwin     (logical) Draw diagram(s) into an external window (for
#'                   better compatibility with RStudio and drawing to an external
#'                   file (pdf, png, ...); default: \code{FALSE}).
#' @param resetpar   Reset graphics parameter (default: \code{TRUE}).
#'                   If \code{FALSE}, the diagram coordinates remain, so that
#'                   further annotations can be made to the diagram (see Details).
#' @param debug      (logical) Print call stack (might help tracking error).
#'
#' @return
#' \code{plotPIccc} returns a nested list containing:
#'       \itemize{
#'       \item one list element per dimension. Named by default \code{F1},
#'             \code{F2}, ..., but can be changed with the \code{dlab=} option.
#'       \item the element \code{freq} with the response frequencies of each
#'             item, and
#'       \item \code{sortvec} with the positions of the items as displayed.
#'       }
#' It further contains the attributes
#'       \itemize{
#'       \item \code{source}, indicating the package used for parameter
#'             estimation and the model,
#'       \item \code{pp}, a matrix with the estimated person parameters
#'             (used for avoiding re-estimation if plotPIccc is repeatedly called,
#'       \item \code{data}, the data set as returned from the originating package
#'       \item \code{theta}, the vector used for the horizontal axis.
#'       }
#'
#'
#' @details
#' See Preston & Reise (2013) for the definition and interpretation of the
#' category boundary discrimination (CBD) and threshold characteristic curves
#' (TCCs) in NRM.
#' See de Ayala (1993) for the definition of the intersections in the NRM.
#' See Samejima (1969) for the definition of the category information curve.
#' For references concerning the supported models see the originating packages'
#' documentation.
#'
#' The \code{resetpar=FALSE} option keeps the actual graphics parameters
#' used for drawing the diagram. It allows for adding further graphic
#' elements like text annotations or auxiliary lines (e.g., a vertical
#' zero-line with \code{abline(v=0)}). It should prove useful especially
#' for the horizontal axis, as the theta-scale remains active.
#' The default TRUE returns to the R-defaults (margins, scale, ...)
#'
#' Note that using one of the internal palette definitions (e.g., warm1),
#' will also set the color of the information function (infcol).
#'
#' Note for R-Studio users: Setting extwin=TRUE will allow for defining
#' the graphics window's exact size. Note further that the new graphics
#' window will open in the back of the R-Studio.
#'
#'
#' @author Milica Kabic, Rainer W. Alexandrowicz
#'
#'
#' @references
#' Preston, K. S. J. & Reise, S. P. (2013).
#'    Estimating the Nominal Response Model Under Nonnormal Conditions.
#'    Educational and Psychological Measurement, 74(3) 377--399.
#'    <doi:10.1177/0013164413507063>
#' de Ayala, R. J. (1993).
#'    An introduction to polytomous item response theory models.
#'    Measurement and Evaluation in Counseling and Development, 25(4), 172--189.
#'    <https://psycnet.apa.org/record/1993-28125-001>
#' Samejima, F. (1969).
#'    Estimation of latent ability using a response pattern of graded scores.
#'    Psychometrika 34 (Supplement 1), 1--97. <doi:10.1007/BF03372160>
#'
#'
#' @examples
#'
#' library(RMX)
#' data(big5)
#'
#'   dat_extra = big5[,c(1,6,11,16)]
#'   dat_agree = big5[,c(2,7,12,17)]
#'   dat_consc = big5[,c(3,8,13,18)]
#'   dat_neuro = big5[,c(4,9,14,19)]
#'
#' library(eRm)
#'
#'# Note: each feature is accessible with each package!
#'
#'# 1 --- PCM with eRm (Extraversion)
#'
#'  res0 = PCM(dat_extra)                    # PCM
#'
#'  plotPIccc(res0)
#'
#'# 1.1 How to select only items Q6 and Q16?
#'  plotPIccc(res0, isel=c("Q6", "Q16"))
#'  plotPIccc(res0, isel=c(2,4))
#'
#'# 1.2 Turn off the Test Item Information (TIF)
#'  plotPIccc(res0, isel=c(2,4), TIF=FALSE)
#'
#'# 1.3 Turn off the selected Test Item Information (sTIF)
#'  plotPIccc(res0, isel=c(2,4), TIF=FALSE, sTIF=FALSE)
#'
#'# 1.4 Change color of functions and thresholds
#'  plotPIccc(res0, funcol="warm1")
#'
#'# 1.5 Create classical PImap
#'  plotPIccc(res0, funcol="warm1", classical=TRUE)
#'
#'\donttest{# examples might take a few seconds
#'# 2 --- GRM with mirt (Agreeableness)
#'
#' library(mirt)
#'
#'  res1 = mirt(dat_agree, 1, "graded")      # GRM
#'
#'  plotPIccc(res1)
#'
#'# 2.1 Sort the items according to their discrimination par (\alpha)
#'  plotPIccc(res1, isort = "disc")
#'
#'# 2.2 Show category + item information Functions
#'  plotPIccc(res1, type="BIF")
#'
#'# 2.3 "Zoom in" to see how item infotmation categories behave
#'  plotPIccc(res1, type="BIF", infomax="auto")
#'
#'# 2.4 Pick your PIccc
#'# ... only item functions and barplots
#'  plotPIccc(res1, type="BIF", funhprop = 1)
#'
#'# ... only item functions
#'  plotPIccc(res1, type="BIF", funhprop = 1, funwprop = 1)
#'
#'# ... only barplots
#'  plotPIccc(res1, type="BIF", funhprop = 1, funwprop = 0)
#'
#'# ... only person parameters
#'  plotPIccc(res1, type="BIF", funhprop = 0, funwprop = 1)
#'
#'# 2.5 Customise person part of the diagram
#'  plotPIccc(res1, type="BIF", funhprop = 0, funwprop = 1, breaks=20,
#'            tmin=-3, tmax=3, se_col = 1, TIF=FALSE, src=FALSE, dimcol="#40E0D0",
#'            main="Person Parameters with SE")
#'
#'# 3 --- GPCM with TAM (Conscienciousness)
#'
#' library(TAM)
#'
#'  res2 = tam.mml.2pl(dat_consc, verbose=FALSE,,control=list(maxiter=20)) # 2PL
#'
#'  plotPIccc(res2)
#'
#'# 3.1 higlight category 3
#'  plotPIccc(res2, highlight = 3)
#'
#'# 3.2 select only category 4
#'  plotPIccc(res2, csel = 4)
#'
#'# 3.3 hide threshold points
#'  plotPIccc(res2, csel = 4, thrpch = "")
#'
#'# 3.4 show detailed analysis of Item Q3
#'  plotPIccc(res2, isel = 1, type = c("CCC", "TCC", "BIF"))
#'
#'# 4 --- NRM with mirt (Neuroticism)
#'
#'  res3 = mirt(dat_neuro, 1, "nominal")     # NRM
#'
#'  plotPIccc(res3,isel=1:2)
#'
#'# 4.1 ordinality check with NRM (CBD Parameter; see Reise et al. 2021)
#'  plotPIccc(res3, isel=3:4, type="TCC") # Discrimination parameters of TCCs = CBD!
#'
#'}

# --------------------------------------------------------------------------------------------------

plotPIccc = function(resobj,
                     pp="auto",           # 2024-07-08 [RWA]
                     breaks="Sturges",
                     type="CCC",          # funtype: any of CCC,TCC,IIF,CIF,BIF
                     tmin=-6,
                     tmax=6,
                     isel=NULL,
                     ilab=NULL,
                     plab=c("none","abs","rel","den"),
                     dsel=NULL,
                     dlab=NULL, 
                     lmar=2,              # left margin (in lines)
                     ylas=0,              # 0=vertical, 1&2=horizontal
                     showlab=c(1:3,5:6),  # 0=FALSE, 1=horizontal, 2=vertical, 3=inner, 4=PP, 5=TIF, 6=SE)
                     isort="none",
                     gsort=FALSE,
                     infomax="equal",
                     height=7,                   # vtot
                     width=height*(sqrt(5)-1)/2, # htot
                     funhprop=0.7,               # vprop
                     funwprop=(sqrt(5)-1)/2,     # hprop
                     main=NULL,
                     csel=NULL, 

                     dimcol=c(grey(0.85),"steelblue1","lightcoral","orange"     ,"lightgreen"),
                     dencol=c(grey(0.5) ,"steelblue3","indianred3","darkorange2","limegreen"), # NA for omission
                     funcol="Rstandard",
                     infcol="black",
                     tifcol="seagreen4",    # NA for omission
                     se_col="firebrick4",   # --"--
                     bgcol=grey(.90),
                     gridcol="white",
                     usedimcol=FALSE,
                     highlight=NULL,

                     dens=FALSE,
                     TIF=TRUE,
                     sTIF=TRUE,
                     SE=TRUE,
                     sSE=TRUE,
                     TIFmax="auto",
                     SEmax="auto",

                     funlty=1, 
                     funlwd=2,  

                     disind="*",          # dispch, disind, disorderd
                     discol= grey(0.2),
                     discex=1.5,

                     legend=TRUE, 
                     legoff=0.1,
                     legcex=0.7,
                     leglsp=1,

                     thrpch=16,           # threshold point size
                     thrsup=TRUE,         # threshold support lines

                     src=TRUE,
                     grid=TRUE,
                     classical=FALSE,
                     plot=TRUE,
                     extwin=NULL,
                     resetpar=TRUE,
                     debug=FALSE
                     ){


  if (is.null(main)) tmar = 0 else tmar = 2

  stopifnot("Argument 'tmax' must be larger than 'tmin'+1" = tmax > tmin+1)
  theta=seq(tmin,tmax,(tmax-tmin)/1000)

  type = toupper(type)
  stopifnot("Argument 'type' must be 'CCC','TCC','IIF','CIF', or 'BIF'" = type %in% c("CCC","TCC","IIF","CIF","BIF"))
  stopifnot("Argument 'showlab' must be any combination of 0,1,2,3,4,5,6" = showlab %in% 0:6)
  stopifnot("Argument 'gsort' must be logical." = is.logical(gsort))


  stopifnot(is.numeric(funhprop) | length(funhprop) > 1)
  if(funhprop < 0 | funhprop > 1) stop("Invalid input for funhprop! Please use only numerical input between 0 and 1.\n")

  stopifnot(is.numeric(funwprop) | length(funwprop) > 1)
  if(funwprop < 0 | funwprop > 1) stop("Invalid input for funwprop! Please use only numerical input between 0 and 1.\n")
  
  if (is.null(isel) & length(type) > 1) stop("Multiple type diagrams require an item specification (isel).")
  if (length(type) > 1 & (length(isel) > 1 | length(dsel) > 1)) stop("Multiple type diagrams only for one item and one dimension possible.")

  if (!is.null(isel) & !is.null(dsel)) {
      warning("Both 'isel' and 'dsel' were set. Only 'dsel' will be used.")
      isel = NULL
  }

  if (is.null(extwin)) {
      if (.Platform$GUI == "RStudio") extwin=FALSE else extwin=TRUE
  }
  
  if(!is.null(csel)) thrsup=FALSE


 # if (TIF  && is.na(tifcol)) stop("Please provide a valid TIF color")
 # if (SE   && is.na(se_col)) stop("Please provide a valid SE color")
 # if (sTIF && is.na(tifcol)) stop("Please provide a valid TIF color")
 # if (sSE  && is.na(se_col)) stop("Please provide a valid SE color")

  if(is.null(isel)){
     sTIF = FALSE
     sSE  = FALSE
  }

  if (!infomax %in% c("auto","equal") & !is.numeric(infomax)) stop(paste("The infomax= option requires one of the keywords 'auto' or 'equal' or a valid numeric maximum:",infomax))

# nix = is.null(dev.list())           # no window open
# orig.par = par(no.readonly = TRUE)  # opens a window
# orig.pal = palette()
# if (nix) dev.off()                  # dirty hack (see https://stackoverflow.com/questions/24850788/)
                                      # r-preventing-par-from-opening-a-new-window-when-querying-for-grahical-paramet?noredirect=1&lq=1

  if (is.character(pp) && pp=="skip") funhprop = 1
  if (classical)  funwprop = 1

  if (funcol[1] == "Rstandard") {
      palette("R4")
      usecol = palette()[2:8]
  } else if (funcol[1] == "rainbow10") {
      usecol = rainbow(10)
      infcol = "black"
  } else if (funcol[1] == "warm1") {
      usecol = c("#ffd438", "#f3a94b", "#e77e5e", "#db5270", "#cf2783")
      infcol = "black"
  } else if (funcol[1] == "cool1") {
      usecol = c("#79fcff", "#6addf9", "#5cbef3", "#4d9eed", "#3e7fe7")
      infcol = "black"
  } else if (funcol[1] == "contrast1") {
      usecol = c("#fbff34", "#edc461", "#df898e", "#d04ebb", "#c213e8")
      infcol = "black"
  } else if (funcol[1] == "colorful1"){
      usecol = c("dodgerblue", "chartreuse3", "gold1", "chocolate3", "darkred")
      infcol = "black"
  } else {
      usecol = funcol
      infcol = infcol
  }
  
  if(!is.null(highlight)) stopifnot(is.numeric(highlight))
#  if(!is.null(csel) & (type=="TCC"| type=="IIF")) csel=NULL

  extobj = decider(resobj, pp=pp, debug=debug)

  clnobj = cleaner(extobj, isel=isel, ilab=ilab, dsel=dsel, dlab=dlab,
                           csel=csel, how_sort=isort, tot_sort=gsort, 
                           theta=theta, highlight=highlight, #ngroups=ngroups,
                           debug=debug)

  if(plot) drawer(clnobj, theta=theta,type=type,height=height,width=width,
                          funhprop=funhprop,funwprop=funwprop,funcol=usecol,
                          infcol=infcol,funlty=funlty,funlwd=funlwd,bg=bgcol,
                          legend=legend,grid=grid,classical=classical,
                          thrsup=thrsup,thrpch=thrpch,src=src,pplab=plab,
                          dcol=dimcol,legcex=legcex,leglsp=leglsp,legoff=legoff,
                          disind=disind,discol=discol,lmar=lmar,tmar=tmar,main=main,
                          ylas=ylas,tifcol=tifcol,se_col=se_col,showlab=showlab,
                          dencol=dencol,gridcol=gridcol,usedimcol=usedimcol,
                          dens=dens,TIF=TIF,sTIF=sTIF,SE=SE,sSE=sSE,SEmax=SEmax,
                          TIFmax=TIFmax,infomax=infomax,extwin=extwin,debug=debug,
                          discex=discex,csel=csel, highlight=highlight,
                         # empirical=empirical,show.size=show.size, ngroups=ngroups,
                          resetpar=resetpar,breaks=breaks)

  return(invisible(clnobj))

} # end of fun plotPIccc

