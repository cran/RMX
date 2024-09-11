#' BIG 5 Example Data Set
#'
#' Description: A data set simulated after a students survey of the BIG 5 
#' (Openness [O], Conscienciousness [C], Extraversion [E], Agreeableness [A], 
#' and Neuroticism [N]). The scale comprises 21 items following the structure 
#' of the BFIK (DOI 10.1026/0012-1924.51.4.195).
#' @name big5
#' @docType data
#' @format A data.frame with 1076 rows and 21 columns.
#' @details
#' The items were originally presented in German. Here is
#' a rough translation (untested):
#' \describe{
#' \item{Q1R }{[E] I ... am rather reserved.}
#' \item{Q2R }{[A] I ... tend to criticize others.}
#' \item{Q3  }{[C] I ... complete tasks thoroughly.}
#' \item{Q4  }{[N] I ... get depressed easily, depressed.}
#' \item{Q5  }{[O] I ... am interested in many things.}
#' \item{Q6  }{[E] I ... am enthusiastic and can easily carry others away.}
#' \item{Q7  }{[A] I ... trust others easily, believe in the good in people.}
#' \item{Q8R }{[C] I ... am comfortable, tend to laziness.}
#' \item{Q9R }{[N] I ... am relaxed, do not let stress upset me.}
#' \item{Q10 }{[O] I ... am profound, like to think about things.}
#' \item{Q11R}{[E] I ... am rather the 'quiet type', taciturn.}
#' \item{Q12R}{[A] I ... can act cold and distant.}
#' \item{Q13 }{[C] I ... am efficient and work briskly.}
#' \item{Q14 }{[N] I ... worry a lot.}
#' \item{Q15 }{[O] I ... have an active imagination, am imaginative.}
#' \item{Q16 }{[E] I ... go out of myself, am sociable.}
#' \item{Q17R}{[A] I ... can be brusque and dismissive towards others.}
#' \item{Q18 }{[C] I ... make plans and carry them out.}
#' \item{Q19 }{[N] I ... get nervous and insecure easily.}
#' \item{Q20 }{[O] I ... appreciate artistic and aesthetic impressions.}
#' \item{Q21R}{[O] I ... have little artistic interest.}
#' }
#' The response format of all items was:
#' \itemize{
#' \item very inapplicable 
#' \item rather inapplicable 
#' \item neither-nor 
#' \item rather true  
#' \item very true
#' }
#'@examples
#'  library(RMX)
#'  summary(big5)
 "big5"