#' Calculate the corresponding y value of arc based on x which input
#'
#' @description
#' A function to calculate the corresponding y value for the output of ArcPlot function based on the x input.
#' @param x numeric, the x value of the point you want to get.
#' @param arcplot list, returned by the ArcPlot function.
#'
#' @author Yi Li  \email{gugeliyiyi@gmail.com; https://github.com/liyi522/ArcPlot/}
#' @return The vector of corresponding y values (1 or 2 numeric variables).
#' A list named 'curve_expr' including: information of this arc,
#' @examples
#' plot(0,0,col="white",bty="o",xlim=c(-10,10),ylim=c(-10,10))
#' p1 <- c(-7,5)
#' p2 <- c(-3,5)
#' points(c(p1[1],p2[1]),c(p1[2],p2[2]))
#' ArcPlot(p1,p2,convex = TRUE,return=FALSE,angle = 90)
#' m <- ArcPlot(p1,p2,convex = FALSE,angle = 200)
#' points(-5,CalArcY(-5,m),col="red")
#' @name  CalArcY
#' @export
#'
CalArcY <- function(x=NA,arcplot=list()){
  judgeExist <- function(x,mat){
    rangeExist <- c()
    for(i in 1:nrow(mat)){
      if(x <= max(mat[i,]) & x >= min(mat[i,])){
        rangeExist <- c(rangeExist,1)
      }else{
        rangeExist <- c(rangeExist,0)
      }
    }
    return(rangeExist)
  }
  if(is.na(x)|length(arcplot)==0){
    print("Warnings: the value of x and the list which returned by ArcPlot are must been input.")
  }else{
    JE <- judgeExist(x,arcplot$xrange)
    if(sum(JE)>=1){
      ys <- c()
      for(j in which(JE==1)){
        current_expr <- arcplot$exprs[j]
        radius <- arcplot$radius
        cc_current <- arcplot$cc_current
        x <- x
        ys <- c(ys,eval(parse(text=current_expr)))
      }
      return(ys)
    }else{
      print("Error: the value of x you input is beyond all the x values of the arc you input.")
    }
  }
}
