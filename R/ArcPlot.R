#' Ploting or adding the arc connected two points
#'
#' @description
#' A generic function ploting or adding the arc linking two points.
#' @param p1 a coordinate (x,y) vectors of one point.
#' @param p2 a coordinate (x,y) vectors of the other point.
#' @param convex logical value indicating the convex arc (up/right) or concave arc(down/left).
#' @param return logical value indicating the list was returned or not.
#' @param angle numeric (<90) indicating the degree of the central Angle corresponding to the arc,.
#' @param add logical value indicating adding this arc to currents plot or not.
#' @param radian numeric indicating the radian of arc you need to plot, default is NA.
#'  It will cover the value of angle.(if you do not want to supply the angle of arc).
#' @param ... Further graphical parameters may also be supplied as arguments, including, line type, lty, line width, lwd, color, col.
#'
#' @author Yi Li  \email{gugeliyiyi@gmail.com; https://github.com/liyi522/ArcPlot/}
#' @return Adding the arc to current plot or plot the arc in new device.
#' A list named 'curve_expr' including: information of this arc,
#' cc_current(vector of numeric), the coordinates of center of circle,
#' radius(numeric), the radius of circle
#' exprs(vector of characters), the expression of piecewise function of this arc
#' xrange(dataframe), the x range of the corresponding piecewise function (per row means the range).
#'
#' @examples
#' #example 1
#' plot(0,0,col="white",bty="o",xlim=c(-10,10),ylim=c(-10,10))
#' p1 <- c(-7,5)
#' p2 <- c(-3,5)
#' points(c(p1[1],p2[1]),c(p1[2],p2[2]))
#' ArcPlot(p1,p2,convex = TRUE,angle = 90)
#' m <- ArcPlot(p1,p2,convex = FALSE,return=TRUE,angle = 200)
#' points(-5,CalArcY(-5,m),col="red")
#' 
#' #example 2
#' #cat data
#' cat_da <- data.frame(c(0,0,-1,1,-2,-1,1,2,-2,-3,-1.5,1.5,3,2,-3,0,3,0),
#'   c(0.5,-0.5,-0.5,-0.5,1.5,1.5,1.5,1.5,2.5,4,3,3,4,2.5,0,3.6,0,-2.5))
#' plot(0,0,bty="o",xlim=c(-5,5),ylim=c(-5,5),col="white")
#' #nose
#' ArcPlot(c(cat_da[1,1],cat_da[1,2]),c(cat_da[2,1],cat_da[2,2]),convex = TRUE,angle = 360)
#' ##mouse
#' ArcPlot(c(cat_da[2,1],cat_da[2,2]),c(cat_da[3,1],cat_da[3,2]),convex = FALSE)
#' ArcPlot(c(cat_da[2,1],cat_da[2,2]),c(cat_da[4,1],cat_da[4,2]),convex = FALSE)
#' #eye
#' ArcPlot(c(cat_da[5,1],cat_da[5,2]),c(cat_da[6,1],cat_da[6,2]),convex = TRUE)
#' ArcPlot(c(cat_da[7,1],cat_da[7,2]),c(cat_da[8,1],cat_da[8,2]),convex = TRUE)
#' #outline
#' left_face <- ArcPlot(c(cat_da[15,1],cat_da[15,2]),c(cat_da[16,1],cat_da[16,2]),convex = TRUE,return=TRUE)
#' right_face <- ArcPlot(c(cat_da[16,1],cat_da[16,2]),c(cat_da[17,1],cat_da[17,2]),convex = TRUE,return=TRUE)
#' #left ear
#' ArcPlot(c(cat_da[9,1],CalArcY(cat_da[9,1],left_face)),c(cat_da[10,1],cat_da[10,2]),convex = FALSE)
#' ArcPlot(c(cat_da[9,1]-0.5,CalArcY(cat_da[9,1]-0.5,left_face)),c(cat_da[10,1],cat_da[10,2]),convex = FALSE)
#' ArcPlot(c(cat_da[10,1],cat_da[10,2]),c(cat_da[11,1],CalArcY(cat_da[11,1],left_face)),convex = TRUE)
#' #right ear
#' ArcPlot(c(cat_da[12,1],CalArcY(cat_da[12,1],right_face)),c(cat_da[13,1],cat_da[13,2]),convex = TRUE)
#' ArcPlot(c(cat_da[13,1],cat_da[13,2]),c(cat_da[14,1],CalArcY(cat_da[14,1],right_face)),convex = FALSE)
#' ArcPlot(c(cat_da[13,1],cat_da[13,2]),c(cat_da[14,1]+0.5,CalArcY(cat_da[14,1]+.5,right_face)),convex = FALSE)
#' #outline
#' ArcPlot(c(cat_da[15,1],cat_da[15,2]),c(cat_da[18,1],cat_da[18,2]),convex = FALSE)
#' ArcPlot(c(cat_da[17,1],cat_da[17,2]),c(cat_da[18,1],cat_da[18,2]),convex = FALSE)
#' #hair
#' lines(c(0,0),c(CalArcY(0,left_face),2.5))
#' lines(c(-0.5,-0.5),c(CalArcY(-.5,left_face),2.5))
#' lines(c(0.5,0.5),c(CalArcY(.5,right_face),2.5))
#' #beard
#' lines(c(-1.5,-4),c(0,0.6))
#' lines(c(1.5,4),c(0,0.6))
#' lines(c(-1.5,-4),c(-0.4,-0.6))
#' lines(c(1.5,4),c(-0.4,-0.6))
#' lines(c(-1.5,-4),c(-0.8,-1.8))
#' lines(c(1.5,4),c(-0.8,-1.8))
#'
#' @name  ArcPlot
#' @export

ArcPlot <- function(p1,p2,convex=TRUE,return=FALSE,angle=90,add = TRUE,radian=NA,col = "black",lty = 1,lwd = 1){
  if(!is.na(radian)){
    angle <- 180*radian/pi
  }
  if(angle <=0 | angle >360){
    print("Warning: the angle you input is not on the 0 to 360")
  }else if(angle==360){
    Arch5(p1,p2,add_info = add,return=return,col_c= col,lty_c= lty,lwd_c= lwd)
  }else{
    if(p1[2] > p2[2]){
      a <- p1
      p1 <- p2
      p2 <- a
    }
    cc <- CalCC(p1,p2,angle_tmp = angle)
    judge_info <- JudgeCase(p1,p2,cc)
    if(judge_info$case==1){
      if(angle <= 180){
        Arch1_1(p1,p2,convex_tmp=convex,cc,judge_info,return=return,add_info = add,col_c= col,lty_c= lty,lwd_c= lwd)
      }else{
        Arch1_2(p1,p2,convex_tmp=convex,cc,judge_info,return=return,add_info = add,col_c= col,lty_c= lty,lwd_c= lwd)
      }
    }else if(judge_info$case==2){
      if(angle <= 180){
        Arch2_1(p1,p2,convex_tmp=convex,cc,judge_info,return=return,add_info = add,col_c= col,lty_c= lty,lwd_c= lwd)
      }else{
        Arch2_2(p1,p2,convex_tmp=convex,cc,judge_info,return=return,add_info = add,col_c= col,lty_c= lty,lwd_c= lwd)
      }
    }else if(judge_info$case==3){
      Arch3(p1,p2,convex_tmp=convex,angle = angle,cc,return=return,judge_info,add_info = add,col_c= col,lty_c= lty,lwd_c= lwd)
    }else{
      Arch4(p1,p2,convex_tmp=convex,angle = angle,cc,return=return,judge_info,add_info = add,col_c= col,lty_c= lty,lwd_c= lwd)
    }
  }
}
CalCC <- function(p1,p2,angle_tmp=90){
  radian <- angle_tmp/180*pi/2
  cc1 <- c((p1[1]+p2[1])/2+(p2[2]-p1[2])/(2*tan(radian)),(p1[2]+p2[2])/2+(p1[1]-p2[1])/(2*tan(radian)))
  cc2 <- c((p1[1]+p2[1])/2+(p1[2]-p2[2])/(2*tan(radian)),(p1[2]+p2[2])/2+(p2[1]-p1[1])/(2*tan(radian)))
  radius <- sqrt((cc1[1]-p1[1])^2+(cc1[2]-p1[2])^2)
  k <- -(p2[1]-p1[1])/(p2[2]-p1[2])
  y <- p1[2]
  x <- (y-(p1[2]+p2[2])/2)/k+(p1[1]+p2[1])/2
  p_now <- c(x,y)
  tan1 <- sqrt((p1[1]-(p1[1]+p2[1])/2)^2+(p1[2]-(p1[2]+p2[2])/2)^2)/sqrt((p_now[1]-(p1[1]+p2[1])/2)^2+(p_now[2]-(p1[2]+p2[2])/2)^2)
  radian_judge <- 2*atan(tan1)
  angle_judge <- 180*radian_judge/pi
  cinfo <- list()
  if(cc1[2]<= cc2[2]){
    cinfo$cc1 <- cc1
    cinfo$cc2 <- cc2
  }else{
    cinfo$cc1 <- cc2
    cinfo$cc2 <- cc1
  }
  cinfo$radius <- radius
  cinfo$angle_judge <- angle_judge
  cinfo$p_now <- p_now
  return(cinfo)
}
JudgeCase <- function(p1,p2,cc){
  judgeRe <- list()
  cc1info <- c(cc$cc1[1]-cc$radius,cc$cc1[1]+cc$radius)
  cc2info <- c(cc$cc2[1]-cc$radius,cc$cc2[1]+cc$radius)
  if(p1[2]==p2[2]){
    judgeRe$case <- 1
    judgeRe$cc1info <- cc1info
    judgeRe$cc2info <- cc2info
    return(judgeRe)
  }else if(p1[1]==p2[1]){
    judgeRe$case <- 2
    judgeRe$cc1info <- cc1info
    judgeRe$cc2info <- cc2info
    return(judgeRe)
  }else if(p1[1]<p2[1]){
    judgeRe$case <- 3
    judgeRe$cc1info <- cc1info
    judgeRe$cc2info <- cc2info
    return(judgeRe)
  }else{
    judgeRe$case <- 4
    judgeRe$cc1info <- cc1info
    judgeRe$cc2info <- cc2info
    return(judgeRe)
  }
}

Arch1_1 <- function(p1,p2,convex_tmp=TRUE,cc,judge_info,return=TRUE,add_info = TRUE,col_c="black",lty_c=1,lwd_c=1){
  curve_expr <- list()
  radius <- cc$radius
  if(convex_tmp){
    cc_current <- cc$cc1
    curve(sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(p1[1],p2[1]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
    exprs <- c("y=sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]")
    xrange <- matrix(c(p1[1],p2[1]),ncol = 2,nrow = length(exprs),byrow = T)
    curve_expr$cc_current <- cc_current
    curve_expr$radius <- radius
    curve_expr$exprs <- exprs
    curve_expr$xrange <- xrange
    if(return){return(curve_expr)}
  }else{
    cc_current <- cc$cc2
    curve(-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(p1[1],p2[1]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
    exprs <- c("y=-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]")
    xrange <- matrix(c(p1[1],p2[1]),ncol = 2,nrow = length(exprs),byrow = T)
    curve_expr$cc_current <- cc_current
    curve_expr$radius <- radius
    curve_expr$exprs <- exprs
    curve_expr$xrange <- xrange
    if(return){return(curve_expr)}
  }
}
Arch1_2 <- function(p1,p2,convex_tmp=TRUE,cc,judge_info,return=TRUE,add_info = TRUE,col_c="black",lty_c=1,lwd_c=1){
  radius <- cc$radius
  curve_expr <- list()
  if(convex_tmp){
    cc_current <- cc$cc2
    fourXs <- sort(c(judge_info$cc1info,p1[1],p2[1]))
    curve(sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(fourXs[1],fourXs[4]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
    curve(-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(fourXs[1],fourXs[2]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
    curve(-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(fourXs[3],fourXs[4]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
    exprs <- c("y=sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]","y=-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]","y=-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]")
    xrange <- matrix(c(fourXs[1],fourXs[4],fourXs[1],fourXs[2],fourXs[3],fourXs[4]),ncol = 2,nrow = length(exprs),byrow = T)
    curve_expr$cc_current <- cc_current
    curve_expr$radius <- radius
    curve_expr$exprs <- exprs
    curve_expr$xrange <- xrange
    if(return){return(curve_expr)}
  }else{
    cc_current <- cc$cc1
    fourXs <- sort(c(judge_info$cc2info,p1[1],p2[1]))
    curve(-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(fourXs[1],fourXs[4]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
    curve(sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(fourXs[1],fourXs[2]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
    curve(sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(fourXs[3],fourXs[4]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
    exprs <- c("y=-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]","y=sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]","y=sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]")
    xrange <- matrix(c(fourXs[1],fourXs[4],fourXs[1],fourXs[2],fourXs[3],fourXs[4]),ncol = 2,nrow = length(exprs),byrow = T)
    curve_expr$cc_current <- cc_current
    curve_expr$radius <- radius
    curve_expr$exprs <- exprs
    curve_expr$xrange <- xrange
    if(return){return(curve_expr)}
  }
}
Arch2_1 <- function(p1,p2,convex_tmp=TRUE,cc,judge_info,return=TRUE,add_info = TRUE,col_c="black",lty_c=1,lwd_c=1){
  radius <- cc$radius
  curve_expr <- list()
  if(convex_tmp){
    cc_current <- c(min(c(cc$cc1[1],cc$cc2[1])),cc$cc2[2])
    curve(sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(p2[1],cc_current[1]+radius),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
    curve(-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(p2[1],cc_current[1]+radius),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
    exprs <- c("y=sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]","y=-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]")
    xrange <- matrix(c(p2[1],cc_current[1]+radius,p2[1],cc_current[1]+radius),ncol = 2,nrow = length(exprs),byrow = T)
    curve_expr$cc_current <- cc_current
    curve_expr$radius <- radius
    curve_expr$exprs <- exprs
    curve_expr$xrange <- xrange
    if(return){return(curve_expr)}

  }else{
    cc_current <- c(max(c(cc$cc1[1],cc$cc2[1])),cc$cc2[2])
    curve(sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(p2[1],cc_current[1]-radius),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
    curve(-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(p2[1],cc_current[1]-radius),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
    exprs <- c("y=sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]","y=-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]")
    xrange <- matrix(c(p2[1],cc_current[1]-radius,p2[1],cc_current[1]-radius),ncol = 2,nrow = length(exprs),byrow = T)
    curve_expr$cc_current <- cc_current
    curve_expr$radius <- radius
    curve_expr$exprs <- exprs
    curve_expr$xrange <- xrange
    if(return){return(curve_expr)}
  }
}
Arch2_2 <- function(p1,p2,convex_tmp=TRUE,cc,judge_info,return=TRUE,add_info = TRUE,col_c="black",lty_c=1,lwd_c=1){
  radius <- cc$radius
  curve_expr <- list()
  if(convex_tmp){
    cc_current <- c(max(c(cc$cc1[1],cc$cc2[1])),cc$cc1[2])
    curve(sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(p2[1],cc_current[1]+radius),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
    curve(-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(p2[1],cc_current[1]+radius),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
    exprs <- c("y=sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]","y=-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]")
    xrange <- matrix(c(p2[1],cc_current[1]+radius,p2[1],cc_current[1]+radius),ncol = 2,nrow = length(exprs),byrow = T)
    curve_expr$cc_current <- cc_current
    curve_expr$radius <- radius
    curve_expr$exprs <- exprs
    curve_expr$xrange <- xrange
    if(return){return(curve_expr)}

  }else{
    cc_current <- c(min(c(cc$cc1[1],cc$cc2[1])),cc$cc2[2])
    curve(sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(p2[1],cc_current[1]-radius),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
    curve(-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(p2[1],cc_current[1]-radius),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
    exprs <- c("y=sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]","y=-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]")
    xrange <- matrix(c(p2[1],cc_current[1]-radius,p2[1],cc_current[1]-radius),ncol = 2,nrow = length(exprs),byrow = T)
    curve_expr$cc_current <- cc_current
    curve_expr$radius <- radius
    curve_expr$exprs <- exprs
    curve_expr$xrange <- xrange
    if(return){return(curve_expr)}
  }
}
Arch3 <- function(p1,p2,convex_tmp=TRUE,angle=90,cc,judge_info,return=TRUE,add_info = TRUE,col_c="black",lty_c=1,lwd_c=1){
  radius <- cc$radius
  curve_expr <- list()
  if(angle <= cc$angle_judge){
    if(convex_tmp){
      cc_current <- cc$cc1
      curve(sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(p1[1],p2[1]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
      exprs <- c("y=sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]")
      xrange <- matrix(c(p1[1],p2[1]),ncol = 2,nrow = length(exprs),byrow = T)
      curve_expr$cc_current <- cc_current
      curve_expr$radius <- radius
      curve_expr$exprs <- exprs
      curve_expr$xrange <- xrange
      if(return){return(curve_expr)}
    }else{
      cc_current <- cc$cc2
      curve(-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(p1[1],p2[1]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
      exprs <- c("y=-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]")
      xrange <- matrix(c(p1[1],p2[1]),ncol = 2,nrow = length(exprs),byrow = T)
      curve_expr$cc_current <- cc_current
      curve_expr$radius <- radius
      curve_expr$exprs <- exprs
      curve_expr$xrange <- xrange
      if(return){return(curve_expr)}
    }
  }else if(angle >= (360-cc$angle_judge)){
    if(convex_tmp){
      cc_current <- cc$cc2
      fourXs <- sort(c(judge_info$cc2info,p1[1],p2[1]))
      curve(sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(fourXs[1],fourXs[4]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
      curve(-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(fourXs[3],fourXs[4]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
      curve(-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(fourXs[1],fourXs[2]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
      exprs <- c("y=sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]","y=-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]","y=-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]")
      xrange <- matrix(c(fourXs[1],fourXs[4],fourXs[3],fourXs[4],fourXs[1],fourXs[2]),ncol = 2,nrow = length(exprs),byrow = T)
      curve_expr$cc_current <- cc_current
      curve_expr$radius <- radius
      curve_expr$exprs <- exprs
      curve_expr$xrange <- xrange
      if(return){return(curve_expr)}

    }else{
      cc_current <- cc$cc1
      fourXs <- sort(c(judge_info$cc1info,p1[1],p2[1]))
      curve(-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(fourXs[1],fourXs[4]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
      curve(sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(fourXs[1],fourXs[2]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
      curve(sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(fourXs[4],fourXs[3]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
      exprs <- c("y=-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]","y=sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]","y=sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]")
      xrange <- matrix(c(fourXs[1],fourXs[4],fourXs[1],fourXs[2],fourXs[3],fourXs[4]),ncol = 2,nrow = length(exprs),byrow = T)
      curve_expr$cc_current <- cc_current
      curve_expr$radius <- radius
      curve_expr$exprs <- exprs
      curve_expr$xrange <- xrange
      if(return){return(curve_expr)}

    }
  }else{
    if(convex_tmp){
      if(angle <= 180){
        cc_current <- cc$cc1
        fourXs <- sort(c(judge_info$cc1info,p1[1],p2[1]))
      }else{
        cc_current <- cc$cc2
        fourXs <- sort(c(judge_info$cc2info,p1[1],p2[1]))
      }
      curve(sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(fourXs[1],fourXs[3]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
      curve(-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(fourXs[1],fourXs[2]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
      exprs <- c("y=sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]","y=-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]")
      xrange <- matrix(c(fourXs[1],fourXs[3],fourXs[1],fourXs[2]),ncol = 2,nrow = length(exprs),byrow = T)
      curve_expr$cc_current <- cc_current
      curve_expr$radius <- radius
      curve_expr$exprs <- exprs
      curve_expr$xrange <- xrange
      if(return){return(curve_expr)}

    }else{
      if(angle <= 180){
        cc_current <- cc$cc2
        fourXs <- sort(c(judge_info$cc2info,p1[1],p2[1]))
      }else{
        cc_current <- cc$cc1
        fourXs <- sort(c(judge_info$cc1info,p1[1],p2[1]))
      }
      curve(-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(fourXs[2],fourXs[4]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
      curve(sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(fourXs[3],fourXs[4]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
      exprs <- c("y=-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]","y=sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]")
      xrange <- matrix(c(fourXs[2],fourXs[4],fourXs[3],fourXs[4]),ncol = 2,nrow = length(exprs),byrow = T)
      curve_expr$cc_current <- cc_current
      curve_expr$radius <- radius
      curve_expr$exprs <- exprs
      curve_expr$xrange <- xrange
      if(return){return(curve_expr)}
    }
  }
}
Arch4 <- function(p1,p2,convex_tmp=TRUE,angle=90,cc,judge_info,return=TRUE,add_info = TRUE,col_c="black",lty_c=1,lwd_c=1){
  radius <- cc$radius
  curve_expr <- list()
  if(angle <= cc$angle_judge){
    if(convex_tmp){
      cc_current <- cc$cc1
      curve(sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(p1[1],p2[1]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
      exprs <- c("y=sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]")
      xrange <- matrix(c(p1[1],p2[1]),ncol = 2,nrow = length(exprs),byrow = T)
      curve_expr$cc_current <- cc_current
      curve_expr$radius <- radius
      curve_expr$exprs <- exprs
      curve_expr$xrange <- xrange
      if(return){return(curve_expr)}

    }else{
      cc_current <- cc$cc2
      curve(-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(p1[1],p2[1]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
      exprs <- c("y=-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]")
      xrange <- matrix(c(p1[1],p2[1]),ncol = 2,nrow = length(exprs),byrow = T)
      curve_expr$cc_current <- cc_current
      curve_expr$radius <- radius
      curve_expr$exprs <- exprs
      curve_expr$xrange <- xrange
      if(return){return(curve_expr)}
    }
  }else if(angle >= (360-cc$angle_judge)){
    if(convex_tmp){
      cc_current <- cc$cc2
      fourXs <- sort(c(judge_info$cc2info,p1[1],p2[1]))
      curve(sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(fourXs[1],fourXs[4]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
      curve(-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(fourXs[3],fourXs[4]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
      curve(-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(fourXs[1],fourXs[2]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
      exprs <- c("y=sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]","y=-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]","y=-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]")
      xrange <- matrix(c(fourXs[1],fourXs[4],fourXs[3],fourXs[4],fourXs[1],fourXs[2]),ncol = 2,nrow = length(exprs),byrow = T)
      curve_expr$cc_current <- cc_current
      curve_expr$radius <- radius
      curve_expr$exprs <- exprs
      curve_expr$xrange <- xrange
      if(return){return(curve_expr)}

    }else{
      cc_current <- cc$cc1
      fourXs <- sort(c(judge_info$cc1info,p1[1],p2[1]))
      curve(-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(fourXs[1],fourXs[4]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
      curve(sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(fourXs[1],fourXs[2]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
      curve(sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(fourXs[4],fourXs[3]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
      exprs <- c("y=-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]","y=sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]","y=sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]")
      xrange <- matrix(c(fourXs[1],fourXs[4],fourXs[1],fourXs[2],fourXs[3],fourXs[4]),ncol = 2,nrow = length(exprs),byrow = T)
      curve_expr$cc_current <- cc_current
      curve_expr$radius <- radius
      curve_expr$exprs <- exprs
      curve_expr$xrange <- xrange
      if(return){return(curve_expr)}

    }
  }else{
    if(convex_tmp){
      if(angle <= 180){
        cc_current <- cc$cc1
        fourXs <- sort(c(judge_info$cc1info,p1[1],p2[1]))
      }else{
        cc_current <- cc$cc2
        fourXs <- sort(c(judge_info$cc2info,p1[1],p2[1]))
      }
      curve(sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(fourXs[2],fourXs[4]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
      curve(-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(fourXs[3],fourXs[4]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
      exprs <- c("y=sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]","y=-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]")
      xrange <- matrix(c(fourXs[2],fourXs[4],fourXs[3],fourXs[4]),ncol = 2,nrow = length(exprs),byrow = T)
      curve_expr$cc_current <- cc_current
      curve_expr$radius <- radius
      curve_expr$exprs <- exprs
      curve_expr$xrange <- xrange
      if(return){return(curve_expr)}
    }else{
      if(angle <= 180){
        cc_current <- cc$cc2
        fourXs <- sort(c(judge_info$cc2info,p1[1],p2[1]))
      }else{
        cc_current <- cc$cc1
        fourXs <- sort(c(judge_info$cc1info,p1[1],p2[1]))
      }
      curve(-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(fourXs[1],fourXs[3]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
      curve(sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(fourXs[1],fourXs[2]),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
      exprs <- c("y=-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]","y=sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]")
      xrange <- matrix(c(fourXs[1],fourXs[3],fourXs[1],fourXs[2]),ncol = 2,nrow = length(exprs),byrow = T)
      curve_expr$cc_current <- cc_current
      curve_expr$radius <- radius
      curve_expr$exprs <- exprs
      curve_expr$xrange <- xrange
      if(return){return(curve_expr)}
    }
  }
}
Arch5 <- function(p1,p2,return=TRUE,add_info = TRUE,col_c="black",lty_c=1,lwd_c=1){
  curve_expr <- list()
  cc_current <- c(sum(p1[1],p2[1])/2,sum(p1[2],p2[2])/2)
  radius <- sqrt((p1[1]-p2[1])^2+(p1[2]-p2[2])^2)/2
  curve(sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(cc_current[1]-radius,cc_current[1]+radius),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
  curve(-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2],xlim=c(cc_current[1]-radius,cc_current[1]+radius),add = add_info,col=col_c,lty=lty_c,lwd=lwd_c)
  exprs <- c("y=sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]","y=-sqrt(abs(radius^2-(x-cc_current[1])^2))+cc_current[2]")
  xrange <- matrix(c(cc_current[1]-radius,cc_current[1]+radius,cc_current[1]-radius,cc_current[1]+radius),ncol = 2,nrow = length(exprs),byrow = T)
  curve_expr$cc_current <- cc_current
  curve_expr$radius <- radius
  curve_expr$exprs <- exprs
  curve_expr$xrange <- xrange
  if(return){return(curve_expr)}
}


