#' OCC Function to create OC Curves
#'Copyright - Harish Jose 11, 2014
#' This function allows you to create OC Curves based on Hypergeometric Distribution.
#' You need to enter the folowing details.
#' c = number of defects (default is 0)
#' n = sample size (default is 60)
#' N = Lot Size (defaut is 1200)
#' alpha = level of significance or Producer's risk (default is 5%)
#' beta = consumer's risk (default is 10%)
#' Enter alpha and beta as values between 1 and 100
#' #' @keywords OC Curve
#' @export
#' @examples
#' hyp_occurve()

hyp_occurve<- function(c=0,n=60,N=1200,alpha=5,beta=10){
require(ggplot2)
  x<-seq(0,0.2,0.005)
  y<- phyper(c,(N*x),(N-(N*x)),n)
  AQL<-qbeta((alpha/100),c+1,(n-c))*100
  RQL<-qbeta(1-(beta/100),c+1,(n-c))*100
  Title<- paste0("OC Curve for AQL: ", round(AQL,2), " and RQL: ", round(RQL,2))
  T1<- paste("N = ", N, ", n = ", n, ", c = ",c, ", alpha = ", alpha, "%, beta =", beta, "%")
  T2<- paste("AQL = ", AQL, ", RQL = ", RQL)
  z<-qplot(x,y,geom="line",xlab="% nonconforming", ylab="Pa", main =Title)
  #force the origin on the plot
  a<-z+ scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+ theme(axis.title.x = element_text(color="cadetblue", vjust=-0.35),axis.title.y = element_text(color="cadetblue" , vjust=0.35))
  b<-a+geom_line(color="firebrick", size =1.5)+theme(panel.background = element_rect(fill = 'grey'))
  theme_set(theme_gray(base_size = 20))
  all<-list(OCplot = b, Selections = T1, Results = T2)
  return(all)
}
