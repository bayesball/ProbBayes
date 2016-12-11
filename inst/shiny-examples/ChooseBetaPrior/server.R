#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

beta.select <- function(quantile1,quantile2){
  betaprior1=function(K,x,p)
  {
    m.lo=0; m.hi=1; flag=0
    while(flag==0)
    {
      m0=(m.lo+m.hi)/2
      p0=pbeta(x,K*m0,K*(1-m0))
      if(p0<p) m.hi=m0 else m.lo=m0
      if(abs(p0-p)<.0001) flag=1
    }
    return(m0)
  }
  
  p1=quantile1$p; x1=quantile1$x
  p2=quantile2$p; x2=quantile2$x
  
  logK=seq(-3,8,length=100); K=exp(logK)
  m=sapply(K,betaprior1,x1,p1)
  
  prob2=pbeta(x2,K*m, K*(1-m))
  ind=((prob2>0)&(prob2<1))
  app=approx(prob2[ind],logK[ind],p2)
  K0=exp(app$y)
  m0=betaprior1(K0,x1,p1)
  
  return(round(K0*c(m0,(1-m0)),2))
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    ab <- beta.select(list(x=input$Prior_median, p=0.5),
                      list(x=input$Prior_90, p=0.9))
    # generate bins based on input$bins from ui.R
    a <- ab[1]
    b <- ab[2]
    
    # draw the histogram with the specified number of bins
    curve(dbeta(x, a, b), 0, 1,
          xlab="P", ylab="Density", col="green", lwd=3,
          main=paste("Beta(", a, ", ", b, 
          ") Density with Middle 50% and 90% Intervals Shown", 
          sep=""))
    q <- qbeta(c(.25, .75), a, b)
    lines(c(q[1], q[1]), 
          c(0, dbeta(q[1], a, b)), col="blue", lwd=3)
    lines(c(q[2], q[2]), 
          c(0, dbeta(q[2], a, b)), col="blue", lwd=3)
    q <- qbeta(c(.05, .95), a, b)
    lines(c(q[1], q[1]), 
          c(0, dbeta(q[1], a, b)), col="red", lwd=3)
    lines(c(q[2], q[2]), 
          c(0, dbeta(q[2], a, b)), col="red", lwd=3)
  })
  
  output$text1 <- renderText({ 
    ab <- beta.select(list(x=input$Prior_median, p=0.5),
                      list(x=input$Prior_90, p=0.9))
    # generate bins based on input$bins from ui.R
    a <- ab[1]
    b <- ab[2]
    q <- round(qbeta(c(.25, .75, .05, .95), a, b), 3)
    paste("The middle 50% (blue) of the density is bounded by", 
                q[1], "and", q[2], ".") 
  })
  
  output$text2 <- renderText({ 
    ab <- beta.select(list(x=input$Prior_median, p=0.5),
                      list(x=input$Prior_90, p=0.9))
    # generate bins based on input$bins from ui.R
    a <- ab[1]
    b <- ab[2]
    q <- round(qbeta(c(.25, .75, .05, .95), a, b), 3)
    paste("The middle 90% (red) of the density is bounded by", q[3],
                "and", q[4], ".")
  })
  
})
