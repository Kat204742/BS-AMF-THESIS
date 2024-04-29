for (a in c(25,50,100)) 
{
  for (b in c(25,50,100)) 
  {
    for (d in c(4,6,8)) 
    {
      for(hypotest in c('ttest','welch'))
      {
        x1 <- a 
        x2 <- b 
        x3 <- d 
        x4 <- hypotest
        text=paste("Normal",a,'.',b,' sdr',d/4,sep='')
        
        if(x4=="ttest")
        {
          text=paste('T',text,sep='')
        }
        else
        {
          text=paste('W',text,sep='')
        }
        
        jpeg(file=paste('C:/Users/acer/Desktop/Second Semester Thesis Files/Graphs/Two-Tailed/',"Normal",as.character(x1),'.',as.character(x2),' sdr',as.character(x3/4),' ',x4,'.jpeg',sep=''), width=2048, height=1024)
        hist(teststatmatrix[,text],
             main=paste("Normal ",x4,": ( n1 , n2 , SDR ) = (",x1,',',x2,',',x3/4,')',sep=' '),
             xlab=paste("Fail to reject Null Hypothesis: ",betahatmatrix[,text],sep=''),
             xlim=c(-3.5,3.5),
             col="white",
             freq=FALSE,
             ylim=c(0,0.7),
             breaks='Scott',
             cex.main=1.75,
             cex.lab=1.5,
             cex.axis=2,
             lty="blank"
        )
        lines(density(teststatmatrix[,text]),col='#0000ff', lwd=3)
        vnum=(x3^2+x1/x2)^2
        vden=(x3^4/(x1-1))+(1/(x2-1))*(x1/x2)^2
        if(x4=="ttest")
        {
          v=x1+x2-2
          segments(x0 = criticalvalueTtestmatrix[,text],x1 = criticalvalueTtestmatrix[,text],y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
          segments(x0 = -criticalvalueTtestmatrix[,text],x1 = -criticalvalueTtestmatrix[,text],y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
          curve(dt(x-expectedvaluematrix[,text], df=v), add=T, col='black', lwd=4)
        }
        else
        {
          v=floor(vnum/vden)
          segments(x0 = criticalvalueperdfmatrix[v,text],x1 = criticalvalueperdfmatrix[v,text],y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
          segments(x0 = -criticalvalueperdfmatrix[v,text],x1 = -criticalvalueperdfmatrix[v,text],y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
          curve(dt(x-expectedvaluematrix[,text], df=v), add=T, col='black', lwd=4)
        }
        dev.off()
      }
    }
  }
}

for (a in c(25,50,100)) 
{
  for (b in c(25,50,100)) 
  {
    for (e in c(0.5,10)) 
    {
      for (SDR in c(1,1.5,2)) 
      {
        for(hypotest in c('ttest','welch'))
        {
          x1 <- a 
          x2 <- b 
          x3 <- e 
          x4 <- SDR
          x5 <- hypotest
          text=paste("GenNormal",a,'.',b,' sdr',SDR,' shape',e,sep='')
          
          if(x5=="ttest")
          {
            text=paste('T',text,sep='')
          }
          else
          {
            text=paste('W',text,sep='')
          }
          
          jpeg(file=paste('C:/Users/acer/Desktop/Second Semester Thesis Files/Graphs/Two-Tailed/',"GenNormal",as.character(x1),'.',as.character(x2),' sdr',as.character(x4),' shape',as.character(x3),' ',x5,'.jpeg',sep=''), width=2048, height=1024)
          hist(teststatmatrix[,text],
               main=paste("Generalized Normal ",x5,": ( n1 , n2 , SDR, Shape ) = (",x1,',',x2,',',x4,',',x3,')',sep=' '),
               xlab=paste("Fail to reject Null Hypothesis: ",betahatmatrix[,text],sep=''),
               xlim=c(-3.5,3.5),
               col="white",
               freq=FALSE,
               ylim=c(0,0.7),
               breaks='Scott',
               cex.main=1.75,
               cex.lab=1.5,
               cex.axis=2,
               lty="blank"
          )
          lines(density(teststatmatrix[,text]),col='#0000ff', lwd=3)
          vnum=(x4^2+x1/x2)^2
          vden=(x4^4/(x1-1))+(1/(x2-1))*(x1/x2)^2
          if(x5=="ttest")
          {
            v=x1+x2-2
            segments(x0 = criticalvalueTtestmatrix[,text],x1 = criticalvalueTtestmatrix[,text],y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
            segments(x0 = -criticalvalueTtestmatrix[,text],x1 = -criticalvalueTtestmatrix[,text],y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
            curve(dt(x-expectedvaluematrix[,text], df=v), add=T, col='black', lwd=4)
          }
          else
          {
            v=floor(vnum/vden)
            segments(x0 = criticalvalueperdfmatrix[v,text],x1 = criticalvalueperdfmatrix[v,text],y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
            segments(x0 = -criticalvalueperdfmatrix[v,text],x1 = -criticalvalueperdfmatrix[v,text],y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
            curve(dt(x-expectedvaluematrix[,text], df=v), add=T, col='black', lwd=4)
          }
          dev.off()
        }
      }
    }
  }
}

for (a in c(25,50,100)) 
{
  for (b in c(25,50,100)) 
  {
    for (d in c(4,1,4/9)) 
    {
      for (e in c(1,1.5,2)) 
      {
        for(hypotest in c('ttest','welch'))
        {
          x1 <- a 
          x2 <- b 
          x3 <- d 
          x4 <- e
          x5 <- hypotest
          sk=2/sqrt(d)
          text=paste("GammaEqual",a,'.',b,' sk',sk,' sdr',e,sep='')
          
          if(x5=="ttest")
          {
            text=paste('T',text,sep='')
          }
          else
          {
            text=paste('W',text,sep='')
          }
          
          jpeg(file=paste('C:/Users/acer/Desktop/Second Semester Thesis Files/Graphs/Two-Tailed/',"GammaEqual",as.character(x1),'.',as.character(x2),' sdr',as.character(x4),' sk',as.character(sk),' ',x5,'.jpeg',sep=''), width=2048, height=1024)
          hist(teststatmatrix[,text],
               main=paste("Gamma Equal Skewness ",x5,": ( n1 , n2 , SDR, Skew ) = (",x1,',',x2,',',x4,',',sk,')',sep=' '),
               xlab=paste("Fail to reject Null Hypothesis: ",betahatmatrix[,text],sep=''),
               xlim=c(-3.5,3.5),
               col="white",
               freq=FALSE,
               ylim=c(0,0.7),
               breaks='Scott',
               cex.main=1.75,
               cex.lab=1.5,
               cex.axis=2,
               lty="blank"
          )
          lines(density(teststatmatrix[,text]),col='#0000ff', lwd=3)
          vnum=(x4^2+x1/x2)^2
          vden=(x4^4/(x1-1))+(1/(x2-1))*(x1/x2)^2
          if(x5=="ttest")
          {
            v=x1+x2-2
            segments(x0 = criticalvalueTtestmatrix[,text],x1 = criticalvalueTtestmatrix[,text],y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
            segments(x0 = -criticalvalueTtestmatrix[,text],x1 = -criticalvalueTtestmatrix[,text],y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
            curve(dt(x-expectedvaluematrix[,text], df=v), add=T, col='black', lwd=4)
          }
          else
          {
            v=floor(vnum/vden)
            segments(x0 = criticalvalueperdfmatrix[v,text],x1 = criticalvalueperdfmatrix[v,text],y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
            segments(x0 = -criticalvalueperdfmatrix[v,text],x1 = -criticalvalueperdfmatrix[v,text],y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
            curve(dt(x-expectedvaluematrix[,text], df=v), add=T, col='black', lwd=4)
          }
          dev.off()
        }
      }
    }
  }
}

for (a in c(25,50,100)) 
{
  for (b in c(25,50,100)) 
  {
    for (d in c(1,4/9)) 
    {
      for (SDR in c(1,1.5,2)) 
      {
        for(hypotest in c('ttest','welch'))
        {
          x1 <- a 
          x2 <- b 
          x3 <- d 
          x4 <- SDR
          x5 <- hypotest
          sk=2/sqrt(d)
          shape2=(2/(2/sqrt(d)-0.5))^2  
          e=SDR*sqrt(shape2/d)
          text=paste("GammaUnequal",a,'.',b,' sk',sk,' sdr',SDR,sep='')
          
          if(x5=="ttest")
          {
            text=paste('T',text,sep='')
          }
          else
          {
            text=paste('W',text,sep='')
          }
          
          jpeg(file=paste('C:/Users/acer/Desktop/Second Semester Thesis Files/Graphs/Two-Tailed/',"GammaUnequal",as.character(x1),'.',as.character(x2),' sdr',as.character(x4),' sk',as.character(sk),' ',x5,'.jpeg',sep=''), width=2048, height=1024)
          hist(teststatmatrix[,text],
               main=paste("Gamma Equal Skewness ",x5,": ( n1 , n2 , SDR, Skew ) = (",x1,',',x2,',',x4,',',sk,')',sep=' '),
               xlab=paste("Fail to reject Null Hypothesis: ",betahatmatrix[,text],sep=''),
               xlim=c(-3.5,3.5),
               col="white",
               freq=FALSE,
               ylim=c(0,0.7),
               breaks='Scott',
               cex.main=1.75,
               cex.lab=1.5,
               cex.axis=2,
               lty="blank"
          )
          lines(density(teststatmatrix[,text]),col='#0000ff', lwd=3)
          vnum=(x4^2+x1/x2)^2
          vden=(x4^4/(x1-1))+(1/(x2-1))*(x1/x2)^2
          if(x5=="ttest")
          {
            v=x1+x2-2
            segments(x0 = criticalvalueTtestmatrix[,text],x1 = criticalvalueTtestmatrix[,text],y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
            segments(x0 = -criticalvalueTtestmatrix[,text],x1 = -criticalvalueTtestmatrix[,text],y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
            curve(dt(x-expectedvaluematrix[,text], df=v), add=T, col='black', lwd=4)
          }
          else
          {
            v=floor(vnum/vden)
            segments(x0 = criticalvalueperdfmatrix[v,text],x1 = criticalvalueperdfmatrix[v,text],y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
            segments(x0 = -criticalvalueperdfmatrix[v,text],x1 = -criticalvalueperdfmatrix[v,text],y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
            curve(dt(x-expectedvaluematrix[,text], df=v), add=T, col='black', lwd=4)
          }
          dev.off()
        }
      }
    }
  }
}
