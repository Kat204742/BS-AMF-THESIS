for (a in c(25,50,100,200,300)) 
{
  for (b in c(25,50,100,200,300)) 
  {
    for (d in c(16,4,16/9,1,.64,4/9)) 
    {
      for (SDR in c(1,1.1,1.25,1.5,2)) 
      {
        x1 <- a 
        x2 <- b 
        x3 <- d 
        x4 <- SDR
        sk=2/sqrt(x3)
        
        jpeg(file=paste('C:/Users/acer/Desktop/Second Semester Thesis Files/Graphs/Gamma Equal Skewness/Ttest/individual/',as.character(x1),'.',as.character(x2),' sk',as.character(sk),' sdr',as.character(x4),'.jpeg',sep=''), width=2048, height=1024)
        hist(ttest_teststatmatrix[,paste('gettest',as.character(x1),'.',as.character(x2),' sk',as.character(sk),' sdr1',sep='')],
             main=paste("Student's T-test Gamma Equal: ( n1 , n2 , Skew , SDR ) = (",x1,',',x2,',',sk,',',x4,')',sep=' '),
             xlab=paste("Fail to reject Null Hypothesis: left=",tleft[,paste('gettest',as.character(x1),'.',as.character(x2),' sk',as.character(sk),' sdr',as.character(x4),sep='')],' right=',tright[,paste('gettest',as.character(x1),'.',as.character(x2),' sk',as.character(sk),' sdr',as.character(x4),sep='')],sep=''),
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
        lines(density(ttest_teststatmatrix[,paste("gettest",as.character(x1),'.',as.character(x2),' sk',as.character(sk),' sdr',as.character(x4),sep='')]),col='#0000ff', lwd=3)
        curve(dt(x, df=x1+x2-2), add=T, col='black', lwd=4)
        segments(x0 = qt(0.2000,x1+x2-2),x1 = qt(0.2000,x1+x2-2),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
        segments(x0 = qt(0.8000,x1+x2-2),x1 = qt(0.8000,x1+x2-2),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
        dev.off()
      }
    }
  }
}
for (a in c(25,50,100,200,300)) 
{
  for (b in c(25,50,100,200,300)) 
  {
    for (d in c(16,4,16/9,1,.64,4/9))
    {
      for (SDR in c(1,1.1,1.25,1.5,2)) 
      {
        x1 <- a 
        x2 <- b 
        x3 <- d 
        x4 <- SDR
        sk=2/sqrt(x3)
        
        jpeg(file=paste('C:/Users/acer/Desktop/Second Semester Thesis Files/Graphs/Gamma Equal Skewness/Welch/individual/',as.character(x1),'.',as.character(x2),' sk',as.character(sk),' sdr',as.character(x4),'.jpeg',sep=''), width=2048, height=1024)
        hist(welch_teststatmatrix[,paste('gewelch',as.character(x1),'.',as.character(x2),' sk',as.character(sk),' sdr1',sep='')],
             main=paste("Welch's T-test Gamma Equal: ( n1 , n2 , Skew , SDR ) = (",x1,',',x2,',',sk,',',x4,')',sep=' '),
             xlab=paste("Fail to reject Null Hypothesis: left=",wleft[,paste('gewelch',as.character(x1),'.',as.character(x2),' sk',as.character(sk),' sdr',as.character(x4),sep='')],' right=',wright[,paste('gewelch',as.character(x1),'.',as.character(x2),' sk',as.character(sk),' sdr',as.character(x4),sep='')],sep=''),
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
        vnum=(x4^2+x1/x2)^2
        vden=(x4^4/(x1-1))+(1/(x2-1))*(x1/x2)^2
        v=vnum/vden
        lines(density(welch_teststatmatrix[,paste("gewelch",as.character(x1),'.',as.character(x2),' sk',as.character(sk),' sdr',as.character(x4),sep='')]),col='#0000ff', lwd=3)
        curve(dt(x, df=x1+x2-2), add=T, col='black', lwd=4)
        segments(x0 = qt(0.2000,v),x1 = qt(0.2000,v),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
        segments(x0 = qt(0.8000,v),x1 = qt(0.8000,v),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
        dev.off()
      }
    }
  }
}




for (a in c(25,50,100,200,300)) 
{
  for (b in c(25,50,100,200,300)) 
  {
    for (d in c(16,4,16/9,1,.64,4/9)) 
    {
      
      x1 <- a 
      x2 <- b 
      x3 <- d 
      sk=2/sqrt(x3)
      
      jpeg(file=paste('C:/Users/acer/Desktop/Second Semester Thesis Files/Graphs/Gamma Equal Skewness/Ttest/sdr/',as.character(x1),'.',as.character(x2),' sk',as.character(sk),'.jpeg',sep=''), width=2048, height=1024)
      hist(ttest_teststatmatrix[,paste('gettest',as.character(x1),'.',as.character(x2),' sk',as.character(sk),' sdr1',sep='')],
           main=paste("Student's T-test Gamma Equal: ( n1 , n2 , Skew ) = (",x1,',',x2,',',sk,')',sep=' '),
           xlab="Test Statistics",
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
      lines(density(ttest_teststatmatrix[,paste("gettest",as.character(x1),'.',as.character(x2),' sk',as.character(sk),' sdr1',sep='')]),col='#b22222', lwd=3)
      lines(density(ttest_teststatmatrix[,paste("gettest",as.character(x1),'.',as.character(x2),' sk',as.character(sk),' sdr1.1',sep='')]),col='#ff8c00', lwd=3)
      lines(density(ttest_teststatmatrix[,paste("gettest",as.character(x1),'.',as.character(x2),' sk',as.character(sk),' sdr1.25',sep='')]),col='#008000', lwd=3)
      lines(density(ttest_teststatmatrix[,paste("gettest",as.character(x1),'.',as.character(x2),' sk',as.character(sk),' sdr1.5',sep='')]),col='#0000ff', lwd=3)
      lines(density(ttest_teststatmatrix[,paste("gettest",as.character(x1),'.',as.character(x2),' sk',as.character(sk),' sdr2',sep='')]),col='#ff69b4', lwd=3)
      legend(2.5, 0.6, legend=c("SDR=1","SDR=1.1","SDR=1.25","SDR=1.5","SDR=2","t distribution"), text.font = 4,cex=2,box.lwd = 3,fill = c("#b22222","#ff8c00","#008000","#0000ff","#ff69b4","black"))
      curve(dt(x, df=x1+x2-2), add=T, col='black', lwd=4)
      segments(x0 = qt(0.2000,x1+x2-2),x1 = qt(0.2000,x1+x2-2),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
      segments(x0 = qt(0.8000,x1+x2-2),x1 = qt(0.8000,x1+x2-2),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
      dev.off()
      
    }
  }
}



for (a in c(25,50,100,200,300)) 
{
  for (b in c(25,50,100,200,300)) 
  {
    for (d in c(16,4,16/9,1,.64,4/9)) 
    {
      
      x1 <- a 
      x2 <- b 
      x3 <- d 
      sk=2/sqrt(x3)
      
      jpeg(file=paste('C:/Users/acer/Desktop/Second Semester Thesis Files/Graphs/Gamma Equal Skewness/Welch/sdr/',as.character(x1),'.',as.character(x2),' sk',as.character(sk),'.jpeg',sep=''), width=2048, height=1024)
      hist(welch_teststatmatrix[,paste('gewelch',as.character(x1),'.',as.character(x2),' sk',as.character(sk),' sdr1',sep='')],
           main=paste("Welch's T-test Gamma Equal: ( n1 , n2 , Skew ) = (",x1,',',x2,',',sk,')',sep=' '),
           xlab="Test Statistics",
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
      lines(density(welch_teststatmatrix[,paste("gewelch",as.character(x1),'.',as.character(x2),' sk',as.character(sk),' sdr1',sep='')]),col='#b22222', lwd=3)
      lines(density(welch_teststatmatrix[,paste("gewelch",as.character(x1),'.',as.character(x2),' sk',as.character(sk),' sdr1.1',sep='')]),col='#ff8c00', lwd=3)
      lines(density(welch_teststatmatrix[,paste("gewelch",as.character(x1),'.',as.character(x2),' sk',as.character(sk),' sdr1.25',sep='')]),col='#008000', lwd=3)
      lines(density(welch_teststatmatrix[,paste("gewelch",as.character(x1),'.',as.character(x2),' sk',as.character(sk),' sdr1.5',sep='')]),col='#0000ff', lwd=3)
      lines(density(welch_teststatmatrix[,paste("gewelch",as.character(x1),'.',as.character(x2),' sk',as.character(sk),' sdr2',sep='')]),col='#ff69b4', lwd=3)
      legend(2.5, 0.6, legend=c("SDR=1","SDR=1.1","SDR=1.25","SDR=1.5","SDR=2","t distribution"), text.font = 4,cex=2,box.lwd = 3,fill = c("#b22222","#ff8c00","#008000","#0000ff","#ff69b4","black"))
      curve(dt(x, df=100), add=T, col='black', lwd=4)
      segments(x0 = qt(0.2000,100),x1 = qt(0.2000,100),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
      segments(x0 = qt(0.8000,100),x1 = qt(0.8000,100),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
      dev.off()
      
    }
  }
}




for (a in c(25,50,100,200,300)) 
{
  for (b in c(25,50,100,200,300))
  {
    for (SDR in c(1,1.1,1.25,1.5,2)) 
    {
      
      x1 <- a 
      x2 <- b 
      x3 <- SDR 
      
      jpeg(file=paste('C:/Users/acer/Desktop/Second Semester Thesis Files/Graphs/Gamma Equal Skewness/Ttest/sk/',as.character(x1),'.',as.character(x2),' sdr',as.character(x3),'.jpeg',sep=''), width=2048, height=1024)
      hist(ttest_teststatmatrix[,paste('gettest',as.character(x1),'.',as.character(x2),' sk0.5 ','sdr',as.character(x3),sep='')],
           main=paste("Student's T-test Gamma Equal: ( n1 , n2 , SDR ) = (",x1,',',x2,',',x3,')',sep=' '),
           xlab="Test Statistics",
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
      lines(density(ttest_teststatmatrix[,paste("gettest",as.character(x1),'.',as.character(x2),' sk0.5',' sdr',as.character(x3),sep='')]),col='#b22222', lwd=3)
      lines(density(ttest_teststatmatrix[,paste("gettest",as.character(x1),'.',as.character(x2),' sk1',' sdr',as.character(x3),sep='')]),col='#ff8c00', lwd=3)
      lines(density(ttest_teststatmatrix[,paste("gettest",as.character(x1),'.',as.character(x2),' sk1.5',' sdr',as.character(x3),sep='')]),col='#008000', lwd=3)
      lines(density(ttest_teststatmatrix[,paste("gettest",as.character(x1),'.',as.character(x2),' sk2',' sdr',as.character(x3),sep='')]),col='#0000ff', lwd=3)
      lines(density(ttest_teststatmatrix[,paste("gettest",as.character(x1),'.',as.character(x2),' sk2.5',' sdr',as.character(x3),sep='')]),col='#ff69b4', lwd=3)
      lines(density(ttest_teststatmatrix[,paste("gettest",as.character(x1),'.',as.character(x2),' sk3',' sdr',as.character(x3),sep='')]),col='#800080', lwd=3)
      legend(2.5, 0.6, legend=c("Skew=0.5","Skew=1","Skew=1.5","Skew=2","Skew=2.5","Skew=3","t distribution"), text.font = 4,cex=2,box.lwd = 3,fill = c("#b22222","#ff8c00","#008000","#0000ff","#ff69b4","#800080","black"))
      curve(dt(x, df=x1+x2-2), add=T, col='black', lwd=4)
      segments(x0 = qt(0.2000,x1+x2-2),x1 = qt(0.2000,x1+x2-2),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
      segments(x0 = qt(0.8000,x1+x2-2),x1 = qt(0.8000,x1+x2-2),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
      dev.off()
      
    }
  }
}

for (a in c(25,50,100,200,300)) 
{
  for (b in c(25,50,100,200,300))
  {
    for (SDR in c(1,1.1,1.25,1.5,2)) 
    {
      
      x1 <- a 
      x2 <- b 
      x3 <- SDR 
      
      jpeg(file=paste('C:/Users/acer/Desktop/Second Semester Thesis Files/Graphs/Gamma Equal Skewness/Welch/sk/',as.character(x1),'.',as.character(x2),' sdr',as.character(x3),'.jpeg',sep=''), width=2048, height=1024)
      hist(welch_teststatmatrix[,paste('gewelch',as.character(x1),'.',as.character(x2),' sk0.5 ','sdr',as.character(x3),sep='')],
           main=paste("Welch's T-test Gamma Equal: ( n1 , n2 , SDR ) = (",x1,',',x2,',',x3,')',sep=' '),
           xlab="Test Statistics",
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
      vnum=(x3^2+x1/x2)^2
      vden=(x3^4/(x1-1))+(1/(x2-1))*(x1/x2)^2
      v=vnum/vden
      lines(density(welch_teststatmatrix[,paste("gewelch",as.character(x1),'.',as.character(x2),' sk0.5',' sdr',as.character(x3),sep='')]),col='#b22222', lwd=3)
      lines(density(welch_teststatmatrix[,paste("gewelch",as.character(x1),'.',as.character(x2),' sk1',' sdr',as.character(x3),sep='')]),col='#ff8c00', lwd=3)
      lines(density(welch_teststatmatrix[,paste("gewelch",as.character(x1),'.',as.character(x2),' sk1.5',' sdr',as.character(x3),sep='')]),col='#008000', lwd=3)
      lines(density(welch_teststatmatrix[,paste("gewelch",as.character(x1),'.',as.character(x2),' sk2',' sdr',as.character(x3),sep='')]),col='#0000ff', lwd=3)
      lines(density(welch_teststatmatrix[,paste("gewelch",as.character(x1),'.',as.character(x2),' sk2.5',' sdr',as.character(x3),sep='')]),col='#ff69b4', lwd=3)
      lines(density(welch_teststatmatrix[,paste("gewelch",as.character(x1),'.',as.character(x2),' sk3',' sdr',as.character(x3),sep='')]),col='#800080', lwd=3)
      legend(2.5, 0.6, legend=c("Skew=0.5","Skew=1","Skew=1.5","Skew=2","Skew=2.5","Skew=3","t distribution"), text.font = 4,cex=2,box.lwd = 3,fill = c("#b22222","#ff8c00","#008000","#0000ff","#FF69b4","#800080","black"))
      curve(dt(x, df=v), add=T, col='black', lwd=4)
      segments(x0 = qt(0.2000,v),x1 = qt(0.2000,v),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
      segments(x0 = qt(0.8000,v),x1 = qt(0.8000,v),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
      dev.off()
      
    }
  }
}



for (b in c(25,50,100,200,300)) 
{
  for (d in c(16,4,16/9,1,.64,4/9)) 
  {
    for (SDR in c(1,1.1,1.25,1.5,2)) 
    {
      
      x1 <- b 
      x2 <- d 
      x3 <- SDR 
      sk=2/sqrt(x2)
      jpeg(file=paste('C:/Users/acer/Desktop/Second Semester Thesis Files/Graphs/Gamma Equal Skewness/Ttest/n1/','.',as.character(x1),' sk',as.character(sk),' sdr',as.character(x3),'.jpeg',sep=''), width=2048, height=1024)
      hist(ttest_teststatmatrix[,paste('gettest','25.',as.character(x1),' ','sk',as.character(sk),' sdr',as.character(x3),sep='')],
           main=paste("Student's T-test Gamma Equal: ( n2 , Skew , SDR ) = (",x1,',',sk,',',x3,')',sep=' '),
           xlab="Test Statistics",
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
      lines(density(ttest_teststatmatrix[,paste("gettest",'25.',as.character(x1),' ','sk',as.character(sk),' sdr',as.character(x3),sep='')]),col='#b22222', lwd=3)
      lines(density(ttest_teststatmatrix[,paste("gettest",'50.',as.character(x1),' ','sk',as.character(sk),' sdr',as.character(x3),sep='')]),col='#ff8c00', lwd=3)
      lines(density(ttest_teststatmatrix[,paste("gettest",'100.',as.character(x1),' ','sk',as.character(sk),' sdr',as.character(x3),sep='')]),col='#008000', lwd=3)
      lines(density(ttest_teststatmatrix[,paste("gettest",'200.',as.character(x1),' ','sk',as.character(sk),' sdr',as.character(x3),sep='')]),col='#0000ff', lwd=3)
      lines(density(ttest_teststatmatrix[,paste("gettest",'300.',as.character(x1),' ','sk',as.character(sk),' sdr',as.character(x3),sep='')]),col='#ff69b4', lwd=3)
      legend(2.5, 0.6, legend=c("n1=25","n1=50","n1=100","n1=200","n1=300","t distribution"), text.font = 4,cex=2,box.lwd = 3,fill = c("#b22222","#ff8c00","#008000","#0000ff","#ff69b4","black"))
      curve(dt(x, df=x1+x2-2), add=T, col='black', lwd=4)
      segments(x0 = qt(0.2000,x1+x2-2),x1 = qt(0.2000,x1+x2-2),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
      segments(x0 = qt(0.8000,x1+x2-2),x1 = qt(0.8000,x1+x2-2),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
      dev.off()
      
    }
  }
}

for (b in c(25,50,100,200,300)) 
{
  for (d in c(16,4,16/9,1,.64,4/9)) 
  {
    for (SDR in c(1,1.1,1.25,1.5,2))
    {
      
      x1 <- b 
      x2 <- d 
      x3 <- SDR 
      sk=2/sqrt(x2)
      jpeg(file=paste('C:/Users/acer/Desktop/Second Semester Thesis Files/Graphs/Gamma Equal Skewness/Welch/n1/','.',as.character(x1),' sk',as.character(sk),' sdr',as.character(x3),'.jpeg',sep=''), width=2048, height=1024)
      hist(welch_teststatmatrix[,paste('gewelch','25.',as.character(x1),' ','sk',as.character(sk),' sdr',as.character(x3),sep='')],
           main=paste("Welch's T-test Gamma Equal: ( n2 , Skew , SDR ) = (",x1,',',sk,',',x3,')',sep=' '),
           xlab="Test Statistics",
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
      lines(density(welch_teststatmatrix[,paste("gewelch",'25.',as.character(x1),' ','sk',as.character(sk),' sdr',as.character(x3),sep='')]),col='#b22222', lwd=3)
      lines(density(welch_teststatmatrix[,paste("gewelch",'50.',as.character(x1),' ','sk',as.character(sk),' sdr',as.character(x3),sep='')]),col='#ff8c00', lwd=3)
      lines(density(welch_teststatmatrix[,paste("gewelch",'100.',as.character(x1),' ','sk',as.character(sk),' sdr',as.character(x3),sep='')]),col='#008000', lwd=3)
      lines(density(welch_teststatmatrix[,paste("gewelch",'200.',as.character(x1),' ','sk',as.character(sk),' sdr',as.character(x3),sep='')]),col='#0000ff', lwd=3)
      lines(density(welch_teststatmatrix[,paste("gewelch",'300.',as.character(x1),' ','sk',as.character(sk),' sdr',as.character(x3),sep='')]),col='#ff69b4', lwd=3)
      legend(2.5, 0.6, legend=c("n1=25","n1=50","n1=100","n1=200","n1=300","t distribution"), text.font = 4,cex=2,box.lwd = 3,fill = c("#b22222","#ff8c00","#008000","#0000ff","#ff69b4","black"))
      curve(dt(x, df=100), add=T, col='black', lwd=4)
      segments(x0 = qt(0.2000,100),x1 = qt(0.2000,100),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
      segments(x0 = qt(0.8000,100),x1 = qt(0.8000,100),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
      dev.off()
      
    }
  }
}


for (a in c(25,50,100,200,300)) 
{
  for (d in c(16,4,16/9,1,.64,4/9)) 
  {
    for (SDR in c(1,1.1,1.25,1.5,2)) 
    {
      
      x1 <- a 
      x2 <- d 
      x3 <- SDR 
      sk=2/sqrt(x2)
      jpeg(file=paste('C:/Users/acer/Desktop/Second Semester Thesis Files/Graphs/Gamma Equal Skewness/Ttest/n2/',as.character(x1),'.',' sk',as.character(sk),' sdr',as.character(x3),'.jpeg',sep=''), width=2048, height=1024)
      hist(ttest_teststatmatrix[,paste('gettest',as.character(x1),'.25',' ','sk',as.character(sk),' sdr',as.character(x3),sep='')],
           main=paste("Student's T-test Gamma Equal: ( n1 , Skew , SDR ) = (",x1,',',sk,',',x3,')',sep=' '),
           xlab="Test Statistics",
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
      lines(density(ttest_teststatmatrix[,paste("gettest",as.character(x1),'.25',' ','sk',as.character(sk),' sdr',as.character(x3),sep='')]),col='#b22222', lwd=3)
      lines(density(ttest_teststatmatrix[,paste("gettest",as.character(x1),'.50',' ','sk',as.character(sk),' sdr',as.character(x3),sep='')]),col='#ff8c00', lwd=3)
      lines(density(ttest_teststatmatrix[,paste("gettest",as.character(x1),'.100',' ','sk',as.character(sk),' sdr',as.character(x3),sep='')]),col='#008000', lwd=3)
      lines(density(ttest_teststatmatrix[,paste("gettest",as.character(x1),'.200',' ','sk',as.character(sk),' sdr',as.character(x3),sep='')]),col='#0000ff', lwd=3)
      lines(density(ttest_teststatmatrix[,paste("gettest",as.character(x1),'.300',' ','sk',as.character(sk),' sdr',as.character(x3),sep='')]),col='#ff69b4', lwd=3)
      legend(2.5, 0.6, legend=c("n2=25","n2=50","n2=100","n2=200","n2=300","t distribution"), text.font = 4,cex=2,box.lwd = 3,fill = c("#b22222","#ff8c00","#008000","#0000ff","#ff69b4","black"))
      curve(dt(x, df=x1+x2-2), add=T, col='black', lwd=4)
      segments(x0 = qt(0.2000,x1+x2-2),x1 = qt(0.2000,x1+x2-2),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
      segments(x0 = qt(0.8000,x1+x2-2),x1 = qt(0.8000,x1+x2-2),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
      dev.off()
      
    }
  }
}

for (a in c(25,50,100,200,300)) 
{
  for (d in c(16,4,16/9,1,.64,4/9)) 
  {
    for (SDR in c(1,1.1,1.25,1.5,2)) 
    {
      
      x1 <- a 
      x2 <- d 
      x3 <- SDR 
      sk=2/sqrt(x2)
      jpeg(file=paste('C:/Users/acer/Desktop/Second Semester Thesis Files/Graphs/Gamma Equal Skewness/Welch/n2/',as.character(x1),'.',' sk',as.character(sk),' sdr',as.character(x3),'.jpeg',sep=''), width=2048, height=1024)
      hist(welch_teststatmatrix[,paste('gewelch',as.character(x1),'.25',' ','sk',as.character(sk),' sdr',as.character(x3),sep='')],
           main=paste("Welch's T-test Gamma Equal: ( n1 , Skew , SDR ) = (",x1,',',sk,',',x3,')',sep=' '),
           xlab="Test Statistics",
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
      lines(density(welch_teststatmatrix[,paste("gewelch",as.character(x1),'.25',' ','sk',as.character(sk),' sdr',as.character(x3),sep='')]),col='#b22222', lwd=3)
      lines(density(welch_teststatmatrix[,paste("gewelch",as.character(x1),'.50',' ','sk',as.character(sk),' sdr',as.character(x3),sep='')]),col='#ff8c00', lwd=3)
      lines(density(welch_teststatmatrix[,paste("gewelch",as.character(x1),'.100',' ','sk',as.character(sk),' sdr',as.character(x3),sep='')]),col='#008000', lwd=3)
      lines(density(welch_teststatmatrix[,paste("gewelch",as.character(x1),'.200',' ','sk',as.character(sk),' sdr',as.character(x3),sep='')]),col='#0000ff', lwd=3)
      lines(density(welch_teststatmatrix[,paste("gewelch",as.character(x1),'.300',' ','sk',as.character(sk),' sdr',as.character(x3),sep='')]),col='#ff69b4', lwd=3)
      legend(2.5, 0.6, legend=c("n2=25","n2=50","n2=100","n2=200","n2=300","t distribution"), text.font = 4,cex=2,box.lwd = 3,fill = c("#b22222","#ff8c00","#008000","#0000ff","#ff69b4","black"))
      curve(dt(x, df=100), add=T, col='black', lwd=4)
      segments(x0 = qt(0.2000,100),x1 = qt(0.2000,100),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
      segments(x0 = qt(0.8000,100),x1 = qt(0.8000,100),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
      dev.off()
      
    }
  }
}