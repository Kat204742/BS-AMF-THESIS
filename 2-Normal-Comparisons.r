for (a in c(25,50,100,200,300)) 
{
  for (b in c(25,50,100,200,300)) 
  {
    for (SDR in c(1,1.1,1.25,1.5,2)) 
    {
      x1 <- a
      x2 <- b 
      x3 <- SDR
      
      jpeg(file=paste('C:/Users/acer/Desktop/Second Semester Thesis Files/Graphs/Normal/Ttest/individual/',as.character(x1),'.',as.character(x2),' sdr',as.character(x3),'.jpeg',sep=''), width=2048, height=1024)
      hist(ttest_teststatmatrix[,paste('ttest',as.character(x1),'.',as.character(x2),' ','sdr1',sep='')],
           main=paste("Student's T-test Normal: ( n1 , n2 , SDR ) = (",x1,',',x2,',',x3,')',sep=' '),
           xlab=paste("Fail to reject Null Hypothesis: left=",tleft[,paste('ttest',as.character(x1),'.',as.character(x2),' sdr',as.character(x3),sep='')],' right=',tright[,paste('ttest',as.character(x1),'.',as.character(x2),' sdr',as.character(x3),sep='')],sep=''),
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
      lines(density(ttest_teststatmatrix[,paste("ttest",as.character(x1),'.',as.character(x2),' ','sdr',as.character(x3),sep='')]),col='#0000ff', lwd=3)
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
      
      jpeg(file=paste('C:/Users/acer/Desktop/Second Semester Thesis Files/Graphs/Normal/Welch/individual/',as.character(x1),'.',as.character(x2),' sdr',as.character(x3),'.jpeg',sep=''), width=2048, height=1024)
      hist(welch_teststatmatrix[,paste('welch',as.character(x1),'.',as.character(x2),' ','sdr1',sep='')],
           main=paste("Welch's T-test Normal: ( n1 , n2 , SDR ) = (",x1,',',x2,',',x3,')',sep=' '),
           xlab=paste("Fail to reject Null Hypothesis: left=",wleft[,paste('welch',as.character(x1),'.',as.character(x2),' sdr',as.character(x3),sep='')],' right=',wright[,paste('welch',as.character(x1),'.',as.character(x2),' sdr',as.character(x3),sep='')],sep=''),
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
      lines(density(welch_teststatmatrix[,paste("welch",as.character(x1),'.',as.character(x2),' ','sdr',as.character(x3),sep='')]),col='#0000ff', lwd=3)
      curve(dt(x, df=v), add=T, col='black', lwd=4)
      segments(x0 = qt(0.2000,v),x1 = qt(0.2000,v),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
      segments(x0 = qt(0.8000,v),x1 = qt(0.8000,v),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
      dev.off()
    }
  }
}


for (a in c(25,50,100,200,300)) 
{
  for (b in c(25,50,100,200,300)) 
  {
      
    x1 <- a 
    x2 <- b 
      
    jpeg(file=paste('C:/Users/acer/Desktop/Second Semester Thesis Files/Graphs/Normal/Ttest/sdr/',as.character(x1),'.',as.character(x2),'.jpeg',sep=''), width=2048, height=1024)
    hist(ttest_teststatmatrix[,paste('ttest',as.character(x1),'.',as.character(x2),' ','sdr1',sep='')],
        main=paste("Student's T-test Normal: ( n1 , n2 ) = (",x1,',',x2,')',sep=' '),
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
    lines(density(ttest_teststatmatrix[,paste("ttest",as.character(x1),'.',as.character(x2),' ','sdr1',sep='')]),col='#b22222', lwd=3)
    lines(density(ttest_teststatmatrix[,paste("ttest",as.character(x1),'.',as.character(x2),' ','sdr1.1',sep='')]),col='#ff8c00', lwd=3)
    lines(density(ttest_teststatmatrix[,paste("ttest",as.character(x1),'.',as.character(x2),' ','sdr1.25',sep='')]),col='#008000', lwd=3)
    lines(density(ttest_teststatmatrix[,paste("ttest",as.character(x1),'.',as.character(x2),' ','sdr1.5',sep='')]),col='#0000ff', lwd=3)
    lines(density(ttest_teststatmatrix[,paste("ttest",as.character(x1),'.',as.character(x2),' ','sdr2',sep='')]),col='#ff69b4', lwd=3)
    legend(2.5, 0.6, legend=c("SDR=1","SDR=1.1","SDR=1.25","SDR=1.5","SDR=2","t distribution"), text.font = 4,cex=2,box.lwd = 3,fill = c("#b22222","#ff8c00","#008000","#0000ff","#ff69b4","black"))
    curve(dt(x, df=x1+x2-2), add=T, col='black', lwd=4)
    segments(x0 = qt(0.2000,x1+x2-2),x1 = qt(0.2000,x1+x2-2),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
    segments(x0 = qt(0.8000,x1+x2-2),x1 = qt(0.8000,x1+x2-2),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
    dev.off()
      

  }
}



for (a in c(25,50,100,200,300)) 
{
  for (b in c(25,50,100,200,300)) 
  {
    
    x1 <- a 
    x2 <- b 
    
    jpeg(file=paste('C:/Users/acer/Desktop/Second Semester Thesis Files/Graphs/Normal/Welch/sdr/',as.character(x1),'.',as.character(x2),'.jpeg',sep=''), width=2048, height=1024)
    hist(welch_teststatmatrix[,paste('welch',as.character(x1),'.',as.character(x2),' ','sdr1',sep='')],
         main=paste("Welch's T-test Normal: ( n1 , n2 ) = (",x1,',',x2,')',sep=' '),
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
    lines(density(welch_teststatmatrix[,paste("welch",as.character(x1),'.',as.character(x2),' ','sdr1',sep='')]),col='#b22222', lwd=3)
    lines(density(welch_teststatmatrix[,paste("welch",as.character(x1),'.',as.character(x2),' ','sdr1.1',sep='')]),col='#ff8c00', lwd=3)
    lines(density(welch_teststatmatrix[,paste("welch",as.character(x1),'.',as.character(x2),' ','sdr1.25',sep='')]),col='#008000', lwd=3)
    lines(density(welch_teststatmatrix[,paste("welch",as.character(x1),'.',as.character(x2),' ','sdr1.5',sep='')]),col='#0000ff', lwd=3)
    lines(density(welch_teststatmatrix[,paste("welch",as.character(x1),'.',as.character(x2),' ','sdr2',sep='')]),col='#ff69b4', lwd=3)
    legend(2.5, 0.6, legend=c("SDR=1","SDR=1.1","SDR=1.25","SDR=1.5","SDR=2","t distribution"), text.font = 4,cex=2,box.lwd = 3,fill = c("#b22222","#ff8c00","#008000","#0000ff","#ff69b4","black"))
    curve(dt(x, df=100), add=T, col='black', lwd=4)
    segments(x0 = qt(0.2000,100),x1 = qt(0.2000,100),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
    segments(x0 = qt(0.8000,100),x1 = qt(0.8000,100),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
    dev.off()
    
    
  }
}



for (b in c(25,50,100,200,300)) 
{
  for (SDR in c(1,1.1,1.25,1.5,2)) 
  {
    
    x1 <- b 
    x2 <- SDR 
    
    jpeg(file=paste('C:/Users/acer/Desktop/Second Semester Thesis Files/Graphs/Normal/Ttest/n1/','.',as.character(x1),' sdr',as.character(x2),'.jpeg',sep=''), width=2048, height=1024)
    hist(ttest_teststatmatrix[,paste('ttest','25.',as.character(x1),' sdr',as.character(x2),sep='')],
         main=paste("Student's T-test Normal: ( n2 , sdr ) = (",x1,',',x2,')',sep=' '),
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
    lines(density(ttest_teststatmatrix[,paste("ttest",'25.',as.character(x1),' sdr',as.character(x2),sep='')]),col='#b22222', lwd=3)
    lines(density(ttest_teststatmatrix[,paste("ttest",'50.',as.character(x1),' sdr',as.character(x2),sep='')]),col='#ff8c00', lwd=3)
    lines(density(ttest_teststatmatrix[,paste("ttest",'100.',as.character(x1),' sdr',as.character(x2),sep='')]),col='#008000', lwd=3)
    lines(density(ttest_teststatmatrix[,paste("ttest",'200.',as.character(x1),' sdr',as.character(x2),sep='')]),col='#0000ff', lwd=3)
    lines(density(ttest_teststatmatrix[,paste("ttest",'300.',as.character(x1),' sdr',as.character(x2),sep='')]),col='#ff69b4', lwd=3)
    legend(2.5, 0.6, legend=c("n1=25","n1=50","n1=100","n1=200","n1=300","t distribution"), text.font = 4,cex=2,box.lwd = 3,fill = c("#b22222","#ff8c00","#008000","#0000ff","#ff69b4","black"))
    curve(dt(x, df=x1+x2-2), add=T, col='black', lwd=4)
    segments(x0 = qt(0.2000,x1+x2-2),x1 = qt(0.2000,x1+x2-2),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
    segments(x0 = qt(0.8000,x1+x2-2),x1 = qt(0.8000,x1+x2-2),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
    dev.off()
    
    
  }
}


for (b in c(25,50,100,200,300)) 
{
  for (SDR in c(1,1.1,1.25,1.5,2)) 
  {
    
    x1 <- b 
    x2 <- SDR 
    
    jpeg(file=paste('C:/Users/acer/Desktop/Second Semester Thesis Files/Graphs/Normal/Welch/n1/','.',as.character(x1),' sdr',as.character(x2),'.jpeg',sep=''), width=2048, height=1024)
    hist(welch_teststatmatrix[,paste('welch','25.',as.character(x1),' sdr',as.character(x2),sep='')],
         main=paste("Welch's T-test Normal: ( n2 , sdr ) = (",x1,',',x2,')',sep=' '),
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
    lines(density(welch_teststatmatrix[,paste("welch",'25.',as.character(x1),' sdr',as.character(x2),sep='')]),col='#b22222', lwd=3)
    lines(density(welch_teststatmatrix[,paste("welch",'50.',as.character(x1),' sdr',as.character(x2),sep='')]),col='#ff8c00', lwd=3)
    lines(density(welch_teststatmatrix[,paste("welch",'100.',as.character(x1),' sdr',as.character(x2),sep='')]),col='#008000', lwd=3)
    lines(density(welch_teststatmatrix[,paste("welch",'200.',as.character(x1),' sdr',as.character(x2),sep='')]),col='#0000ff', lwd=3)
    lines(density(welch_teststatmatrix[,paste("welch",'300.',as.character(x1),' sdr',as.character(x2),sep='')]),col='#ff69b4', lwd=3)
    legend(2.5, 0.6, legend=c("n1=25","n1=50","n1=100","n1=200","n1=300","t distribution"), text.font = 4,cex=2,box.lwd = 3,fill = c("#b22222","#ff8c00","#008000","#0000ff","#ff69b4","black"))
    curve(dt(x, df=100), add=T, col='black', lwd=4)
    segments(x0 = qt(0.2000,100),x1 = qt(0.2000,100),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
    segments(x0 = qt(0.8000,100),x1 = qt(0.8000,100),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
    dev.off()
    
    
  }
}




for (a in c(25,50,100,200,300)) 
{
  for (SDR in c(1,1.1,1.25,1.5,2)) 
  {
    
    x1 <- a 
    x2 <- SDR 
    
    jpeg(file=paste('C:/Users/acer/Desktop/Second Semester Thesis Files/Graphs/Normal/Ttest/n2/',as.character(x1),'.',' sdr',as.character(x2),'.jpeg',sep=''), width=2048, height=1024)
    hist(ttest_teststatmatrix[,paste('ttest',as.character(x1),'.25',' sdr',as.character(x2),sep='')],
         main=paste("Student's T-test Normal: ( n1 , sdr ) = (",x1,',',x2,')',sep=' '),
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
    lines(density(ttest_teststatmatrix[,paste("ttest",as.character(x1),'.25',' sdr',as.character(x2),sep='')]),col='#b22222', lwd=3)
    lines(density(ttest_teststatmatrix[,paste("ttest",as.character(x1),'.50',' sdr',as.character(x2),sep='')]),col='#ff8c00', lwd=3)
    lines(density(ttest_teststatmatrix[,paste("ttest",as.character(x1),'.100',' sdr',as.character(x2),sep='')]),col='#008000', lwd=3)
    lines(density(ttest_teststatmatrix[,paste("ttest",as.character(x1),'.200',' sdr',as.character(x2),sep='')]),col='#0000ff', lwd=3)
    lines(density(ttest_teststatmatrix[,paste("ttest",as.character(x1),'.300',' sdr',as.character(x2),sep='')]),col='#ff69b4', lwd=3)
    legend(2.5, 0.6, legend=c("n2=25","n2=50","n2=100","n2=200","n2=300","t distribution"), text.font = 4,cex=2,box.lwd = 3,fill = c("#b22222","#ff8c00","#008000","#0000ff","#ff69b4","black"))
    curve(dt(x, df=x1+x2-2), add=T, col='black', lwd=4)
    segments(x0 = qt(0.2000,x1+x2-2),x1 = qt(0.2000,x1+x2-2),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
    segments(x0 = qt(0.8000,x1+x2-2),x1 = qt(0.8000,x1+x2-2),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
    dev.off()
    
    
  }
}


for (a in c(25,50,100,200,300)) 
{
  for (SDR in c(1,1.1,1.25,1.5,2)) 
  {
    
    x1 <- a 
    x2 <- SDR 
    
    jpeg(file=paste('C:/Users/acer/Desktop/Second Semester Thesis Files/Graphs/Normal/Welch/n2/',as.character(x1),'.',' sdr',as.character(x2),'.jpeg',sep=''), width=2048, height=1024)
    hist(welch_teststatmatrix[,paste('welch',as.character(x1),'.25',' sdr',as.character(x2),sep='')],
         main=paste("Welch's T-test Normal: ( n1 , sdr ) = (",x1,',',x2,')',sep=' '),
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
    lines(density(welch_teststatmatrix[,paste("welch",as.character(x1),'.25',' sdr',as.character(x2),sep='')]),col='#b22222', lwd=3)
    lines(density(welch_teststatmatrix[,paste("welch",as.character(x1),'.50',' sdr',as.character(x2),sep='')]),col='#ff8c00', lwd=3)
    lines(density(welch_teststatmatrix[,paste("welch",as.character(x1),'.100',' sdr',as.character(x2),sep='')]),col='#008000', lwd=3)
    lines(density(welch_teststatmatrix[,paste("welch",as.character(x1),'.200',' sdr',as.character(x2),sep='')]),col='#0000ff', lwd=3)
    lines(density(welch_teststatmatrix[,paste("welch",as.character(x1),'.300',' sdr',as.character(x2),sep='')]),col='#ff69b4', lwd=3)
    legend(2.5, 0.6, legend=c("n2=25","n2=50","n2=100","n2=200","n2=300","t distribution"), text.font = 4,cex=2,box.lwd = 3,fill = c("#b22222","#ff8c00","#008000","#0000ff","#ff69b4","black"))
    curve(dt(x, df=100), add=T, col='black', lwd=4)
    segments(x0 = qt(0.2000,100),x1 = qt(0.2000,100),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
    segments(x0 = qt(0.8000,100),x1 = qt(0.8000,100),y0 = -0.3,y1 = 0.4,lwd = 2,lty=5,col = "black")
    dev.off()
    
    
  }
}