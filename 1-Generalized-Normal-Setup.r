install.packages("gnorm")
library("gnorm")
install.packages("moments")
library("moments")

numrep <- 10000 
numresamples <- 1000 

counterleft <- 0
counterright <- 0
count10k <- 0


boots.count10kmatrix <- matrix(nrow=1,ncol=0)
perms.count10kmatrix <- matrix(nrow=1,ncol=0)

testvector <- NULL

ttest.teststatmatrix <- matrix(nrow=numrep,ncol=0)
welch.teststatmatrix <- matrix(nrow=numrep,ncol=0)
boots.teststatmatrix <- matrix(nrow=numrep,ncol=0)
perms.teststatmatrix <- matrix(nrow=numrep,ncol=0)

pvalleftvector <- NULL
pvalrightvector <- NULL

boots.pvalleftmatrix <- matrix(nrow=numrep,ncol=0)
perms.pvalleftmatrix <- matrix(nrow=numrep,ncol=0)
boots.pvalrightmatrix <- matrix(nrow=numrep,ncol=0)
perms.pvalrightmatrix <- matrix(nrow=numrep,ncol=0)

ttest.betahatleftmatrix <- matrix(nrow=1,ncol=0)
ttest.betahatrightmatrix <- matrix(nrow=1,ncol=0)
welch.betahatleftmatrix <- matrix(nrow=1,ncol=0)
welch.betahatrightmatrix <- matrix(nrow=1,ncol=0)
boots.betahatleftmatrix <- matrix(nrow=1,ncol=0)
boots.betahatrightmatrix <- matrix(nrow=1,ncol=0)
perms.betahatleftmatrix <- matrix(nrow=1,ncol=0)
perms.betahatrightmatrix <- matrix(nrow=1,ncol=0)

ttest.column.labelspar <- NULL
welch.column.labelspar <- NULL
boots.column.labelspar <- NULL
perms.column.labelspar <- NULL


funcpar <- function(a, b, e, SDR, hypotest) 
{
  gnormal.1 <- rgnorm(a, mu=5, alpha=2*sqrt(2)*sqrt(SDR), beta=e) 
  gnormal.2 <- rgnorm(b, mu=0, alpha=2*sqrt(2), beta=e) 
  
  if(hypotest=="ttest")
  {
    x <- t.test(gnormal.1, gnormal.2, var.equal = TRUE) 
    x.stat <- x$statistic 
  }
  
  if(hypotest=="welch")
  {
    x <- t.test(gnormal.1, gnormal.2, var.equal = FALSE) 
    x.stat <- x$statistic 
  }
  
  n1=a
  n2=b
  return(list(x.stat=x.stat, x=x, gnormal.1=gnormal.1, gnormal.2=gnormal.2, n1=n1, n2=n2))
}

funcnonpar <- function(a, b, e, SDR, hypotest) 
{
  gnormal.1 <- rgnorm(a, mu=5, alpha=2*sqrt(2)*sqrt(SDR), beta=e) 
  gnormal.2 <- rgnorm(b, mu=0, alpha=2*sqrt(2), beta=e) 
  
 
  overall <- c(gnormal.1,gnormal.2)
  
  
  stat1 <- gnormal.1-mean(gnormal.1)+mean(overall)
  stat2 <- gnormal.2-mean(gnormal.2)+mean(overall)
  
 
  x <- (t.test(gnormal.1, gnormal.2, var.equal = FALSE))
  tobs=x$statistic-5/sqrt(var(gnormal.1)/a+var(gnormal.2)/b)
  tvec <- rep(0,numresamples)
  
 
  if(hypotest=="boots")
  {
    
    for (i in 1:numresamples)
    {
      newdata1 <- sample(stat1,a,replace=T)
      newdata2 <- sample(stat2,b,replace=T)
      t <- (t.test(newdata1, newdata2, var.equal = FALSE))$statistic 
      tvec[i] <- t
    }
  }
  
  if(hypotest=="perms")
  {
    for (i in 1:numresamples)
    {
      permsample <- sample(overall) 
      permsample1 <- permsample[1:a]
      permsample2 <- permsample[a+1:b] 
      tvec[i] <- (t.test(permsample1, permsample2, var.equal = FALSE))$statistic
    }
  }
  
  pvalleft <- mean(tvec <= tobs) 
  pvalright <- mean(tvec >= tobs) 
  n1=a
  n2=b
  return(list(pvalleft=pvalleft, pvalright=pvalright, tobs=tobs, gnormal.1=gnormal.1, gnormal.2=gnormal.2, n1=n1, n2=n2))
}





for (a in c(25,50,100,200,300)) 
{
  for (b in c(25,50,100,200,300)) 
  {
    for (e in c(0.5,1,3,10)) 
    {
      for (SDR in c(1,1.1,1.25,1.5,2)) 
      {
        for (hypotest in c("ttest","welch"))
        {
          for(i in 1:numrep)
          { 
            u=funcpar(a,b,e,SDR,hypotest)
            sx=sd(u$gnormal.1)
            sy=sd(u$gnormal.2)
            if(hypotest=="ttest")
            {
              tbetaleft=qt(0.2000,u$n1+u$n2-2)
              tbetaright=qt(0.8000,u$n1+u$n2-2)
              sp=sqrt((((u$n1-1)*(sx^2))+((u$n2-1)*(sy^2)))/(u$n1+u$n2-2))
              sq=sqrt(1/u$n1+1/u$n2)
              adjstat=u$x.stat-5/(sp*sq)
            }
            if(hypotest=="welch")
            {
              tbetaleft=qt(0.2000,u$x$parameter)
              tbetaright=qt(0.8000,u$x$parameter)
              sq=sqrt((sx^2)/u$n1+(sy^2)/u$n2)
              adjstat=u$x.stat-5/(sq)
            }
            if(adjstat < tbetaleft){counterleft <- counterleft+1}
            if(adjstat > tbetaright){counterright <- counterright+1}
            count10k <- count10k+1
            testvector[i] <- adjstat
          }
          if(hypotest=="ttest")
          {
            ttest.teststatmatrix <- cbind(ttest.teststatmatrix,testvector)
            ttest.betahatleftmatrix <- cbind(ttest.betahatleftmatrix,counterleft)
            ttest.betahatrightmatrix <- cbind(ttest.betahatrightmatrix,counterright)
            ttest.column.labelspar <- cbind(ttest.column.labelspar,paste("ttest",a,'.',b,' sdr',SDR,' shape',e,sep=''))
          }
          if(hypotest=="welch")
          {
            welch.teststatmatrix <- cbind(welch.teststatmatrix,testvector)
            welch.betahatleftmatrix <- cbind(welch.betahatleftmatrix,counterleft)
            welch.betahatrightmatrix <- cbind(welch.betahatrightmatrix,counterright)
            welch.column.labelspar <- cbind(welch.column.labelspar,paste("welch",a,'.',b,' sdr',SDR,' shape',e,sep=''))
          }
          counterleft <-0
          counterright <-0
          testvector <- NULL
          count10k <- 0
        }
      }
    }
  }
}
colnames(ttest.teststatmatrix) <- ttest.column.labelspar
colnames(ttest.betahatleftmatrix) <- ttest.column.labelspar
colnames(ttest.betahatrightmatrix) <- ttest.column.labelspar
colnames(welch.teststatmatrix) <- welch.column.labelspar
colnames(welch.betahatleftmatrix) <- welch.column.labelspar
colnames(welch.betahatrightmatrix) <- welch.column.labelspar





for (a in c(25)) 
{
  for (b in c(25)) 
  {
    for (e in c(0.5))
    {
      for (SDR in c(1)) 
      {
        for (hypotest in c("boots","perms")) 
        {
          for(i in 1:numrep)
          { 
            u=funcnonpar(a,b,e,SDR,hypotest)
            if(u$pvalleft < 0.2){counterleft <- counterleft+1}
            if(u$pvalright < 0.2){counterright <- counterright+1}
            count10k <- count10k+1
            testvector[i] <- u$tobs
            pvalleftvector[i] <- u$pvalleft
            pvalrightvector[i] <- u$pvalright
          }
          if(hypotest=="boots")
          {
            boots.pvalleftmatrix <- cbind(boots.pvalleftmatrix,pvalleftvector)
            boots.pvalrightmatrix <- cbind(boots.pvalrightmatrix,pvalrightvector)
            boots.teststatmatrix <- cbind(boots.teststatmatrix,testvector)
            boots.betahatleftmatrix <- cbind(boots.betahatleftmatrix,counterleft)
            boots.betahatrightmatrix <- cbind(boots.betahatrightmatrix,counterright)
            boots.column.labelspar <- cbind(boots.column.labelspar,paste("boots",a,'.',b,' sdr',SDR,' shape',e,sep=''))
            boots.count10kmatrix <- cbind(boots.count10kmatrix,count10k)
          }
          if(hypotest=="perms")
          {
            perms.pvalleftmatrix <- cbind(perms.pvalleftmatrix,pvalleftvector)
            perms.pvalrightmatrix <- cbind(perms.pvalrightmatrix,pvalrightvector)
            perms.teststatmatrix <- cbind(perms.teststatmatrix,testvector)
            perms.betahatleftmatrix <- cbind(perms.betahatleftmatrix,counterleft)
            perms.betahatrightmatrix <- cbind(perms.betahatrightmatrix,counterright)
            perms.column.labelspar <- cbind(perms.column.labelspar,paste("perms",a,'.',b,' sdr',SDR,' shape',e,sep=''))
            perms.count10kmatrix <- cbind(perms.count10kmatrix,count10k)
          }
          print(paste("left ",hypotest,a,'.',b,' sdr',SDR,' shape',e,": ",counterleft,sep=''))
          print(paste("right ",hypotest,a,'.',b,' sdr',SDR,' shape',e,": ",counterright,sep=''))
          counterleft <- 0
          counterright <- 0
          testvector <- NULL
          pvalleftvector <- NULL
          pvalrightvector <- NULL
          count10k <- 0
        }
      }
    }
  }
}
colnames(boots.pvalleftmatrix) <- boots.column.labelspar
colnames(boots.pvalrightmatrix) <- boots.column.labelspar
colnames(perms.pvalleftmatrix) <- perms.column.labelspar
colnames(perms.pvalrightmatrix) <- perms.column.labelspar

colnames(boots.teststatmatrix) <- boots.column.labelspar
colnames(boots.betahatleftmatrix) <- boots.column.labelspar
colnames(boots.betahatrightmatrix) <- boots.column.labelspar
colnames(perms.teststatmatrix) <- perms.column.labelspar
colnames(perms.betahatleftmatrix) <- perms.column.labelspar
colnames(perms.betahatrightmatrix) <- perms.column.labelspar

colnames(boots.count10kmatrix) <- boots.column.labelspar
colnames(perms.count10kmatrix) <- perms.column.labelspar



ttest_teststatmatrix <- as.matrix(ttest_teststatmatrix)
welch_teststatmatrix <- as.matrix(welch_teststatmatrix)

for (a in c(25,50,100,200,300)) 
{
  for (b in c(25,50,100,200,300)) 
  {
    for (e in c(0.5,1,3,10)) 
    {
      for (SDR in c(1,1.1,1.25,1.5,2)) 
      {
        x1 <- a 
        x2 <- b 
        x3 <- e 
        x4 <- SDR 
        
        jpeg(file=paste('C:/Users/acer/Desktop/Second Semester Thesis Files/Graphs/Generalized Normal/Ttest/individual/',as.character(x1),'.',as.character(x2),' ','sdr',as.character(x4),' shape',as.character(x3),'.jpeg',sep=''), width=2048, height=1024)
        hist(ttest_teststatmatrix[,paste('ttest',as.character(x1),'.',as.character(x2),' ','sdr',as.character(x4),' shape',as.character(x3),sep='')],
             main=paste("Student's T-test Gen Normal: ( n1 , n2 , sdr, shape ) = (",x1,',',x2,',',x4,',',x3,')',sep=' '),
             xlab=paste("Test Statistics; Failure to Reject the Null Hypothesis: ",ttest_betahatleftmatrix[,paste('ttest',as.character(x1),'.',as.character(x2),' ','sdr',as.character(x4),' shape',as.character(x3),sep='')],sep=''),
             xlim=c(-3.5,3.5),
             col="white",
             freq=FALSE,
             ylim=c(0,0.6),
             breaks='Scott',
             cex.main=1.75,
             cex.lab=1.5
        )
        lines(density(ttest_teststatmatrix[,paste('ttest',as.character(x1),'.',as.character(x2),' ','sdr',as.character(x4),' shape',as.character(x3),sep='')]),col='blue', lwd=3)
        curve(dt(x, df=x1+x2-2), add=T, col='black', lwd=3)
        abline(v=qt(0.2000,x1+x2-2), col="red", lwd=3)
        
        dev.off()
      }
    }
  }
}



for (a in c(25,50,100,200,300)) 
{
  for (b in c(25,50,100,200,300)) 
  {
    for (e in c(0.5,1,3,10)) 
    {
      for (SDR in c(1,1.1,1.25,1.5,2)) 
      {
        x1 <- a 
        x2 <- b 
        x3 <- e 
        x4 <- SDR 
        
        jpeg(file=paste('C:/Users/acer/Desktop/Second Semester Thesis Files/Graphs/Generalized Normal/Welch/individual/',as.character(x1),'.',as.character(x2),' ','sdr',as.character(x4),' shape',as.character(x3),'.jpeg',sep=''), width=2048, height=1024)
        hist(welch_teststatmatrix[,paste('welch',as.character(x1),'.',as.character(x2),' ','sdr',as.character(x4),' shape',as.character(x3),sep='')],
             main=paste("Welch's T-test Gen Normal: ( n1 , n2 , sdr, shape ) = (",x1,',',x2,',',x4,',',x3,')',sep=' '),
             xlab=paste("Test Statistics; Failure to Reject the Null Hypothesis: ",welch_betahatleftmatrix[,paste('welch',as.character(x1),'.',as.character(x2),' ','sdr',as.character(x4),' shape',as.character(x3),sep='')],sep=''),
             xlim=c(-3.5,3.5),
             col="white",
             freq=FALSE,
             ylim=c(0,0.6),
             breaks='Scott',
             cex.main=1.75,
             cex.lab=1.5
        )
        
        vnum=(x3^2+x1/x2)^2
        vden=(x3^4/(x1-1))+(1/(x2-1))*(x1/x2)^2
        v=vnum/vden
        lines(density(welch_teststatmatrix[,paste('welch',as.character(x1),'.',as.character(x2),' ','sdr',as.character(x4),' shape',as.character(x3),sep='')]),col='blue', lwd=3)
        curve(dt(x, df=v), add=T, col='black', lwd=3)
        abline(v=qt(0.2000,v), col="red", lwd=3)
        
        dev.off()
      }
    }
  }
}



save.image(file="TtestNormal.RData")