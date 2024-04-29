install.packages("gnorm")
library("gnorm")
install.packages("moments")
library("moments")

numrep <- 10000 
counter <- 0
count10k <- 0

testvector <- NULL


teststatmatrix <- matrix(nrow=numrep,ncol=0)


betahatmatrix <- matrix(nrow=1,ncol=0)

column.labelspar <- NULL


HTNormal <- function(a, b, d, hypotest)
{
  text=paste("Normal",a,'.',b,' sdr',d/4,sep='')
  if(hypotest=='ttest'){text=paste('T',text,sep='')}else{text=paste('W',text,sep='')}
  
  normal.1 <- rnorm(a, mean=mumatrix[1,text], sd=d) 
  normal.2 <- rnorm(b, mean=0, sd=4)

  if(hypotest=='ttest')
  {
    x <- t.test(normal.1, normal.2, var.equal = TRUE) 
    x.stat <- x$statistic 
    nbhdlength=criticalvalueTtestmatrix[1,text]
  }
  else
  {
    x <- t.test(normal.1, normal.2, var.equal = FALSE) 
    x.stat <- x$statistic 
    df=floor(x$parameter)
    nbhdlength=criticalvalueperdfmatrix[df,text]
  }
  if(abs(x.stat)<nbhdlength){result="FAIL"}else{result="REJECT"}
  return(list(result=result,x.stat=x.stat,x=x,text=text))
}
HTGenNormal <- function(a, b, e, SDR, hypotest)
{
  text=paste("GenNormal",a,'.',b,' sdr',SDR,' shape',e,sep='')
  if(hypotest=='ttest'){text=paste('T',text,sep='')}else{text=paste('W',text,sep='')}
  
  gnormal.1 <- rgnorm(a, mu=mumatrix[1,text], alpha=2*sqrt(2)*sqrt(SDR), beta=e) 
  gnormal.2 <- rgnorm(b, mu=0, alpha=2*sqrt(2), beta=e)
  
  if(hypotest=='ttest')
  {
    x <- t.test(gnormal.1, gnormal.2, var.equal = TRUE) 
    x.stat <- x$statistic 
    nbhdlength=criticalvalueTtestmatrix[1,text]
  }
  else
  {
    x <- t.test(gnormal.1, gnormal.2, var.equal = FALSE) 
    x.stat <- x$statistic
    df=floor(x$parameter)
    nbhdlength=criticalvalueperdfmatrix[df,text]
  }
  
  if(abs(x.stat)<nbhdlength){result="FAIL"}else{result="REJECT"}
  return(list(result=result,x.stat=x.stat,x=x,text=text))
}
HTGammaeq <- function(a, b, d, e, hypotest)
{
  sk=2/sqrt(d)
  text=paste("GammaEqual",a,'.',b,' sk',sk,' sdr',e,sep='')
  if(hypotest=='ttest'){text=paste('T',text,sep='')}else{text=paste('W',text,sep='')}
  
  gamma.1 <- rgamma(a, shape=d, scale=e) 
  gamma.2 <- rgamma(b, shape=d, scale=1) 
  
  gamma.1 = gamma.1-d*e+mumatrix[1,text]
  gamma.2 = gamma.2-d
  
  if(hypotest=='ttest')
  {
    x <- t.test(gamma.1, gamma.2, var.equal = TRUE) 
    x.stat <- x$statistic  
    nbhdlength=criticalvalueTtestmatrix[1,text]
  }
  else
  {
    x <- t.test(gamma.1, gamma.2, var.equal = FALSE) 
    x.stat <- x$statistic 
    df=floor(x$parameter)
    nbhdlength=criticalvalueperdfmatrix[df,text]
  }
  
  if(abs(x.stat)<nbhdlength){result="FAIL"}else{result="REJECT"}
  return(list(result=result,x.stat=x.stat,x=x,text=text))
}
HTGammauneq <- function(a, b, d, SDR, hypotest)
{
  sk=2/sqrt(d)
  shape2=(2/(2/sqrt(d)-1))^2  
  e=SDR*sqrt(shape2/d)
  text=paste("GammaUnequal",a,'.',b,' sk',sk,' sdr',SDR,sep='')
  if(hypotest=='ttest'){text=paste('T',text,sep='')}else{text=paste('W',text,sep='')}
  
  gamma.1 <- rgamma(a, shape=d, scale=e) 
  gamma.2 <- rgamma(b, shape=shape2, scale=1)
  
  gamma.1 = gamma.1-d*e+mumatrix[1,text]
  gamma.2 = gamma.2-shape2
  
  if(hypotest=='ttest')
  {
    x <- t.test(gamma.1, gamma.2, var.equal = TRUE)
    x.stat <- x$statistic 
    nbhdlength=criticalvalueTtestmatrix[1,text]
  }
  else
  {
    x <- t.test(gamma.1, gamma.2, var.equal = FALSE) 
    x.stat <- x$statistic 
    df=floor(x$parameter)
    nbhdlength=criticalvalueperdfmatrix[df,text]
  }
  
  if(abs(x.stat)<nbhdlength){result="FAIL"}else{result="REJECT"}
  return(list(result=result,x.stat=x.stat,x=x,text=text))
}



for (a in c(25,50,100)) 
{
  for (b in c(25,50,100)) 
  {
    for (d in c(4,6,8)) 
    {
      for(hypotest in c('ttest','welch'))
      {
        counter=0
        for(i in 1:numrep)
        { 
          u=HTNormal(a, b, d, hypotest) 
          if(u$result=='FAIL')
          {
            counter <- counter+1
          }
          testvector[i]=u$x.stat
        }
        teststatmatrix <- cbind(teststatmatrix,testvector)
        betahatmatrix <- cbind(betahatmatrix,counter)
        column.labelspar <- cbind(column.labelspar,u$text)
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
          counter=0
          for(i in 1:numrep)
          { 
            u=HTGenNormal(a, b, e, SDR, hypotest)  
            if(u$result=='FAIL')
            {
              counter <- counter+1
            }
            testvector[i]=u$x.stat
          }
          teststatmatrix <- cbind(teststatmatrix,testvector)
          betahatmatrix <- cbind(betahatmatrix,counter)
          column.labelspar <- cbind(column.labelspar,u$text)
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
          counter=0
          for(i in 1:numrep)
          { 
            u=HTGammaeq(a, b, d, e, hypotest) 
            if(u$result=='FAIL')
            {
              counter <- counter+1
            }
            testvector[i]=u$x.stat
          }
          teststatmatrix <- cbind(teststatmatrix,testvector)
          betahatmatrix <- cbind(betahatmatrix,counter)
          column.labelspar <- cbind(column.labelspar,u$text)
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
          counter=0
          for(i in 1:numrep)
          { 
            u=HTGammauneq(a, b, d, SDR, hypotest)
            if(u$result=='FAIL')
            {
              counter <- counter+1
            }
            testvector[i]=u$x.stat
          }
          teststatmatrix <- cbind(teststatmatrix,testvector)
          betahatmatrix <- cbind(betahatmatrix,counter)
          column.labelspar <- cbind(column.labelspar,u$text)
        }
      }
    }
  }
}


colnames(teststatmatrix) <- column.labelspar
colnames(betahatmatrix) <- column.labelspar


