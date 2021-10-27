7.2 Welch test R-code:
  7.3 Kruskal-Wallis R-code:
  #_________Analysis of variance_________#
  #--------------------------------------#
  # Normal (0,1) #
  #--------------------------------------#
  #-----Small sample sizes (5,5,5)------#
  #...... Type I error .....#
  n1=5; n2=5;n3=5
  s1=1; s2=1; s3=1; mu1=0; mu2=0;mu3=0 
  m=1000
  pvalue=NULL
  stat=NULL
  x=matrix(rnorm(n1*m,mu1,s1),nrow=m)
  y=matrix(rnorm(n2*m,mu2,s2),nrow=m)
  z=matrix(rnorm(n3*m,mu3,s3),nrow=m)
  for (i in 1:m) {
    set.seed(6)
    d=stack(list(sample1=x[i,],sample2=y[i,],sample3=z[i,])) 
    pvalue[i]=oneway.test(values~ind,data=d,var.equal=T)$p.value 
    stat[i]=oneway.test(values~ind,data=d,var.equal=T)$statistic
  }
  Type.I.error=sum(pvalue<=0.05)/m ;Type.I.error
  plot(density(stat),ylim=c(0,0.9),main="The shape of the empirical and theoretical 
distribution",xlim=c(-10,10))
  curve(dnorm(x,0,1),add=TRUE,lty=2,col=4)
  
  
  
  
  
  
  #______________Welch test______________#
  #--------------------------------------#
  # Normal (0,1) #
  #--------------------------------------#
  #-----Small sample sizes (5,5,5)------#
  #...... Type I error .....#
  n1=5; n2=5; s1=1; s2=1; mu1=0; mu2=0
  m=1000;pvalue=NULL
  z=NULL
  x=matrix(rnorm(n1*m,mu1,s1),nrow=m)
  y=matrix(rnorm(n2*m,mu2,s2),nrow=m)
  for(i in 1:m){ pvalue[i]= t.test(x[i,],y[i,],equal.var=F, alt='t')$p.value
  z[i]=t.test(x[i,],y[i,],equal.var=F, alt='t')$statistic
  }
  Type.I.error=sum(pvalue<=0.05)/m
  Type.I.error
  plot(density(z),ylim = c(0,0.5) ,main = "The shape of the Empirical and Theoretical distribution for 
normal when n=5",xlim=c(-10,10))
  curve(dnorm(x),col=5,add=T)
  
  #______________Welch test______________#
  #--------------------------------------#
  # Normal (0,1) #
  #--------------------------------------#
  #-----Small sample sizes (5,5,5)------#
  #...... Power .....#
  n1=5; n2=5; s1=1; s2=1; mu1=1; mu2=0
  m=1000;pvalue=NULL
  x=matrix(rnorm(n1*m,mu1,s1),nrow=m) 
  y=matrix(rnorm(n2*m,mu2,s2),nrow=m) 
  for(i in 1:m) 
  { pvalue[i]= t.test(x[i,],y[i,],equal.var=F, alt='t')$p.value} 
  Power = sum(pvalue<=0.05)/m
  Power
  p1=c(0.282,0.248,0.26,0.271,0.256,0.254)
  p1
  plot(p1,xlim=c(0.5,6),type="l" , main = " power plot normal distribution with n=5")
  
  
  #______________Welch test______________#
  #--------------------------------------#
  # Normal (0,1) #
  #--------------------------------------#
  #----Large sample sizes (30,30,30)----#
  #...... Type I error .....#
  n1=30; n2=30; s1=1; s2=1; mu1=0; mu2=0
  m=1000;pvalue=NULL
  z=NULL
  x=matrix(rnorm(n1*m,mu1,s1),nrow=m)
  y=matrix(rnorm(n2*m,mu2,s2),nrow=m)
  for(i in 1:m){ pvalue[i]= t.test(x[i,],y[i,],equal.var=F, alt='t')$p.value
  z[i]=t.test(x[i,],y[i,],equal.var=F, alt='t')$statistic
  }
  Type.I.error=sum(pvalue<=0.05)/m
  Type.I.error
  plot(density(z),ylim = c(0,0.5), main = "The shape of the Empirical and Theoretical distribution for 
normal when n=30",xlim=c(-10,10))
  curve(dnorm(x),col=5,add=T)
  
  
  #______________Welch test______________#
  #--------------------------------------#
  # Normal (0,1) #
  #--------------------------------------#
  #----Large sample sizes (30,30,30)----#
  #...... Power .....#
  n1=30; n2=30; s1=1; s2=1; mu1=1; mu2=0
  m=1000;pvalue=NULL
  x=matrix(rnorm(n1*m,mu1,s1),nrow=m) 
  y=matrix(rnorm(n2*m,mu2,s2),nrow=m) 
  for(i in 1:m) 
  { pvalue[i]= t.test(x[i,],y[i,],equal.var=F, alt='t')$p.value} 
  Power=sum(pvalue<=0.05)/m
  Power
  p=c(0.953,0.966,0.971,0.98,0.997,1)
  plot(p,xlim=c(0.5,6),type="l" ,main = "power plot normal distribution when n=30")
  
  
  #______________Welch test______________#
  #--------------------------------------#
  # Chi-square(3) #
  #--------------------------------------#
  #-----Small sample sizes (5,5,5)------#
  #...... Type I error .....#
  n1=5;n2=5
  m=1000;pvalue=NULL
  z=NULL
  x=matrix(rchisq(n1*m,df=3),nrow=m)
  y=matrix(rchisq(n2*m,df=3),nrow=m)
  for(i in 1:m){ pvalue[i]= t.test(x[i,],y[i,],equal.var=F, alt='t')$p.value
  z[i]=t.test(x[i,],y[i,],equal.var=F, alt='t')$statistic
  }
  Type.I.error=sum(pvalue<=0.05)/m
  Type.I.error
  plot(density(z),ylim = c(0,0.5) ,main = "The shape of the Empirical and Theoretical distribution for 
chi-square when n=5",xlim=c(-10,10))
  curve(dchisq(x,3),col=5,add=T)
  
  
  #______________Welch test______________#
  #--------------------------------------#
  # Chi-square(3) #
  #--------------------------------------#
  #-----Small sample sizes (5,5,5)------#
  #...... Power .....#
  n1=5;n2=5
  m=1000;pvalue=NULL
  x=matrix(rchisq(n1*m,df=3),nrow=m)
  x1=x+2
  y=matrix(rchisq(n2*m,df=3),nrow=m)
  y1=y+4
  for(i in 1:m) 
  { pvalue[i]= t.test(x[i,],y[i,],equal.var=F, alt='t')$p.value } 
  Power=sum(pvalue<=0.05)/m
  Power
  p2=c(0.033,0.031,0.027,0.034,0.03,0.034)
  plot(p2,xlim=c(0,9),type="l" ,main="power plot for chi-square when n=5")
  
  
  #______________Welch test______________#
  #--------------------------------------#
  # Chi-square(3) #
  #--------------------------------------#
  #----Large sample sizes (30,30,30)----#
  #...... Type I error .....#
  n1=30;n2=30
  m=1000;pvalue=NULL
  z=NULL
  x=matrix(rchisq(n1*m,df=3),nrow=m)
  y=matrix(rchisq(n2*m,df=3),nrow=m)
  for(i in 1:m){ pvalue[i]= t.test(x[i,],y[i,],equal.var=F, alt='t')$p.value
  z[i]=t.test(x[i,],y[i,],equal.var=F, alt='t')$statistic
  }
  Type.I.error=sum(pvalue<=0.05)/m
  Type.I.error
  plot(density(z),ylim = c(0,0.5) ,main="The shape of the Empirical and Theoretical distribution for chisquare when n=30",,xlim=c(-10,10))
  curve(dchisq(x,3),col=5,add=T)
  
  
  
  #______________Welch test______________#
  #--------------------------------------#
  # Chi-square(3) #
  #--------------------------------------#
  #----Large sample sizes (30,30,30)----#
  #...... Power .....#
  n1=30;n2=30
  m=1000;pvalue=NULL
  x=matrix(rchisq(n1*m,df=3),nrow=m)
  x1=x+2
  y=matrix(rchisq(n2*m,df=3),nrow=m)
  y1=y+4
  for(i in 1:m) 
  { pvalue[i]= t.test(x[i,],y[i,],equal.var=F, alt='t')$p.value } 
  Power=sum(pvalue<=0.05)/m
  Power
  p=c(0.865,0.87,0.87,0.862,0.873,0.854,0.87,0.875,0.897,0.998,1)
  plot(p,xlim=c(0,8),type="l" ,main="power plot for chi-square when n=30")
  
  
  
  #______________Welch test______________#
  #--------------------------------------#
  # t(3) #
  #--------------------------------------#
  #-----Small sample sizes (5,5,5)------#
  #...... Type I error .....#
  n1=5;n2=5
  m=1000;pvalue=NULL
  z=NULL
  x=matrix(rt(n1*m,df=3),nrow=m)
  y=matrix(rt(n2*m,df=3),nrow=m)
  for(i in 1:m) { pvalue[i]= t.test(x[i,],y[i,],equal.var=F, alt='t')$p.value
  z[i]=t.test(x[i,],y[i,],equal.var=F, alt='t')$statistic }
  Type.I.error=sum(pvalue<=0.05)/m
  Type.I.error
  plot(density(z),ylim = c(0,0.5),main = " The shape of the Empirical and Theoretical distribution for tdistribution when n=5",,xlim=c(-10,10))
  curve(dt(x,n1+n2-2),col=5,add=T)
  
  
  
  #______________Welch test______________#
  #--------------------------------------#
  # t(3) #
  #--------------------------------------#
  #-----Small sample sizes (5,5,5)------#
  #...... Power .....#
  n1=5;n2=5
  m=1000;pvalue=NULL
  x=matrix(rt(n1*m,df=3),nrow=m)
  y=matrix(rt(n2*m,df=3),nrow=m)
  y1=y+4
  for(i in 1:m) { pvalue[i]= t.test(x1[i,],y1[i,],equal.var=F, alt='t')$p.value}
  Power=sum(pvalue<=0.05)/m
  Power
  p3=c(0.049,0.051,0.042,0.05,0.062,0.055)
  plot(p3,type="l" ,main="power plot for t distribution when n=5")
  # sample size small
  # plot the power in same plot
  p1=c(0.282,0.248,0.26,0.271,0.256,0.254)
  p2=c(0.033,0.031,0.027,0.034,0.03,0.034)
  p3=c(0.049,0.051,0.042,0.05,0.062,0.055)
  p=c(0,1,2,3,4,5)
  plot(p,p1,type = "l",col="black",lwd=2,lty=1,ylim =c(0,1))
  lines(p,p2,lwd=2,col="red",lty=2)
  lines(p,p3,lwd=2,col="blue",lty=3)
  legend("topleft",legend=c("normal","chi-square","t"),col = c("black","red","blue"),lty = 1:3,title = 
           "sample size 5")
 
  
  #______________Welch test______________#
  #--------------------------------------#
  # t(3) #
  #--------------------------------------#
  #----Large sample sizes (30,30,30)----#
  #...... Type I error .....#
  n1=30;n2=30
  m=1000;pvalue=NULL
  z=NULL
  x=matrix(rt(n1*m,df=3),nrow=m)
  y=matrix(rt(n2*m,df=3),nrow=m)
  for(i in 1:m) { pvalue[i]= t.test(x[i,],y[i,],equal.var=F, alt='t')$p.value
  z[i]=t.test(x[i,],y[i,],equal.var=F, alt='t')$statistic }
  Type.I.error=sum(pvalue<=0.05)/m
  Type.I.error
  plot(density(z),ylim = c(0,0.5) , main="The shape of the Empirical and Theoretical distribution for tdistribution when n=30",xlim=c(-10,10))
  curve(dt(x,n1+n2-2),col=5,add=T)
 
  
  
  #______________Welch test______________#
  #--------------------------------------#
  # t(3) #
  #--------------------------------------#
  #----Large sample sizes (30,30,30)----#
  #...... Power .....#
  n1=30;n2=30
  m=1000;pvalue=NULL
  x=matrix(rt(n1*m,df=3),nrow=m)
  y=matrix(rt(n2*m,df=3),nrow=m)
  y1=y+4
  for(i in 1:m) { pvalue[i]= t.test(x1[i,],y1[i,],equal.var=F, alt='t')$p.value}
  Power=sum(pvalue<=0.05)/m
  Power
  p=c(0.467,0.704,.753,.798,.838,.856,.885,.903,.93,.943,.949,.953,.967,0.971)
  plot(p,type="l" ,main="power plot for t distribution when n=30")
  # sample size large 
  # plot the power in same plot
  p4=c(0.953,0.966,0.971,0.98,0.997,1)
  p5=c(0.865,0.87,0.87,0.862,0.873,0.854)
  p6=c(0.467,0.704,0.753,0.798,0.838,0.856)
  plot(p,p4,type = "l",col="black",lwd=2,ylim =c(0.20,1))
  lines(p,p5,lwd=2,col="red",lty=2)
  lines(p,p6,lwd=2,col="blue",lty=2)
  legend("bottomleft",legend=c("normal","chi-square","t"),col = c("black","red","blue"),lty = 
           1:3,title="sample size 30")
  
  
  
  
  
  
  #__________Krusker-Walis test__________#
  #--------------------------------------#
  # Normal (0,1) #
  #--------------------------------------#
  #-----Small sample sizes (5,5,5) ------#
  #...... Type I error .... #
  n1=5; n2=5;n3=5
  s1=1; s2=1; s3=1; mu1=0; mu2=0;mu3=0
  m=1000; pvalue=NULL;H=NULL
  x=matrix(rnorm(n1*m,mu1,s1),nrow=m)
  y=matrix(rnorm(n2*m,mu2,s2),nrow=m)
  z=matrix(rnorm(n3*m,mu3,s3),nrow=m)
  for(i in 1:m){
    set.seed(110)
    d=stack(list(sample1=x[i,],sample2=y[i,],sample3=z[i,]))
    pvalue[i]=kruskal.test(values~ind,data=d)$p.value
    H[i]=kruskal.test(values~ind,data=d)$statistic}
  Type.I.error=sum(pvalue<=0.05)/m
  Type.I.error
  #[1] 0.044
  #_______graph________#
  plot(density(H),ylim=c(0,0.5),main="The Shape of The Empirical and
Theoretical Distribution ",xlim=c(-10,10))
  curve(dnorm(x,0,1), col=5,add=T)
  STAT-442 Project 52
  #__________Krusker-Walis test__________#
  #--------------------------------------#
  # Normal (0,1) #
  #--------------------------------------#
  #-----Small sample sizes (5,5,5) ------#
  #...... Power .... #
  n1=5; n2=5;n3=5
  s1=1; s2=1; s3=1
  mu1=2+seq(0,0.12,by=0.02); mu2=0;mu3=0
  m=1000; pvalue=NULL ; Power= NULL
  for(j in 1:length(mu1)){
    set.seed(110)
    x=matrix(rnorm(n1*m,mu1[j],s1),nrow=m)
    y=matrix(rnorm(n2*m,mu2,s2),nrow=m)
    z=matrix(rnorm(n3*m,mu3,s3),nrow=m)
    for(i in 1:m){
      d=stack(list(sample1=x[i,],sample2=y[i,],sample3=z[i,]))
      pvalue[i]=kruskal.test(values~ind,data=d)$p.value}
    
    Power[j]=sum(pvalue<=0.05)/m
  }
  Power
  
  #_______graph________#
  plot(Power,type = "l")
  
  
  
  
  
  
  
  #__________Krusker-Walis test__________#
  #--------------------------------------#
  # Normal (0,1) #
  #--------------------------------------#
  #----Large sample sizes (30,30,30)----#
  #...... Type I error .....#
  n1=30; n2=30;n3=30
  s1=1; s2=1; s3=1; mu1=0; mu2=0;mu3=0
  m=1000;pvalue=NULL;y=NULL
  x=matrix(rnorm(n1*m,mu1,s1),nrow=m)
  y=matrix(rnorm(n2*m,mu2,s2),nrow=m)
  z=matrix(rnorm(n3*m,mu3,s3),nrow=m)
  for(i in 1:m){
    set.seed(110)
    d=stack(list(sample1=x[i,],sample2=y[i,],sample3=z[i,]))
    pvalue[i]=kruskal.test(values~ind,data=d)$p.value
    y[i]=kruskal.test(values~ind,data=d)$statistic
  }
  Type.I.error=sum(pvalue<=0.05)/m
  Type.I.error
  
  #_______graph________#
  plot(density(y),ylim=c(0,0.5),main="The Shape of The Empirical and
Theoretical Distribution ",xlim=c(-10,10)))
curve(dnorm(x,0,1),col=5,add=T)


#__________Krusker-Walis test__________#
#--------------------------------------#
# Normal (0,1) #
#--------------------------------------#
#----Large sample sizes (30,30,30)----#
#...... Power .....#
n1=30; n2=30;n3=30
s1=1; s2=1; s3=1
mu1=2+seq(0,0.12,by=0.02); mu2=0;mu3=0
m=1000;pvalue=NULL
for(j in 1:length(mu1)){
  set.seed(110)
  x=matrix(rnorm(n1*m,mu1[j],s1),nrow=m)
  y=matrix(rnorm(n2*m,mu2,s2),nrow=m)
  z=matrix(rnorm(n3*m,mu3,s3),nrow=m)
  for(i in 1:m){
    d=stack(list(sample1=x[i,],sample2=y[i,],sample3=z[i,]))
    pvalue[i]=kruskal.test(values~ind,data=d)$p.value}
  Power[j]=sum(pvalue<=0.05)/m}
Power


#_______graph________#
plot(Power,type="l")

#__________Krusker-Walis test__________#
#--------------------------------------#
# Chi-square(3) #
#--------------------------------------#
#-----Small sample sizes (5,5,5)------#
#...... Type I error .....#
n1=5; n2=5;n3=5
s1=1; s2=1; s3=1
df1=3; df2=3;df3=3
m=1000 ;pvalue=NULL ; H=NULL
x=matrix(rchisq(n1*m,df1),nrow=m)
y=matrix(rchisq(n2*m,df2),nrow=m)
z=matrix(rchisq(n3*m,df3),nrow=m)
for(i in 1:m){
  set.seed(110)
  d=stack(list(sample1=x[i,],sample2=y[i,],sample3=z[i,]))
  pvalue[i]=kruskal.test(values~ind,data=d)$p.value
  H[i]=kruskal.test(values~ind,data=d)$statistic}
Type.I.error=sum(pvalue<=0.05)/m
Type.I.error

#_______graph________#
plot(density(y),ylim=c(0,0.5),main="The Shape of The Empirical and
Theoretical Distribution ",xlim=c(-10,10))
curve(dchisq(x,5),col=5,add=T)



#__________Krusker-Walis test__________#
#--------------------------------------#
# Chi-square(3) #
#--------------------------------------#
#-----Small sample sizes (5,5,5)------#
#...... Power .....#
n1=5; n2=5;n3=5;power=NULL
s1=1; s2=1; s3=1
df1=2+seq(0,0.12,by=0.02); df2=0;df3=0
m=1000;pvalue=NULL
for(j in 1:length(df1)){
  x=matrix(rchisq(n1*m,df1[j]),nrow=m)
  y=matrix(rchisq(n2*m,df2),nrow=m)
  z=matrix(rchisq(n3*m,df3),nrow=m)
  for(i in 1:m){
    set.seed(110)
    d=stack(list(sample1=x[i,],sample2=y[i,],sample3=z[i,]))
    pvalue[i]=kruskal.test(values~ind,data=d)$p.value}
  power[j]=sum(pvalue<=0.05)/m }
power
# [1] 1 1 1 1 1 1 1

#_______graph________#
plot(power,type="l")



#__________Krusker-Walis test__________#
#--------------------------------------#
# Chi-square(3) #
#--------------------------------------#
#----Large sample sizes (30,30,30)----#
#...... Type I error .....#
n1=30; n2=30;n3=30
s1=1; s2=1; s3=1
df1=3; df2=3;df3=3
m=1000 ;pvalue=NULL ;H=NULL
x=matrix(rchisq(n1*m,df1),nrow=m)
y=matrix(rchisq(n2*m,df2),nrow=m)
z=matrix(rchisq(n3*m,df3),nrow=m)
for(i in 1:m){
  set.seed(110)
  d=stack(list(sample1=x[i,],sample2=y[i,],sample3=z[i,]))
  pvalue[i]=kruskal.test(values~ind,data=d)$p.value
  H[i]=kruskal.test(values~ind,data=d)$statistic
}
Type.I.error=sum(pvalue<=0.05)/m
Type.I.error
# [1] 0.048


#_______graph________#
plot(density(y),ylim=c(0,0.5),main="The Shape of The Empirical and
Theoretical Distribution ",xlim=c(-10,10))
curve(dchisq(x,30),col=5,add=T)




#__________Krusker-Walis test__________#
#--------------------------------------#
# Chi-square(3) #
#--------------------------------------#
#----Large sample sizes (30,30,30)----#
#...... Power .....#


n1=30; n2=30;n3=30;Power=NULL
s1=1; s2=1; s3=1
df1=2+seq(0,0.12,by=0.02); df2=3;df3=3
m=1000;pvalue=NULL
for(j in 1:length(df1)){
  x=matrix(rchisq(n1*m,df1[j]),nrow=m)
  y=matrix(rchisq(n2*m,df2),nrow=m)
  z=matrix(rchisq(n3*m,df3),nrow=m)
  for(i in 1:m){
    set.seed(110)
    d=stack(list(sample1=x[i,],sample2=y[i,],sample3=z[i,]))
    pvalue[i]=kruskal.test(values~ind,data=d)$p.value}
  Power[j]=sum(pvalue<=0.05)/m}
Power
# [1] 0.510 0.490 0.451 0.447 0.415 0.394 0.379


#_______graph________#
plot(Power,type = "l")



#__________Krusker-Walis test__________#
#--------------------------------------#
# t(3) #
#--------------------------------------#
#-----Small sample sizes (5,5,5)------#
#...... Type I error .....#

n1=5; n2=5;n3=5;
s1=1; s2=1; s3=1
df1=3; df2=3;df3=3
m=1000;pvalue=NULL;H=NULL
x=matrix(rt(n1*m,df1),nrow=m)
y=matrix(rt(n2*m,df2),nrow=m)
z=matrix(rt(n3*m,df3),nrow=m)
for(i in 1:m){
  set.seed(110)
  d=stack(list(sample1=x[i,],sample2=y[i,],sample3=z[i,]))
  pvalue[i]=kruskal.test(values~ind,data=d)$p.value
  H[i]=kruskal.test(values~ind,data=d)$statistic}
Type.I.error=sum(pvalue<=0.05)/m
Type.I.error
# [1] 0.048

#_______graph________#
plot(density(y),ylim=c(0,0.5),main="The Shape of The Empirical and
Theoretical Distribution ",xlim=c(-10,10))
curve(dt(x,5),col=5,add=T)




#__________Krusker-Walis test__________#
#--------------------------------------#
# t(3) #
#--------------------------------------#
#-----Small sample sizes (5,5,5)------#
#...... Power .....#


n1=5; n2=5;n3=5;power=NULL
s1=1; s2=1; s3=1
df1=2+seq(0,0.12,by=0.02); df2=0;df3=0
m=1000;pvalue=NULL
for(j in 1:length(df1)){
  x=matrix(rchisq(n1*m,df1[j]),nrow=m)
  y=matrix(rchisq(n2*m,df2),nrow=m)
  z=matrix(rchisq(n3*m,df3),nrow=m)
  for(i in 1:m){
    set.seed(110)
    d=stack(list(sample1=x[i,],sample2=y[i,],sample3=z[i,]))
    pvalue[i]=kruskal.test(values~ind,data=d)$p.value}
  power[j]=sum(pvalue<=0.05)/m }
power
# [1] 1 1 1 1 1 1 1


#_______graph________#
plot(power,type="l")



#__________Krusker-Walis test__________#
#--------------------------------------#
# t(3) #
#--------------------------------------#
#----Large sample sizes (30,30,30)----#
#...... Type I error .....#

n1=30; n2=30;n3=30;
s1=1; s2=1; s3=1
df1=3; df2=3;df3=3
m=1000;pvalue=NULL;y=NULL
x=matrix(rt(n1*m,df1),nrow=m)
y=matrix(rt(n2*m,df2),nrow=m)
z=matrix(rt(n3*m,df3),nrow=m)
for(i in 1:m){
  set.seed(110)
  d=stack(list(sample1=x[i,],sample2=y[i,],sample3=z[i,]))
  pvalue[i]=kruskal.test(values~ind,data=d)$p.value
  y[i]=kruskal.test(values~ind,data=d)$statistic}
Type.I.error=sum(pvalue<=0.05)/m
Type.I.error
# [1] 0.055


#_______graph________#
plot(density(y),ylim=c(0,0.5),main="The Shape of The Empirical and
Theoretical Distribution ",xlim=c(-10,10))
curve(dt(x,5),col=5,add=T)




#__________Krusker-Walis test__________#
#--------------------------------------#
# t(3) #
#--------------------------------------#
#----Large sample sizes (30,30,30)----#
#...... Power .....#

n1=30; n2=30;n3=30;Power=NULL
s1=1; s2=1; s3=1
df1=2+seq(0,0.12,by=0.02); df2=3;df3=3
m=1000;pvalue=NULL
for(j in 1:length(df1)){
  x=matrix(rt(n1*m,df1[j]),nrow=m)
  y=matrix(rt(n2*m,df2),nrow=m)
  z=matrix(rt(n3*m,df3),nrow=m)
  for(i in 1:m){
    set.seed(110)
    d=stack(list(sample1=x[i,],sample2=y[i,],sample3=z[i,]))
    pvalue[i]=kruskal.test(values~ind,data=d)$p.value}
  Power[j]=sum(pvalue<=0.05)/m}
Power



#_______graph________#
plot(Power,type = "l")



#__________Krusker-Walis test__________#
# The power curve for Normal(0,1),Chi-Square(3) and t(3),with Small sample size (5,5,5)#

par(mfrow = c(1,3))
plot(Power1,type = "l")
plot(Power3,type = "l")
plot(Power5,type = "l")
#and 
k = c(0,1,2,3,4,5,6)
plot( k , Power1 , type = "l" , col = "black" , lwd = 1 , ylim = c(0,1.2))
lines( k , Power3 , lwd = 2 , col = "red" , lty = 2 )
lines( k , Power5 , lwd = 4 , col = "blue" , lty = 2 )
legend( "bottomright" , legend = c( "Normal" , "Chi" , "t(3)" ), col = c( "black" , "red" , "blue"),
        lty = 1:3 , title = "Power for n=5")
# The power curve for Normal(0,1),Chi-Square(3) and t(3),with Large sample size (30,30,30)#
par(mfrow = c(1,3))
plot(Power2,type = "l")
plot(Power4,type = "l")
plot(Power6,type = "l")
#and
k = c(0,1,2,3,4,5,6)
plot( k , Power2 , type = "l" , col = "black" , lwd = 1 , ylim = c(0,1.2))
lines( k , Power4 , lwd = 2 , col = "red" , lty = 2 )
lines( k , Power6 , lwd = 4 , col = "blue" , lty = 2 )
legend( "topright" , legend = c( "Normal" , "Chi" , "t(3)" ), col = c( "black" , "red" , "blue"),
        lty = 1:3 , title = "Power for n=5")



  