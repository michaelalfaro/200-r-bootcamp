#R code to make the figures in Kirk's slides from R bootcamp:
#Updated for 2015.

#make a basic histogram in R:

#first, draw 1000 random values from a standard normal distribution (SD=1):
s1<-rnorm(1000,mean=0,sd=1)

#now do 1000 drawn from a normal distribution with SD=3.
s3<-rnorm(1000,mean=0,sd=3)

#plot histograms of both on same panel and save to a file:
pdf(file="~/Dropbox/Kirk_stuff/KEL_bootcamp/Normal_hist.pdf", width=4,height=7); 
#open the file

par(mfrow=c(2,1), mar=c(4, 4, 3, 2)) #sets plotting area and margins

hist(s1,col=2,xlab="Value",main="Sigma=1") #make first hist

hist(s3,col=4,xlab="Value",main="Sigma=3") #make second hist

dev.off() #shuts off current output device


#Now for the fancy one:


#plot histograms of both on same panel and save to a file:
pdf(file="~/Dropbox/Kirk_stuff/KEL_bootcamp/Normal_hist.fancy.pdf", width=4,height=7); 
#open the file

par(mfrow=c(2,1), mar=c(4, 4, 3, 2)) #sets plotting area and margins

hist(s1,col=2,xlab="Value",main=expression(paste(sigma,"=1"))) #make first hist

hist(s3,col=4,xlab="Value",main=expression(paste(sigma,"=3")))  #make second hist

dev.off() #shuts off current output device






#make smooth density plot:

pdf(file="~//Dropbox/Kirk_stuff/KEL_bootcamp/Normal_density.pdf", width=6,height=6); #open the file

par(mfrow=c(1,1), mar=c(4, 4, 3, 2)) #sets plotting area and margins

plot(density(s1),col=2,lwd=4,xlab="Value",xlim=c(-15,15),main="Normal distribution")

lines(density(s3),col=4,lwd=4) #add the SD=3 values

legend(-15,0.4,c("sigma=1","sigma=3"),lwd=4,col=c(2,4),cex=1.5) #put a legend on

#we can highlight the upper 10% of each distribution with a vertical line:
abline(v=quantile(s1,0.9),lty=2,lwd=3,col=2) #puts a vertical line onto the plot for s1
abline(v=quantile(s3,0.9),lty=2,lwd=3,col=4) #puts a vertical line onto the plot for s3
dev.off()

#quantile take a vector of stuff, and returns the value q such that p% of your distribution is less than q.

#for example, find the 75th percentile of the standard normal distribution:
quantile(s1,0.75)

#quantile with just a vector gives some interesting stuff:
quantile(s1)


#boxplot:
pdf(file="/Users/kirk/Dropbox/Kirk_stuff/KEL_bootcamp/Normal_boxplot.pdf", width=6,height=6); #open the file

par(mfrow=c(1,1), mar=c(4, 4, 3, 2)) #sets plotting area and margins

boxplot(cbind(s1,s3),names=c("Sigma=1","Sigma=3"),main="Draws from a normal distribution",col=c(2,4))

dev.off()


######################################

#Let's make a histogram of these values, but putting boht on the same axes.
#We can do that...
#But, we need to have the same bin widths for both datasets:

bins<-seq(-10,10,by=1)

hist(s1,breaks=bins)$breaks

hist(s3,breaks=bins)$breaks

#This looks good

counts_s1<-hist(s1,breaks=bins)$counts
counts_s3<-hist(s3,breaks=bins)$counts

#Note, If right = TRUE (default), the histogram cells are intervals of the form (a, b], i.e., they include their right-hand endpoint, but not their left one, with the exception of the first cell when include.lowest is TRUE.

#now make the plot:
pdf(file="~/Dropbox/Kirk_stuff/KEL_bootcamp/normal_barplot.pdf", width=6,height=6); #open the file

par(mfrow=c(1,1), mar=c(4, 4, 3, 2)) #sets plotting area and margins

barplot(rbind(counts_s1,counts_s3),col=c(2,4),beside=T,names.arg=seq(-10,9.5,by=1),xlab="Value",ylab="Count")

legend(6,350,c(expression(paste(sigma,"=3")),expression(paste(sigma,"=6"))),col=c(2,4),lwd=4)

dev.off()


#Simple scatterplot:
pdf(file="/Users/kirk/Dropbox/Kirk_stuff/KEL_bootcamp/scatter_small.pdf", width=10,height=10); #open the file

par(mfrow=c(1,1), mar=c(4, 4, 3, 2)) #sets plotting area and margins

x<-rnorm(100)
y<-x+rnorm(100,sd=0.2)

plot(x,y,pch=19)

dev.off()


#now, try again, make the labels bigger:
#Simple scatterplot:
pdf(file="/Users/kirk/Dropbox/Kirk_stuff/KEL_bootcamp/scatter_large.pdf", width=10,height=10); #open the file

par(mfrow=c(1,1), mar=c(5, 5, 3, 2)) #sets plotting area and margins

x<-rnorm(100)
y<-x+rnorm(100,sd=0.2)

plot(x,y,pch=19,cex.lab=2,cex.axis=2)

dev.off()






######################################
#In 2014, we'll skip the binomial stuff. Make the hist for the normal dist:


#Now, let's do something different:

#we can simulate from a binomial distribution pretty easily too:
p_0.1<-rbinom(1000,10,0.1) #draw 1000 values from a binomial distribution with n=10 and p=0.1

p_0.6<-rbinom(1000,10,0.6) #draw 1000 values from a binomial distribution with n=10 and p=0.6

mean(p_0.1)

mean(p_0.6)

#can easily figure out the % of values <some threshold:
mean(p_0.1<3)

mean(p_0.6<3)


#Let's plot the distribution of these values:
#need to set the same bins widths for the histogram:
bins<-seq(0,11,by=1)

hist(p_0.1,breaks=breaks)$breaks

hist(p_0.6,breaks=breaks)$breaks

#this looks good!

counts_0.1<-hist(p_0.1,breaks=breaks)$counts
counts_0.6<-hist(p_0.6,breaks=breaks)$counts

#now make the plot:
pdf(file="/Users/kirk/Dropbox/Kirk_stuff/KEL_bootcamp/binomial_barplot.pdf", width=6,height=6); #open the file

par(mfrow=c(1,1), mar=c(4, 4, 3, 2)) #sets plotting area and margins

barplot(rbind(counts_0.1,counts_0.6),col=c(3,6),beside=T,names.arg=seq(0,10,by=1),xlab="Number of successes",ylab="Count")

legend(6,600,c("p=0.1","p=0.6"),col=c(3,6),lwd=4)

dev.off()


########



