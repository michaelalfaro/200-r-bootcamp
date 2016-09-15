# solutions to exercises in class
###
# ex 2
fit_with_sp_with_int=lm(Petal.Length ~ Sepal.Width * Species - 1, data=iris)
summary(fit_with_sp_with_int)

windows()
plot(iris$Sepal.Width,iris$Petal.Length, xlab='Petal length',ylab='Sepal length',
		col=c("hotpink", "plum", "cornflowerblue")[unclass(iris$Species)], pch=16,las=1,cex.axis=1.5,cex.lab=1.5)
legend(4,4, unique(iris$Species), text.col=c("hotpink", "plum", "cornflowerblue"),pch=16, col=c("hotpink", "plum", "cornflowerblue"))
abline(lm(iris$Petal.Length[iris$Species=='setosa']~iris$Sepal.Width[iris$Species=='setosa']), col="hotpink", lty=2, lwd=2)
abline(lm(iris$Petal.Length[iris$Species=='versicolor']~iris$Sepal.Width[iris$Species=='versicolor']), col="plum", lty=2, lwd=2)
abline(lm(iris$Petal.Length[iris$Species=='virginica']~iris$Sepal.Width[iris$Species=='virginica']), col="cornflowerblue", lty=2, lwd=2)

## with a 'for' loop and only lines:
unq_sp=unique(iris$Species)
cols=c("hotpink", "plum", "cornflowerblue")
windows()
plot(iris$Sepal.Width,iris$Petal.Length, xlab='Petal length',ylab='Sepal length',
		pch="",las=1,cex.axis=1.5,cex.lab=1.5)
legend("topleft", legend=unq_sp, text.col=c("hotpink", "plum", "cornflowerblue"),lty=1, col=c("hotpink", "plum", "cornflowerblue"), bty="n")
for (i in 1:length(unq_sp)){
	ix_sp=unq_sp[i]
	abline(lm(iris$Petal.Length[iris$Species==ix_sp]~iris$Sepal.Width[iris$Species==ix_sp]), col=cols[i], lty=1, lwd=2)
}

###
# ex. 3

geomFun <- function(RR, N0, ttMax){
# Initialize vector to hold output
	NN <- rep(NA,ttMax+1)
	NN[1] <- N0
# Use a for loop to step forward
	for (tt in 1:ttMax){
		NN[tt+1] <- RR*NN[tt]
	}
	plot(1:(ttMax+1),NN,lty=2,type='l', xlab='Time', ylab='Population size', las=1)
}
windows()
geomFun(0.5,100,50)

