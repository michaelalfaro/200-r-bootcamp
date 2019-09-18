## This code will be used during the bootcamp to show examples for stats, 
# plotting and simple simulations

# initialize workspace
rm(list=ls()) # clean workspace
graphics.off() # close all open plot windows

# Load libraries
library('igraph') # library for network analysis

# load data
iris=iris # this data is already in the computer's memory, but this line will show it in your 'environment' window 

# look at the data
head(iris) # shows the first 6 lines of the data frame
summery(iris)
unique(iris$Species) # shows all the species 

# get the subset data from two species
setosa=iris[iris$Species=='setosa',] ## assign only the rows that belong to setosa and all the columns in the data frame to a variable named "setosa"
virginica=iris[iris$Species=='virginica',] ## assign only the rows that belong to virginica and all the columns in the data frame to a variable named "virginica"

# plot the sepal lengths of  setosa and virginica
# histograms:
windows() # opens the figure in a new window. use the command quartz() for macs
par(mfrow =c(1,2)) # set subplots
hist(setosa$Sepal.Length, # the data to plot
		col='hotpink',# set color
		las=1, # set the numbers on the axis so that they are upright
		main='Setosa',xlab="Sepal Length")# labels
hist(virginica$Sepal.Length,# the data to plot
		col='plum',# set color
		las=1, # set the numbers on the axis so that they are upright
		main='Virginica',xlab="Sepal Length")# labels

windows() # opens the figure in a new window. use the command quartz() for macs
boxplot(setosa$Sepal.Length, virginica$Sepal.Length, # the data to plot
		names=c('Setosa','Virginica'),ylab="Sepal Length", xlab="Species", # label the axes 
		col=c("hotpink", "plum"), # set the colors of the boxplots
		las=1, # set the numbers on the axis so that they are upright
		cex.axis=1.5,cex.lab=1.5) # make sure the fonts are large enough to read

# are these two statistically different?
# use a t-test:
t_test_result=t.test(setosa$Sepal.Length, virginica$Sepal.Length) 
t_test_result

###############
# Ex 1 (in class): now plot and compare two other measures on your own....
###############





# comparing means of more than two groups
# plot all three species
windows() # open a new figure window
plot(iris$Species, iris$Sepal.Length, # data to plot
		ylab="Sepal Length", # label axes
	col=c("hotpink", "plum", "cornflowerblue"), las=1, cex.axis=1.5,cex.lab=1.5) # make figure pretty

## add a legend to the plot:
legend(0.5,8, # set the location x, y 
		unique(iris$Species), # text to display
		text.col=c("hotpink", "plum", "cornflowerblue"), cex=2, #text color and size
		bty ='n') # remove outline of legend box

# are these three statistically different?
# run an ANOVA:
anova1=aov(iris$Sepal.Length~iris$Species) # set up the statistical test
summary(anova1) # look at the results of the statistical test
TukeyHSD(anova1) # examine each pair using a Tukey test


## Add the results of the Tukey test to the plot:
windows() # open a new figure window
plot(iris$Species, iris$Sepal.Length,ylab="Sepal Length", # data to plot and label axes
		ylim=c(4,10), # set the y limits to make space for stats test results
		col=c("hotpink", "plum", "cornflowerblue"), las=1, cex.axis=1.5,cex.lab=1.5) # make figure pretty
# after you call plot you can add a various graphical features: 
# add letters above the boxes
text(c(1,2,3),c(9,9,9), # set the location of the letters 
		c('A','B','C'), # set which letters to display  on the plot
		cex=2) # make sure they are large enough to read

# and just for fun...
# add circles  around the letters
points(c(1,2,3),c(9,9,9), # location of circles
		cex=10) # size of circles (default of points is empty circles - look up  in your R reference card what other shapes you can set using pch= )

## are sepal and petal length correlated?
# let's plot these first
windows() # set a new figure
plot(iris$Petal.Length,iris$Sepal.Length, # data to plot 
		xlab='Petal length',ylab='Sepal length', # label your axes
		pch=16,las=1,cex.axis=1.5,cex.lab=1.5) # make the figure pretty

# run a Pearson's correlation test to see if traits are statistically correlated:
cor_S_P=cor.test(iris$Petal.Length,iris$Sepal.Length)

## let's color code by species
# set a vector with a different  color for each species that matches their order in the data frame
cols=sub('setosa',"hotpink",iris$Species)
cols=sub('versicolor',"plum",cols)
cols=sub('virginica',"cornflowerblue",cols)

## another shorter way:
cols2=c("hotpink", "plum", "cornflowerblue")[unclass(iris$Species)]

# plot with color codes by species
windows()
plot(iris$Petal.Length,iris$Sepal.Length, xlab='Petal length',ylab='Sepal length', # same as above...
		col=cols, # add color
		pch=16,las=1,cex.axis=1.5,cex.lab=1.5) # same as above
# add a legend:
legend("topleft", legend=unique(iris$Species), text.col=c("hotpink", "plum", "cornflowerblue"),pch=16, col=c("hotpink", "plum", "cornflowerblue"))

# is there a significant effect of species on the relationship between sepal and petal length?
# set up a linear model with both petal length and species as effects:
fit_with_sp=lm(Sepal.Length ~ Petal.Length + Species - 1, data=iris)
summary(fit_with_sp) # look at the results

# add an interaction term:
fit_with_sp_with_int=lm(Sepal.Length ~ Petal.Length * Species - 1, data=iris)
summary(fit_with_sp_with_int)

# exploratory analysis...
windows()
plot(iris, col=cols2, pch=16)

################
# Ex 2 (in class): 
# are Petal Length and Sepal width related? by species? is there an interaction term? 
# Plot the interaction term (HINT: look up the abline function).
# next, use a 'for' loop to create the above plot.
################


fit_with_sp_with_int=lm(Petal.Length ~ Sepal.Width * Species , data=iris)
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
legend("topleft", legend=unq_sp, text.col=c("hotpink", "plum", "cornflowerblue"),
		lty=1, col=c("hotpink", "plum", "cornflowerblue"), bty="n")

for (i in 1:length(unq_sp)){
	ix_sp=unq_sp[i]
	abline(lm(iris$Petal.Length[iris$Species==ix_sp]~iris$Sepal.Width[iris$Species==ix_sp]), 
			col=cols[i], lty=1, lwd=2)
}


###########################

## gradient colors - color code the points by petal length
windows()
plot(iris$Petal.Length,iris$Sepal.Length, xlab='Petal length',ylab='Sepal length',pch=16, las=1,# same as above...
		 col=rainbow(max(iris$Petal.Length)*10)[iris$Petal.Length*10]) # set a color Palette 'rainbow' and assign colors by petal length 

###########################
###########################