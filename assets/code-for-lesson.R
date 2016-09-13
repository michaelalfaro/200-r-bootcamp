## This code will be used during the bootcamp to show examples for stats, plotting and simple simulations

# initialize workspace
rm(list=ls()) # clean workspace
graphics.off() # close all open plot windows

# Load libraries
library('igraph') # library for network analysis

# load data
iris=iris # this data is already in the computer's memory, but this line will show it in your 'environment' window 

# look at the data
head(iris) # shows the first 6 lines of the data frame
unique(iris$Species) # shows all the species 

# get the subset data from two species
setosa=iris[iris$Species=='setosa',] ## assign only the rows that belong to setosa and all the columns in the data frame to a variable named "setosa"
virginica=iris[iris$Species=='virginica',] ## assign only the rows that belong to virginica and all the columns in the data frame to a variable named "virginica"

# plot the sepal lengths of  setosa and virginica
windows() # opens the figure in a new window. use the command quartz() for macs
par(mar=c(4, 4, 3, 2)) # set the margins
boxplot(setosa$Sepal.Length, virginica$Sepal.Length, # the data to plot
		names=c('Setosa','Virginica'),ylab="Sepal Length", # label the axes 
		col=c("hotpink", "plum"), # set the colors of the boxplots
		las=1, # set the numbers on the axis so that they are upright
		cex.axis=1.5,cex.lab=1.5) # make sure the fonts are large enough to read

# are these two statistically different?
# use a t-test:
t.test(setosa$Sepal.Length, virginica$Sepal.Length) # this will display the result in the command line

###############
# Ex 1 (in class): now plot and compare two other measures on your own....
###############

# comparing means of more than two groups
# plot all three species
windows() # open a new figure window
plot(iris$Species, iris$Sepal.Length, # data to plot
		ylab="Sepal Length", # label axes
	col=c("hotpink", "plum", "cornflowerblue"), las=1, cex.axis=1.5,cex.lab=1.5) # make figure pretty

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
## add a legend to the plot:
legend(2,10, # set the location x, y 
		unique(iris$Species), # text to display
		text.col=c("hotpink", "plum", "cornflowerblue"), #text color
		bty ='n') # remove outline of legend box
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
cor.test(iris$Petal.Length,iris$Sepal.Length)

## let's color code by species
# set a vector with a different  color for each species that matches their order in the data frame
cols=sub('setosa',"hotpink",iris$Species)
cols=sub('versicolor',"plum",cols)
cols=sub('virginica',"cornflowerblue",cols)

## another shorter way:
cols2=c("hotpink", "plum", "cornflowerblue")[unclass(iris$Species)]

# plot the correlation with color codes by species
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

## gradient colors - color code the points by petal length
windows()
plot(iris$Petal.Length,iris$Sepal.Length, xlab='Petal length',ylab='Sepal length',pch=16, # same as above...
		 col=rainbow(max(iris$Petal.Length)*10)[iris$Petal.Length*10]) # set a color Palette 'rainbow' and assign colors by petal length 

###########################
###########################

## interaction data:
# create a random interaction matrix:
mat_data=matrix(data =  rnorm(225,0,5) , nrow = 15, ncol = 15) ## you can potentially replace rnorm with rexp(225,0.75) and with runif(225,0,5) 
diag(mat_data)=1 # set the diagonal to 1 cause each individual interacts with itself by definition
windows() # plot the interaction matrix
image(mat_data) 
# turn the adjacency matrix to a network object
net=graph.adjacency(mat_data,diag=FALSE,mode="undirected", weighted=TRUE)
E(net) # look at the edges of the object
E(net)$width <- E(net)$weight # set edge width to the weight
windows() # plot the network
plot(net,vertex.color=rainbow(10),layout=layout.fruchterman.reingold)


###########################
###########################

# Geometric Growth 
# a script to simulate and plot the discrete logistic model
# Set initial conditions and parameter values
N0 <- 100
RR <- 1.05
ttMax <- 10
# initialize variable to a vector of NA values - where we will store the simulation results
NN <- matrix(NA, nrow=1, ncol=ttMax+1)
NN[1] <- N0 # set first value to initial condition
# use a loop to iterate the model the desired number of times
for (tt in 1:ttMax) {
	NN[tt+1] <- RR*NN[tt]
}
# plot the results
windows()
plot(1:(ttMax+1), NN, xlab="Time", ylab="Population size (N)", col='blue', pch=16, las=1)
# add a line:
lines(1:(ttMax+1), NN, xlab="Time", ylab="Population size (N)", col='blue', las=1)

##################
## ex 3 (in class): convert the above simulation into a function
################