## This code will be used during the bootcamp to show examples for stats, 
# plotting and simple simulations

# initialize workspace
rm(list=ls()) # clean workspace
graphics.off() # close all open plot windows

# Load library
library('palmerpenguins')

# load data
data <- penguins 

# look at the data
head(data) # shows the first 6 lines of the data frame
summary(data)
unique(data$species) # shows all three species 

# get the subset data from two species
adelie=data[data$species == 'Adelie',] ## assign only the rows that belong to Adelie and all the columns in the data frame to a variable named "adelie"
gentoo=data[data$species == 'Gentoo',] ## assign only the rows that belong to Gentoo and all the columns in the data frame to a variable named "gentoo"

# plot the flipper lengths of  Adelie and Gentoo
# histograms:
windows() # opens the figure in a new window. use the command quartz() for macs
par(mfrow =c(1,2)) # set subplots
hist(adelie$flipper_length_mm, # the data to plot
		col='hotpink',# set color
		las=1, # set the numbers on the axis so that they are upright
		main='Adelie',xlab="Flipper Length (mm)")# labels
hist(gentoo$flipper_length_mm,# the data to plot
		col='plum',# set color
		las=1, # set the numbers on the axis so that they are upright
		main='Gentoo',xlab="Flipper Length (mm)")# labels

windows() # opens the figure in a new window. use the command quartz() for macs
boxplot(adelie$flipper_length_mm, gentoo$flipper_length_mm, # the data to plot
		names=c('Adelie','Gentoo'),ylab="Flipper Length (mm)", xlab="Species", # label the axes 
		col=c("hotpink", "plum"), # set the colors of the boxplots
		las=1, # set the numbers on the axis so that they are upright
		cex.axis=1.5,cex.lab=1.5) # make sure the fonts are large enough to read

# are these two statistically different?
# use a t-test:
t_test_result=t.test(adelie$flipper_length_mm, gentoo$flipper_length_mm) 
t_test_result

###############
# Ex 1 (in class): now plot and compare two other measures on your own....
###############


# comparing means of more than two groups
# plot all three species
#windows() # open a new figure window
plot(data$species, data$flipper_length_mm, # data to plot
		ylab="Flipper Length (mm)", xlab="", # label axes
		main= 'Compare flipper length',
	col=c("hotpink", "plum", "cornflowerblue"), las=1, cex.axis=1.5,cex.lab=1.5) # make figure pretty

## add a legend to the plot:
legend(0.3,235, # set the location x, y 
		unique(data$species), # text to display
		text.col=c("hotpink", "plum", "cornflowerblue"), cex=2, #text color and size
		bty ='n') # remove outline of legend box

# are these three statistically different?
# run an ANOVA:
anova1=aov(data$flipper_length_mm ~ data$species) # set up the statistical test
summary(anova1) # look at the results of the statistical test
TukeyHSD(anova1) # examine each pair using a Tukey test


## Add the results of the Tukey test to the plot:
windows() # open a new figure window
plot(data$species, data$flipper_length_mm, ylab="Flipper Length (mm)", xlab="", # data to plot and label axes
		ylim=c(170,240), # set the y limits to make space for stats test results
		col=c("hotpink", "plum", "cornflowerblue"), las=1, cex.axis=1.5,cex.lab=1.5) # make figure pretty
# after you call plot you can add a various graphical features: 
# add letters above the boxes
text(c(1,2,3),c(240,240,240), # set the location of the letters 
		c('A','B','C'), # set which letters to display  on the plot
		cex=2) # make sure they are large enough to read

# and just for fun...
# add circles  around the letters
points(c(1,2,3),c(240,240,240), # location of circles
		cex=10) # size of circles (default of points is empty circles - look up  in your R reference card what other shapes you can set using pch= )



## are body mass and flipper length correlated?
# let's plot these first
windows() # set a new figure
plot(data$body_mass_g,data$flipper_length_mm, # data to plot 
		xlab='Body Mass (g)',ylab='Flipper Length (mm)', # label your axes
		pch=16,las=1,cex.axis=1.5,cex.lab=1.5) # make the figure pretty

# run a Pearson's correlation test to see if traits are statistically correlated:
cor_S_P=cor.test(data$body_mass_g,data$flipper_length_mm)

## let's color code by species
# set a vector with a different  color for each species that matches their order in the data frame
cols=sub('Adelie',"hotpink",data$species)
cols=sub('Gentoo',"plum",cols)
cols=sub('Chinstrap',"cornflowerblue",cols)

## another shorter way:
cols2=c("hotpink", "plum", "cornflowerblue")[unclass(data$species)]

# plot with color codes by species
windows()
plot(data$body_mass_g,data$flipper_length_mm, xlab='Body Mass (g)',ylab='Flipper Length (mm)', # same as above...
		col=cols, # add color
		pch=16,las=1,cex.axis=1.5,cex.lab=1.5) # same as above
# add a legend:
legend("topleft", legend=unique(data$species), text.col=c("hotpink", "plum", "cornflowerblue"),pch=16, col=c("hotpink", "plum", "cornflowerblue"))

# is there a significant effect of species on the relationship between flipper length and body mass?
# set up a linear model with both  flipper length and species as effects:
fit_with_sp=lm(body_mass_g  ~ flipper_length_mm + species - 1, data=data)
summary(fit_with_sp) # look at the results

# add an interaction term:
fit_with_sp_with_int=lm(body_mass_g  ~ flipper_length_mm * species - 1, data=data)
summary(fit_with_sp_with_int)

# exploratory analysis...
windows()
plot(data, col=cols2, pch=16)

################
# Ex 2 (in class - includes some older prompts for further exploration): 
# are  bill length and flipper length related? by species? is there an interaction term? 
# Plot the relationships (HINT: look up the abline function).
# next, use a 'for' loop to create the above plot.
################


fit_with_sp_with_int2=lm( bill_length_mm ~ flipper_length_mm  * species , data=data)
summary(fit_with_sp_with_int2)

windows()
plot(data$flipper_length_mm,data$bill_length_mm, xlab='Flipper Length (mm)',ylab='Bill length (mm)',
		col=c("hotpink", "plum", "cornflowerblue")[unclass(data$species)], pch=16,las=1,cex.axis=1.5,cex.lab=1.5)
legend(4,4, unique(data$species), text.col=c("hotpink", "plum", "cornflowerblue"),pch=16, col=c("hotpink", "plum", "cornflowerblue"))
abline(lm(data$bill_length_mm[data$species=='Adelie']~data$flipper_length_mm[data$species=='Adelie']), col="hotpink", lty=2, lwd=2)
abline(lm(data$bill_length_mm[data$species=='Gentoo'] ~ data$flipper_length_mm[data$species=='Gentoo'] ), col="cornflowerblue", lty=2, lwd=2)
abline(lm(data$bill_length_mm[data$species=='Chinstrap'] ~ data$flipper_length_mm[data$species=='Chinstrap']), col="plum", lty=2, lwd=2)

## with a 'for' loop and only lines:
unq_sp=unique(data$species)
cols=c("hotpink", "plum", "cornflowerblue")
windows()
plot(data$flipper_length_mm,data$bill_length_mm,xlab='Flipper Length (mm)',ylab='Bill length (mm)',
		pch="",las=1,cex.axis=1.5,cex.lab=1.5)
legend("topleft", legend=unq_sp, text.col=c("hotpink", "plum", "cornflowerblue"),
		lty=1, col=c("hotpink", "plum", "cornflowerblue"), bty="n")

for (i in 1:length(unq_sp)){
	ix_sp=unq_sp[i]
	abline(lm(data$bill_length_mm[data$species==ix_sp]~data$flipper_length_mm[data$species==ix_sp]),
			col=cols[i], lty=1, lwd=2)
}


