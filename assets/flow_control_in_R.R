## ------------------------------------------------------------------------
setwd("~/Dropbox/bootcamp_examples")

## ------------------------------------------------------------------------
getwd()

## ------------------------------------------------------------------------
2 + 2
#2 + 3

## ------------------------------------------------------------------------
help(lm)

## ---- eval=F-------------------------------------------------------------
## ?lm

## ---- eval-F-------------------------------------------------------------
??lm

## ------------------------------------------------------------------------
grad.school.tips <- c( "use a reference manager", "learn a programming language", "write lots of papers")

## ------------------------------------------------------------------------
cat(grad.school.tips, sep = "\n")

## ---- eval=FALSE---------------------------------------------------------
## install.packages(c("geiger", "laser"), dep = T)

## ------------------------------------------------------------------------
xx <-1000
ls()

## ------------------------------------------------------------------------
ls()
rm(xx)
ls()


## ------------------------------------------------------------------------
xx <- 100
names <- c("Paul", "Griffin", "Pierce")
numbers <- runif(100)
ls()
rm(list = ls())
ls() #character(0) means the function has returned an empty value


## ----eval=F--------------------------------------------------------------
## q()
## q(save = 'no')
## 

## ------------------------------------------------------------------------
getwd()
source("/Users/michael_alfaro/Dropbox/bootcamp_examples/source.example.R") 
#make sure the path to the source file is specified correctly (should equal output from getwd())
all.I.know.about.life.I.learned.in.grad.school() #a function from the source file


## ------------------------------------------------------------------------
library(ape)
tt <- read.tree("/Users/michael_alfaro/Dropbox/bootcamp_examples/tree.tre")
###see elements of an object
attributes(tt)
###access those elements with $
tt$tip.label[1:10]
head(tt$tip.label)


## ---- echo = F-----------------------------------------------------------
pruned.tree <- drop.tip(tt, tt$tip.label[1:7900])
plot(ladderize(pruned.tree), cex = 0.5, type = "radial")

## ------------------------------------------------------------------------

# d contains length data, family, species, order, etc
inpath = "/Users/michael_alfaro/Dropbox/bootcamp_examples/data.txt"
dd <- read.table(inpath, header=T, sep='\t', as.is = T);

###NOTE: R by default reads character columns as FACTORS. This data structure behaves very differently from a string!  Use as.is = T when reading in data to make R treat these columns as characters.

## ------------------------------------------------------------------------
str(dd)
#a data frame is a collection of columns where every object within the column vector is the same data type
#get the dimensions of a data frame
dim(dd)
length.dd <- dim(dd)[1] #what does this line do?
#dimensions are rows, columns
attributes(dd)

## ---- cache=F------------------------------------------------------------
#get 92 random variables
size <- runif(length.dd)

#you can add columns to an existing data frame with cbind
head(dd) #before
dd<- cbind(dd, size)
head(dd) #after

## ------------------------------------------------------------------------
names(dd) #these are the names of the columns we could access
#dd$species #all the species
head(dd$species)
tail(dd$species) # use these functions to check that data has been read into R correctly
#you can pull out individual columns 
swimming_mode <- dd$mode

## ------------------------------------------------------------------------
dd[1,1] # entry in row 1, column 1
dd[1,2] # entry in row 1, column 2
dd[1,3] # entry in row 1, column 3
dd[1,] # row 1, all columns
dd[,2] # all rows, column 2

## ------------------------------------------------------------------------
head(rownames(dd))
rownames(dd) <- dd$species
head(rownames(dd))
str(dd)
# if you name the columns you can access a row by name
dd['Pomacentrus_brachialis',]


## ------------------------------------------------------------------------
#a bit on subseting
dd[5:10,] # rows 5-10, all columns
dd[5:10,3] # rows 5-10, column 3



## ------------------------------------------------------------------------
#if you want only the MPF swimmers, you can use the which() function
which(dd$mode == 'MPF')
mpfs <- which(dd$mode == 'MPF') #stores rows of mpf swimmers
mpf_swimmers <- dd[mpfs,] #stored this as a seperate df
head(mpf_swimmers)

## ------------------------------------------------------------------------
head(dd)
which(dd$size > 0.9) #shows us rosw with large fish in them

## ------------------------------------------------------------------------
big.fish <- dd[which(dd$size > 0.9),] #remember the , after the which command says "select all columns"
head(big.fish)

## ------------------------------------------------------------------------
#ways to check for NAs
head(dd) # there are NAs in the data
head(is.na(dd))
which(is.na(dd$mode)) #item 2
complete.cases(dd)

## ------------------------------------------------------------------------
#one way to get only complete cases
cleaned_1 <- dd[complete.cases(dd),]
#another
cleaned_2 <- na.omit(dd)

dd <- cleaned_1

## ------------------------------------------------------------------------
setdiff(dd$species, tt$tip.label)


## ------------------------------------------------------------------------
dd$species[which(dd$species == 'Chaetodon_plebius')]<-'Chaetodon_plebeius' #taxonomic inconsistency

## ------------------------------------------------------------------------
del_from_data <- setdiff(dd$species, tt$tip.label)
match(del_from_data, rownames(dd)) #row numbers of dd that match not.in.tt
dd.pruned <- dd[-match(del_from_data, rownames(dd)),]
###ok now lets check for overlap
setdiff(dd.pruned$species, tt$tip.label) # this should produce "character(0)" if empty.

#this would also work
#pruned_data <- dd[!(dd$species %in% del_from_data),]

## ------------------------------------------------------------------------
not.in.dd <-setdiff(tt$tip.label, dd.pruned$species )
length(not.in.dd) #this will be a large number because the tree has so many tips!
head(not.in.dd)

## ------------------------------------------------------------------------
pruned.tree <- drop.tip(tt, not.in.dd)
setdiff(pruned.tree$tip.label, dd.pruned$species) #should be "character 0" if these objects match
plot(pruned.tree, cex = 0.5)

## ------------------------------------------------------------------------
for (ii in 1:5){
  cat("\nthe number is ", ii)
}

## ------------------------------------------------------------------------
notfish <- c("bat", "dolphin", "toad", "soldier")

for(animal in notfish){
  cat(animal, "fish\n", sep="")
}

## ---- eval=FALSE---------------------------------------------------------
## while(thesis_idea_sucks){
##     get_New_Thesis_Idea();
## }

## ------------------------------------------------------------------------
xx <- 1
xx <- 1
while(xx < 5) {
  xx <- xx+1; 
  if (xx == 3) {
    break; }
  }
print(xx)

## ---- eval=FALSE---------------------------------------------------------
## if (xx == 'a') doSomething1;
## if (xx == 'b') doSomething2;
## if (xx=='c') doSomething3;

## ------------------------------------------------------------------------
for(ii in 1:6){
  if (ii %% 2) {
    cat(ii, " is odd\n")
    }
  else{
    cat(ii, " is even\n")
    }
  }

## ---- eval=FALSE---------------------------------------------------------
## if (x == 'a'){
##   doSomething1;
## }
## else if (x == 'b'){
## 	doSomething2;
## }...
## else if (x == 'z'){
## 	doSomething26;
## }
## else{
## cat('x != a letter\n')
## }

## ------------------------------------------------------------------------
# write a script that prints a number and its square over a given range on integers and then 


## ------------------------------------------------------------------------
# set lower and upper range values
# set squaresum to 0

# loop over the range and for each value print
  # currentvalue and the currentvalue^2
  # add currentvalue^2 to squaresum
# print "here is the sum of it all"m squaresum


## ------------------------------------------------------------------------
lower = 1; upper = 5; squaresum = 0

for (ii in lower:upper){
  cat(ii, ii^2, "\n")
  squaresum <- squaresum +  ii^2
}
cat("the sum of it all is ", squaresum)


## ------------------------------------------------------------------------
doubler <- function(num){
  doubled = 2 * num
  cat("witness the awesome power of the doubler\n")
  cat("I changed ", num, " to ", doubled, "\n")
  cat("you're welcome!\n")
  return(doubled)
}

## ------------------------------------------------------------------------
takeNoArguments <- function() {
cat('this function takes no arguments\n'); cat('it also\n');
cat('returns nothing\n');
cat('you never get something for nothing.\n')
}
takeNoArguments()

## ---- eval=FALSE---------------------------------------------------------
## myFunction <- function(arg1, arg2)

## ---- eval=FALSE---------------------------------------------------------
## {
##    cat(‘this is my function’);
##    cat(‘dont mess with it’);
## }

## ------------------------------------------------------------------------
greeter <- function(name) {
  cat('Hello, ', name, '\n'); 
}

