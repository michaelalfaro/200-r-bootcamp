#Additional in class excercises for R bootcamp:
#EEB 201, September 17, 2015
#Kirk Lohmueller


################# BEFORE BEGINNING: ######################

# First we'll load in a matrix of genotype data

# Copy the data files into a directory you will work in for this part of the course.
# Recommendation:  work/eebQuantIntro/Rintro/

# Set working directory:
#change this to whatever you want
setwd("~/Dropbox/Kirk_stuff/KEL_bootcamp/2014")

# Load a data set
snpsDataFrame=read.table('hapmap_CEU_r23a_chr2_ld.txt',header=TRUE)

# What are the dimensions of the data?
dim(snpsDataFrame)


# Let's look at the top (or head) of the data set:
head(snpsDataFrame)

# Note:  If A is the more common allele and B is the more rare allele (i.e. major and minor alleles, repsectively)
#        Then, AA= 0,  AB= 1, BB = 2


# What are the column names? 
names(snpsDataFrame)

# What are the row names? 
row.names(snpsDataFrame)

# Because the data are really just a large numeric matrix, we convert the dataframe to a matrix:
snps=as.matrix(snpsDataFrame)

################### LOOKING CLOSELY AT ONE SNP #######################

# With row names we can easily extract certain SNPs using the id's
testSNP=snps["rs218206_G",]

table(testSNP)

# What is proportion of heterozygotes at this locus?
het=sum(testSNP==1)/length(testSNP)

# What if there is missing data?
testSNP=snps["rs6717613_A",]

# Try these commands
table(testSNP)
testSNP==1
length(testSNP)
is.na(testSNP)

# Now let's compute the observed heterozygosity
het=sum(testSNP==1)/length(testSNP)  # Note how this fails
het=sum(testSNP==1,na.rm=TRUE)/sum(!is.na(testSNP))  # but this doesn't 


###### EXPLORATORY PLOT OF SNP ALLELE FREQUENCY VS. OBSERVED HETEROZYGOSITY #####

# To inspect the data, let's compute the frequency of each SNP and compare it 
# to the observed heterozygosity (i.e. the proportion of individuals who are heterozygotes)

# What is the frequency of the minor allele?
freq=sum(testSNP,na.rm=TRUE)/(2.0*sum(!is.na(testSNP)))

# Now, let's define functions that do this for a generic set of SNP data
calc_freq=function(x){
	return(sum(x,na.rm=TRUE)/(2.0*sum(!is.na(x))))
}

calc_het=function(x){
	return(sum(x==1,na.rm=TRUE)/(sum(!is.na(x))))
}

# And now let's apply the functions to each and every SNP
freq=apply(snps,1,calc_freq)
het=apply(snps,1,calc_het)

# And now we can make exploratory plots
plot(freq,het,xlab="Frequency",ylab="Heterozygosity")  # Scatter plot

# Let's add a line to show what relationship we'd expect under Hardy-Weinberg expectations
p=seq(0,0.5,by=0.05)   # Set-up a vector with a sequence of allele frequencies
points(p,2*p*(1-p),type="l",col=2) # Plot the HW expectation as a line in red


## APPYLING A CHI-SQUARE TEST TO EACH SNP TO FORMALLY LOOK FOR DEPARTURES FROM HARDY-WEINBERG EXPECTATIONS ###

compute_chisquare=function(x){
	freq=sum(x,na.rm=TRUE)/(2.0*sum(!is.na(x)))
	cnt0=sum(x==0,na.rm=TRUE)
	cnt1=sum(x==1,na.rm=TRUE)
	cnt2=sum(x==2,na.rm=TRUE)
	obscnts=c(cnt0,cnt1,cnt2)
	#print(obscnts)
	n=sum(obscnts)
	expcnts=c((1-freq)^2,2*freq*(1-freq),freq^2)*n
	chisq=sum((obscnts-expcnts)^2/expcnts)
	return(chisq)
}

#let's make a second funtion that makes use of R's built in chisq.test function

compute_chisquare_2=function(x){
	freq=sum(x,na.rm=TRUE)/(2.0*sum(!is.na(x)))
	cnt0=sum(x==0,na.rm=TRUE)
	cnt1=sum(x==1,na.rm=TRUE)
	cnt2=sum(x==2,na.rm=TRUE)
	obscnts=c(cnt0,cnt1,cnt2)
	#print(obscnts)
	n=sum(obscnts)
	#here we use the built-in function for the chi-sq distribution:
	exp_probs=c((1-freq)^2,2*freq*(1-freq),freq^2) #note, here we don't multiply by n
	chisq<-chisq.test(obscnts,p=exp_probs, correct = FALSE)$statistic
	return(chisq)
}




# Apply the compute_chi_square function to each snp
chisqs=apply(snps,1,compute_chisquare)
chisqs2=apply(snps,1,compute_chisquare_2)

#check to see that the chisquare statistcs are the same:
#first do this by computing Pearson's correlation coefficient:
cor.test(chisqs,chisqs2)

#we can also do a quick scatterplot:
plot(chisqs,chisqs2)

# Compute p-values for each chi-square value using the pchisq function
pvals=pchisq(chisqs,1,lower.tail=FALSE)



# Count the number of pvals smaller than the significance threshold
signifthres<-0.05
sum(pvals<signifthres) 
mean(pvals<signifthres) 

#which SNPs?
sig_snps<-subset(pvals,pvals<0.05) #pull out the snps with P-vals<0.05
sig_snp_ids<-names(sig_snps)


#now pull out the genotypes for all teh significant SNPs:
sig_geno<-snps[sig_snp_ids,]

#sig_geno is a matrix that has all the genotypes for the 181 SNPs with a P-value<0.05

###################### PLAYING "FIND-THE-SNP" ##########################
################################################PROBABLY WON'T GET THIS FAR!


# In this section, we will read in data that provides a phenotype for each individual, and 
# then search our list of 

# Read in phenotype data file
z=read.table("pheno.sim.2014.txt",header=TRUE)
# Extract the glucose level column as our phenotype of interest
pheno=z$glucose_mmolperL
# Check length is equal to number of individuals
length(pheno)
# Plot histogram
hist(pheno)

# Fit a linear model where the genotype is a single categorical variable (equivalent to one-way anova)
genofactor=factor(snps["rs218206_G",])
m=lm(pheno~genofactor)
summary(m)
# Make a plot of the phenotype as a function of the categorical variable 
plot(pheno~factor(snps["rs218206_G",]))  # Note: plot automatically produces a boxplot

# Write a function to return the Ftest pvalue from the one-way anova
calc_Ftest_pval=function(geno){
	m=lm(pheno~factor(geno))
	fstat=summary(m)$fstatistic
	pval=pf(fstat[1],fstat[2],fstat[3],lower.tail=FALSE)
	return(pval)
}

# Apply to all SNPs (may be slow)
pvals=apply(snps,1,calc_Ftest_pval)

# Make a "Manhattan plot"  
plot(-log10(pvals),pch=16,main="Genome-wide association scan results")

# Note: with Bonferroni correction - singificance treshold is roughly 10^-5 or ~5 on -log10 scale.
# Add a line to show significance threshold
abline(h=5,col=2)
log_pval<-(-log10(pvals))

#now get the SNPs that are more significant than the threshold:
best_hit_snp<-names(subset(log_pval,log_pval>5))
#now get teh -log10 pvalue
subset(log_pval,log_pval>5)

# Look at the results for the best hit SNP more carefully to confirm the result
m=lm(pheno~factor(snps[best_hit_snp,]))
plot(pheno~factor(snps[best_hit_snp,]))  # Note: plot automatically produces a boxplot
summary(m)


######################################################################
