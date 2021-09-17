## R bootcamp Alfaro problem set 2

Here are additional challenges to cement some of the ideas we covered in my lecture today. The lecture pdf on the website has examples that will help you with these problems. 


### Data set matching and file input/output

1. Create a vector of random body sizes and add this to the data frame we created in class from the `data.txt` file. Write a comma separated text file that contains two columns: the species name and the body size for median paired fin (MPF) swimmers only. Read this file back in to R and plot a histogram of body size.
2. Prune both the tree `tree.tre` and the data file you created above so that the tree and data are matched. You will need to use subset functions on the dataframe and drop.tip on the tree. `setdiff()` will help you identify taxa present in one object that are missing from the other. Print this tree as a pdf.
3. (bonus) Use the `dotTree()` function from the phytools library to create a phylogenetic dotplot of the tree and data from 2 and save that figure as a pdf. 

## functions and flow control

1. Write pseudocode for a function that takes a vector of integers (e.g. 4,2,7) and prints the square root of the sums of values that are 1 greater and 1 less than the current value. Return the sum of square roots. 
2.  Write the r code for your pseudocode and save the function.
3. Use the `source()` function to call your function and pass it the following vector: (100, 200, 3)