all.I.know.about.life.I.learned.in.grad.school <- function()
{
	aa <- "Don't turn away free food."
	bb <- "Why would I want to go to med school when I can have all this stress with less career certainty?"
	cc <- "I want to thank the reviewer for their 15 pages of single spaced helpful comments on my submission."
	dd <- "Papers aren't written, they are rewritten."
	ee <- "Time to push back the frontiers of knowledge."
	ff <- "Need...more...coffee...."
	bromides <- c(aa,bb,cc,dd,ee, ff)
	cat(sample(bromides, size = 1))
}