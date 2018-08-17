my.display <- function(x, display=FALSE, type="None", prob=TRUE) {

if (display) 
	if (type=="hist") hist(x, freq=prob)
	else if (type=="density") plot(density(x))
		else cat("Please specify type as either hist or density\n")


cat("summary of input:\n")
return(summary(x))
}