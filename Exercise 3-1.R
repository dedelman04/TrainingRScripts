line_ctr <- 0
for(i in 1:length(my.data[,1])) { 
	val_cntr <- 0
	for (k in 1:length(my.data[1,])) {
	if (my.data[i,k] < 0) val_cntr <- val_cntr+1
 }
	if (val_cntr == 0) cat("The mean of row number",i,"is", mean(my.data[i,]), "\n")
	else {line_ctr <- line_ctr+1;
		if (line_ctr <= 3) cat("<Row",i,"contains negative values>\n")
		else {
			cat("Too many negative values\n");
			break
		}
	}
}

