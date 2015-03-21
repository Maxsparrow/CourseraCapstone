dirpath = 'final/en_US/'
readtext <- function(filename) {
	print(paste0('Reading in file: ',filename))
	con<-file(paste0(dirpath,filename),open="r")
	data<-readLines(con)
	print(paste0('Number of lines in file: ',length(data)))
	return(data)
	close(con)
}
longestline <- function(data) {
	longestline <- 0
	curline <- 0
	for (line in data) {
		curline<-nchar(line)
		if(curline>longestline) longestline<-curline
	}
	print(paste0('Length of longest line in file: ',longestline))
}
stringsearch <- function(data,searchstring,verbose=T) {
	filtereddata <- data[grep(searchstring,data)]
	numoflines<-length(filtereddata)
	if(verbose) print(filtereddata)
	print(paste0('number of matching lines: ',numoflines))
	return(numoflines)
}
twitter <- readtext('en_US.twitter.txt')
news <- readtext('en_US.news.txt')
blogs <- readtext('en_US.blogs.txt')

print(stringsearch(twitter,'love',verbose=F)/stringsearch(twitter,'hate',verbose=F))
print(stringsearch(twitter,'biostats'))
print(stringsearch(twitter,'A computer once beat me at chess, but it was no match for me at kickboxing'))
