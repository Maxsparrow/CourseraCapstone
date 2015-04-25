dirpath<-"Data Scientist Specialization/CourseraCapstone/"
rawfilepath<-"final/en_US/"

findtop<-function(filtered,top) {
    if(NROW(filtered)>0) counts<-aggregate(predictor~output,data=filtered,length)
    else return(NA)    
    colnames(counts)[2]<-"count"
    counts<-counts[order(-counts$count),]
    counts<-head(counts,top)
    counts$perc<-round(counts$count/sum(counts$count),2)
    return(subset(counts,select=-count))    
}

findoutput<-function(ngrams,input,top) {
    input<-tolower(input)
    filtered<-ngrams[ngrams$predictor==input,]
    return(findtop(filtered,top))
}

##Not using currently, too inaccurate, consider lowing jw score limit
fuzzyoutput<-function(ngrams,input,top) {
    scores<-stringdist(ngrams$predictor,input,method="jw",p=0.1)
    filtered<-ngrams[scores<0.06,]
    return(findtop(filtered,top))
}

inputreducer<-function(input) {
    splitinput<-strsplit(input," ")[[1]]
    if(length(splitinput)==1) return(NA)
    input<-paste(splitinput[2:length(splitinput)],collapse=" ")
    return(input)
}

loopfindoutput<-function(input) {
    tempinput<-strsplit(input," ")[[1]]
    if(length(tempinput)>3) {
        print("Too many words, using last 3 words for input")
        input<-paste(tempinput[(length(tempinput)-2):length(tempinput)],collapse=" ")
    }
    
    runcount<-0
    repeat{
        runcount<-runcount+1
        print(paste("run number:",runcount))
        print(paste("current input:",input))
        
        numwords<-length(strsplit(input," ")[[1]])             
        ngrams<-read.table(paste0(dirpath,numwords+1,"grams.txt"))  
        
        results<-ngrams[ngrams$predictor==input,"output"]      
        
        if(length(results)==1) return(results)
        else input<-inputreducer(input)
        if(is.na(input)) {
            print("Cannot reduce input any further. No prediction found")
            #fuzzyoutput?
            return(NA)
        }
    }
}