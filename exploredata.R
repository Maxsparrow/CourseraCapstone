dirpath<-"Data Scientist Specialization/Capstone/final/en_US/"

readfile<-function(filetype) {
    curfile<-file(paste0(dirpath,"en_US.",filetype,".txt"))
    output<-readLines(curfile)
    close(curfile)
    return(output)
}

percofdocs<-function(text,file) {
    count<-0
    for(line in file) {
        if(grepl(text,line)) {
            print(line)
            count<-count+1
        }
    }
    print(paste0("Percentage of docs with this string: ",round(count/length(file)*100,2),"%"))
}

if(!exists("twitter")) twitter<-readfile("twitter")
if(!exists("news")) news<-readfile("news")
if(!exists("blogs")) blogs<-readfile("blogs")

tokenizesample<-function(file,size) {
    set<-sample(file,size)
    wordlist<-vector()
    for(item in set) {
        cleaneditem<-gsub("[[:punct:]]"," ",item)
        words<-strsplit(cleaneditem," ")[[1]]
        for(word in words) {
            if (word != "") wordlist<-c(wordlist,word)
        }
    }
    return(wordlist)
}

analyzesample<-function(sample) {
    wordlist<-unique(sample)
    wordlist<-wordlist[sapply(wordlist,function(x) nchar(x)>2)]
    wordframe<-data.frame(word=wordlist,count=0)
    for(word in wordlist) {
        wordcount<-sum(grepl(word,sample))
        wordframe[wordframe$word==word,"count"]<-wordcount
    }
    wordframe<-wordframe[order(-wordframe$count),]
    wordframe<-wordframe[wordframe$count>1,]
    return(head(wordframe,100))
}