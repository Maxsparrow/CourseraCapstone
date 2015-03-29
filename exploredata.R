dirpath<-"Data Scientist Specialization/Capstone/final/en_US/"

readfile<-function(filetype) {
    curfile<-file(paste0(dirpath,"en_US.",filetype,".txt"))
    output<-readLines(curfile)
    close(curfile)
    return(output)
}

if(!exists("twitter")) twitter<-readfile("twitter")
if(!exists("news")) news<-readfile("news")
if(!exists("blogs")) blogs<-readfile("blogs")

summarizefile<-function(file) {
    print(paste0('Number of documents in file: ',length(file)))
    count<-0
    sample<-sample(file,trunc(length(file)/10))
    for(line in sample) {count<-count+length(strsplit(line," ")[[1]])}
    print(paste0('Estimated number of words: ',count*10))
}

percofdocs<-function(searchtext,file) {
    count<-0
    sample<-sample(file,trunc(length(file)/10))
    for(line in sample) {
        if(grepl(searchtext,line)) {
            count<-count+1
        }
    }
    print(paste0("Estimated percentage of docs with this string: ",round(count*10/length(file)*100,2),"%"))
}

getsample<-function(file,size) {    
    sample<-sample(file,size)
    return(sample)
}

scrubsample<-function(sample) {
    set<-vector()
    for(item in sample) {
        item<-gsub("<3"," heartsymbolplaceholder ",item)
        item<-gsub("[-?:.\"<>!()_~`$%&*+,/;=@^]"," ",item)
        item<-gsub("heartsymbolplaceholder","<3",item)
        item<-gsub("[[:digit:]]+"," ::number:: ",item)
        set<-append(set,item)
    }
    return(set)
}

tokenizesample<-function(sample,ngramsize) {
    ngrams<-list()
    item<-sample[1]
    for(item in sample) {
        words<-strsplit(item," ")[[1]]
        counter<-1
        while ((counter+ngramsize-1)<length(words)) {
            set<-words[counter:(counter+ngramsize-1)]
            counter<-counter+1
            ngrams<-append(ngrams,list(set))
        }
    }
    
    return(ngrams)
}

analyzesample<-function(sample) {
    wordlist<-vector()
    for(line in sample) {
        for(word in line) {
            wordlist<-append(wordlist,word)
        }
    }
    wordlist<-unique(wordlist)
    wordlist<-wordlist[sapply(wordlist,function(x) nchar(x)>2)]
    wordframe<-data.frame(word=wordlist,count=0)
    for(word in wordlist) {
        wordcount<-sum(grepl(word,sample,fixed=T))
        wordframe[wordframe$word==word,"count"]<-wordcount
    }
    wordframe<-wordframe[order(-wordframe$count),]
    wordframe<-wordframe[wordframe$count>1,]
    return(head(wordframe,100))
}