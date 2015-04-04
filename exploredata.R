dirpath<-"Data Scientist Specialization/CourseraCapstone/final/en_US/"

readfile<-function(filetype) {
    curfile<-file(paste0(dirpath,"en_US.",filetype,".txt"))
    output<-readLines(curfile)
    close(curfile)
    return(output)
}

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

countwords<-function(sample) {
    count<-0
    for(line in sample) {count<-count+length(strsplit(line," ")[[1]])}
    return(count)
}

scrubsample<-function(sample) {
    set<-vector()
    for(item in sample) {
        Encoding(item)<-"latin1"
        item<-iconv(item,"latin1","ASCII",sub="")
        item<-gsub("[-?:.\"<>!()_~`$%&*+,/;=@^]"," ",item)
        item<-gsub("[[:digit:]]+"," ::number:: ",item)
        set<-append(set,item)
    }
    return(set)
}

analyzesample<-function(sample,top) {
    wordlist<-data.frame()
    for(line in sample) {
        words<-unique(strsplit(line," ")[[1]])
        for(word in words) {
            if(nchar(word)>2) {
                if(word %in% wordlist$word) wordlist$count[wordlist$word==word]<-wordlist$count[wordlist$word==word]+1
                else wordlist<-rbind(wordlist,data.frame(word=word,count=1))                
            }
        }
    }
    wordlist$percofdocs<-round(wordlist$count/length(sample)*100,2)
    wordlist<-subset(wordlist,select= -count)
    wordlist<-wordlist[order(-wordlist$percofdocs),]
    return(head(wordlist,top))
}

tokenizesample<-function(sample,ngramsize) {
    for(item in sample) {
        words<-strsplit(item," ")[[1]]
        words<-words[words!=""]
        counter<-1
        while ((counter+ngramsize-2)<length(words)) {
            set<-words[counter:(counter+ngramsize-1)]
            output<-set[length(set)]
            predictor<-paste(set[1:(length(set)-1)],collapse=" ")
            counter<-counter+1
            if(!exists("ngrams")) ngrams<-data.frame(predictor=predictor,output=output)
            else ngrams<-rbind(ngrams,data.frame(predictor=predictor,output=output))
        }
    }
    
    return(ngrams)
}

trainsplit <- function(ngrams,perc) {
    keep<-trunc(length(ngrams)*perc)
    trainset<-ngrams[keep]
    testset<-ngrams[!keep]
    splitdata<-list(trainset=trainset,testset=testset)
    return(splitdata)
}

##TestRun
if(!exists("twitter")) twitter<-readfile("twitter")
twitter<-sample(twitter,trunc(length(twitter)/10))
twitter<-scrubsample(twitter)
print(system.time(ngrams<-tokenizesample(twitter,3)))
splitdata<-trainsplit(ngrams,0.6)
trainset<-splitdata$trainset
testset<-splitdata$testset