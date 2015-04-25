library(stringdist)

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

inputsearch<-function(file,input) {
    output<-file[grepl(input,file)]
    if(length(output)<500) {
        splitinput<-strsplit(input," ")[[1]]
        minmatch<-round(length(splitinput)*0.75)
        for(i in 1:(length(splitinput)-minmatch+1)) {
            output<-c(output,file[grepl(paste(splitinput[i:(i+minmatch-1)],collapse=" "),file)])
        }
    }
    return(output)
}

countwords<-function(sample) {
    count<-0
    for(line in sample) {count<-count+length(strsplit(line," ")[[1]])}
    return(count)
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

improvedtokenizesample<-function(sample,input) {
    ngramsize<-length(strsplit(input," ")[[1]])+1
    for(item in sample) {
        words<-strsplit(item," ")[[1]]
        words<-words[words!=""]
        counter<-1
        while ((counter+ngramsize-2)<length(words)) {
            set<-words[counter:(counter+ngramsize-1)]
            output<-set[length(set)]
            predictor<-paste(set[1:(length(set)-1)],collapse=" ")
            counter<-counter+1
            if(predictor==input) {
                if(!exists("ngrams")) ngrams<-data.frame(predictor=predictor,output=output)
                else ngrams<-rbind(ngrams,data.frame(predictor=predictor,output=output))
            }
            gc()
        }
    }
    if(!exists("ngrams") || nrow(ngrams)==0) return(NA)
    else {
        ngrams$predictor<-as.character(ngrams$predictor)
        ngrams$output<-as.character(ngrams$output)    
        return(ngrams)
    }
}

makengrams<-function(textsample,input,subsize=1000) {   
    if(length(textsample)==0) return(NA)
    print(paste("Size of text sample:",length(textsample)))
    if(length(textsample)>subsize) {
        print(paste0("Sample too large, taking subsample of size ",subsize))
        textsample<-sample(textsample,subsize)
    }
    gc()
    textsample<-scrubsample(textsample)    
    ngrams<-improvedtokenizesample(textsample,input)
    return(ngrams)
}

loopfindoutput<-function(input,filename="ALL",maxsize=100,samplesize=100,filenumlines=1000000,top=1) {
    tempinput<-strsplit(input," ")[[1]]
    if(length(tempinput)>3) {
        print("Too many words, using last 3 words for input")
        input<-paste(tempinput[(length(tempinput)-2):length(tempinput)],collapse=" ")
    }    
    
    finaloutput<-data.frame()
    output<-data.frame() 
    runcount<-0
    error<-FALSE
    
    repeat{                
        runcount<-runcount+1
        print(paste("run number:",runcount))
        print(paste("current input:",input))
        
        textsample<-inputsearch(file,input)
        rm(file)
        gc()
        
        ngrams<-makengrams(textsample,input,maxsize)
        
        if(is.na(ngrams)) {
            print("Sample too small, retrying with smaller ngram")
            input<-inputreducer(input)
            if(is.na(input)) {
                error<-TRUE
                break
            }
            next
        }        
        
        print(paste("Size of tokenized ngrams:",nrow(ngrams)))
        
        output<-findoutput(ngrams,input,top)
        print("Exact matches:")
        print(output)        
        if(!is.na(output)) finaloutput<-rbind(finaloutput,data.frame(output))
        
        if(nrow(finaloutput)>=top) {
            break
        }
        print("Match not sufficient, retrying with fewer ngrams")
        input<-inputreducer(input)
        if(is.na(input)) {
            error<-TRUE
            break
        }
    }

    if(error==TRUE) return(enderror())
    else return(finaloutput)
}

enderror<-function() {    
    print("Cannot reduce input any further. No prediction found")
    return(NA)
}