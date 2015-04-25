library(plyr)

dirpath<-"Data Scientist Specialization/CourseraCapstone/"
rawfilepath<-"final/en_US/"

readfile<-function(filetype,numlines) {
    curfile<-file(paste0(dirpath,rawfilepath,"en_US.",filetype,".txt"))
    if(numlines=="ALL") output<-readLines(curfile)        
    else output<-readLines(curfile,n=numlines)
    close(curfile)
    gc()
    return(output)
}

readallfiles<-function(numlines) {
    if(numlines!="ALL") numlines<-trunc(numlines/3)
    twitter<-readfile("twitter",n=numlines)
    news<-readfile("news",n=numlines)
    blogs<-readfile("blogs",n=numlines)
    all<-c(twitter,news,blogs)
    rm(twitter,news,blogs)
    gc()
    return(all)
}

scrubsample<-function(sample) {
    set<-vector()
    for(item in sample) {
        Encoding(item)<-"latin1"
        item<-iconv(item,"latin1","ASCII",sub="")
        item<-gsub("[-?:.\"<>!()_~`$%&*+,/;=@^\\]"," ",item)
        item<-gsub("[[:digit:]]+"," ::number:: ",item)
        item<-tolower(item)
        set<-append(set,item)
    }
    return(set)
}

tokenizesample<-function(sample,ngramsize) {
    itemcounter<-0
    for(item in sample) {
        itemcounter<-itemcounter+1
        if(itemcounter %% 100==0) {
            print(paste("Processing ngramsize",ngramsize,": On item #",itemcounter,"out of",length(sample),"items"))
            gc(verbose=T)
        }
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
            gc()
        }
    }
    ngrams$predictor<-as.character(ngrams$predictor)
    ngrams$output<-as.character(ngrams$output)
    return(ngrams)
}

createngramfile<-function(ngramsize,desiredfilelength) {
    outputname<-paste0(ngramsize,"grams.txt")
    currfilelength<-0
    if(file.exists(paste0(dirpath,outputname))) currfilelength<-nrow(read.table(paste0(dirpath,outputname)))
    gc()
    while(currfilelength<desiredfilelength) {    
        print(paste("On run number:",counter))
        print(paste("This is the current time",Sys.time())) 
        print(paste(outputname,"currently has",currfilelength,"rows out of",desiredfilelength,"needed"))
        file<-readallfiles("ALL")
        textsample<-sample(file,trunc(length(file)/1000))
        rm(file)
        gc(verbose=T)
        textsample<-scrubsample(textsample)
        ngrams<-tokenizesample(textsample,ngramsize)
        if(file.exists(paste0(dirpath,outputname))) {
            currenttable<-read.table(paste0(dirpath,outputname))
            currenttable<-rbind(currenttable,ngrams)
        }
        else currenttable<-ngrams
        write.table(currenttable,paste0(dirpath,outputname))
    }
    print(paste("Execution complete:",outputname,"currently has",currfilelength,"rows out of",desiredfilelength,"needed"))
    aggregatengramfile(outputname)
}

aggregatengramfile<-function(filename) {
    ngrams<-read.table(paste0(dirpath,filename))
    ngrams<-count(ngrams)
    uniquepreds<-unique(ngrams$predictor)
    for(pred in uniquepreds) {
        temp<-ngrams[ngrams$predictor==pred,]
        temp<-temp[order(-temp$freq),]
        temp<-head(temp,1)
        if(!exists("finalngrams")) finalngrams<-data.frame(temp)
        else finalngrams<-rbind(finalngrams,temp)
    }
    write.table(finalngrams,paste0(dirpath,filename,"_filtered"))
}

#if(!file.exists(paste0(dirpath,"2grams.txt"))) createngramfile(2)
#if(!file.exists(paste0(dirpath,"3grams.txt"))) createngramfile(3)
#if(!file.exists(paste0(dirpath,"4grams.txt"))) createngramfile(4)