file.name <- "C:/Users/jjborrelli/Downloads/savedrecs (3).txt"
data <- readLines(file.name)

authors <- function(data){
  data2 <- paste(" ", data)
  strt <- grep("  AU ", data2)
  ends <- grep("  AF ", data2) - 1
  
  AUs <- matrix(c(strt, ends), ncol = 2)
  
  allAU <- list()
  
  for(i in 1:nrow(AUs)){
    rws <- seq(AUs[i,1], AUs[i,2], 1)
    auths <- strsplit(data[rws], ",")
    allAU[[i]] <- sapply(lapply(auths, function(x) {as.character(substr(x, start=4, stop=nchar(x)))}), "[[", 1)
  }
  
  allAU <- sapply(allAU, function(x){
    if(length(x) > 1){
      #x <- paste(x[1], "et. al")
      x <- x[1]
    }else{x}
  })
  
  
  return(allAU)
}

journal <- function(data){
  data2 <- paste(" ", data)
  journ <- unlist(lapply(data[grep("  J9 ", data2)], function(x){as.character(substr(x, start = 4, stop = nchar(x)))}))
  
  return(journ)
}

years <- function(data){
  data2 <- paste(" ", data)
  yrs <- sapply(data[grep("  PY ", data2)], function(x){strsplit(x, " ")[[1]]})[2,]
  names(yrs) <- NULL
  
  return(yrs)
}


titles <- function(data){
  data2 <- paste(" ", data)
  strt <- grep("  TI ", data2)
  ends <- grep("  SO ", data2) - 1
  
  TIs <- matrix(c(strt, ends), ncol = 2)
  
  allTI <- c()
  
  for(i in 1:nrow(TIs)){
    rws <- seq(TIs[i,1], TIs[i,2], 1)
    wrds <- unlist(strsplit(data[rws], " "))[-1]
    ti <- tolower(paste(wrds[sapply(wrds, nchar) >= 1], collapse = " "))
    allTI[i] <- paste(toupper(substr(ti, 1, 1)), substr(ti, 2, nchar(ti)), sep="")
  }
  return(allTI)
}


citingREFs <- function(data){
  data2 <- paste(" ", data)
  strt <- grep("  CR ", data2)
  ends <- grep("  NR ", data2) - 1
  
  CRs <- matrix(c(strt, ends), ncol = 2)
  
  cr.list <- list()
  
  for(i in 1:nrow(CRs)){
    rws <- seq(CRs[i,1], CRs[i,2], 1)
    crs <- substring(data[rws], first = 4)
    
    spt <- strsplit(crs, ", ")
    #only get citing references with data for au, yr, and jrn
    spt <- spt[sapply(spt, length) >= 3]
    
    aus <- tolower(sapply(spt, function(x){strsplit(x, " ")[[1]][1]}))
    aus <- paste(toupper(substr(aus, 1, 1)), substr(aus, 2, nchar(aus)), sep="")
   
    yrs <- sapply(spt, "[[", 2)
    
    jrn <- sapply(spt, "[[", 3)
    
    cr.list[[i]] <- matrix(c(aus, yrs, jrn), ncol = 3)
  }
  
  return(cr.list)
}



papers <- matrix(c(authors(data), publi(data), titles(data)), ncol = 4)

papers <- matrix(c(authors(data), years(data)), ncol = 2)

crefs <- citingREFs(data) 

el1 <- lapply(1:nrow(papers), function(x){cbind(paper = rep(paste(papers[x, 1:2], collapse = " "), nrow(crefs[[x]])), citing = apply(crefs[[x]][,1:2], 1, paste, collapse = " "))})
el2 <- do.call("rbind", el1)

library(igraph)


library(plyr)
test <- lapply(crefs, function(x) apply(x[,1:2], 1, paste, collapse = " "))
df <- data.frame(a = do.call("c", test))
t2 <- ddply(data.frame(el2), .(citing) ,nrow)
t3 <- t2[t2$V1 > 5,]

el3 <- el2[el2[,"citing"] %in% t3$citing,]
g <- graph.edgelist(el3)
plot(g, layout = matrix(runif(198*2), ncol = 2), vertex.label = NA)
degree(g)


colnames(get.adjacency(g))
date <- as.numeric(sapply(strsplit(colnames(get.adjacency(g)), " "), "[", 2))
date
sapply(strsplit(colnames(get.adjacency(g)), " "), "[", 2)
colnames(get.adjacency(g))[189]
date <- as.numeric(sapply(strsplit(colnames(get.adjacency(g)), " "), function(x){x[length(x)]}))
date
plot(g, layout = matrix(runif(198*2), ncol = 2), vertex.label = NA, vertex.color = date)
