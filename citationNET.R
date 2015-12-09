# Download Web of Science Marked List with the following boxes checked: 
## Authors
## ISSN
## Title
## Cited references
## Cited Reference Count
## Accession Number
## Source
## Keywords
## Source Abbrev
## Author Identifiers
## Publisher Info
## Page Count


file.name <- "C:/Users/jjborrelli/Downloads/savedrecs (3).txt"
file.name <- "C:/Users/jjborrelli/Desktop/GitHub/citationNetwork/AG1989_refs.txt"
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
      x <-  x[1]
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
    
    #aus <- tolower(sapply(spt, function(x){strsplit(x, " ")[[1]][1]}))
    #aus <- paste(toupper(substr(aus, 1, 1)), substr(aus, 2, nchar(aus)), sep="")
    aus <- toupper(sapply(spt, function(x){strsplit(x, " ")[[1]][1]}))
   
    
    yrs <- sapply(spt, "[[", 2)
    
    jrn <- sapply(spt, "[[", 3)
    
    cr.list[[i]] <- matrix(c(aus, yrs, jrn), ncol = 3)
  }
  
  return(cr.list)
}



papers <- matrix(c(authors(data), years(data)), ncol = 2)
papers[,1] <- toupper(papers[,1])

crefs <- citingREFs(data) 

el1 <- lapply(1:nrow(papers), function(x){cbind(paper = rep(paste(papers[x, 1:2], collapse = " "), nrow(crefs[[x]])), citing = apply(crefs[[x]][,1:2], 1, paste, collapse = " "))})
el2 <- do.call("rbind", el1)

library(igraph)


library(plyr)
test <- lapply(crefs, function(x) apply(x[,1:2], 1, paste, collapse = " "))
df <- data.frame(a = do.call("c", test))
t2 <- ddply(data.frame(el2), .(citing) ,nrow)
t3 <- t2[t2$V1 > 10,]

el3 <- el2[el2[,"citing"] %in% t3$citing,]
g <- graph.edgelist(unique(el3))
plot(g, layout = matrix(runif(1092*2), ncol = 2), vertex.label = NA, vertex.size = 2, edge.arrow.size = .5)
degree(g)


which(colnames(get.adjacency(g)) == "ARDITI 1989")

date <- as.numeric(sapply(strsplit(colnames(get.adjacency(g)), " "), function(x){x[length(x)]}))
date

paperDATE <- as.numeric(t(sapply(strsplit(el3[,1], " "), function(x){x[length(x)]})))
citingDATE <- as.numeric(sapply(strsplit(el3[,2], " "), function(x){x[length(x)]}))



allyrs <- seq(1990, 2015, 1) 

g.list <- list()
for(i in 1:length(allyrs)){
  g.list[[i]] <- graph.edgelist(unique(el3[paperDATE <= allyrs[i],]))
}

d.ag1989 <- c()
md <- c()
for(i in 1:length(allyrs)){
  d.ag1989[i] <- degree(g.list[[i]])[which(colnames(get.adjacency(g)) == "ARDITI 1989")]
  md[i] <- degree(g.list[[i]])[which.max(degree(g.list[[i]]))]
}

n.nodes <- sapply(g.list, function(x){length(V(x))})
plot(n.nodes~allyrs)
n.edges <- sapply(g.list, function(x){length(E(x))})
plot(n.edges~allyrs)

con <- n.edges/(n.nodes*(n.nodes -1))

library(rnetcarto)
conversion <- function(tm){
  for(i in 1:nrow(tm)){
    for(j in 1:ncol(tm)){
      if(tm[i,j] == 1){tm[j,i] <- 1}
    }
  }
  return(tm)
}
g.list.con <- lapply(lapply(lapply(g.list, get.adjacency, sparse = F), conversion), graph.adjacency)
#mod.all <- lapply(lapply(g.list, get.adjacency, sparse = F), netcarto)
mod.all2 <- lapply(lapply(g.list.con, get.adjacency, sparse = F), netcarto)

#n.mods <- sapply(mod.all, function(x){max(x[[1]]$module) + 1})
n.mods2 <- sapply(mod.all2, function(x){max(x[[1]]$module) + 1})

#mods <- sapply(mod.all, "[[", 2)
mods2 <- sapply(mod.all2, "[[", 2)

plot(mods2~allyrs)


g.m1 <- mod.all2[[26]][[1]][mod.all2[[26]][[1]]$module == 0,]
g.m2 <- mod.all2[[26]][[1]][mod.all2[[26]][[1]]$module == 1,]
g.m3 <- mod.all2[[26]][[1]][mod.all2[[26]][[1]]$module == 2,]
g.m4 <- mod.all2[[26]][[1]][mod.all2[[26]][[1]]$module == 3,]

g.n1 <- sapply(strsplit(as.character(g.m1$name), " "), "[", 1)
g.n2 <- sapply(strsplit(as.character(g.m2$name), " "), "[", 1)
g.n3 <- sapply(strsplit(as.character(g.m3$name), " "), "[", 1)
g.n4 <- sapply(strsplit(as.character(g.m4$name), " "), "[", 1)

wordcloud(paste(g.n1, collapse = " "), min.freq = 1)
wordcloud(paste(g.n2, collapse = " "), min.freq = 1)
wordcloud(paste(g.n3, collapse = " "), min.freq = 1)
wordcloud(paste(g.n4, collapse = " "), min.freq = 1)

y = 26
lay <- matrix(ncol =2, nrow = n.nodes[26])
lay[mod.all2[[26]][[1]]$module == 0,] <- cbind(runif(sum(mod.all2[[26]][[1]]$module == 0), -.5, .5), runif(sum(mod.all2[[26]][[1]]$module == 0), .5, 1))
lay[mod.all2[[26]][[1]]$module == 1,] <- cbind(runif(sum(mod.all2[[26]][[1]]$module == 1), -1, -.2), runif(sum(mod.all2[[26]][[1]]$module == 1), -.5, 0))
lay[mod.all2[[26]][[1]]$module == 2,] <- cbind(runif(sum(mod.all2[[26]][[1]]$module == 2), .2, 1), runif(sum(mod.all2[[26]][[1]]$module == 2), -.5, 0))
plot(g.list[[y]], layout = lay, vertex.color = mod.all2[[y]][[1]]$module, vertex.label = NA, vertex.label.cex = .7, vertex.size = 3, edge.arrow.size = .5, edge.width = .05)
 


innerhubs <- lapply(lapply(mod.all2, "[[", 1), function(q){q$name[aggregate(q$connectivity, list(q$module), which.max)$x]})

lapply(lapply(mod.all2, "[[", 1), function(q){q$name[aggregate(q$participation, list(q$module), which.max)$x]})
conhubs <- lapply(lapply(mod.all2, "[[", 1), function(q){q$name[grep("Connector", q$role)]})


for(i in 2:26){
  plot(graph.adjacency(get.adjacency(g)[conhubs[[i]],conhubs[[i]]]), 
       layout= lay[V(g)$name %in% conhubs[[i]],], 
       vertex.label.cex = .25, 
       vertex.color = mod.all2[[i]][[1]][mod.all2[[i]][[1]]$name %in% conhubs[[i]], "module"], 
       vertex.size = 3, 
       edge.arrow.size = .5)
  
  text(x = -1.5, y = 1, i)
}


conhubs[[26]]
degree(g)[names(degree(g)) %in% conhubs[[26]]]


## Coyne and Orr 1989 

file.name1 <- "C:/Users/jjborrelli/Desktop/CoyneOrr1989refs.txt"
data1 <- readLines(file.name1)

papers1 <- matrix(c(authors(data1), years(data1)), ncol = 2)
papers1[,1] <- toupper(papers1[,1])

crefs1 <- citingREFs(data1) 

el1.1 <- lapply(1:nrow(papers1), function(x){cbind(paper = rep(paste(papers1[x, 1:2], collapse = " "), nrow(crefs1[[x]])), citing = apply(crefs1[[x]][,1:2], 1, paste, collapse = " "))})
el2.1 <- do.call("rbind", el1.1)

library(igraph)


library(plyr)
test <- lapply(crefs1, function(x) apply(x[,1:2], 1, paste, collapse = " "))
df <- data.frame(a = do.call("c", test))
t2.1 <- ddply(data.frame(el2.1), .(citing), nrow)
t3.1 <- t2.1[t2.1$V1 > 10,]

el3.1 <- el2.1[el2.1[,"citing"] %in% t3.1$citing,]
g1 <- graph.edgelist(unique(el3.1))

ad <- get.adjacency(g1, sparse = F)
n.paps <- nrow(ad)
con1 <- sum(ad)/(nrow(ad)*(nrow(ad) - 1))

paperDATE1 <- as.numeric(t(sapply(strsplit(el3.1[,1], " "), function(x){x[length(x)]})))

g.list1 <- list()
for(i in 1:length(allyrs)){
  g.list1[[i]] <- graph.edgelist(unique(el3.1[paperDATE1 <= allyrs[i],]))
}

g.list.con1 <- lapply(lapply(lapply(g.list1, get.adjacency, sparse = F), conversion), graph.adjacency)

mod.all2.1 <- lapply(lapply(g.list.con1, get.adjacency, sparse = F), netcarto)

n.mods2.1 <- sapply(mod.all2.1, function(x){max(x[[1]]$module) + 1})

mods2 <- sapply(mod.all2.1, "[[", 2)

mod1 <- netcarto(conversion(ad))
mod <- netcarto(conversion(get.adjacency(g, sparse = F)))
