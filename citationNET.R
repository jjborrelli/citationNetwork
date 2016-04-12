#########################################################
#########################################################
###### Initial requirements

library(igraph)
library(plyr)
library(ggplot2)
library(rnetcarto)
library(wordcloud)
library(animation)

#### Notes on the data

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

# get the citation text data
file.name <- "C:/Users/jjborrelli/Desktop/GitHub/citationNetwork/AG1989_refs.txt"
data <- readLines(file.name)

#########################################################
#########################################################
###### Functions

# find author names for each paper
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


# get author names for all the citations within citations
CITING_authors <- function(data){
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
  return(allAU)
}

# get the journal name
journal <- function(data){
  data2 <- paste(" ", data)
  journ <- unlist(lapply(data[grep("  J9 ", data2)], function(x){as.character(substr(x, start = 4, stop = nchar(x)))}))
  
  return(journ)
}

# get the date for each publication
years <- function(data){
  data2 <- paste(" ", data)
  yrs <- sapply(data[grep("  PY ", data2)], function(x){strsplit(x, " ")[[1]]})[2,]
  names(yrs) <- NULL
  
  return(yrs)
}

# retrieve title information for each publication
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

# retrieve data for citations within citations
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

# convert directional to nondirectional matrix
conversion <- function(tm){
  for(i in 1:nrow(tm)){
    for(j in 1:ncol(tm)){
      if(tm[i,j] == 1){tm[j,i] <- 1}
    }
  }
  return(tm)
}

#plot a particular module of the network
plotMOD <- function(mod, vcol, y = 26, mod.all = mod.all2, g.l = g.list, lay1 = lay){
  m <- colnames(get.adjacency(g.l[[y]])) %in% mod.all[[y]][[1]]$name[mod.all[[y]][[1]]$module == mod]
  g <- graph.adjacency(get.adjacency(g.l[[y]], sparse = F)[m, m])
  
  print(plot(g, layout = lay1[mod.all[[y]][[1]]$module == mod,], vertex.color = vcol, 
             vertex.label.cex = .6, vertex.label.color = "black", vertex.label.dist = .15, vertex.size = 4, 
             vertex.frame.color = "green", edge.arrow.size = .2, edge.width = .01, margin = 0))
}


#########################################################
#########################################################
###### Pull the information out of the data

# create a matrix of first author and the year of the citation
papers <- matrix(c(authors(data), years(data)), ncol = 2)
papers[,1] <- toupper(papers[,1])

# get the data on the citations within the citations
# name year and journal for each article citation within AG citations
crefs <- citingREFs(data) 

# create an edgelist for the references of each citing article
el1 <- lapply(1:nrow(papers), function(x){cbind(paper = rep(paste(papers[x, 1:2], collapse = " "), nrow(crefs[[x]])), citing = apply(crefs[[x]][,1:2], 1, paste, collapse = " "))})
# bind all edgelists together
el2 <- do.call("rbind", el1)

# select only papers that are cited more than 10 times
test <- lapply(crefs, function(x) apply(x[,1:2], 1, paste, collapse = " "))
df <- data.frame(a = do.call("c", test))
t2 <- ddply(data.frame(el2), .(citing) ,nrow)
t3 <- t2[t2$V1 > 10,]

# recreate edgelist with the common references
el3 <- el2[el2[,"citing"] %in% t3$citing,]
# turn the edgelist into a graph
g <- graph.edgelist(unique(el3))
# plot
plot(g, layout = matrix(runif(1092*2), ncol = 2), vertex.label = NA, vertex.size = 2, edge.arrow.size = .5)
# quick look at the degree of each node
degree(g)

which(colnames(get.adjacency(g)) == "ARDITI 1989")

# get the year each article was published
date <- as.numeric(sapply(strsplit(colnames(get.adjacency(g)), " "), function(x){x[length(x)]}))
date

# get year of the paper and the reference
paperDATE <- as.numeric(t(sapply(strsplit(el3[,1], " "), function(x){x[length(x)]})))
citingDATE <- as.numeric(sapply(strsplit(el3[,2], " "), function(x){x[length(x)]}))



# create a list of graphs, one for each year
allyrs <- seq(1990, 2015, 1) 

g.list <- list()
for(i in 1:length(allyrs)){
  g.list[[i]] <- graph.edgelist(unique(el3[paperDATE <= allyrs[i],]))
}


# compute the degree of the focal paper, and the largest degree in the network at each time point
d.ag1989 <- c()
md <- c()
for(i in 1:length(allyrs)){
  d.ag1989[i] <- degree(g.list[[i]])[which(colnames(get.adjacency(g)) == "ARDITI 1989")]
  md[i] <- degree(g.list[[i]])[which.max(degree(g.list[[i]]))]
}
qplot(x = allyrs, y = d.ag1989, geom = "line") + geom_point(size = 5) + xlab("YEAR") + ylab("CITATIONS TO A&G1989") + theme_bw(base_size = 30)


# nodes over time
n.nodes <- sapply(g.list, function(x){length(V(x))})
qplot(x = allyrs, y = n.nodes, geom = "line") + geom_point(size = 5) + xlab("YEAR") + ylab("NUMBER OF NODES") + theme_bw(base_size = 30)

# edges over time
n.edges <- sapply(g.list, function(x){length(E(x))})
qplot(x = allyrs, y = n.edges, geom = "line") + geom_point(size = 5) + xlab("YEAR") + ylab("NUMBER OF EDGES") + theme_bw(base_size = 30)

# connectance over time
con <- n.edges/(n.nodes*(n.nodes -1))
qplot(x = allyrs, y = con, geom = "line") + geom_point(size = 5) + xlab("YEAR") + ylab("CONNECTANCE") + theme_bw(base_size = 30)


# compute the modularity of the network at each time point
g.list.con <- lapply(lapply(lapply(g.list, get.adjacency, sparse = F), conversion), graph.adjacency)
#mod.all <- lapply(lapply(g.list, get.adjacency, sparse = F), netcarto)
mod.all2 <- lapply(lapply(g.list.con, get.adjacency, sparse = F), netcarto)

#n.mods <- sapply(mod.all, function(x){max(x[[1]]$module) + 1})
n.mods2 <- sapply(mod.all2, function(x){max(x[[1]]$module) + 1})

#mods <- sapply(mod.all, "[[", 2)
mods2 <- sapply(mod.all2, "[[", 2)

qplot(x = allyrs, y = mods2, geom = "line") + geom_point(size = 5) + xlab("YEAR") + ylab("MODULARITY") + theme_bw(base_size = 30)
qplot(x = allyrs, y = n.mods2, geom = "line") + geom_point(size = 5) + xlab("YEAR") + ylab("NUMBER OF MODULES") + theme_bw(base_size = 30)

# split whole network into its component modules
g.m1 <- mod.all2[[26]][[1]][mod.all2[[26]][[1]]$module == 0,]
g.m2 <- mod.all2[[26]][[1]][mod.all2[[26]][[1]]$module == 1,]
g.m3 <- mod.all2[[26]][[1]][mod.all2[[26]][[1]]$module == 2,]
g.m4 <- mod.all2[[26]][[1]][mod.all2[[26]][[1]]$module == 3,]

# get the names of authors in each module
g.n1 <- sapply(strsplit(as.character(g.m1$name), " "), "[", 1)
g.n2 <- sapply(strsplit(as.character(g.m2$name), " "), "[", 1)
g.n3 <- sapply(strsplit(as.character(g.m3$name), " "), "[", 1)
g.n4 <- sapply(strsplit(as.character(g.m4$name), " "), "[", 1)

# wordcloud of author names in each module
wordcloud(paste(g.n1, collapse = " "), min.freq = 1)
wordcloud(paste(g.n2, collapse = " "), min.freq = 1)
wordcloud(paste(g.n3, collapse = " "), min.freq = 1)
wordcloud(paste(g.n4, collapse = " "), min.freq = 1)

wordcloud(paste(do.call("c",CITING_authors(data)), collapse = " "), min.freq = 3)


# modular layout of the final network
y = 26
lay <- matrix(ncol =2, nrow = n.nodes[y])
lay[mod.all2[[y]][[1]]$module == 0,] <- cbind(runif(sum(mod.all2[[y]][[1]]$module == 0), -1, -.2), 
                                               runif(sum(mod.all2[[y]][[1]]$module == 0), .5, 1))
lay[mod.all2[[y]][[1]]$module == 1,] <- cbind(runif(sum(mod.all2[[y]][[1]]$module == 1), -1, -.2), 
                                               runif(sum(mod.all2[[y]][[1]]$module == 1), -.5, 0))
lay[mod.all2[[y]][[1]]$module == 2,] <- cbind(runif(sum(mod.all2[[y]][[1]]$module == 2), .2, 1), 
                                               runif(sum(mod.all2[[y]][[1]]$module == 2), -.5, 0))
lay[mod.all2[[y]][[1]]$module == 3,] <- cbind(runif(sum(mod.all2[[y]][[1]]$module == 3), .2, 1),
                                               runif(sum(mod.all2[[y]][[1]]$module == 3), .5, 1))
plot(g.list[[y]], layout = lay, vertex.color = mod.all2[[y]][[1]]$module, vertex.label = NA, vertex.label.cex = .7, vertex.size = 3, edge.arrow.size = .2, edge.width = .01)

# take a closer look at each module 
plotMOD(mod = 1, vcol = "darkgreen")

# animate the growth of the citation network over the last 26 years
ani.options(interval = .25)
saveGIF(
  {
    for(i in 1:26){
      par(mar = c(0,0,0,0))
      adj <- get.adjacency(g.list[[i]])
      l1 <- lay[mod.all2[[26]][[1]]$name %in% colnames(adj),]
        
      plot(g.list[[i]], layout = l1, vertex.color = mod.all2[[26]][[1]]$module[mod.all2[[26]][[1]]$name %in% colnames(adj)],
           vertex.label = NA, vertex.label.cex = .7, vertex.size = 4, edge.arrow.size = .4, edge.width = .01,
           xlim = c(-1, 1), ylim = c(-1, 1), rescale = F)
    }
  },
  htmlfile = "ag89citations.gif", interval = .75, nmax =26, ani.width = 500, ani.height = 500,
  outdir = "C:/Users/jjborrelli/Desktop/"
)



rand.mod <- c()
for(i in 1:200){
  erg <- erdos.renyi.game(n = 725, type = "gnm", p.or.m = 7738, directed = T)
  ergA <- conversion(get.adjacency(erg, sparse = F))
  rand.mod[i] <- netcarto(ergA)[[2]]
  print(i)
}





for(i in 1:26){
  par(mar = c(0,0,0,0))
  adj <- get.adjacency(g.list[[i]])
  l1 <- lay[mod.all2[[26]][[1]]$name %in% colnames(adj),]
  
  plot(g.list[[i]], layout = l1, vertex.color = mod.all2[[26]][[1]]$module[mod.all2[[26]][[1]]$name %in% colnames(adj)],
       vertex.label = NA, vertex.label.cex = .7, vertex.size = 4, edge.arrow.size = .4, edge.width = .01,
       xlim = c(-1, 1), ylim = c(-1, 1), rescale = F)
}

innerhubs <- lapply(lapply(mod.all2, "[[", 1), function(q){q$name[aggregate(q$connectivity, list(q$module), which.max)$x]})

lapply(lapply(mod.all2, "[[", 1), function(q){q$name[aggregate(q$participation, list(q$module), which.max)$x]})
conhubs <- lapply(lapply(mod.all2, "[[", 1), function(q){q$name[grep("Connector", q$role)]})


for(i in 26){
  plot(graph.adjacency(get.adjacency(g)[conhubs[[i]],conhubs[[i]]]), 
       layout= lay[V(g)$name %in% conhubs[[i]],], 
       vertex.label = NA, 
       vertex.size = 4, 
       vertex.frame.color = "green", 
       edge.arrow.size = .2, 
       edge.width = .01)
  
  text(x = -1.5, y = 1, i)
}


conhubs[[26]]
degree(g)[names(degree(g)) %in% conhubs[[26]]]

#########################################################
#########################################################
###### Coyne and Orr 1989 

file.name1 <- "C:/Users/jjborrelli/Desktop/GitHub/citationNetwork/CoyneOrr1989refs.txt"
data1 <- readLines(file.name1)

papers1 <- matrix(c(authors(data1), years(data1)), ncol = 2)
papers1[,1] <- toupper(papers1[,1])

crefs1 <- citingREFs(data1) 

el1.1 <- lapply(1:nrow(papers1), function(x){cbind(paper = rep(paste(papers1[x, 1:2], collapse = " "), nrow(crefs1[[x]])), citing = apply(crefs1[[x]][,1:2], 1, paste, collapse = " "))})
el2.1 <- do.call("rbind", el1.1)

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
qplot(x = allyrs, y = n.mods2.1, geom = "line") + geom_point(size = 5) + xlab("YEAR") + ylab("NUMBER OF MODULES") + theme_bw(base_size = 30)

mods2.1 <- sapply(mod.all2.1, "[[", 2)
qplot(x = allyrs, y = mods2, geom = "line") + geom_point(size = 5) + xlab("YEAR") + ylab("NUMBER OF MODULES") + theme_bw(base_size = 30)

mod1 <- netcarto(conversion(ad))
mod <- netcarto(conversion(get.adjacency(g, sparse = F)))

df.mod <- data.frame(mod = c(mods2, mods2.1), web = c(rep("AG89", 26), rep("CO89", 26)), yr = rep(allyrs, 2))
ggplot(df.mod, aes(x = yr, y = mod, col = web)) + geom_line() + geom_point(size = 5) + xlab("YEAR") + ylab("MODULARITY") + theme_bw(base_size = 24) 


n.nodes1 <- sapply(g.list1, function(x){length(V(x))})
plot(n.nodes~allyrs)
qplot(x = allyrs, y = n.nodes, geom = "line") + geom_point(size = 5) + xlab("YEAR") + ylab("NUMBER OF NODES") + theme_bw(base_size = 30)

n.edges1 <- sapply(g.list1, function(x){length(E(x))})
plot(n.edges1~allyrs)
qplot(x = allyrs, y = n.edges, geom = "line") + geom_point(size = 5) + xlab("YEAR") + ylab("NUMBER OF EDGES") + theme_bw(base_size = 30)

con <- n.edges1/(n.nodes1*(n.nodes1 -1))
qplot(x = allyrs, y = con, geom = "line") + geom_point(size = 5) + xlab("YEAR") + ylab("CONNECTANCE") + theme_bw(base_size = 30)


df.nod <- data.frame(nod = c(n.nodes, n.nodes1), web = c(rep("AG89", 26), rep("CO89", 26)), yr = rep(allyrs, 2))
ggplot(df.nod, aes(x = yr, y = nod, col = web)) + geom_line() + geom_point(size = 5) + xlab("YEAR") + ylab("NUMBER OF NODES") + theme_bw(base_size = 24) 

df.ed <- data.frame(ed = c(n.edges, n.edges1), web = c(rep("AG89", 26), rep("CO89", 26)), yr = rep(allyrs, 2))
ggplot(df.ed, aes(x = yr, y = ed, col = web)) + geom_line() + geom_point(size = 5) + xlab("YEAR") + ylab("NUMBER OF EDGES") + theme_bw(base_size = 24)

df.n_mod <- data.frame(mod = c(n.mods2, n.mods2.1), web = c(rep("AG89", 26), rep("CO89", 26)), yr = rep(allyrs, 2))
ggplot(df.n_mod, aes(x = yr, y = mod, col = web)) + geom_line() + geom_point(size = 5) + xlab("YEAR") + ylab("NUMBER OF NODES") + theme_bw(base_size = 24) 
