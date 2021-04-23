library(tidyverse)
library(corrplot)
library(rjson)
library(RJSONIO)
library(dplyr)
library(lsa)
library(data.table)

####################################
####################################
#### MAKE ACTIVITY CORR MATRIX ####
####################################
####################################

signal.1 <- read_csv('WALKER_LOG/2-23-21/activity_2-23-21.csv')

activations.1 <- signal.1[1,-1]
activity.1 <- signal.1[-1,-1]
net1.corr.matrix <- cor(activity.1, activity.1)
net1.corr.matrix <- net1.corr.matrix[!is.na(net1.corr.matrix[,2]),!is.na(net1.corr.matrix[2,])]

signal.2 <- read_csv('WALKER_LOG/2-26-21/activity_2-26-21.csv')

activations.2 <- signal.2[1,-1]
activity.2 <- signal.2[-1,-1]
net2.corr.matrix <- cor(activity.2, activity.2)
net2.corr.matrix <- net2.corr.matrix[!is.na(net2.corr.matrix[,2]),!is.na(net2.corr.matrix[2,])]

signal.3 <- read_csv('WALKER_LOG/2-28-21/activity_2-28-21.csv')

activations.3 <- signal.3[1,-1]
activity.3 <- signal.3[-1,-1]
net3.corr.matrix <- cor(activity.3, activity.3)
net3.corr.matrix <- net3.corr.matrix[!is.na(net3.corr.matrix[,2]),!is.na(net3.corr.matrix[2,])]

signal.4 <- read_csv('WALKER_LOG/3-7-21/activity_3-7-21.csv')

activations.4 <- signal.4[1,-1]
activity.4 <- signal.4[-1,-1]
net4.corr.matrix <- cor(activity.4, activity.4)
net4.corr.matrix <- net4.corr.matrix[!is.na(net4.corr.matrix[,2]),!is.na(net4.corr.matrix[2,])]

signal.5 <- read_csv('WALKER_LOG/3-20-21/activity_3-20-21.csv')

activations.5 <- signal.5[1,-1]
activity.5 <- signal.5[-1,-1]
net5.corr.matrix <- cor(activity.5, activity.5)
net5.corr.matrix <- net5.corr.matrix[!is.na(net5.corr.matrix[,2]),!is.na(net5.corr.matrix[2,])]

activity.corr.matrix12 <- cor(activity.1, activity.2)
activity.corr.matrix12 <- activity.corr.matrix12[!is.na(activity.corr.matrix12[,2]),!is.na(activity.corr.matrix12[2,])]

activity.corr.matrix13 <- cor(activity.1, activity.3)
activity.corr.matrix13 <- activity.corr.matrix13[!is.na(activity.corr.matrix13[,2]),!is.na(activity.corr.matrix13[2,])]

activity.corr.matrix14 <- cor(activity.1, activity.4)
activity.corr.matrix14 <- activity.corr.matrix14[!is.na(activity.corr.matrix14[,2]),!is.na(activity.corr.matrix14[2,])]

activity.corr.matrix15 <- cor(activity.1, activity.5)
activity.corr.matrix15 <- activity.corr.matrix15[!is.na(activity.corr.matrix15[,2]),!is.na(activity.corr.matrix15[2,])]

## MAKE ALL PLOTS
## add 'is.corr=FALSE' if not square
corrplot(net1.corr.matrix, method="color")
corrplot(net2.corr.matrix, method="color")
corrplot(net3.corr.matrix, method="color")
corrplot(net4.corr.matrix, method="color")
corrplot(net5.corr.matrix, method="color")

corrplot(activity.corr.matrix12, method="color", is.corr=FALSE, tl.pos = 'n',
         xlab = "Network 2", ylab = "Network 1")
corrplot(activity.corr.matrix13, method="color", is.corr=FALSE)
corrplot(activity.corr.matrix14, method="color", is.corr=FALSE)
corrplot(activity.corr.matrix15, method="color", is.corr=FALSE)


####################################
####################################
#### FIRST NETWORK TABLE ##########
####################################
####################################

### FIRST 14 ARE INPUT NODES (not necessarily connected, always included)
### LAST 4 ARE OUTPUT NODES (signal, but no degree)

## INITIATE TABLE
network.1.nodes <- row.names(activity.corr.matrix12)
network.1.nodes <- as.numeric(network.1.nodes)

## ADD DEGREE & ADJACENCY
info.1 <- read_csv('WALKER_LOG/2-23-21/info_2-23-21.csv')
network.1 <- fromJSON("WALKER_LOG/2-23-21/adjacency_2-23-21.json", nullValue=NA)

# labels.1 <- info.1[2,] ### DO YOU NEED THIS
adjacency.1 <- info.1[3,]
adjacency.fix.1 <- list()
for(i in 1:length(adjacency.1)){
  x <- adjacency.1[i]
  x <- regmatches(x, gregexpr("[[:digit:]]+", x))
  x <- as.numeric(unlist(x))
  x <- list(x)
  adjacency.fix.1 <- append(adjacency.fix.1, x)
}

degree.info.1 <- info.1[1,]
degree.label.1 <- list()
degree.value.1 <- list()
for(i in 0:length(degree.info.1)){
  x <- degree.info.1[i]
  x <- regmatches(x, gregexpr("[[:digit:]]+", x))
  x <- as.numeric(unlist(x))
  degree.label.1[i] <- first(x)
  degree.value.1[i] <- last(x)
}

degree.1 <- data.frame(matrix(ncol = 0, nrow = length(degree.label.1)))
degree.1$label <- degree.label.1
degree.1$degree <- degree.value.1
degree.1[] <- lapply(degree.1, unlist)
degree.1$conns <- adjacency.fix.1

relevant.degree.1 <- degree.1 %>% 
  filter(label %in% network.1.nodes)
relevant.degree.1 <- arrange(relevant.degree.1, label)

degree.fix.1 <- function(){
  filler <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(filler) <- c("label", "degree", "conns")
  for(i in 1:14){
    if(i %in% relevant.degree.1$label){
      x <- which(relevant.degree.1$label == i)
      filler <- rbind(filler, relevant.degree.1[x,])
      }else{
        filler <- rbind(filler, c(i,NA,NA))
      }
  }
  return(filler)
}

# Add input nodes (14)
degree.1.1 <- degree.fix.1()
# Add middle nodes
degree.1.2 <- relevant.degree.1 %>% 
  filter(label > 14)
# Add filler output nodes (4)
degree.1.3 <- data.frame(matrix(ncol = 3, nrow = 4))
colnames(degree.1.3) <- c("label", "degree", "conns")
degree.1.3$label[1] <- length(activity.1) - 3
degree.1.3$label[2] <- length(activity.1) - 2
degree.1.3$label[3] <- length(activity.1) - 1
degree.1.3$label[4] <- length(activity.1)

network.1.matrix <- rbind(degree.1.1, degree.1.2, degree.1.3)

activations.1 <- as.list(activations.1)
activations.1 <- gsub(1, "linear", activations.1)
activations.1 <- gsub(2, "unsigned_step", activations.1)
activations.1 <- gsub(3, "sine", activations.1)
activations.1 <- gsub(4, "gausian", activations.1)
activations.1 <- gsub(5, "hyperbolic_tan", activations.1)
activations.1 <- gsub(6, "sigmoid_unsigned", activations.1)
activations.1 <- gsub(7, "inverse", activations.1)
activations.1 <- gsub(8, "abs", activations.1)
activations.1 <- gsub(9, "relu", activations.1)
activations.1 <- gsub(10, "cosine", activations.1)
activations.mat.1 <- data.frame(matrix(ncol=0, nrow=length(activations.1)))
activations.mat.1$label <- c(1:length(activations.1))
activations.mat.1$activations <- activations.1
activations.mat.1 <- activations.mat.1 %>% 
  filter(label %in% network.1.matrix$label)

network.1.matrix$activation <- activations.mat.1$activations






####################################
####################################
#### SECOND NETWORK TABLE #########
####################################
####################################

### FIRST 14 ARE INPUT NODES (not necessarily connected, always included)
### LAST 4 ARE OUTPUT NODES (signal, but no degree)

## INITIATE TABLE
network.2.nodes <- colnames(activity.corr.matrix12)
network.2.nodes <- as.numeric(network.2.nodes)

## ADD DEGREE & ADJACENCY
info.2 <- read_csv('WALKER_LOG/2-26-21/info_2-26-21.csv')
network.2 <- fromJSON("WALKER_LOG/2-26-21/adjacency_2-26-21.json", nullValue=NA)

# labels.2 <- info.2[2,] ### DO YOU NEED THIS
adjacency.2 <- info.2[3,]
adjacency.fix.2 <- list()
for(i in 1:length(adjacency.2)){
  x <- adjacency.2[i]
  x <- regmatches(x, gregexpr("[[:digit:]]+", x))
  x <- as.numeric(unlist(x))
  x <- list(x)
  adjacency.fix.2 <- append(adjacency.fix.2, x)
}

degree.info.2 <- info.2[1,]
degree.label.2 <- list()
degree.value.2 <- list()
for(i in 0:length(degree.info.2)){
  x <- degree.info.2[i]
  x <- regmatches(x, gregexpr("[[:digit:]]+", x))
  x <- as.numeric(unlist(x))
  degree.label.2[i] <- first(x)
  degree.value.2[i] <- last(x)
}

degree.2 <- data.frame(matrix(ncol = 0, nrow = length(degree.label.2)))
degree.2$label <- degree.label.2
degree.2$degree <- degree.value.2
degree.2[] <- lapply(degree.2, unlist)
degree.2$conns <- adjacency.fix.2

relevant.degree.2 <- degree.2 %>% 
  filter(label %in% network.2.nodes)
relevant.degree.2 <- arrange(relevant.degree.2, label)

degree.fix.2 <- function(){
  filler <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(filler) <- c("label", "degree", "conns")
  for(i in 1:14){
    if(i %in% relevant.degree.2$label){
      x <- which(relevant.degree.2$label == i)
      filler <- rbind(filler, relevant.degree.2[x,])
    }else{
      filler <- rbind(filler, c(i,NA,NA))
    }
  }
  return(filler)
}

# Add input nodes (14)
degree.2.1 <- degree.fix.2()
# Add middle nodes
degree.2.2 <- relevant.degree.2 %>% 
  filter(label > 14)
# Add filler output nodes (4)
degree.2.3 <- data.frame(matrix(ncol = 3, nrow = 4))
colnames(degree.2.3) <- c("label", "degree", "conns")
degree.2.3$label[1] <- length(activity.2) - 3
degree.2.3$label[2] <- length(activity.2) - 2
degree.2.3$label[3] <- length(activity.2) - 1
degree.2.3$label[4] <- length(activity.2)

network.2.matrix <- rbind(degree.2.1, degree.2.2, degree.2.3)

activations.2 <- as.list(activations.2)
activations.2 <- gsub(1, "linear", activations.2)
activations.2 <- gsub(2, "unsigned_step", activations.2)
activations.2 <- gsub(3, "sine", activations.2)
activations.2 <- gsub(4, "gausian", activations.2)
activations.2 <- gsub(5, "hyperbolic_tan", activations.2)
activations.2 <- gsub(6, "sigmoid_unsigned", activations.2)
activations.2 <- gsub(7, "inverse", activations.2)
activations.2 <- gsub(8, "abs", activations.2)
activations.2 <- gsub(9, "relu", activations.2)
activations.2 <- gsub(10, "cosine", activations.2)
activations.mat.2 <- data.frame(matrix(ncol=0, nrow=length(activations.2)))
activations.mat.2$label <- c(1:length(activations.2))
activations.mat.2$activations <- activations.2
activations.mat.2 <- activations.mat.2 %>% 
  filter(label %in% network.2.matrix$label)

network.2.matrix$activation <- activations.mat.2$activations



####################################
####################################
#### THIRD NETWORK TABLE ##########
####################################
####################################

### FIRST 14 ARE INPUT NODES (not necessarily connected, always included)
### LAST 4 ARE OUTPUT NODES (signal, but no degree)

## INITIATE TABLE
network.3.nodes <- colnames(activity.corr.matrix13)
network.3.nodes <- as.numeric(network.3.nodes)

## ADD DEGREE & ADJACENCY
info.3 <- read_csv('WALKER_LOG/2-28-21/info_2-28-21.csv')
network.3 <- fromJSON("WALKER_LOG/2-28-21/adjacency_2-28-21.json", nullValue=NA)

# labels.3 <- info.3[2,] ### DO YOU NEED THIS
adjacency.3 <- info.3[3,]
adjacency.fix.3 <- list()
for(i in 1:length(adjacency.3)){
  x <- adjacency.3[i]
  x <- regmatches(x, gregexpr("[[:digit:]]+", x))
  x <- as.numeric(unlist(x))
  x <- list(x)
  adjacency.fix.3 <- append(adjacency.fix.3, x)
}

degree.info.3 <- info.3[1,]
degree.label.3 <- list()
degree.value.3 <- list()
for(i in 0:length(degree.info.3)){
  x <- degree.info.3[i]
  x <- regmatches(x, gregexpr("[[:digit:]]+", x))
  x <- as.numeric(unlist(x))
  degree.label.3[i] <- first(x)
  degree.value.3[i] <- last(x)
}

degree.3 <- data.frame(matrix(ncol = 0, nrow = length(degree.label.3)))
degree.3$label <- degree.label.3
degree.3$degree <- degree.value.3
degree.3[] <- lapply(degree.3, unlist)
degree.3$conns <- adjacency.fix.3
relevant.degree.3 <- degree.3 %>% 
  filter(label %in% network.3.nodes)
relevant.degree.3 <- arrange(relevant.degree.3, label)

degree.fix.3 <- function(){
  filler <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(filler) <- c("label", "degree", "conns")
  for(i in 1:14){
    if(i %in% relevant.degree.3$label){
      x <- which(relevant.degree.3$label == i)
      filler <- rbind(filler, relevant.degree.3[x,])
    }else{
      filler <- rbind(filler, c(i,NA,NA))
    }
  }
  return(filler)
}

# Add input nodes (14)
degree.3.1 <- degree.fix.3()
# Add middle nodes
degree.3.2 <- relevant.degree.3 %>% 
  filter(label > 14)
# Add filler output nodes (4)
degree.3.3 <- data.frame(matrix(ncol = 3, nrow = 4))
colnames(degree.3.3) <- c("label", "degree", "conns")
degree.3.3$label[1] <- length(activity.3) - 3
degree.3.3$label[2] <- length(activity.3) - 2
degree.3.3$label[3] <- length(activity.3) - 1
degree.3.3$label[4] <- length(activity.3)

network.3.matrix <- rbind(degree.3.1, degree.3.2, degree.3.3)

activations.3 <- as.list(activations.3)
activations.3 <- gsub(1, "linear", activations.3)
activations.3 <- gsub(2, "unsigned_step", activations.3)
activations.3 <- gsub(3, "sine", activations.3)
activations.3 <- gsub(4, "gausian", activations.3)
activations.3 <- gsub(5, "hyperbolic_tan", activations.3)
activations.3 <- gsub(6, "sigmoid_unsigned", activations.3)
activations.3 <- gsub(7, "inverse", activations.3)
activations.3 <- gsub(8, "abs", activations.3)
activations.3 <- gsub(9, "relu", activations.3)
activations.3 <- gsub(10, "cosine", activations.1)
activations.mat.3 <- data.frame(matrix(ncol=0, nrow=length(activations.3)))
activations.mat.3$label <- c(1:length(activations.3))
activations.mat.3$activations <- activations.3
activations.mat.3 <- activations.mat.3 %>% 
  filter(label %in% network.3.matrix$label)

network.3.matrix$activation <- activations.mat.3$activations






####################################
####################################
#### FOURTH NETWORK TABLE #########
####################################
####################################

### FIRST 14 ARE INPUT NODES (not necessarily connected, always included)
### LAST 4 ARE OUTPUT NODES (signal, but no degree)

## INITIATE TABLE
network.4.nodes <- colnames(activity.corr.matrix14)
network.4.nodes <- as.numeric(network.4.nodes)

## ADD DEGREE & ADJACENCY
info.4 <- read_csv('WALKER_LOG/3-7-21/info_3-7-21.csv')
network.4 <- fromJSON("WALKER_LOG/3-7-21/adjacency_3-7-21.json", nullValue=NA)

# labels.4 <- info.4[2,] ### DO YOU NEED THIS
adjacency.4 <- info.4[3,]
adjacency.fix.4 <- list()
for(i in 1:length(adjacency.4)){
  x <- adjacency.4[i]
  x <- regmatches(x, gregexpr("[[:digit:]]+", x))
  x <- as.numeric(unlist(x))
  x <- list(x)
  adjacency.fix.4 <- append(adjacency.fix.4, x)
}

degree.info.4 <- info.4[1,]
degree.label.4 <- list()
degree.value.4 <- list()
for(i in 0:length(degree.info.4)){
  x <- degree.info.4[i]
  x <- regmatches(x, gregexpr("[[:digit:]]+", x))
  x <- as.numeric(unlist(x))
  degree.label.4[i] <- first(x)
  degree.value.4[i] <- last(x)
}

degree.4 <- data.frame(matrix(ncol = 0, nrow = length(degree.label.4)))
degree.4$label <- degree.label.4
degree.4$degree <- degree.value.4
degree.4[] <- lapply(degree.4, unlist)
degree.4$conns <- adjacency.fix.4

relevant.degree.4 <- degree.4 %>% 
  filter(label %in% network.4.nodes)
relevant.degree.4 <- arrange(relevant.degree.4, label)

degree.fix.4 <- function(){
  filler <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(filler) <- c("label", "degree", "conns")
  for(i in 1:14){
    if(i %in% relevant.degree.4$label){
      x <- which(relevant.degree.4$label == i)
      filler <- rbind(filler, relevant.degree.4[x,])
    }else{
      filler <- rbind(filler, c(i,NA,NA))
    }
  }
  return(filler)
}

# Add input nodes (14)
degree.4.1 <- degree.fix.4()
# Add middle nodes
degree.4.2 <- relevant.degree.4 %>% 
  filter(label > 14)
# Add filler output nodes (4)
degree.4.3 <- data.frame(matrix(ncol = 3, nrow = 4))
colnames(degree.4.3) <- c("label", "degree", "conns")
degree.4.3$label[1] <- length(activity.4) - 3
degree.4.3$label[2] <- length(activity.4) - 2
degree.4.3$label[3] <- length(activity.4) - 1
degree.4.3$label[4] <- length(activity.4)

network.4.matrix <- rbind(degree.4.1, degree.4.2, degree.4.3)

activations.4 <- as.list(activations.4)
activations.4 <- gsub(1, "linear", activations.4)
activations.4 <- gsub(2, "unsigned_step", activations.4)
activations.4 <- gsub(3, "sine", activations.4)
activations.4 <- gsub(4, "gausian", activations.4)
activations.4 <- gsub(5, "hyperbolic_tan", activations.4)
activations.4 <- gsub(6, "sigmoid_unsigned", activations.4)
activations.4 <- gsub(7, "inverse", activations.4)
activations.4 <- gsub(8, "abs", activations.4)
activations.4 <- gsub(9, "relu", activations.4)
activations.4 <- gsub(10, "cosine", activations.4)
activations.mat.4 <- data.frame(matrix(ncol=0, nrow=length(activations.4)))
activations.mat.4$label <- c(1:length(activations.4))
activations.mat.4$activations <- activations.4
activations.mat.4 <- activations.mat.4 %>% 
  filter(label %in% network.4.matrix$label)

network.4.matrix$activation <- activations.mat.4$activations








####################################
####################################
#### FIFTH NETWORK TABLE #########
####################################
####################################

### FIRST 14 ARE INPUT NODES (not necessarily connected, always included)
### LAST 4 ARE OUTPUT NODES (signal, but no degree)

## INITIATE TABLE
network.5.nodes <- colnames(activity.corr.matrix15)
network.5.nodes <- as.numeric(network.5.nodes)

## ADD DEGREE & ADJACENCY
info.5 <- read_csv('WALKER_LOG/3-20-21/info_3-20-21.csv')
network.5 <- fromJSON("WALKER_LOG/3-20-21/adjacency_3-20-21.json", nullValue=NA)

# labels.5 <- info.5[2,] ### DO YOU NEED THIS
adjacency.5 <- info.5[3,]
adjacency.fix.5 <- list()
for(i in 1:length(adjacency.5)){
  x <- adjacency.5[i]
  x <- regmatches(x, gregexpr("[[:digit:]]+", x))
  x <- as.numeric(unlist(x))
  x <- list(x)
  adjacency.fix.5 <- append(adjacency.fix.5, x)
}

degree.info.5 <- info.5[1,]
degree.label.5 <- list()
degree.value.5 <- list()
for(i in 0:length(degree.info.5)){
  x <- degree.info.5[i]
  x <- regmatches(x, gregexpr("[[:digit:]]+", x))
  x <- as.numeric(unlist(x))
  degree.label.5[i] <- first(x)
  degree.value.5[i] <- last(x)
}

degree.5 <- data.frame(matrix(ncol = 0, nrow = length(degree.label.5)))
degree.5$label <- degree.label.5
degree.5$degree <- degree.value.5
degree.5[] <- lapply(degree.5, unlist)
degree.5$conns <- adjacency.fix.5

relevant.degree.5 <- degree.5 %>% 
  filter(label %in% network.5.nodes)
relevant.degree.5 <- arrange(relevant.degree.5, label)

degree.fix.5 <- function(){
  filler <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(filler) <- c("label", "degree", "conns")
  for(i in 1:14){
    if(i %in% relevant.degree.5$label){
      x <- which(relevant.degree.5$label == i)
      filler <- rbind(filler, relevant.degree.5[x,])
    }else{
      filler <- rbind(filler, c(i,NA,NA))
    }
  }
  return(filler)
}

# Add input nodes (14)
degree.5.1 <- degree.fix.5()
# Add middle nodes
degree.5.2 <- relevant.degree.5 %>% 
  filter(label > 14)
# Add filler output nodes (4)
degree.5.3 <- data.frame(matrix(ncol = 3, nrow = 4))
colnames(degree.5.3) <- c("label", "degree", "conns")
degree.5.3$label[1] <- length(activity.5) - 3
degree.5.3$label[2] <- length(activity.5) - 2
degree.5.3$label[3] <- length(activity.5) - 1
degree.5.3$label[4] <- length(activity.5)

network.5.matrix <- rbind(degree.5.1, degree.5.2, degree.5.3)

activations.5 <- as.list(activations.5)
activations.5 <- gsub(1, "linear", activations.5)
activations.5 <- gsub(2, "unsigned_step", activations.5)
activations.5 <- gsub(3, "sine", activations.5)
activations.5 <- gsub(4, "gausian", activations.5)
activations.5 <- gsub(5, "hyperbolic_tan", activations.5)
activations.5 <- gsub(6, "sigmoid_unsigned", activations.5)
activations.5 <- gsub(7, "inverse", activations.5)
activations.5 <- gsub(8, "abs", activations.5)
activations.5 <- gsub(9, "relu", activations.5)
activations.5 <- gsub(10, "cosine", activations.5)
activations.mat.5 <- data.frame(matrix(ncol=0, nrow=length(activations.5)))
activations.mat.5$label <- c(1:length(activations.5))
activations.mat.5$activations <- activations.5
activations.mat.5 <- activations.mat.5 %>% 
  filter(label %in% network.5.matrix$label)

network.5.matrix$activation <- activations.mat.5$activations










####################################
####################################
#### COMPARISONS ##################
####################################
####################################

######################
######################
## PRUNING Analysis ## 
######################
######################

## Narrow down to correlations over a certain threshold:
prune <- 0.625

pruned.data12 <- (which(abs(activity.corr.matrix12) > prune, arr.ind=TRUE))
pruned.data12 <- data.frame(pruned.data12)
colnames(pruned.data12) <- c("Node1", "Node2")
pruned.data12$Node1 <- network.1.nodes[pruned.data12[,"Node1"]]
pruned.data12$Node2 <- network.2.nodes[pruned.data12[,"Node2"]]
rep1.12 <- rep("A", times = length(pruned.data12$Node1))
rep2.12 <- rep("B", times = length(pruned.data12$Node2))
pruned.data12$Net1 <- rep1.12
pruned.data12$Net2 <- rep2.12

pruned.data13 <- (which(abs(activity.corr.matrix13) > prune, arr.ind=TRUE))
pruned.data13 <- data.frame(pruned.data13)
colnames(pruned.data13) <- c("Node1", "Node2")
pruned.data13$Node1 <- network.1.nodes[pruned.data13[,"Node1"]]
pruned.data13$Node2 <- network.3.nodes[pruned.data13[,"Node2"]]
rep1.13 <- rep("A", times = length(pruned.data13$Node1))
rep2.13 <- rep("C", times = length(pruned.data13$Node2))
pruned.data13$Net1 <- rep1.13
pruned.data13$Net2 <- rep2.13

pruned.data14 <- (which(abs(activity.corr.matrix14) > prune, arr.ind=TRUE))
pruned.data14 <- data.frame(pruned.data14)
colnames(pruned.data14) <- c("Node1", "Node2")
pruned.data14$Node1 <- network.1.nodes[pruned.data14[,"Node1"]]
pruned.data14$Node2 <- network.4.nodes[pruned.data14[,"Node2"]]
rep1.14 <- rep("A", times = length(pruned.data14$Node1))
rep2.14 <- rep("D", times = length(pruned.data14$Node2))
pruned.data14$Net1 <- rep1.14
pruned.data14$Net2 <- rep2.14

pruned.data15 <- (which(abs(activity.corr.matrix15) > prune, arr.ind=TRUE))
pruned.data15 <- data.frame(pruned.data15)
colnames(pruned.data15) <- c("Node1", "Node2")
pruned.data15$Node1 <- network.1.nodes[pruned.data15[,"Node1"]]
pruned.data15$Node2 <- network.5.nodes[pruned.data15[,"Node2"]]
rep1.15 <- rep("A", times = length(pruned.data15$Node1))
rep2.15 <- rep("E", times = length(pruned.data15$Node2))
pruned.data15$Net1 <- rep1.15
pruned.data15$Net2 <- rep2.15

pruned.final <- rbind(pruned.data12, pruned.data13, pruned.data14, pruned.data15)

## Remove input to input correlation
pruned.final <- pruned.final %>% 
  filter(!(pruned.final$Node1 == pruned.final$Node2 & pruned.final$Node1 < 15))

## Which node is maximally similar and investigate the node across the different networks (original question) 

unique.nodes <- unique(pruned.final$Node1)
fives <- list()

for(i in 1:length(unique.nodes)){
 a <- unique.nodes[i]
 b <- c("B", "C", "D", "E")
 x <- which(pruned.final$Node1 == a)
 y <- pruned.final$Net2[x]
 z <- b %in% y
 if(FALSE %in% z){
   fives <- fives
 }else{
   fives <- append(fives, a)
 }
}

pruned.final <- pruned.final %>% 
  filter(Node1 %in% fives)

pruned.crossover <- pruned.final %>% 
  group_by(Node1) %>% 
  summarize(is.correlated.input = any(Node2 <= 14)) %>% 
  filter(Node1 > 14)
pruned.crossover <- pruned.crossover %>% 
  filter(is.correlated.input == FALSE)

pruned.similar.nodes <- pruned.final %>% 
  filter(Node1 %in% pruned.crossover$Node1)

network.1.final <- network.1.matrix %>% 
  filter(label %in% pruned.similar.nodes$Node1)
rep.network.1.final <- rep("A", times = length(network.1.final$label))
network.1.final$net <- rep.network.1.final

network.2.conn.nodes <- pruned.similar.nodes %>% 
  filter(Net2 == "B")
network.2.conn.nodes <- network.2.conn.nodes$Node2
network.2.final <- network.2.matrix %>% 
  filter(label %in% network.2.conn.nodes)
rep.network.2.final <- rep("B", times = length(network.2.final$label))
network.2.final$net <- rep.network.2.final

network.3.conn.nodes <- pruned.similar.nodes %>% 
  filter(Net2 == "C")
network.3.conn.nodes <- network.3.conn.nodes$Node2
network.3.final <- network.3.matrix %>% 
  filter(label %in% network.3.conn.nodes)
rep.network.3.final <- rep("C", times = length(network.3.final$label))
network.3.final$net <- rep.network.3.final

network.4.conn.nodes <- pruned.similar.nodes %>% 
  filter(Net2 == "D")
network.4.conn.nodes <- network.4.conn.nodes$Node2
network.4.final <- network.4.matrix %>% 
  filter(label %in% network.4.conn.nodes)
rep.network.4.final <- rep("D", times = length(network.4.final$label))
network.4.final$net <- rep.network.4.final

network.5.conn.nodes <- pruned.similar.nodes %>% 
  filter(Net2 == "E")
network.5.conn.nodes <- network.5.conn.nodes$Node2
network.5.final <- network.5.matrix %>% 
  filter(label %in% network.5.conn.nodes)
rep.network.5.final <- rep("E", times = length(network.5.final$label))
network.5.final$net <- rep.network.5.final

nodes.final <- rbind(network.1.final, network.2.final, network.3.final, network.4.final, network.5.final)

double.conn.check <- function(net, label){
  if(net == "A"){
    network.matrix.conns <- network.1.matrix
    network.final.conns <- network.1.final
  }else{
    if(net == "B"){
      network.matrix.conns <- network.2.matrix
      network.final.conns <- network.2.final
    }else{
      if(net == "C"){
        network.matrix.conns <- network.3.matrix
        network.final.conns <- network.3.final
      }else{
        if(net == "D"){
          network.matrix.conns <- network.4.matrix
          network.final.conns <- network.4.final
        }else{
          network.matrix.conns <- network.5.matrix
          network.final.conns <- network.5.final
        }
      }
    }
  }
  conn.list <- list()
  a <- which(network.final.conns$label == label)
  b <- unlist(network.final.conns$conns[a])
  for(i in 1:length(b)){
    w <- unlist(b[i])
    x <- which(network.matrix.conns$label == w)
    y <- unlist(network.matrix.conns$conns[x])
    if(any(c(1:14) %in% y)){
      conn.list <- append(conn.list, TRUE)
    }else{
      conn.list <- append(conn.list, FALSE)
    }
  }
  return(unlist(conn.list))
}

second.degree.check <- list()
for(i in 1:length(nodes.final$label)){
  x <- list(double.conn.check(nodes.final$net[i], nodes.final$label[i]))
  second.degree.check <- append(second.degree.check, x)
}
nodes.final$second.degree <- second.degree.check






####### DATA IS SAVED UP TO HERE ######



######################
######################
## OUTPUTS Analysis ##
######################
######################

## Cosine Similarity
net.1.output <- signal.1[-1,244:247]
colnames(net.1.output) <- c("Net1", "Net1", "Net1", "Net1")
net.1.output <- rbind(net.1.output[,1], net.1.output[,2], net.1.output[,3], net.1.output[,4])

net.2.output <- signal.2[-1,118:121]
colnames(net.2.output) <- c("Net2", "Net2", "Net2", "Net2")
net.2.output <- rbind(net.2.output[,1], net.2.output[,2], net.2.output[,3], net.2.output[,4])

net.3.output <- signal.3[-1,209:212]
colnames(net.3.output) <- c("Net3", "Net3", "Net3", "Net3")
net.3.output <- rbind(net.3.output[,1], net.3.output[,2], net.3.output[,3], net.3.output[,4])

net.4.output <- signal.4[-1,233:236]
colnames(net.4.output) <- c("Net4", "Net4", "Net4", "Net4")
net.4.output <- rbind(net.4.output[,1], net.4.output[,2], net.4.output[,3], net.4.output[,4])

net.5.output <- signal.5[-1,262:265]
colnames(net.5.output) <- c("Net5", "Net5", "Net5", "Net5")
net.5.output <- rbind(net.5.output[,1], net.5.output[,2], net.5.output[,3], net.5.output[,4])

cosine.data <- as.matrix(cbind(net.1.output, net.2.output, net.3.output, net.4.output, net.5.output))

cosine.result <- cosine(cosine.data) 

### MAKE PLOTS OF OUTPUT NODES TOO!!

## Look at similar behavior and see if those 2 networks in particular are more correlated in some way
## cosine similarity? i don't just have one signal for a bunch of inputs, you have a 14x4 mapping problem
## 2 vectors, want to figure out if they 'point in the same direction' --> hold inputs constant and then give net1 to those inputs and net2 to those inputs, 
## then whats the cosine similarity to those networks? 
## Test them on a bunch of input patterns, then look at average cosine similarity (USE THE SIGNAL OUTPUT DATA)
## First example of a follow-up question *after the first attempt* --> lets look at similarity of how they walk

## Negative cosine similarity implies they can predict each other, but they're functionally more different 

## Look at networks 1 & 5 and look for correlations (similar analysis to above but just with the 2)

input.pattern.1 <- data.frame(1:1000)
names(input.pattern.1)[1] <- "index"
input.1.csv <- read_csv('WALKER_LOG/2-23-21/walk_2-23-21.csv')
input.pattern.1$node.activation <- unlist(input.1.csv[,156])
plot(input.pattern.1)

windowing.cutoff.1 <- max(input.pattern.1$node.activation)

for(i in 1:length(input.pattern.1$index)){
  if(input.pattern.1$node.activation[i] < 0.6){
    input.pattern.1$node.activation[i] = input.pattern.1$node.activation[i] + windowing.cutoff
  }else{
    input.pattern.1$node.activation[i] = input.pattern.1$node.activation[i] 
  }
}
plot(input.pattern.1)

## Plot Apparent Sine Wave
x <- input.pattern.1$index
y <- input.pattern.1$node.activation

ssp <- spectrum(y)  
per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
reslm <- lm(y ~ sin(2*pi/per*x)+cos(2*pi/per*x))
summary(reslm)

rg <- diff(range(y))
plot(y~x,ylim=c(min(y)-0.1*rg,max(y)+0.1*rg))
reslm2 <- lm(y ~ sin(2*pi/per*x)+cos(2*pi/per*x)+sin(4*pi/per*x)+cos(4*pi/per*x))
summary(reslm2)
lines(fitted(reslm2)~x,col=3)    # solid green line is periodic with second harmonic

## 

input.pattern.5 <- data.frame(1:1000)
names(input.pattern.5)[1] <- "index"
input.5.csv <- read_csv('WALKER_LOG/3-20-21/walk_3-20-21.csv')
input.pattern.5$node.activation <- unlist(input.5.csv[,169])
plot(input.pattern.5)

windowing.cutoff.5 <- max(input.pattern.5$node.activation)

for(i in 1:length(input.pattern.5$index)){
  if(input.pattern.5$node.activation[i] < 0.6){
    input.pattern.5$node.activation[i] = input.pattern.5$node.activation[i] + windowing.cutoff
  }else{
    input.pattern.5$node.activation[i] = input.pattern.5$node.activation[i] 
  }
}
plot(input.pattern.5)

## Plot Apparent Sine Wave
a <- input.pattern.5$index
b <- input.pattern.5$node.activation

ssp <- spectrum(b)  
per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
reslm <- lm(y ~ sin(2*pi/per*a)+cos(2*pi/per*a))
summary(reslm)

rg <- diff(range(b))
plot(b~a,ylim=c(min(b)-0.1*rg,max(b)+0.1*rg))
reslm2 <- lm(b ~ sin(2*pi/per*a)+cos(2*pi/per*a)+sin(4*pi/per*a)+cos(4*pi/per*a))
summary(reslm2)
lines(fitted(reslm2)~a,col=3)    # solid green line is periodic with second harmonic

















############################
############################
#### UNUSED AFTER HERE #####
############################
############################


input.pattern.2 <- read_csv('WALKER_LOG/2-26-21/walk_2-26-21.csv')
input.pattern.2 <- input.pattern.2[,c(38, 101, 102, 103, 105)]
input.2.check <- as.matrix(cbind(input.pattern.1, input.pattern.2))
input.2.check.result <- cosine(input.2.check)
## NODE 100 MOST CORRELATED
input.pattern.2 <- input.pattern.2$`100`

input.pattern.3 <- read_csv('WALKER_LOG/2-28-21/walk_2-28-21.csv')
input.pattern.3 <- input.pattern.3[,42]

input.pattern.4 <- read_csv('WALKER_LOG/3-7-21/walk_3-7-21.csv')
input.pattern.4 <- input.pattern.4[,c(48, 128, 142, 154, 161, 162, 167, 172, 178, 184, 212)]
input.4.check <- as.matrix(cbind(input.pattern.1, input.pattern.4))
input.4.check.result <- cosine(input.4.check)
## NODES 141 & 183 MOST CORRELATED
input.pattern.4 <- input.pattern.4[,c(142, 184)]




## ARGUMENT STRUCTURE
## 1st strategy was this, i had to drop the pruning value this low to get one, and then what does that node 
## tell me?
## Second strategy was cosine, which revealed these two networks based on output similarity (behavior?)
##

## HOW TO CONNECT BACK TO ENV
## Ideal would be to connect signaling pattern to behavioral traces (e.g. last summer)

## Best case would record the input pattern as its actually walking and then see if you can identify the 
## observable function of that unit --> run the video code and save the network activity as you go (or
## the input)

## You could look at this nodes relationships to particular patterns in the input, you can see what they're 
## connected 

## Cast nets and figure out stuff... 




## A good achievement would be to reflect on how you COULD do this 
## (what does this teach us about the relevant questions; what strategy might someone take going forward)






















##### EXTRA SHIT ######

#####################
#####################
## INPUTS Analysis ##
#####################
#####################

input.names <- c("Hull Angle","Hull Angle Velocity","X Velocity","Y Velocity","Hip 1 Angle","Hip 1 Speed",
                 "Knee 1 Angle","Knee 1 Speed","Leg 1 Contact","Hip 2 Angle","Hip 2 Speed",
                 "Knee 2 Angle","Knee 2 Speed","Leg 2 Contact")

inputs.network.1 <- network.1.matrix %>% 
  filter(network.1.matrix$label < 15)
inputs.network.1$label <- input.names

inputs.network.2 <- network.2.matrix %>% 
  filter(network.2.matrix$label < 15)
inputs.network.2$label <- input.names

inputs.network.3 <- network.3.matrix %>% 
  filter(network.3.matrix$label < 15)
inputs.network.3$label <- input.names

inputs.network.4 <- network.4.matrix %>% 
  filter(network.4.matrix$label < 15)
inputs.network.4$label <- input.names

inputs.network.5 <- network.5.matrix %>% 
  filter(network.5.matrix$label < 15)
inputs.network.5$label <- input.names


# corrplot(activity.corr.matrix23, method="color", is.corr=FALSE)
# corrplot(activity.corr.matrix24, method="color", is.corr=FALSE)
# corrplot(activity.corr.matrix25, method="color", is.corr=FALSE)
# corrplot(activity.corr.matrix34, method="color", is.corr=FALSE)
# corrplot(activity.corr.matrix35, method="color", is.corr=FALSE)
# corrplot(activity.corr.matrix45, method="color", is.corr=FALSE)

# activity.corr.matrix23 <- cor(activity.2, activity.3)
# activity.corr.matrix23 <- activity.corr.matrix23[!is.na(activity.corr.matrix23[,2]),!is.na(activity.corr.matrix23[2,])]
# 
# activity.corr.matrix24 <- cor(activity.2, activity.4)
# activity.corr.matrix24 <- activity.corr.matrix24[!is.na(activity.corr.matrix24[,2]),!is.na(activity.corr.matrix24[2,])]
# 
# activity.corr.matrix25 <- cor(activity.2, activity.5)
# activity.corr.matrix25 <- activity.corr.matrix25[!is.na(activity.corr.matrix25[,2]),!is.na(activity.corr.matrix25[2,])]
# 
# activity.corr.matrix34 <- cor(activity.3, activity.4)
# activity.corr.matrix34 <- activity.corr.matrix34[!is.na(activity.corr.matrix34[,2]),!is.na(activity.corr.matrix34[2,])]
# 
# activity.corr.matrix35 <- cor(activity.3, activity.5)
# activity.corr.matrix35 <- activity.corr.matrix35[!is.na(activity.corr.matrix35[,2]),!is.na(activity.corr.matrix35[2,])]
# 
# activity.corr.matrix45 <- cor(activity.4, activity.5)
# activity.corr.matrix45 <- activity.corr.matrix45[!is.na(activity.corr.matrix45[,2]),!is.na(activity.corr.matrix45[2,])]



# pruned.data23 <- (which(abs(activity.corr.matrix23) > prune, arr.ind=TRUE))
# pruned.data23 <- data.frame(pruned.data23)
# colnames(pruned.data23) <- c("Node1", "Node2")
# pruned.data23$Node1 <- network.2.nodes[pruned.data23[,"Node1"]]
# pruned.data23$Node2 <- network.3.nodes[pruned.data23[,"Node2"]]
# rep1.23 <- rep("B", times = length(pruned.data23$Node1))
# rep2.23 <- rep("C", times = length(pruned.data23$Node2))
# pruned.data23$Net1 <- rep1.23
# pruned.data23$Net2 <- rep2.23
# 
# pruned.data24 <- (which(abs(activity.corr.matrix24) > prune, arr.ind=TRUE))
# pruned.data24 <- data.frame(pruned.data24)
# colnames(pruned.data24) <- c("Node1", "Node2")
# pruned.data24$Node1 <- network.2.nodes[pruned.data24[,"Node1"]]
# pruned.data24$Node2 <- network.4.nodes[pruned.data24[,"Node2"]]
# rep1.24 <- rep("B", times = length(pruned.data24$Node1))
# rep2.24 <- rep("D", times = length(pruned.data24$Node2))
# pruned.data24$Net1 <- rep1.24
# pruned.data24$Net2 <- rep2.24
# 
# pruned.data25 <- (which(abs(activity.corr.matrix25) > prune, arr.ind=TRUE))
# pruned.data25 <- data.frame(pruned.data25)
# colnames(pruned.data25) <- c("Node1", "Node2")
# pruned.data25$Node1 <- network.2.nodes[pruned.data25[,"Node1"]]
# pruned.data25$Node2 <- network.5.nodes[pruned.data25[,"Node2"]]
# rep1.25 <- rep("B", times = length(pruned.data25$Node1))
# rep2.25 <- rep("E", times = length(pruned.data25$Node2))
# pruned.data25$Net1 <- rep1.25
# pruned.data25$Net2 <- rep2.25
# 
# pruned.data34 <- (which(abs(activity.corr.matrix34) > prune, arr.ind=TRUE))
# pruned.data34 <- data.frame(pruned.data34)
# colnames(pruned.data34) <- c("Node1", "Node2")
# pruned.data34$Node1 <- network.3.nodes[pruned.data34[,"Node1"]]
# pruned.data34$Node2 <- network.4.nodes[pruned.data34[,"Node2"]]
# rep1.34 <- rep("C", times = length(pruned.data34$Node1))
# rep2.34 <- rep("D", times = length(pruned.data34$Node2))
# pruned.data34$Net1 <- rep1.34
# pruned.data34$Net2 <- rep2.34
# 
# pruned.data35 <- (which(abs(activity.corr.matrix35) > prune, arr.ind=TRUE))
# pruned.data35 <- data.frame(pruned.data35)
# colnames(pruned.data35) <- c("Node1", "Node2")
# pruned.data35$Node1 <- network.3.nodes[pruned.data35[,"Node1"]]
# pruned.data35$Node2 <- network.5.nodes[pruned.data35[,"Node2"]]
# rep1.35 <- rep("C", times = length(pruned.data35$Node1))
# rep2.35 <- rep("E", times = length(pruned.data35$Node2))
# pruned.data35$Net1 <- rep1.35
# pruned.data35$Net2 <- rep2.35
# 
# pruned.data45 <- (which(abs(activity.corr.matrix45) > prune, arr.ind=TRUE))
# pruned.data45 <- data.frame(pruned.data45)
# colnames(pruned.data45) <- c("Node1", "Node2")
# pruned.data45$Node1 <- network.4.nodes[pruned.data45[,"Node1"]]
# pruned.data45$Node2 <- network.5.nodes[pruned.data45[,"Node2"]]
# rep1.45 <- rep("D", times = length(pruned.data45$Node1))
# rep2.45 <- rep("E", times = length(pruned.data45$Node2))
# pruned.data45$Net1 <- rep1.45
# pruned.data45$Net2 <- rep2.45





# fives <- list()
# 
# for(i in 1:length(pruned.final$Node1)){
#   if(pruned.final$Net1 == "A"){
#     a <- pruned.final$Node1[i]
#     b <- c("B", "C", "D", "E")
#     x <- which(pruned.final$Node1 == a)
#     y <- pruned.final$Net2[x]
#     z <- b %in% y
#     if(FALSE %in% z){
#       fives <- fives
#     }else{
#       fives <- append(fives, a)
#     }
#   }else{
#     if(pruned.final$Net1 == "B"){
#       a <- pruned.final$Node1[i]
#       b <- c("A", "C", "D", "E")
#       x <- which(pruned.final$Node1 == a)
#       y <- pruned.final$Net2[x]
#       z <- b %in% y
#       if(FALSE %in% z){
#         fives <- fives
#       }else{
#         fives <- append(fives, a)
#       }
#     }else{
#       if(pruned.final$Net1 == "C"){
#         a <- pruned.final$Node1[i]
#         b <- c("A", "B", "D", "E")
#         x <- which(pruned.final$Node1 == a)
#         y <- pruned.final$Net2[x]
#         z <- b %in% y
#         if(FALSE %in% z){
#           fives <- fives
#         }else{
#           fives <- append(fives, a)
#         }
#       }else{
#         if(pruned.final$Net1 == "D"){
#           a <- pruned.final$Node1[i]
#           b <- c("A", "B", "C", "E")
#           x <- which(pruned.final$Node1 == a)
#           y <- pruned.final$Net2[x]
#           z <- b %in% y
#           if(FALSE %in% z){
#             fives <- fives
#           }else{
#             fives <- append(fives, a)
#           }
#         }else{
#           pruned.final$Net1 == "E"
#           a <- pruned.final$Node1[i]
#           b <- c("A", "B", "C", "D")
#           x <- which(pruned.final$Node1 == a)
#           y <- pruned.final$Net2[x]
#           z <- b %in% y
#           if(FALSE %in% z){
#             fives <- fives
#           }else{
#             fives <- append(fives, a)
#           }
#         }
#       }
#     }
#   }
# }











####################################
####################################
####################################
####################################
####################################
####################################
####################################
####################################
####################################
####################################






#########################################################################

## # of nodes

## Strong correlations?

## can always use more signals

## find nodes that seem to be most strongly conserved across the population (what signals persist across agents)
## once you have, look for where those nodes are (degree, adjacency, activation function), what does the pathway look like?
## Then you could analyze actual patterns the walkers experience in relation to things in the environment and see what the signal
## looks like in those cases (what are they doing) 

## not super interesting if there are perfectly correlated nodes connected to same inputs (frequency of ones will also
## lower with tuned networks)

## looking for correlated nodes without the same direct connections


#########################################################################



