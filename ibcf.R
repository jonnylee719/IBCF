# Item based collaborative filtering
# Adjusted cosine is used to compute similarity
# Steps:
# 1. Build premodel on item similarity before runtime
# 2. Compute predictions on particular items at runtime

# Functions to be used

adjCosine <- function(v1, v2){
  # dot product of item v1 and item v2
  top <- sum(unlist(v1) * unlist(v2))
  v1SqR <- sqrt(sum(unlist(v1)^2))
  v2SqR <- sqrt(sum(unlist(v2)^2))
  return(top/(v1SqR * v2SqR))
}

# function that computes the item similarity of a particular item 
# with all available items. 
combmat <- function(l, mat){
  # returns a list where each element is the item similarity between
  # item l and item[c(i)] of the matrix
  return(lapply(mat, adjCosine, v1 = l))
}

# Get the raw data
rateRaw <- read.csv("D:/Dropbox/school/201617 semester 2_PUC/coursera_recommender_system/assignments/raw_data.csv")
# Discard extra user mean column and the squared row
rateRaw <- rateRaw[-c(21), -c(22)]

# Compute user rating means
# Discard the first column which is the User ids
userAR <- apply(rateRaw[-c(1)], 1, mean, na.rm = T)
names(userAR) <- rateRaw[,c(1)]

# Create a rating dataframe that is normalized by each user's 
# mean rating
rateNorm <- data.frame()

for(i in 1:nrow(rateRaw)){
  rowNorm <- rateRaw[-c(1)][i, , drop = F] - userAR[[i]]
  rateNorm <- rbind(rateNorm, rowNorm)
}

# Replace all NA values with 0
rateNormZ <- apply(rateNorm[c(2:21)], 2, toZero)

# Create a rating matrix to hold the item similarity scores
rateMatrixN <- data.frame()

for(i in 2:21){
  # compute and row bind the item similarity list for each item
  # in the matrix
  r <- combmat(rateNormZ[c(i)], rateNormZ[c(2:21)])
  rateMatrixN <- rbind(rateMatrixN, r)
}

# Add item names as row names
rownames(rateMatrixN) <- colnames(rateMatrixN)

# Filter normalized rating matrix so that negative similarity scores
# are converted to 0s
rateMatrixNF <- rateMatrixN
rateMatrixNF[rateMatrixNF < 0] <- 0
View(rateMatrixNF)

# write as csv file to folder
# write.csv(rateMatrixNF, file = "D:/Dropbox/school/201617 semester 2_PUC/coursera_recommender_system/assignments/ratingMatrixNF.csv", row.names = T)

# a particular item's similarity row
r1 <- rateMatrixNF[c(1), ,drop = F]
View(r1)
r1 <- r1[order(r1, decreasing = T)]
View(r1)
r2 <- r1[order(r1, decreasing = T) & colnames(r1) != rownames(r1)]
View(r2)

# Build the item model 
imodel <- list()
for(i in 1:nrow(rateMatrixNF)){
  r <- rateMatrixNF[c(i), , drop = F]
  r <- r[order(r, decreasing = T)]
  r[colnames(r) == rownames(r)] <- NULL
  imodel[[length(imodel) + 1]] <- r
}
View(imodel)

# Sample prediction for User 5277 on the movie ShawShank redemption 
# Known user rating on the movie is: 2

getitem <- function(name){
  # Function to search through item model to get its similar items
  for(i in 1:length(imodel)){
    item <- imodel[[i]]
    if(rownames(item) == name){
      return(item)
    }
  }
  return(NULL)
}

pred <- function(neighbour, simscores, userrates){
  # cat("Neighbour item name: ", neighbour)
  simscore <- simscores[, neighbour]
  itemrate <- userrate[, neighbour]
  return(simscore * itemrate)
}


ibcf <- function(u, i, k){
  # predicting function that takes in
  # - a particular userid
  # - the item to be predicted
  # - an integer k to find top-k neighbours
  # Get user's rating row from raw rating
  # User ID should be a string
  userrate <- rateRaw[rateRaw[c(1)] == u, , drop = F]
  # Filter out the unrated items
  userrate <- userrate[, !is.na(userrate), drop = F]
  item <- getitem(i)
  # Find similar items that user has rated 
  rated <- intersect(colnames(userrate), colnames(item))
  # Order the items and return top-k or the length
  item <- item[, rated, drop = F]
  item <- item[order(item, decreasing = T)]
  # truncate item list to top-k if length of item list is longer than k
  if(k < length(rated)){
    item <- item[,c(1:k), drop = F]
  }
  # Prediction computation
  neighbours <- colnames(item)
  top <- sum(unlist(lapply(neighbours, pred, simscores = item, userrates = userrate)))
  ratePredict <- top / sum(unlist(item))
  return(ratePredict)
}

