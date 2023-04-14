# This program follows on from nbclassifier.r #

# Set classified class value to be a function: #
classified_i <- function(i){
  nbclassifier(BreastCancer.2, 2:10, 11, BreastCancer.2[i,])
}

# Set actual class value to be a function: #
actualclass_i <- function(i){
  as.character(BreastCancer.2[i,11])
}
# Examples: #
classified_i(1)
actualclass_i(1)

# Create matrices corresponding to if actual class value is benign or malignant: #
matrix_i <- function(i){
  if(actualclass_i(i)=='malignant'){
    print(matrix(c(1,0,0,0), nrow=2, ncol=2))
  }else {
    print(matrix(c(0,0,0,1), nrow=2, ncol=2))
  }
}
# Print all if desried: #
for(i in 1:dim(BreastCancer.2)[1]){
  print(matrix_i(i))
}

# Create matrices corresponding to if classified class value is the same as the actual class value or not, and sum these together. #
# First need to create a zero matrix for the matrices to be added on to, we call this s: #
s <- matrix(c(0,0,0,0), nrow=2, ncol=2)

# Now create and sum the other matrices: #
for(i in 1:dim(BreastCancer.2)[1]){
  if(classified_i(i)==actualclass_i(i)){
    toprint<- matrix_i(i)
  }else{ if(classified_i(i)=='malignant'){
    toprint <- matrix(c(0,1,0,0), nrow=2, ncol=2)
  }else{
    toprint<- matrix(c(0,0,1,0), nrow=2, ncol=2)
  }
  }
s <- s + toprint
print(s)
}

# Create confusion matrix: #
confusionmatrix <- matrix(c(237,13,2,431), nrow=2, ncol=2)
# Note the columns correspond to the classified class values and the rows correspond to the actual class values. #
