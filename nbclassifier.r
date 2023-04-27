# Install required packages (needed to download the data). #
install.packages('mlbench')
library('mlbench')

# Load data. #
data(BreastCancer)

# We need to omit any NA values: #
anyNA(BreastCancer)
BreastCancer.2 <- na.omit(BreastCancer)
dim(BreastCancer.2)

# Find the marginal (prior) probabiltiies of observing each class value. #
prior.probs <- function(class){
  counts <- table(class)
  prob <- counts/sum(counts)
  return(prob)
}
# Example: #
prior.probs(BreastCancer.2$Class)

# Find the probabilities of observing atrribute values a_i conditional on observing class value c: #
conditional.probs.table <- function(class, attribute){
  counts <- table(class, attribute)
  counts / apply(counts, 1, sum)
}
# Example: #
conditional.probs.table(BreastCancer.2$Class, BreastCancer.2$Cl.thickness)

# Find the joint probability of observing class value c and vector of attribute values a (using the naive assumtpion): #
probca <- function(dataframe, attrcols, classcol, testrow, classvalue){
  classprobs <- prior.probs(dataframe[classcol])
  attrprobs <- lapply(attrcols, 
                      function(colid) conditional.probs.table(dataframe[,classcol], dataframe[,colid]))
  condprobsac <- sapply(
    1:length(attrcols),
    function(i){
      colid <- attrcols[i]
      a.level <- testrow[1, colid]
      prob.table.a.given.c <- attrprobs[[i]]
      prob.table.a.given.c[classvalue, a.level]
    }
  )
  prob.c <- classprobs[classvalue]
  prob.c*prod(condprobsac)
}
# Example: #
probca(BreastCancer.2, 2:10, 11, BreastCancer.2[1,], 'benign')
probca(BreastCancer.2, 2:10, 11, BreastCancer.2[1,], 'malignant')

# Now build the classifier: #
nbclassifier <- function(dataframe, attrcols, classcol, testrow){
  classvalues <- levels.default(BreastCancer.2$Class)
  probcas <- lapply(
    classvalues,
    function(classvalue) {
      probca(dataframe, attrcols, classcol, testrow, classvalue)
    }
  )
  classvalues[which.max(probcas)]
}
# Example: #
nbclassifier(BreastCancer.2, 2:10, 11, BreastCancer.2[1,])






