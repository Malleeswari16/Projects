#including the libraries..
library(MASS)
library(rpart)
suppressWarnings(library(rattle))
suppressWarnings(library(rpart.plot))
suppressWarnings(library(RColorBrewer))

#View(iris)

#read data file
mydata=read.csv("C:\\Users\\Malleeswari\\Downloads\\Iris.csv")
View(mydata)
# Check attributes of data
str(mydata)

# Check number of rows and columns
dim(mydata)

# Make dependent variable as a factor (categorical)
mydata$Species = as.factor(mydata$Species)

# Split data into training (70%) and validation (30%)
dt = sort(sample(nrow(mydata), nrow(mydata)*.7))
train<-mydata[dt,]
val<-mydata[-dt,] # Check number of rows in training data set
nrow(train)

# To view dataset
edit(train)
edit(val)

# Decision Tree Model

mtree <- rpart(Species~., data = train, method="class", control = rpart.control(minsplit = 20, minbucket = 7, maxdepth = 10, usesurrogate = 2, xval =10 ))

mtree

#Plot tree
plot(mtree)
text(mtree)

#view1
prp(mtree, faclen = 0, cex = 0.8, extra = 1)

#view2 - total count at each node
tot_count <- function(x, labs, digits, varlen)
{paste(labs, "\n\nn =", x$frame$n)}

prp(mtree, faclen = 0, cex = 0.8, node.fun=tot_count)

#view3- fancy Plot
rattle()
fancyRpartPlot(mtree)

#pruning
printcp(mtree)
bestcp <- mtree$cptable[which.min(mtree$cptable[,"xerror"]),"CP"]

# Prune the tree using the best cp.
pruned <- prune(mtree, cp = bestcp)

# Plot pruned tree
prp(pruned, faclen = 0, cex = 0.8, extra = 1)

# confusion matrix (training data)
conf.matrix <- table(train$Species, predict(pruned,type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)
