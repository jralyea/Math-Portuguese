## Split the data for model building

por <- sample_n(por, 395, replace = TRUE)

## Create the partition
set.seed(2000) # ensure reproducibility
math.part <- createDataPartition(math$binned, times=1, p=0.8, list=FALSE)
set.seed(2000) # ensure reproducibility
por.part <- createDataPartition(por$binned, times=1, p=0.8, list = FALSE)

## Math training and testing data
math.training <- math[math.part,]
math.test <- math[-math.part,]

## Portuguese training and testing data
por.training <- por[por.part,]
por.test <- por[-por.part,]

por = lapply(por, function(x) as.factor(x))
math = lapply(math, function(x) as.factor(x))

por.training = lapply(por.training, function(x) as.factor(x))
math.training = lapply(math.training, function(x) as.factor(x))

por.test = lapply(por.test, function(x) as.factor(x))
math.test = lapply(math.test, function(x) as.factor(x))

math_forest <- randomForest(binned ~., #all variables
                            data = math.training, mtry = 5,  ntree = 500,importance = TRUE)

por_forest <- randomForest(binned ~., #all variables
                           data = por.training, mtry = 5, ntree = 1000, importance = TRUE)


varImpPlot(math_forest,     #<- the randomForest model to use
           sort = TRUE,        #<- whether to sort variables by decreasing order of importance
           n.var = 10,        #<- number of variables to display
           main = "Important Factors",
           #cex = 2,           #<- size of characters or symbols
           bg = "white",       #<- background color for the plot
           color = "blue",     #<- color to use for the points and labels
           lcolor = "orange")

varImpPlot(por_forest,     #<- the randomForest model to use
           sort = TRUE,        #<- whether to sort variables by decreasing order of importance
           n.var = 10,        #<- number of variables to display
           main = "Important Factors",
           #cex = 2,           #<- size of characters or symbols
           bg = "white",       #<- background color for the plot
           color = "blue",     #<- color to use for the points and labels
           lcolor = "orange")