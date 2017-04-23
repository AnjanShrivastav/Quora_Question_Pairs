#####################@#### QUORA QUESTION PAIRS #################################

########################### PROBLEM STATEMENT ####################################
# Predict which of the provided pairs of questions contain two questions with the same meaning.

# Clearing R environment
rm(list = ls())

# Setting working directory
path <- "D:/Data Science/Edwisor/Project/Quora Question Pairs" # Change WD as per system use
setwd(path)

# install and load the packages
x <- c("ROSE", "ggplot2", "data.table", "tm", "syuzhet", "caret", "RecordLinkage", "stringdist", "wordcloud", "SnowballC")
install.packages(x) #Installs all the packages
sapply(x, require, character.only = TRUE)

# Reading data in R
train <- fread("train.csv", nrows = 200000) #Total 404290 obs. of  6 variables
test <- fread("test.csv", nrows = 200000) #Total 2345796 obs. of  3 variables


######################### DATA/TEXT PRE-PROCESSING #############################

######################### Data Preprocessing 1 ################################
match.calc <- function(input){
  for(i in 1:nrow(input)){
    
    ########## Creating Corpus for Question 1 ###########
    Corp.Q1 <- Corpus(VectorSource(input$question1[i])) # Creating corpus of each row in Question 1
    #inspect(Corp.Q1)
    
    Corp.Q1 <- tm_map(Corp.Q1, tolower)
    #inspect(Corp.Q1)
    
    Corp.Q1 <- tm_map(Corp.Q1, removeNumbers)
    #inspect(Corp.Q1)
    
    Corp.Q1 <- tm_map(Corp.Q1, removeWords, stopwords("english"))
    #inspect(Corp.Q1)
    
    Corp.Q1 <- tm_map(Corp.Q1, removePunctuation)
    #inspect(Corp.Q1)
    
    Corp.Q1 <- tm_map(Corp.Q1, stemDocument)
    #inspect(Corp.Q1)
    
    Corp.Q1 <- tm_map(Corp.Q1, stripWhitespace)
    #inspect(Corp.Q1)
    
    # Creating a Document Term matrix
    dtm <- DocumentTermMatrix(Corp.Q1)
    #print(dtm)
    #inspect(dtm)
    dtm <- removeSparseTerms(dtm, 0.99)
    a <- dtm$dimnames$Terms
    #print(a)
    
    ########## Creating Corpus for Question 2 ###########
    Corp.Q2 <- Corpus(VectorSource(input$question2[i])) # Creating corpus of each row in Question 2
    #inspect(Corp.Q2)
    
    Corp.Q2 <- tm_map(Corp.Q2, tolower)
    #inspect(Corp.Q2)
    
    Corp.Q2 <- tm_map(Corp.Q2, removeNumbers)
    #inspect(Corp.Q2)
    
    Corp.Q2 <- tm_map(Corp.Q2, removeWords, stopwords("english"))
    #inspect(Corp.Q2)
    
    Corp.Q2 <- tm_map(Corp.Q2, removePunctuation)
    #inspect(Corp.Q2)
    
    Corp.Q2 <- tm_map(Corp.Q2, stemDocument)
    #inspect(Corp.Q2)
    
    Corp.Q2 <- tm_map(Corp.Q2, stripWhitespace)
    #inspect(Corp.Q2)
    
    # Creating a Document Term matrix
    dtm <- DocumentTermMatrix(Corp.Q2)
    #print(dtm)
    #inspect(dtm)
    dtm <- removeSparseTerms(dtm, 0.99)
    b <- dtm$dimnames$Terms
    #print(b)
    
    ################ FEATURE ENGINEERING ###########################
    # Calculating distance between the strings using different methods
    
    dist.lvstn <-  stringsim(input$question1[i], input$question2[i], method="lv")
    #print(dist.lvstn)
    
    dist.jcrd <-  stringsim(input$question1[i], input$question2[i], method="jaccard")
    #print(dist.jcrd)
    
    dist.sndx <-  stringsim(input$question1[i], input$question2[i], method="soundex")
    #print(dist.sndx)
    
    #dist.osa <-  stringsim(input$question1[i], input$question2[i], method="osa")
    #print(dist.osa)
    
    #dist.cosn <-  stringsim(input$question1[i], input$question2[i], method="cosine")
    #print(dist.cosn)
    
    #dist.jw <-  stringsim(input$question1[i], input$question2[i], method="jw")
    #print(dist.jw)
    
    
    # Calculating score based on similar words
    count.same <- sum(a %in% b)
    count.total <-sum(length(a),length(b))
    score.match <- (2 * count.same)/count.total
    score.match2 <- ifelse(score.match < 0.25, 1 , ifelse(score.match >= 0.25 & score.match < 0.5, 2 ,ifelse(score.match >= 0.5 & score.match < 0.75, 3 , ifelse(score.match >= 0.75, 4, NA))))
    
    
    ################ Calculating sentiments ####################
    S.Q1 <- get_nrc_sentiment(input$question1[i])
    S.Q2 <- get_nrc_sentiment(input$question2[i])
    
    #print(S.Q1)
    #print(S.Q2)
    
    # Calculating total postive sentiments
    Pos1 <- sum(S.Q1$positive)
    Pos2 <- sum(S.Q2$positive)
    
    # Calculating total negative sentiments
    Neg1 <- sum(S.Q1$negative)
    Neg2 <- sum(S.Q2$negative)
    
    count.Pos <- 0
    count.Pos <- ifelse(Pos1 == Pos2, Pos1, 0)
    count.Neg <- 0
    count.Neg <- ifelse(Neg1 == Neg2, Neg1, 0)
    
    #count.both <- ifelse(Pos1 == Pos2 && Neg1 == Neg2, 2, 0)
    #count.both <- ifelse(Pos1 == 0 & Pos2 == 0 & Neg1 == 0 & Neg2 == 0, 0, count.both)
    
    # Assigning variables to an object df.temp
    df.temp <<- cbind(score.match, count.same, Pos1, Pos2, Neg1, Neg2, dist.jcrd, dist.lvstn, dist.sndx)
    
    df <<- rbind(df, df.temp)
    
  }
}

######################## Data Preprocessing 2 using TFIDF ######################
weight.calc <- function(input){
  
  ########## Creating Corpus ###########
  Corp.Q1 <- Corpus(VectorSource(input$question1)) # Creating corpus of each row in Question 1
  inspect(Corp.Q1[1])
  
  Corp.Q1 <- tm_map(Corp.Q1, tolower)
  inspect(Corp.Q1[1])
  
  Corp.Q1 <- tm_map(Corp.Q1, removeNumbers)
  inspect(Corp.Q1[1])
  
  Corp.Q1 <- tm_map(Corp.Q1, removeWords, stopwords("english"))
  inspect(Corp.Q1[1])
  
  Corp.Q1 <- tm_map(Corp.Q1, removePunctuation)
  inspect(Corp.Q1[1])
  
  Corp.Q1 <- tm_map(Corp.Q1, stemDocument)
  inspect(Corp.Q1[1])
  
  Corp.Q1 <<- tm_map(Corp.Q1, stripWhitespace)
  inspect(Corp.Q1[1])
  
  # Creating a Document Term matrix
  dtm1 <- DocumentTermMatrix(Corp.Q1, control = list(weighting = weightTf))
  #print(dtm1)
  #inspect(dtm1)
  dtm1 <<- removeSparseTerms(dtm1, 0.99)
  
  ########## Creating Corpus for Question 2 ###########
  Corp.Q2 <- Corpus(VectorSource(input$question2)) # Creating corpus of each row in Question 2
  #inspect(Corp.Q2)
  
  Corp.Q2 <- tm_map(Corp.Q2, tolower)
  #inspect(Corp.Q2)
  
  Corp.Q2 <- tm_map(Corp.Q2, removeNumbers)
  #inspect(Corp.Q2)
  
  Corp.Q2 <- tm_map(Corp.Q2, removeWords, stopwords("english"))
  #inspect(Corp.Q2)
  
  Corp.Q2 <- tm_map(Corp.Q2, removePunctuation)
  #inspect(Corp.Q2)
  
  Corp.Q2 <- tm_map(Corp.Q2, stemDocument)
  #inspect(Corp.Q2)
  
  Corp.Q2 <<- tm_map(Corp.Q2, stripWhitespace)
  #inspect(Corp.Q2)
  
  # Creating a Document Term matrix
  dtm2 <- DocumentTermMatrix(Corp.Q2, control = list(weighting = weightTf))
  #print(dtm2)
  #inspect(dtm2)
  dtm2 <<- removeSparseTerms(dtm2, 0.99)
  
}


# Creating temp dataframes for assigning new variables inside function
df.temp <- data.frame()
df <- data.frame()

################ Preparing train and test data ####################
############################ TRAIN ################################

#Creating sample data for train
set.seed(12345)
data.train <- train[sample(nrow(train), 5000),]

# Cleaning data and assigning feature engineered variables 
# Calling match.calc()
match.calc(data.train) 
t.train <- cbind(df, data.train[,6]) 

# Calling weight.calc()
weight.calc(data.train) 
#Converting document term matrix into matrix and dataframe
train.dtm1 <- as.data.frame(as.matrix(dtm1))
train.dtm2 <- as.data.frame(as.matrix(dtm2))
train.dtm1$Freq.Q1 <- rowSums(train.dtm1)
train.dtm2$Freq.Q2 <- rowSums(train.dtm1)

# Adding new column freq in train and test data
t.train <- cbind(t.train, Freq.Q1 = train.dtm1$Freq.Q1)
t.train <- cbind(t.train, Freq.Q2 = train.dtm2$Freq.Q2)

t.train <- t.train[,c(1:9, 11:12, 10)]
#t.train <- t.train[,c(1:8, 10:11, 9)]
#t.train <- t.train[,c(1:7, 9:10, 8)]

t.train <- as.data.frame(lapply(t.train, as.numeric))
t.train$is_duplicate <- as.factor(t.train$is_duplicate)

############################ TEST ################################

#Creating sample data for test
set.seed(54321) # Seed for train 1234 #Seed for test 4321
data.test <- train[sample(nrow(train), 2500),]

# Cleaning data and assigning feature engineered variables 
df <- data.frame() # Cleaning object df for test data processing
# Calling match.calc()
match.calc(data.test) 
t.test <- df

# Calling weight.calc()
weight.calc(data.test) 

# Converting document term matrix into matrix and dataframe
test.dtm1 <- as.data.frame(as.matrix(dtm1))
test.dtm2 <- as.data.frame(as.matrix(dtm2))
test.dtm1$Freq.Q1 <- rowSums(test.dtm1)
test.dtm2$Freq.Q2 <- rowSums(test.dtm1)

# Adding new column freq in train and test data
t.test <- cbind(t.test, Freq.Q1 = test.dtm1$Freq.Q1)
t.test <- cbind(t.test, Freq.Q2 = test.dtm2$Freq.Q2)

t.test <- as.data.frame(lapply(t.test, as.numeric))

#################### TRIED SAMPLING TECHNIQUE #######################

# Using ROSE package we tried to balance the slightly unbalanced data
# Although this did not helped in the prediction of outcome hence not using.

# Balancing the data using synthetic data generation sampling using 'ROSE'
#rose.train <- ROSE(is_duplicate ~ ., data = t.train, seed = 1)$data
#table(rose.train$is_duplicate) #Data is balanced

# Balancing the data using Over sampling
#over.train <- ovun.sample(is_duplicate ~ ., data = t.train,method = "over", N = 1500)$data
#table(over.train$is_duplicate)


##################### Visualization ########################
# Visualising proportion of similar and not similar questions
prop.table(table(train$is_duplicate)) 

prop <- ggplot(data = train, aes(x = is_duplicate, fill = is_duplicate)) + geom_density()
print(prop)

# Visualising frequent words using wordcloud
wordcloud(Corp.Q1,min.freq = 12, scale=c(7,.2), colors=brewer.pal(6, "Dark2"))
wordcloud(Corp.Q2,min.freq = 10, scale=c(7,.2), colors=brewer.pal(6, "Dark2"))


########################### Predictive Modelling ##################################
############ launch the H2O cluster ################
install.packages("h2o")
require(h2o)
localh2o <- h2o.init(nthreads = -1)

# Creating h2o environment object
train.h2o <- as.h2o(t.train)
test.h2o <- as.h2o(t.test)

#check column index number
colnames(train.h2o)

#dependent variable (Purchase)
y.dep <- ncol(train.h2o)

#independent variables (dropping ID variables)
x.indep <- 1:(ncol(train.h2o)-1)

################# Binomial classification using Regression ###################
#GLM algorithm in H2O can be used for all types of regression such as lasso, ridge, logistic, linear etc. 
#only needs to modify the family parameter accordingly
#logistic regression,write family = "binomial".

system.time(
  regression.model <- h2o.glm( y = y.dep, x = x.indep,
                               training_frame = train.h2o,
                               family = "binomial")
)

h2o.performance(regression.model)
#h2o.r2(regression.model)

#make predictions
predict.reg <- as.data.frame(h2o.predict(regression.model, test.h2o))
predict.reg$predict <- ifelse(predict.reg$p0 >= 0.5 ,0 , 1)

#Builing Confusion matrix
tab <- table(actual = data.test$is_duplicate, Predicted = predict.reg$predict)
confusionMatrix(tab)

roc.curve(data.test$is_duplicate, predict.reg$predict, plotit = T)


########################## Random Forest ################################
system.time(
  rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, 
                                    training_frame = train.h2o,
                                    ntrees = 1000, mtries = 3,
                                    max_depth = 4, seed = 1122))

h2o.performance(rforest.model)
#h2o.r2(rforest.model)

#check variable importance
#h2o.varimp(rforest.model)

#making predictions on unseen data
system.time(
  predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o))
)
predict.rforest$predict <- ifelse(predict.rforest$p0 >= 0.5 ,0 , 1)

#Builing Confusion matrix
tab <- table(actual = data.test$is_duplicate, Predicted = predict.rforest$predict)
confusionMatrix(tab)

roc.curve(data.test$is_duplicate, predict.rforest$predict, plotit = T)


##################### Deep Learning Model ##############################
system.time(
  dlearning.model <- h2o.deeplearning(y = y.dep,
                                      x = x.indep,
                                      training_frame = train.h2o,
                                      epoch = 60,
                                      hidden = c(50,50),
                                      activation = "Rectifier",
                                      seed = 1122
  )
)

h2o.performance(dlearning.model)
#h2o.r2(dlearning.model)

#making predictions
predict.dl2 <- as.data.frame(h2o.predict(dlearning.model, test.h2o))
predict.dl2$predict <- ifelse(predict.dl2$p0 >= 0.5 ,0 , 1)

#Builing Confusion matrix
tab <- table(actual = data.test$is_duplicate, Predicted = predict.dl2$predict)
confusionMatrix(tab)

roc.curve(data.test$is_duplicate, predict.dl2$predict, plotit = F)


######################### FINALIZED MODEL #############################

##################### Gradient Boosting Model ##########################
system.time(
  gbm.model <- h2o.gbm(y=y.dep, x=x.indep,
                       training_frame = train.h2o,
                       ntrees = 1000, max_depth = 4,
                       learn_rate = 0.01, seed = 1122)
)

h2o.performance(gbm.model)
#h2o.r2(gbm.model)

#making prediction and writing submission file
system.time(
  predict.gbm <- as.data.frame(h2o.predict(gbm.model, test.h2o))
  
)

predict.gbm$predict <- ifelse(predict.gbm$p0 >= 0.5 ,0 , 1)

#Builing Confusion matrix
tab <- table(actual = data.test$is_duplicate, Predicted = predict.gbm$predict)
confusionMatrix(tab)

roc.curve(data.test$is_duplicate, predict.gbm$predict, plotit = T)


############################# THANK YOU #################################

