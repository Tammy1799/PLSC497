----------------------
# Set up environment                   ---
#----------------------------------------
# clear global environment
rm(list = ls())

setwd("C:/Users/kevin/Documents/GitHub/TAD_2021/R lessons/")

# load required libraries
library(quanteda)
install.packages('quanteda.textmodels')
#The textmodel_*() functions formerly in quanteda have 
#now been moved to the quanteda.textmodels package.
library(quanteda.textmodels)
install.packages('readtext')
install.packages('dplyr')
library(readtext)
library(dplyr)

#----------------------------------------
# 1 Supervised Learning: Naive Bayes     ---
#----------------------------------------
#source of data: https://www.kaggle.com/rmisra/news-category-dataset#News_Category_Dataset_v2.json

# load data
news_data <- readRDS("news_data.rds")

#predict category of news based on dataset

## BIG Dataset!

# subset data and keep relevant variables
#filter-- restricts dataset, dropping rows
#we are only keeping those observations which the variable matches the category
news_samp <- filter(news_data, category %in% c("CRIME", "SPORTS"))
  
##select --- keeps only named columns

news_samp1 <- select(news_samp, headline, category)


##setNames --- rename the variables in our new dataset
#class is what we are trying to predict and text is the text of the headline of the news articles
news_samp2<-setNames(object = news_samp1, nm = c("text", "class"))


# get a sense of how the text looks
#to look for dimensions of news article
dim(news_samp2)
#select headlines where class is crime
head(news_samp2$text[news_samp2$class == "CRIME"])
head(news_samp2$text[news_samp2$class == "SPORTS"])

# some pre-processing (the rest will let dfm do)
#look for patterns of apostrophes and replace with nothing
news_samp2$text <- gsub(pattern = "'", "", news_samp2$text)  # replace apostrophes
head(news_samp2$text[news_samp2$class == "SPORTS"])

# what's the distribution of classes?
#3405 is crome and 4884 is crime
prop.table(table(news_samp2$class))

# split sample into training & test sets
#seed means gives same exact randomization
set.seed(1984L)

###what does this do??
#we are creating a training set that is 80% of our dataset
prop_train <- 0.8

###need to create a training set that's 80% of the data
#vector of IDS, 1 to the number of rows that are in our dataframe
ids <- 1:nrow(news_samp2)

#sample randomly generates
#we are multiplying 80% by 8000 cuz we have 8000 ids
#sampling without replacement
ids_train <- sample(ids, ceiling(prop_train*length(ids)), replace = FALSE)

## the - sign is "not"
#- means is not
#so we are getting set of ids where they aren't ids_train
ids_test <- ids[-ids_train]
train_set <- news_samp2[ids_train,]
test_set <- news_samp2[ids_test,]

# get dfm for each set
#look at text of each dataset
train_dfm <- dfm(train_set$text, stem = TRUE, remove_punct = TRUE, remove = stopwords("english"))
test_dfm <- dfm(test_set$text, stem = TRUE, remove_punct = TRUE, remove = stopwords("english"))

# how does this look?
as.matrix(train_dfm)[1:5,1:5]

# match test set dfm to train set dfm features
#there are likely to be words that appear in one set, may not appear in other set
#so we are basically only showing features from test set that occur in train set
test_dfm <- dfm_match(test_dfm, features = featnames(train_dfm))


# w/o smoothing ----------------

# train model on the training set
#prior is uniform so assigning 50/50 chance of being crime or sports
nb_model <- textmodel_nb(train_dfm, train_set$class, smooth = 0, prior = "uniform")
# evaluate on test set
#predict is gonna apply the model we just made to new data
predicted_class <- predict(nb_model, newdata = test_dfm)
# baseline --- This is important, to see how much our model beats a model that just picks the modal class 
#baseline accuracy is 59% in this case
baseline_acc <- max(prop.table(table(test_set$class)))

# get confusion matrix
cmat <- table(test_set$class, predicted_class)
nb_acc <- sum(diag(cmat))/sum(cmat) # accuracy = (TP + TN) / (TP + FP + TN + FN)
nb_recall <- cmat[2,2]/sum(cmat[2,]) # recall = TP / (TP + FN)
nb_precision <- cmat[2,2]/sum(cmat[,2]) # precision = TP / (TP + FP)
nb_f1 <- 2*(nb_recall*nb_precision)/(nb_recall + nb_precision)


# print
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "Accuracy:",  nb_acc, "\n",
  "Recall:",  nb_recall, "\n",
  "Precision:",  nb_precision, "\n",
  "F1-score:", nb_f1
)

# w smoothing ----------------

# train model on the training set using Laplace smoothing
##Recall what this does --- want to avoid having any zeroes, which happens if there's a novel word in the test set

nb_model_sm <- textmodel_nb(train_dfm, train_set$class, smooth = 1, prior = "docfreq")

# evaluate on test set
predicted_class_sm <- predict(nb_model_sm, newdata = test_dfm)

# get confusion matrix
cmat_sm <- table(test_set$class, predicted_class_sm)
nb_acc_sm <- sum(diag(cmat_sm))/sum(cmat_sm) # accuracy = (TP + TN) / (TP + FP + TN + FN)
nb_recall_sm <- cmat_sm[2,2]/sum(cmat_sm[2,]) # recall = TP / (TP + FN)
nb_precision_sm <- cmat_sm[2,2]/sum(cmat_sm[,2]) # precision = TP / (TP + FP)
nb_f1_sm <- 2*(nb_recall_sm*nb_precision_sm)/(nb_recall_sm + nb_precision_sm)

# print
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "Accuracy:",  nb_acc_sm, "\n",
  "Recall:",  nb_recall_sm, "\n",
  "Precision:",  nb_precision_sm, "\n",
  "F1-score:", nb_f1_sm
)

# take a look at the most discriminant features (get some face validity)
posterior <- data.frame(feature = rownames(t(nb_model_sm$param)), 
                        post_CRIME = t(nb_model_sm$param)[,1],
                        post_SPORTS = t(nb_model_sm$param)[,2])

#we will see weight assgined. so higher weight, the higher something appears in something like sports and nfl
head(arrange(posterior, -post_SPORTS))
head(arrange(posterior, -post_CRIME))


# what does smoothing do? More generally, reduces the "weight" place on new information (the likelihood) vis-a-vis the prior. 
plot(nb_model$param[1,], nb_model_sm$param[1,], xlim = c(0,0.02), ylim = c(0,0.02), xlab="No Smooth", ylab="Smooth") + abline(a = 0, b = 1, col = "red")

##don't worry about that code






#########Practice questions:


####I'd like you to have experience creating documents from scratch



# 1. We originally set the proportion of the training set to be .8 --- what happens to performance when we set it at .2? Re-run all the code 
##after that point (with smoothing)
The performance is lower because the model has less features to train on. 

With smoothing for .8:
Baseline Accuracy:  0.5944478 
Accuracy: 0.9511165 
Recall: 0.951269 
Precision: 0.9659794 
F1-score: 0.9585678

With smoothing for .2:
Baseline Accuracy:  0.5944478 
Accuracy: 0.9251998 
Recall: 0.9082335 
Precision: 0.9632333 
F1-score: 0.9349252



# 2. Read the help file about textmodel_nb . What does the "prior" argument do in Naive Bayes? What is the default value for this 
# argument in quanteda?
The prior argument does prior distribution on texts and the default is uniform.


#3. Re-run the code with prior = "docfreq". Think about the proportions of classes in the dataset. How should this affect your outcome?
# Looking at the results of the code, were you right?
Since the proportions of classes are about 50/50, there really shouldn't be much change in the outcome since when training classes are balanced in their number of documents, docfreq and uniform should perform about the same.

Yes I was correct. These were the results after changing uniform to docfreq with smoothing and a size of .8:
Baseline Accuracy:  0.5944478 
Accuracy: 0.952927 
Recall: 0.9573604 
Precision: 0.9632278 
F1-score: 0.9602851


