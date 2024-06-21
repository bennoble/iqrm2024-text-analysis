## Computer Assisted Text Analysis, Lab 4
# Benjamin Noble
# Summer 2024

library(glmnet)
library(tidyverse)

# Part I: Predicting Nostalgia

# you will need to install the `dataverse` package to run this line of code
library(dataverse)
library(quanteda)
# Data comes from Muller and Proksch (2023)
# (https://www.cambridge.org/core/journals/british-journal-of-political-science/article/nostalgia-in-european-party-politics-a-textbased-measurement-approach/41C48D60B039F3081EB522FB76646E96#article)
nostalgia <- get_dataframe_by_name(
  filename = "data_coded_all.tab",
  dataset = "https://doi.org/10.7910/DVN/W8VGJF", 
  server = "dataverse.harvard.edu")

# here, we will create three different sets, our training, test, and validation
# sets
set.seed(20240612)
# randomly sample 30% of the data for testing and validating
test_ids <- sample(nostalgia$doc_id, nrow(nostalgia) * 0.3)
# hold out 10% of the total df for our final validation
held_out_ids <- sample(test_ids, length(test_ids) * (1/3))
# get the other 20% as test data
test_set <- nostalgia %>% filter(doc_id %in% test_ids & !doc_id %in% held_out_ids)
# finally, get our training data
train_set <- nostalgia %>% filter(!doc_id %in% test_ids)

# use some standard pre-processing to construct our dfm
nostalgia_train_dfm <- corpus(train_set) %>% 
    tokens(remove_numbers = TRUE, 
        remove_punct = TRUE, 
        remove_symbols = TRUE,
        remove_separators = TRUE) %>% 
    tokens_tolower() %>%
    tokens_remove(c(stopwords("english"))) %>%
    tokens_select(min_nchar = 3) %>% 
    dfm() %>% 
    dfm_wordstem() %>% 
    dfm_trim(min_termfreq = 10, termfreq_type = 'count',
        min_docfreq = 10, docfreq_type = 'count')

# train our lasso model, note alpha = 1 is for lasso
# we will also use a logit model here to predict a binary class
# note, this will do cross validation to select the best lambda 
cv_model <- cv.glmnet(nostalgia_train_dfm, train_set$nostalgic, alpha = 1, family = "binomial")  
# we can view this cross validation process, notice how the first solid line 
# is at about -4.3. this value of lambda minimized prediction error
plot(cv_model)
# we can view that inside the model object here
log(cv_model$lambda.min)
# we can also visualize the shrinkage process
plot(cv_model$glmnet.fit,xvar="lambda",label=TRUE) 
# let's look at the most important coefficients as identified by the model
best_coefs <- coef(cv_model, s = "lambda.min")
# positive coefficients are stronger predictors of nostalgia
# we see words like history, heritage, and tradition show up (makes sense!)
# we also see words like new, women, and european are more predictive of a 
# document being not nostalgia (makes sense!)
sort(best_coefs[,1])

# let's turn to testing how well our model performs on our test data
# to do so, we need to create a dfm that only includes words used to train the
# model (our model wouldn't know what to do with a word outside of its vocab)
# pre-process test data using the same pre-processing steps
test_dfm <- corpus(test_set)  %>% 
    tokens(remove_numbers = TRUE, 
        remove_punct = TRUE, 
        remove_symbols = TRUE,
        remove_separators = TRUE) %>% 
    tokens_tolower() %>%
    tokens_remove(c(stopwords("english"))) %>%
    tokens_select(min_nchar = 3) %>% 
    dfm() %>% 
    dfm_wordstem() %>% 
    dfm_trim(min_termfreq = 10, termfreq_type = 'count',
        min_docfreq = 10, docfreq_type = 'count')

# match terms between training and test
matched_test_terms <- dfm_match(test_dfm, features = featnames(nostalgia_train_dfm)) 
# we can now predict the probability any given document in our test set is 
# nostalgic, the output is a probability
predictions <- predict(cv_model, newx = matched_test_terms, s = "lambda.min", type = "response")
# let's look to see some representative texts
# this is the text that is predicted to be most nostalgic
nostalgia %>% filter(doc_id == rownames(predictions)[which.max(predictions)]) %>% pull(text)
# here is the least nostalgic
nostalgia %>% filter(doc_id == rownames(predictions)[which.min(predictions)]) %>% pull(text)
# seems pretty good
# we can also use predict to "round" off the probabilities and give us a 0/1
predictions_class <- predict(cv_model, newx = matched_test_terms, s = "lambda.min", type = "class")
# we can create a confusion matrix to see how we did
conf_mat <- table(true = test_set$nostalgic, pred = as.numeric(predictions_class))
# accuracy is 92%, which is quite good
(conf_mat[1,1] + conf_mat[2,2])/sum(conf_mat)
# but note that nostalgia is rare, so we can get good accuracy just by 
# defaulting to 0
# precision tells us proportion of 1s that were actually correct
# 87% because it very rarely said 1 when it was 0
conf_mat[2,2]/sum(conf_mat[,2])
# recall tell us proportion of actual positives that were identified correctly
# only 46%, very bad!
conf_mat[2,2]/sum(conf_mat[2,])
# so the model under-predicts nostalgia, what should we do? 
# we could try different pre-processing, use different model(s), code more docs

# (Time permitting)
# Try to replicate the training process on the training and test data and 
# predict on the test data
# here are the datasets you'll need to get your started
held_out <- nostalgia %>% filter(doc_id %in% held_out_ids)
train_and_test <- nostalgia %>% filter(!doc_id %in% held_out_ids)



