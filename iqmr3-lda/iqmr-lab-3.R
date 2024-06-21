## Computer Assisted Text Analysis, Lab 3
# Benjamin Noble
# Summer 2024

## Part I: Coding LDA from Scratch
# In the first part of today's lab, we are going to work through the code 
# needed to produce documents according to the generative LDA model. Then, 
# we're going to work through the code needed to reverse engineer the 
# process and recover the underlying data generating parameters.

# We will need the MCMCpack library for the `rdirichlet` function.
library(MCMCpack)

## 1.1 Generating Documents
# In reality, you will never need to actually generate documents. However, the 
# process of working through the code needed to generate documents will help us 
# better understand what LDA thinks is going on, and also allow us to know the 
# exact parameters we want to recover when we code LDA ourselves. That will 
# allow us to test our intuition.

# Let's start with a five word vocabulary that falls into two topics. 
# We have five words, two about cute animals, two about food, and one that 
# could be associated with both categories.
vocabulary <- c("dog", "cat", "broccoli", "banana", "eat")
# Set K (our number of topics) to 2.
K <- 2 

# Here, we will set our prior on alpha, the prior for the document-topic 
# distribution. We will use a uniform prior where topics are equally likely to
# appear in documents and the draws will be closer to the corners than the
# center.
alpha <- rep(1, K)  

# We will set up our dirichlet distribution that pushes words towards our 
# desired associations.
# Topic 1: more likely to have "dog" and "cat".
# Topic 2: more likely to have "broccoli" and "banana".
# "eat" can belong to both topics.

beta <- matrix(c(
  10, 10, 1, 1, 5,  # Topic 1: dog, cat, broccoli, banana, eat
  1, 1, 10, 10, 5   # Topic 2: dog, cat, broccoli, banana, eat
), nrow = K, byrow = TRUE)

set.seed(20240606)
# Here, we generate the topic distributions for each document
# We'll create 100 documents
num_docs <- 100
# We will create an empty theta (document-topic) matrix. Each row is a document
# and each column is the proportion of words that go with that topic
theta <- matrix(0, nrow = num_docs, ncol = K)
for (d in 1:num_docs) {
  theta[d, ] <- rdirichlet(1, alpha)
}

# Here, we can see that e.g., the first document is 49% topic 1 and 51% topic 
# 2. Document 2 is 21% topic 1 and 80% topic 2.
head(theta)

# Here, generate the word distributions for each topic
phi <- matrix(0, nrow = K, ncol = length(vocabulary))
for (k in 1:K) {
  phi[k, ] <- rdirichlet(1, beta[k, ])
}
colnames(phi) <- vocabulary
# Each row is a topic (topic 1, topic 2) and each column is the proportion of
# that topic made up by that word. So dog is 36% of topic 1 (animals) but only
# 1% of topic 2 (food). Broccoli is much more associated with topic 2 than 1, 
# and eat is equally associated with both.
phi

# Now, let's make some documents:
generate_document <- function(num_words, theta_d, phi) {
  document <- c()
  for (n in 1:num_words) {
    # Sample a topic from the document's topic distribution
    topic <- sample(1:K, 1, prob = theta_d)
    # Sample a word from the topic's word distribution
    word <- sample(vocabulary, 1, prob = phi[topic, ])
    # add it to the document
    document <- c(document, word)
  }
  return(document)
}

set.seed(20240607)  
# We'll create documents with 100 words
num_words_per_doc <- 100
generated_documents <- list()
for (d in 1:num_docs) {
  generated_documents[[d]] <- generate_document(num_words_per_doc, theta[d, ], phi)
}
# This is what the first document looks like:
generated_documents[[1]]

# To better illustrate how this worked, I am going to run the same function,
# but it provides some comments to help us understand what is going on.
generate_document_verbose <- function(num_words, theta_d, phi) {
  document <- c()
  for (n in 1:num_words) {
    # Sample a topic from the document's topic distribution
    topic <- sample(1:K, 1, prob = theta_d)
    print(paste(c('word num:', n, 'topic:', topic, 'prob:', theta_d[topic]), collapse = ' '))
    # Sample a word from the topic's word distribution
    word <- sample(vocabulary, 1, prob = phi[topic, ])
    print(paste(c('word num', n, 'word', word, 'prob', phi[topic, which(vocabulary == word)]), collapse = ' '))
    document <- c(document, word)
    print(document)
  }
  return(document)
}

set.seed(20240606) 
# We are going to generate a five word document that has the word associations
# discussed above and that is 82% topic 1 (animals) and 18% topic 2 (food) 
generate_document_verbose(5, theta[3, ], phi)

## 1.2: Reverse Engineering with LDA Code
# The goal of LDA is, assuming the process we just used to generate documents,
# we want to recover `theta` and `phi`. Recall, theta is the proportion of each
# document allocated to topics 1 and 2 and phi the word proportions within each
# topic. We will use the 100 generated documents of 100 words each to do so.

# We need to convert our words to integers:
prepare_data <- function(documents) {
    # create a word index, dog = 1, cat = 2, etc.
    word_to_index <- setNames(seq_along(vocabulary), vocabulary)
    # apply this conversion to all words in all documents
    doc_words <- lapply(documents, function(doc) word_to_index[doc])
    # put it together
    list(word_to_index = word_to_index, doc_words = doc_words)
}

# run the function
data_prepared <- prepare_data(generated_documents)
# rename our document matrix 
documents <- generated_documents
# Save the number of words in our vocabulary (5)
V <- length(data_prepared$word_to_index)
# Save the number of documents (100)
D <- length(documents)

set.seed(20240606)
# Initialize topic assignments and count matrices
# Randomly assign every word in every document (10,000 total) to a topic
topic_assignments <- lapply(data_prepared$doc_words, function(words) sample(1:K, length(words), replace = TRUE))
# create empty matrices which we will fill in the next step
doc_topic_count <- matrix(0, nrow = D, ncol = K)
word_topic_count <- matrix(0, nrow = V, ncol = K)
topic_totals <- numeric(K)

# Fill count matrices
for (d in seq_len(D)) { # for each document (100)
  for (n in seq_along(documents[[d]])) { # for each word in document d (100)
    # extract the word (which is it's id number) from that cell. so [[1]][1] is the first word in the first document,
    # [[10]][15] is the 15th word in the 10th document
    word <- data_prepared$doc_words[[d]][n]
    # this is a randomly assigned topic to word n in document d
    topic <- topic_assignments[[d]][n]
    # increment the number of topics that topic appears in the document (needed)
    # for theta (document-topic distribution)
    doc_topic_count[d, topic] <- doc_topic_count[d, topic] + 1
    # increment the number of times that word is associated with that topic
    # across the corpus (needed for phi, topic-word distribution)
    word_topic_count[word, topic] <- word_topic_count[word, topic] + 1
    # total of all words assigned to each topic to normalize probabilities later
    topic_totals[topic] <- topic_totals[topic] + 1
  }
}

# e.g., the second document has 47 words in topic 1 and 53 in topic 2
# this was totally random, we don't expect it to be correct. The true value is
# above in `theta[2,]` and is 21/79.
head(doc_topic_count)
# This is our randomized starting point for phi. E.g., We assume dog (col 1) is
# abut equally associated with each topic (but of course that's false) 
t(word_topic_count)

# These values are the hyperparamers for our dirichlet distributions (we set 
# them to 1, all equal) and the number of iterations we are going to run of 
# our model (i.e., how many times we will go through every word in the corpus)
# and re-assign it.
alpha <- 1  # Hyperparameter for documents
beta <- 1  # Hyperparameter for words
iterations <- 100  # Number of iterations for Gibbs sampling

# Ok, here we go!
for (iter in 1:iterations) {
  print(iter)
  for (d in seq_len(D)) { # for in the the document list (100 total)
    for (n in seq_along(documents[[d]])) { # for each word in document d (100) 
      # take word n in document d
      word <- data_prepared$doc_words[[d]][n]
      # take the current topic assignment for word n in document d
      topic <- topic_assignments[[d]][n]
      
      # Decrement counts
      # we need to remove the current word from our data so our model is blind
      # to its current position when it computes p(topic | word, document).
      doc_topic_count[d, topic] <- doc_topic_count[d, topic] - 1
      word_topic_count[word, topic] <- word_topic_count[word, topic] - 1
      topic_totals[topic] <- topic_totals[topic] - 1
      
      # Here is the magic! Just two lines of code.
      # Calculate probabilities for each topic
      # compute p(topic | word, document)
      # part 1 accounts for the number of times topic k appears in document d + alpha (smoothing), 
      # this is the document-topic affinity: how strongly d is associated with topic k 
      # part 2 accounts for how often the word we are focused on appears in any of the topics + beta (smoothing)
      # this is topic-word affinity: how strongly w is associated with topic k
      # the denominator helps normalize by accounting for how many words appear across all topics k with extra smoothing of V + beta
      # note, we are implicitly accounting for relationships between words here because if two words (e.g., dog, cat) are assigned
      # to the same topic, then that document is going to have a larger share of that topic
      prob_topics <- (doc_topic_count[d, ] + alpha) * (word_topic_count[word, ] + beta) / (topic_totals + V * beta) 
      prob_topics <- prob_topics / sum(prob_topics)  # Normalize to 1
      
      # another way to do this that matches steyvers and griffiths (2007)
      # word_contribution <- (word_topic_count[word, ] + beta) / (topic_totals + V * beta)  # Topic-word affinity
      # doc_contribution <- (doc_topic_count[d, ] + alpha)  # Document-topic affinity
      # prob_topics1 <- word_contribution * doc_contribution
      # prob_topics1 <- prob_topics1 / sum(prob_topics1)  # Normalize to form a probability distribution

     # Sample a new topic based on probabilities we just calculated
      topic <- sample(1:K, 1, prob = prob_topics)

      # Update counts with new topic assignment
      topic_assignments[[d]][n] <- topic
      doc_topic_count[d, topic] <- doc_topic_count[d, topic] + 1
      word_topic_count[word, topic] <- word_topic_count[word, topic] + 1
      topic_totals[topic] <- topic_totals[topic] + 1
    }
  }
}

# Estimate theta (document-topic distribution)
# this is proportion of words in the document_d associated with topic k, rows sum to 1
# answers: "What portion of this document is about a particular topic?"
theta_hat <- (doc_topic_count + alpha) / rowSums(doc_topic_count + alpha)

# Estimate phi (topic-word distribution)
# what proportion of topic k is word w_i, rows sum to 1
# answers: "How central or important is this word to this topic?"
phi_hat <- ((t(word_topic_count) + beta) / (topic_totals + V * beta))
colnames(phi_hat) <- vocabulary

# This phi is the topic-word proportions we were trying to match.
phi
# Here is the phi we estimated:
# this tells us that topic 1 is heavily food but low animal
rev(sort(phi_hat[1,]))
# this tells us topic 2 is heavily animal but low food
rev(sort(phi_hat[2,]))
# these have been permuted (topic 1 is 2 and vice versa because LDA doesn't 
# actually understand our ordering we set up). We pretty much recover our 
# original phi matrix

# This theta is the document-topic proportions we were trying to match
head(theta_hat)
head(theta)
# note, the topics are permuted so topic 1 is the food topic, 2 is the animal topic
# comparing topic 1 in theta_hat to topic 2 in theta (food to food), but  see 
# that we almost recover the proportions of words!

# Let's do it with a package:
library(quanteda)
library(topicmodels)
# create a dfm for topic modeling
qt <- tokens(generated_documents)
qt_dfm <- dfm(qt)
lda_model <- LDA(qt_dfm, k = 2, control = list(seed = 1234), method = 'Gibbs', burnin = 1000, iter = 2000, thin = 500)
# Print the top terms in each topic
top_terms <- terms(lda_model, 10)  # Get 10 terms per topic
# note, our topic 1 is their topic 2, but it "gets" the differences
print(top_terms) 

# Optionally, assign topics to documents
# Assuming 'lda_model' is your fitted LDA model from the `topicmodels` package
posterior_results <- posterior(lda_model)
 # This is a matrix of document-topic probabilities, again the topics are 
# flipped, but it does pretty well in recovering the results
document_topic_probs <- posterior_results$topics 
head(document_topic_probs)
head(theta)

## Part II: Using Topic Modeling Software
library(tidyverse)
library(stm)
# Let's return to Donald Trump's tweets from the last lab
trump_tweets <- read_csv('https://raw.githubusercontent.com/MarkHershey/CompleteTrumpTweetsArchive/master/data/realDonaldTrump_in_office.csv') %>% 
    rename(id = ID, time = Time, url = `Tweet URL`, text = `Tweet Text`)
trump_tweets$uid <- 1:nrow(trump_tweets)

# Using our code from last time, let's create a dfm.
tt_corp <- corpus(trump_tweets)
tt_dfm <- tokens(tt_corp, what = 'word',
    # here, we remove several types of tokens we do not want, including 
    # numbers, punctuation, symbols, and separators.
        remove_numbers = TRUE, 
        remove_punct = TRUE, 
        remove_symbols = TRUE,
        remove_separators = TRUE) %>% 
    # we convert all tokens to lowercase
    tokens_tolower() %>%
    # we remove a set of stopwords that are standard in the quanteda package
    tokens_remove(c(stopwords("english"))) %>%
    # we remove all words with only one or two characters (e.g., 'a', 'at')
    tokens_select(min_nchar = 3) %>% 
    # we create a dfm
    dfm() %>% 
    # we stem words
    dfm_wordstem() %>% 
    # we remove rare words---those appearing less than 5 times in total and
    # those that appear in fewer than five documents
    dfm_trim(min_termfreq = 5, termfreq_type = 'count',
        min_docfreq = 5, docfreq_type = 'count')

# due to the pre-processing, some of the documents have no tokens, let's remove
tt_omit <- tt_dfm[!rowSums(tt_dfm) == 0,]
# This line of code runs our LDA model. This can take a while as the model 
# runs through several iterations. The default tolerance is 1e-5, but that is
# going to take too long for a demo, I set it to 5e-4 but you can remove this
# in actual analysis.
tt_mod <- stm(tt_omit, K = 15, init.type = 'LDA', seed = 20221208, emtol = 5e-4)

# Once the model fits, use the following code to inspect the top words 
# associated with each topic. What do you think each topic represents?
labelTopics(tt_mod)

# For one topic, use the following code to inspect some representative docs
# First, we need to remove the "empty" documents from our corpus
tt_corp_edit <- tt_corp[tt_corp$uid %in% tt_omit$uid,]
# This code finds 3 representative docs for topic 4 (but you can change 
# these settings)
top4 <- findThoughts(tt_mod, tt_corp_edit, n = 3, topic = 4)
plot(top4, width = 80)

# You can also see the topic proportions within each document.
head(as.data.frame(tt_mod$theta))
# let's create a vector of which topic is the most prevalent in each tweet
max_topic <- c()
for(i in 1:nrow(tt_mod$theta)){
  max_topic <- c(max_topic, which.max(tt_mod$theta[i,]))
}
# reassign that vector to our df
df_omit <- trump_tweets[trump_tweets$uid %in% tt_corp_edit$uid,]
df_omit$max_topic <- max_topic
# see how often each topic is discussed
df_omit %>% 
  group_by(max_topic) %>% 
  summarise(n = n())

# Time permitting, change the settings. Try a different number of topics 
# and see what (if anything) changes. Try to change the pre-processing and see
# if 15 topics is different from the original 15 topics.

