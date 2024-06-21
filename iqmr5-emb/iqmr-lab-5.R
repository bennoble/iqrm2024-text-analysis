## Computer Assisted Text Analysis, Lab 5
# Benjamin Noble
# Summer 2024

library(tidyverse)
# Part I: GloVe "by hand"
# Let's get a better understanding of how the GloVe algorithm (and word 
# embeddings generally) work by walking through the GloVe algorithm. To begin
# our discussion, let's try to fit a GloVe embedding model on the full text of 
# Dr. Seuss's Green Eggs and Ham. Famously, Dr. Suess's editor bet him that he
# could not write a book using just 50 words, and Green Eggs and Ham was the 
# result. 

# We can read in the text of green eggs and ham
corpus <- readLines('https://www.site.uottawa.ca/~lucia/courses/2131-02/A2/trythemsource.txt')[-c(1,117)] 
# Lower case and create individual tokens
tokens <- unlist(strsplit(tolower(corpus), "\\W+"))
# Remove empty space
tokens <- tokens[tokens != '']
# unique vocabulary
vocab <- unique(tokens)
# number of unique terms
vocab_size <- length(vocab)

# Assign each word a unique id number
vocab_index <- setNames(seq_along(vocab), vocab)

# Function to create a co-occurrence matrix
# note, we set the context window size to three words on either side
create_cooccurrence_matrix <- function(tokens, vocab_index, window_size = 3) {
  # Initialize an empty co-occurrence matrix with dimensions equal to the vocabulary size
  cooccurrence_matrix <- matrix(0, nrow = length(vocab_index), ncol = length(vocab_index))
  # Loop over each token in the tokenized text
  for (i in seq_along(tokens)) {
    # Print the current index to track progress
    print(i)
    # Get the current token and its index in the vocabulary
    token <- tokens[i]
    token_index <- vocab_index[[token]]
    # Define the window range around the current token
    window_start <- max(1, i - window_size)
    window_end <- min(length(tokens), i + window_size)
    # Loop over the context window
    for (j in window_start:window_end) {
      # Skip the current token itself
      if (i != j) {
        # Get the context token and its index in the vocabulary
        context_token <- tokens[j]
        context_index <- vocab_index[[context_token]]
        # Increment the co-occurrence count for the token-context pair
        cooccurrence_matrix[token_index, context_index] <- 
          cooccurrence_matrix[token_index, context_index] + 1
      }
    }
  }
  # Return the filled co-occurrence matrix
  return(cooccurrence_matrix)
}

# create co-occurrence matrix
cooccurrence_matrix <- create_cooccurrence_matrix(tokens, vocab_index)
# assign text names
rownames(cooccurrence_matrix) <- vocab
colnames(cooccurrence_matrix) <- vocab
# print first few rows
head(cooccurrence_matrix)

set.seed(20240616)
# Initialize word vectors and biases
# Set our embedding dimensions to 50
embedding_dim <- 50
# We initialize word and context vectors with random values
word_vectors <- matrix(runif(vocab_size * embedding_dim, -0.5, 0.5), nrow = vocab_size)
context_vectors <- matrix(runif(vocab_size * embedding_dim, -0.5, 0.5), nrow = vocab_size)
# Same for the bias terms
word_bias <- runif(vocab_size, -0.5, 0.5)
context_bias <- runif(vocab_size, -0.5, 0.5)

# We may have some intuition that the words "ham" and "eggs" should be quite
# similar in this corpus due to their frequent co-occurrence; the words ham 
# and fox should not be very similar. We can see at the start of the training
# process that the cosine similarity for cos(ham, eggs) is basically 0 and same
# for cos(ham, fox) due to the random initialization 

# Cosine similarity function
cos_sim <- function(vector1, vector2){
  # numerator is the dot product of both vectors
  numerator <- sum(vector1 * vector2)
  # denominator is the product of the sqrt of the squared sum of both vectors
  denominator <- sqrt(sum(vector1^2)) * sqrt(sum(vector2^2))
  cs <- numerator/denominator
  return(cs)
}

# Note: we combine word and context vectors to capture word similarities when 
# word_i is the target and when it is in the context of other words
first_embeddings <- word_vectors + context_vectors
ham_emb <- first_embeddings[vocab_index['ham'],]
egg_emb <- first_embeddings[vocab_index['eggs'],]
fox_emb <- first_embeddings[vocab_index['fox'],]

# cosine similarity of ham and eggs, of ham and fox
# Note that ham and eggs are less similar than ham and fox to start due to 
# randomization 
cos_sim(ham_emb, egg_emb)
cos_sim(ham_emb, fox_emb)

# Training parameters
# how quickly the model updates
learning_rate <- 0.05
# these two parameters control the weights applied which helps account for 
# words that co-occur frequently (e.g., "the")
x_max <- 50
alpha <- 0.75
# how many model iterations we'll run
iterations <- 100

# create some empty vectors to store the running cosine similarity of vectors
# of interest for demo purposes
cs_hamegg <- cs_hamfox <- c()

# Number of iterations for training
for (iter in 1:iterations) {
  # Print the current iteration to track progress
  print(iter)
  # Loop over each word in the vocabulary
  for (i in 1:vocab_size) {
    # Loop over each context word in the vocabulary
    for (j in 1:vocab_size) {
      # Only update if the co-occurrence count is greater than zero
      if (cooccurrence_matrix[i, j] > 0) {
        # Calculate the weighting function f(X_ij)
        weight <- (cooccurrence_matrix[i, j] / x_max)^alpha
        # Cap the weight at 1
        weight <- ifelse(weight > 1, 1, weight)

        # Compute the dot product of the word and context vectors
        dot_product <- sum(word_vectors[i, ] * context_vectors[j, ])
        # Calculate the cost function J for the given word-context pair
        cost <- dot_product + word_bias[i] + context_bias[j] - log(cooccurrence_matrix[i, j])
        
        # Compute the gradient for the word vector
        word_grad <- weight * cost * context_vectors[j, ]
        # Compute the gradient for the context vector
        context_grad <- weight * cost * word_vectors[i, ]
        # Compute the gradient for the word bias
        word_bias_grad <- weight * cost
        # Compute the gradient for the context bias
        context_bias_grad <- weight * cost
        
        # Update the word vector using the gradient
        word_vectors[i, ] <- word_vectors[i, ] - learning_rate * word_grad
        # Update the context vector using the gradient
        context_vectors[j, ] <- context_vectors[j, ] - learning_rate * context_grad
        # Update the word bias using the gradient
        word_bias[i] <- word_bias[i] - learning_rate * word_bias_grad
        # Update the context bias using the gradient
        context_bias[j] <- context_bias[j] - learning_rate * context_bias_grad
      }
    }
  }
  # Print a message indicating the completion of the current iteration
  cs_hamegg <- c(cs_hamegg, cos_sim(word_vectors[vocab_index['ham'],] + context_vectors[vocab_index['ham'],], word_vectors[vocab_index['eggs'],] + context_vectors[vocab_index['eggs'],]))
  cs_hamfox <- c(cs_hamfox, cos_sim(word_vectors[vocab_index['ham'],] + context_vectors[vocab_index['ham'],], word_vectors[vocab_index['fox'],] + context_vectors[vocab_index['fox'],]))
  cat("Iteration:", iter, "completed\n")
}

# Here, we plot the cosine similarity of each iteration of ham and egg (red)
# and ham and fox (blue). Over time, cos(ham, egg) increases substantially and
# cs(ham, fox) modestly declines.
ggplot() + 
  geom_line(aes(x = 1:100, y = cs_hamegg), color = 'red') + 
  geom_line(aes(x = 1:100, y = cs_hamfox), color = 'blue') 

# Combine word and context vectors
final_embeddings <- word_vectors + context_vectors

# Show final embedding cosine similarity---ham and eggs are much more similar
# than ham and fox
cos_sim(final_embeddings[vocab_index['ham'],], final_embeddings[vocab_index['eggs'],])
cos_sim(final_embeddings[vocab_index['ham'],], final_embeddings[vocab_index['fox'],])
# Sam and am are also similar
cos_sim(final_embeddings[vocab_index['sam'],], final_embeddings[vocab_index['am'],])

# Part II: Using pre-training embeddings
# Sometimes you will train your own embeddings, but often, you can use 
# pre-trained embeddings. 
library(tidytext)
library(quanteda)
library(lsa)
# This will read in the 300d glove vectors pre-trained on wikipedia from my 
# dropbox, it will take a minute
glove300 <- read_csv('https://www.dropbox.com/scl/fi/tv2nmic7bduiz9rhe1ud3/iqmr-glove300.csv?rlkey=vu4z5vbyb2h5x3bc4joekxrop&raw=1')
head(glove300)

# just for demonstration, let's find the words in our embedding space most
# similar to 'president'
# extract the embedding for president
pres_emb <- unlist(glove300 %>% filter(word == 'president') %>% select(-word))
# create an empty vector
cs_vec <- c()
for(i in 1:nrow(glove300)){
  print(i)
  # compute cosine similarity between the president embedding and every word in
  # the embedding space (note, this can take a long time)
  cs_vec <- c(cs_vec, cosine(pres_emb, unlist(glove300[i,-1])))
}
# align scores and words
cs_scores <- tibble(cs_vec, row = 1:nrow(glove300))
# look at the top 20 nearest neighbors---they are all relevant
glove300[cs_scores %>% arrange(desc(cs_vec)) %>% head(n = 20) %>% pull(row), 'word']


# read in trump tweets, just using those sent in 2017 for size/speed
trump_tweets <- read_csv('https://raw.githubusercontent.com/MarkHershey/CompleteTrumpTweetsArchive/master/data/realDonaldTrump_in_office.csv') %>% 
    rename(id = ID, time = Time, url = `Tweet URL`, text = `Tweet Text`) %>% 
    filter(time < '2018-01-01')  %>% 
     mutate(text = tolower(text))

# tokenize 
tt_pp <- trump_tweets %>% 
  unnest_tokens(word, text) 
  # remove stop words, this helps with making sure tweets aren't similar based
  # only on frequent words
  anti_join(tibble(word = stopwords('en')))
# merge words with pre-trained embeddings
tt_words <- tt_pp %>% 
  left_join(glove300)
# average across the embeddings at the tweet level, this creates a tweet-level
# embedding
tt_emb_avg <- tt_words %>% 
  group_by(url) %>% 
  summarise(across(x1:x300, mean, na.rm = TRUE))
# this tweet is about buying american, what are the most similar tweets?
trump_tweets[8,'text']
# this is the same process as we did above (doesn't take too long)
cs_vec <- c()
for(i in 1:nrow(tt_emb_avg)){
  cs_vec <- c(cs_vec, cosine(unlist(tt_emb_avg[8,-1]), unlist(tt_emb_avg[i,-1])))
}
# align tweets and rows
cs_df <- tibble(cs = cs_vec, i = 1:nrow(tt_emb_avg))
# look at scores
head(cs_df %>% arrange(desc(cs)))
# top tweets, not bad!
trump_tweets[cs_df %>% arrange(desc(cs)) %>% head() %>% pull(i), 'text']

# We can do other kinds of analysis as well, for instance, are trump's tweets
# about democrats or republicans more positive? 
# this code will extract urls for tweets that use the word "democrat"
d_tweets <- trump_tweets %>% filter(grepl('democrat', text)) %>% pull(url)
# this code will extract urls for tweets that use the word "republican"
r_tweets <- trump_tweets %>% filter(grepl('republican', text)) %>% pull(url)
# we'll use the word "good" as a proxy for positivity
good <- glove300 %>% filter(word == 'good') %>% select(-word) %>% unlist()
# extract the emebddings for all democrat referencing tweets
dem_emb <- tt_emb_avg %>% filter(url %in% d_tweets) %>% select(-url)
# extract the emebddings for all republican referencing tweets
rep_emb <- tt_emb_avg %>% filter(url %in% r_tweets) %>% select(-url)

# compute similarity of each democrat speech embedding to "good"
cs_dem <- c()
for(i in 1:nrow(dem_emb)){
  cs_dem <- c(cs_dem, cosine(good, dem_emb[i,] %>% unlist()))
}

# compute similarity of each republican speech embedding to "good"
cs_rep <- c()
for(i in 1:nrow(rep_emb)){
  cs_rep <- c(cs_rep, cosine(good, rep_emb[i,] %>% unlist()))
}

# average cosine similarity of democrat-referencing speeches
mean(cs_dem)
# average cosine similarity of republican-referencing speeches
mean(cs_rep)