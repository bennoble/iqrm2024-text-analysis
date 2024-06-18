## Computer Assisted Text Analysis, Lab 1
# Benjamin Noble
# Summer 2024

## Part I: Discovering SOTU
# Throughout these two modules, we are going to focus our analytical efforts on 
# the president's State of the Union Address. Before we dive into measurement
# in the coming lectures, we are going to start by practicing our discovery
# skills by familiarizing ourselves with the text.

# Visit the American Presidency Project Website. Choose one State of the Union 
# Address delivered orally after 1981 and one written State of the Union 
# Address delivered before 1900.

# Oral addresses: https://www.presidency.ucsb.edu/documents/app-categories/spoken-addresses-and-remarks/presidential/state-the-union-addresses
# Written address: https://www.presidency.ucsb.edu/documents/app-categories/citations/presidential/state-the-union-written-messages

# As you read these two speeches, what do you notice? What is similar? What is
# different? What sorts of questions arise and what hypotheses do you have? 

# In a bit, we will come together as a group to discuss:
# - Which speeches did you choose? What years and which presidents?
# - What is one similarity you noticed across the two speeches?
# - What is one difference you noticed?
# - What is one hypothesis you could consider testing if you were to have a 
#   magic way to measure any concept in the text of all State of the Union 
#   Addresses? (don't worry if it's not very good or you don't know that much
#   about presidential rhetoric)

## Part II: Some R Basics
# I am assuming basic familiarity with R. However, if you need a quick tune
# up, you can refer to Data Science in R: A Gentle Introduction 
# (https://bookdown.org/jgscott/DSGI/) by James Scott (UT Austin). Also, 
# ChatGPT is your friend if and when you get stuck!

# In order to launch right into measurement in the next session, you will need 
# to install a few packages if you have not done so already. Using the 
# `install.packages('package_name')` command, install the following packages
# and make sure the following lines of code run.

library(quanteda)
library(quanteda.corpora)
library(stringr)
library(tidyverse)

# The package `quanteda.corpora` contains a `corpus` object that contains all
# State of the Union Addresses.

data_corpus_sotu

# The `corpus` class is a unique data structure from the `quanteda` package 
# that is helpful when working with large text databases. We will get more 
# familiar with this package in the next session. However, you may be more 
# familiar with rectangular dataframes that are similar to spreadsheets. We
# can convert our `corpus` object to a data frame with the following code:

sou_df <- as_tibble(convert(data_corpus_sotu, to = 'data.frame'))
sou_df

# You can see that our data is now structured like a spreadsheet. Columns 
# contain metadata like the speaker, the date, and the party. The `text` column
# contains the raw text of all State of the Union addresses.

# You can view the full text of a speech by selecting the row using indexing
# and pulling the text column using `$`:
sou_df[1,]$text

# Using the tidyverse suite of tools, you can also perform other operations on 
# our dataframe. For example, if you want to see the State of the Union 
# Addresses given by Lincoln, you can use the `%>%` and `filter()` commands:

sou_df %>% 
    filter(President == 'Lincoln')

# You might also be interested in creating your own variables. For example,
# you can use `mutate()` and `if_else(condition, true, false)` to create a 
# variable for whether a State of the Union Address was given before or after
# the U.S. Civil War.

sou_df2 <- sou_df %>% 
    mutate(post_war = if_else(Date > '1865-04-09', 1, 0))

# You can also use `group_by()` and `summarise()` to aggregate and summarize
# your data.

sou_df2 %>% 
    group_by(post_war) %>% 
    summarise(n = n())

# Here, we can seen 76 speeches were given before the Civil War and 165 after.

# Finally, `tolower()` converts text to lower case; this is often helpful 
# because the mix of upper and lowercase can mess with our analysis.

sou_df_lower <- sou_df %>% 
    mutate(text = tolower(text))



