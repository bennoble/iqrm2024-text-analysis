## Computer Assisted Text Analysis, Hack-A-Thon
# Benjamin Noble
# Summer 2024

# For this hack-a-thon, you will be using your newfound text analysis skills to
# answer a question using presidents' State of the Union Addresses. You can
# come up with one of your own, or try one of these:

# 1) Do Republicans talk more about foreign policy and Democrats more about 
#    domestic issues?
# 2) Do presidents talk more about cultural/social issues today and more about 
#    economic issues in the past?
# 3) Have presidents' State of the Union Addresses gotten more positive or 
#    negative over time?
# 4) Do written State of the Union Addresses focus more on elite issues while
#    spoken State of the Union Addresses focus on more on popular/public-facing
#    issues?

library(tidyverse)
library(quanteda)
library(quanteda.corpora)

sou_text <- as_tibble(convert(data_corpus_sotu, 'data.frame'))

# If you'd like to attempt supervised learning, you can use the comparative
# agendas project topic coding of the State of the Union Addresses since 1945
# cap_sou <- read_csv('https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Exec_SOTU_2024.csv')