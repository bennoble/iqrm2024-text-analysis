# Computational Text Analysis, IQMR 2024

The State of the Union Address. A UN General Assembly meeting. Attack ads. Constitutions. International treaties. Court rulings. Twitter/X posts. 

Politics takes place through language—the spoken and written word. As social scientists understanding and analyzing these words are key to our study of politics. Yet, this is a difficult task. Simply reading the transcript of every political speech delivered or message posted online would be a full-time job (or many full-time jobs)—which is to say nothing of more complicated tasks like identifying the topics or sentiment of these texts.

In this two-part module, we will tackle these challenges by learning the foundational principles and methods of computational text analysis. Here, we will transform written text into quantitative data, and we will leverage our computers to read and examine these texts with (and for) us. You will leave this class with the skills to automatically count keywords, measure sentiment, discover topics, build a pattern-learning model, discover surprising synonyms, and much more. 

Each session will begin with a lecture followed by an in-class group lab where you will apply and hone your skills.

The sequence will rely primarily on a textbook (in addition to article or online readings): 

> Grimmer, Roberts, and Stewart (2022). Text as Data. Princeton University Press. 

This is referred to below as TAD. As this is an applied course, we will focus on developing a conceptual understanding of these tools and how they are applied in social science research. When you encounter difficult technical material in this book, consider it, but do not feel pressure to fully understand it before class. 

Please read all required reading before class. Feel free to review the recommended material if and when you are interested. 

## Lesson 1: Text is Data?

We will consider what it means to treat text as data. Unlike counting the number of votes for a candidate or tracking fluctuations in president approval, when we work with text, we must begin by transforming words on the page into numbers in a spreadsheet. During this session, we will discuss foundational principles behind computational text analysis and learn how social scientists measure the concepts we care about. 

- TAD, Chapters 2, 15.
- [Lecture slides](iqmr1-intro/iqmr1-intro.pdf)
- [Lab code](iqmr1-intro/iqmr-lab-1.R): document discovery, `R` basics.

### Further Reading
- Grimmer, J., & Stewart, B. (2013). Text as Data: The Promise and Pitfalls of Automatic Content Analysis Methods for Political Texts. Political Analysis, 21(3), 267-297.


## Lesson 2: Pulling Words Out of a Bag

We will discuss one of the two major frameworks for treating text as data: as a “bag of words.” We will also cover count and dictionary-based methods of measurement. 

- TAD, Chapters 5, 16.
- [Lecture slides](iqmr2-dicts/iqmr2-dicts.pdf)
- [Lab code](iqmr2-dicts/iqmr-lab-2.R): using `quanteda` to create document feature matrices, keyword and dictionary analysis. 

### Further Reading
- [Tokenization in large language models, explained](https://seantrott.substack.com/p/tokenization-in-large-language-models?publication_id=1003231&post_id=143614250&isFreemail=true&r=ecfmx&triedRedirect=true) by Sean Trott.
- Denny, Matthew J., and Arthur Spirling. 2018. “Text Preprocessing For Unsupervised Learning: Why It Matters, When It Misleads, And What To Do About It.” Political Analysis 26(2): 168–89. 
- Application: Noble, Benjamin S. 2023. “Presidential Cues and the Nationalization of Congressional Rhetoric, 1973-2016.” American Journal of Political Science.

## Lesson 3: Discovering Hidden Topics

Building on the bag of words assumption, we will learn about one of the most popular methods for text-based discovery: LDA topic modeling. This method will allow us to automatically discover topics hidden in text (but not without potential pitfalls!). 

- TAD, Chapters 6, 13.
- [Lecture slides](iqmr3-lda/iqmr3-lda.pdf)
- [Lab code](iqmr3-lda/iqmr-lab-3.R): building LDA from scratch, using the `stm` package to run topic models.

### Further Reading:
- Eshima, Shusei, Kosuke Imai, and Tomoya Sasaki. 2023. “Keyword-Assisted Topic Models.” American Journal of Political Science. 
- [Latent Dirichelt Allocation (video)](https://www.youtube.com/watch?v=T05t-SqKArY) by Serrano.Academy
- [Introduction to Latent Dirichlet Allocation](https://arc.net/l/quote/ddscyjzt) by Edwin Chen
- [Probabilistic Topic Models](https://d1wqtxts1xzle7.cloudfront.net/81715/sbmq8q4bog4w4yg3ip1.pdf?1425068854=&response-content-disposition=inline%3B+filename%3Dpdf.pdf&Expires=1717897918&Signature=IyuZ1fehyoWkWrdKvBXboUV~bKu29ngNASDoECSpWaoSc4CUO88YIyNO4Sz6cMUR07gWP2ydOsfTlyCbo~r4oCZcwzrhbqGfEKeyPlq7dypt0~AfS5mwVk3QhxaxwhstqLgpmZwQZo1vnD8vA3yOvOJYxKbY6N2mq~Y5-tZP26LZ53T0MMVFB4qooofg1HF8ZJL6-g~dV-7jvylNpaHkU7fd6RmX3XMYlKh38KlB3DxXEY3pVTAMl84lVaAz57WPdCaQF2Nu3GqQuJDdFtItRxvv8LSC5NwzEDAoLuNvGiiGjG~63VNM9nyh2RP7kpxvZV3A-Uze0VERdkWSy-FzRw__&Key-Pair-Id=APKAJLOHF5GGSLRBV4ZA) by Mark Steyvers and Tom Griffiths.
- Application: Grimmer, Justin. 2013. “Appropriators Not Position Takers: The Distorting Effects of Electoral Incentives on Congressional Representation.” American Journal of Political Science 57(3): 624–42. 

## Lesson 4: How to Train Your Model

We will shift gears from discovery to detection through the use of supervised machine learning. Using a small sample of human-coded documents, we will train a model to discover the patterns linking text to labels. Then, we’ll use this model to predict the codes of unlabeled text. Of course, no model is perfect. Like a research assistant, they require supervision and assessment.

- TAD, Chapters 17, 18, 19. 
- [Lecture slides](iqmr4-ml/iqmr4-ml.pdf)
- [Lab code](iqmr4-ml/iqmr-lab-4.R): predicting nostalgia in text using LASSO.

### Further Reading:
- Osnabrügge, Moritz, Elliott Ash, and Massimo Morelli. 2021. “Cross-Domain Topic Classification for Political Texts.” Political Analysis: 1–22.
- Application: Müller, Stefan, and Sven-Oliver Proksch. 2023. “Nostalgia in European Party Politics: A Text-Based Measurement Approach.” British Journal of Political Science: 1–13.

## Lesson 5: Words in Space

In this session, we will discuss the second major framework for conceptualizing text as data: as vectors in space. We'll learn about word embeddings (a foundational concept underlying more complicated language models like ChatGPT) and how to use them. 

- TAD, Chapters 7, 8.
- Kovendhan Venugopal. May 2021. [Mathematical Introduction to GloVe Word Embedding](https://becominghuman.ai/mathematical-introduction-to-glove-word-embedding-60f24154e54c). Becoming Human: Artificial Intelligence Magazine
- [Lecture slides](iqmr5-emb/iqmr5-emb.pdf)
- [Lab code](iqmr5-emb/iqmr-lab-5.R): building GloVe from scratch to embed *Green Eggs and Ham*, working with pre-trained GloVe embeddings.

### Further Reading
- Rodriguez, Pedro L., and Arthur Spirling. 2022. “Word Embeddings: What Works, What Doesn’t, and How to Tell the Difference for Applied Research.” The Journal of Politics 84(1): 101–15.
- Rodriguez, Pedro L., Arthur Spirling, and Brandon M. Stewart. 2023. “Embedding Regression: Models for Context-Specific Description and Inference.” American Political Science Review 117(4): 1255–74.
- Application: Bellodi, Luca. 2023. “A Dynamic Measure of Bureaucratic Reputation: New Data for New Theory.” American Journal of Political Science 67(4): 880–97. 
- [Mapping the Mind of a Large Language Model](https://www.anthropic.com/news/mapping-mind-language-model) by Anthropic

## Hack-A-Thon
In this concluding session, we will apply our newfound text analysis skills in a fast-paced and (hopefully) fun hack-a-thon. In the first half of class, you will work in groups to develop and test a hypothesis using your expert text analysis skills. In the second half, you will present your approach and findings to the class.

- [Materials](hack-a-thon.R)
