# Spectre Media Analysis
An R framework for newsflow and social media analysis, primarily in Russian. Based on RSpectre independent nonprofit organization capacities. *This is rather a manifest for internal use than a user-aimed desription*.

## Primary tasks

### 1. Newsflow partition

Detecting news stories in an unstructured set of news articles.

There is a decent R package for this task: [RNewsflow](https://github.com/kasperwelbers/RNewsflow). Some fine-tuning of data preparation is necessary to maximise its performance:

* Document tokenisation:
  + N-grams;
  + limited window between tokens (skip ngrams).
* Token weighting:
  + inverse term frequency (TfIdf);
  + document length correction;
  + consider relative token position in a document (critical for news articles);
  + weighting based on grammatical features (POS tagging).
* Document clusterization (partition):
  + graph clustering algorithms ([igraph](https://igraph.org/r/) etc.).
  
### 2. Social media document tagging

Social media post allocation to a particular news subject:

* based on unique tokens and ngrams;
* based on machine learning algorithms (lasso regression [dictionaries](https://github.com/sfeuerriegel/SentimentAnalysis) etc.);
* based on Markov chains.

## Advanced tasks

### 1. Media Analysis tasks

* semantical differentiation;
* object sentiment analysis (as a special case of semantical differentiation);
* named entity recognition;
* quotation and direct speech recognition;
* opinion extraction;
* image recognition.

### 2. Social Analysis tasks

* topic modelling with socioeconomic and ideological grouping;
* key words extraction ([Wierzbicka](https://books.google.ru/books/about/Understanding_Cultures_Through_Their_Key.html?id=Un0bAQAAIAAJ&redir_esc=y) and lasso regression);
* judgement extraction;
* value extraction;
* cultural paradigm modelling.
