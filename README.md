# Frog2Features
An R Library to create high quality features from FROG output.

# 1. Introduction
The `Frog2Features` library for R aims to streamline, document and consolidate the workflow in feature construction procedures that involve corpora of unstructured text that have been pre-processed by [FROG](https://github.com/LanguageMachines/frog/) (an integrated set of memory-based NLP modules)FROG. The `Frog2Features` library starts with the output produced by FROG, and allows for the creation of a wide range of text-based features that can be used in subsequent applications. 

Use cases:
   1. Summarize and describe various textual characteristics of a corpus.
   2. Create and select features for use in a supervised learning procedure.
   3. Reproduce an existing feature construction method.
   4. Describe, summarize and simplify the data handling of an existing data handling workflow. 

# 2. Main goals

## 2.1. Accessibility

## 2.2. Reproducability 

## 2.3. Scalability and speed


# 3. Supported types of text-based features

#### a. Indicator features
Indicator features summarize the occurrence of a certain item (i.e., the indicator) in each document of a corpus. Typically, such features include counts, such as the frequency with which a certain word occurs in each document. In the linguistic literature, these features are typically also termed unigrams (a single indicator) or n-grams (a combined set of indicators). 

There exists a wide range of potential indicator features. Frog2Features introduces a typology that can classify a range of indicator features according to three properties, namely: 
   
   i. the primary unit of information conveyed in the indicator
   ii. whether the indicator is singular or plural (and, in case of plural indicators, whether to use positional proximity or syntactic dependency to form ngrams)
   iii. what unit of measurement to report (e.g., boolean indicators, raw counts, TF-IDF-weighed figures)

Frog2Features supports the following primary units of information: 
   * raw words
   * lemmatized words
   * part-of-speech annotation (POS-tag or MAJOR POS-tag)
   * morphological codes
   * recognized entity tag  
   * semantically enriched codes (e.g., 
   * ... or any other property that can be merged to a FROG `data.frame`.
      


#### b. Meta-features 
todo.


# Installation:
## 1. Requirements:
   * `FROG`
     * Output in tab-delimited column-formatted output (see [this](http://languagemachines.github.io/frog/) example)
     * 
   * `R` Packages 
     * `R6` - required for the object- and class structure of Frog2Features
     * `data.table`
     * `stringr`
     * `igraph`

## 2. Install or load the corresponding `R` package:

```splus
# Method 1: install into local library
install.packages(/path/to/frog2features/)

# Method 2: load into local session 
devtools::load_all(/path/to/frog2features/)

```



# Frequently Asked Questions:

## What is FROG?
[FROG](https://languagemachines.github.io/frog) is an integrated set of NLP-tools developed by Busser, Van den Bosch, Van der Sloot, & Van Gombel (2006; 2017). FROG takes raw text data as its input, and returns a preprocessed dataset which can be used for subsequent feature construction steps. Depending on the configuration used, FROG's preprocessing steps involve tokenization, lemmatization, part-of-speech tagging, morphological segmentation, syntactic dependency parsing, named-entity recognition and semantic 

## What is the difference between FROG and Frog2Features?
FROG is a stand alone application that produces preprocessed output. Frog2Features takes that output and creates a set of usable features. 

## Will there be support for other languages? 
Possibly. 
