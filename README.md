# Frog2Features
An R library to create high quality textual features from FROG output objects. 

# 1. Introduction
The `Frog2Features` library for R was developed at the Wetenschappelijk Onderzoeks- en Documentatiecentrum [WODC](https://www.wodc.nl/), as part of a study on the usability of textmining methods in the detection of cybercrime. Since the library solves a number of generic data handling issues involved with text mining research, it was decided to release the library to the general public. 

Frog2Features allows researchers to streamline, customize, document and consolidate the feature construction workflow. The library starts with a dataset that has been pre-processed by [FROG](https://github.com/LanguageMachines/frog/) (an integrated set of memory-based NLP modules). Next, a wide range of indicator and meta features can be created. 

Examples of use cases are: 

   1. Summarize and describe textual characteristics of a corpus;
   2. Create and select features for use in a supervised learning procedure;
   3. Reproduce an existing feature construction method;
   4. Describe, summarize and simplify the data handling of an existing data handling workflow;

# 2. Goals

## 2.1. Ease of use

Frog2Features was designed with two distinct `target audiences` mind. The first comprises (applied) researchers with a basic understanding of R, text mining and linguistics, that (simply) want a textmining tool that works 'out-of-the' box. If you don't want to be bothered by all-too technical details, the Frog2Features library contains a large number of presets that readily create acceptable features. 

The second target audience comprises researchers with a more technical background. For these researchers, a large number of lower-level data handling functions is accessible, to allow for fine-grained control of all sorts of parameters during feature construction and extraction. 

## 2.2. Reproducability 

Frog2Features was originally designed to assist supervised learning methods in crime research. In such studies, it is crucial that training- and testdatasets yield the exact same features. Also, it is desirable that a particular featureconstruction method that has been proven in one study can be used easily in another study. Frog2Features therefore contains extensive (internal) checks to ensure reproducability. In addition, various split-sample evaluations can be made to evaluate stability. 

## 2.3. Scalability and speed

A number of existing textmining libraries for R already exist (most notably, tm), but these libraries scale poorly to large samples. Frog2Features makes extensive use of novel R techniques to optimize code for scalability and speed. This was done by making use of the fast `data.table` package for common rectangular operations, the use of `sparse matrices` for the storage of large term-document matrices (planned!), and the use of fast regular expression matching. 

## 2.4. Flexibility

The various feature construction methods can be adapted to tailor one's specific needs. This is done by using an object-oriented design (R6-classes) with a large number of built in preset objects, that can be customized to the specific problem at hand.

Examples include: 

  - Custom filters of raw data and acquired features; 
  - Semantic enrichment with custom libraries
  - Composite features, created by combining different aggregation levels within FROG (i.e., lemma-POS bigrams)

# 3. Supported types of features

#### a. Indicator features
Indicator features summarize the occurrence of a certain item (i.e., the indicator) in each document. Such features are typically summarized as simple counts, such as the frequency with which a certain word occurs in each document. In the linguistic literature these features are usually referred to as unigrams (a single indicator) or n-grams (a combined set of indicators). 

A wide range of textual information can be used to construct indicator features. Frog2Features' indicator features can be classified according to the following typolocy: 

   i. the primary unit of information conveyed in the indicator (i.e., lemmas, part-of-speech codes, semantically enriched data)
   ii. whether the indicator is singular or plural (and, in case of plural indicators, whether to use positional proximity or syntactic dependency to form ngrams)
   iii. what unit of measurement to report (e.g., boolean indicators, raw counts, TF-IDF-weighed figures)

Frog2Features supports the following primary units of information: 
   * words (raw or lemmatized)
   * part-of-speech (major POS-categories, or fine-grained POS categories)
   * morphological codes
   * recognized-entity tags  
   * semantically-enriched codes (e.g., 
   * ... or any other property that can be merged to a FROG `data.frame`.
      
#### b. Meta-features 
These features summarize texts without taking into account its content. Examples are the number of words in a sentence, or the average character length of words in a sentence. Frog2Features contains a large set of built in meta-features, that can be readily employed. 

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

## Will there be support for other languages besides Dutch? 
Possibly. Frog2Features can be used on any dataset that conforms to the output of FROG. Since FROG can be trained on non-Ducth languages, it is in principle possible to use Frog2Features on other languages. However, all of FROG's presets currently target the Dutch language. 
