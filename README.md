# Frog2Features
An R Library to create high quality features from FROG output.

# Introduction
Frog2Features (F2F) is an R library that can be used to streamline, document and consolidate workflows in feature construction procedures. F2F starts with the output of [FROG](https://github.com/LanguageMachines/frog/) (an integrated set of memory-based NLP modules) and allows for the creation of a wide range of text-based features that can be used in subsequent machine learning applications. 

# Feature classes supported by F2F




Frog is a sophisticated natural language processing tool, designed for the Dutch Language. The application scans output

# Installation:
## 1. Requirements:
   * FROG
     * Output in tab-delimited column-formatted output (see [this](http://languagemachines.github.io/frog/) example)
     * 
   * R Packages 
     * R6 - required for the object- and class structure of Frog2Features
     * data.table
     * stringr
     * igraph

## 2. Install or load the corresponding R package:

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
FROG is a stand alone application that produces preprocessed 



## Will there be support for other languages? 





