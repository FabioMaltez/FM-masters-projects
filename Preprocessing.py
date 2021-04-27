#!/usr/bin/env python
# coding: utf-8

# In[1]:


import csv
import json
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from textblob import TextBlob
#nltk.download('stopwords')
import time

import re                                  
import string             
import gzip
import re
import collections
import nltk
from nltk.corpus import stopwords
from nltk.stem import WordNetLemmatizer
from nltk.tokenize import word_tokenize
from nltk.corpus import wordnet
from nltk.probability import FreqDist
import sys

import contractions
import sklearn
import sklearn.metrics as metrics
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn import svm

from sklearn.naive_bayes import MultinomialNB
from sklearn.metrics import classification_report


# In[2]:


data_train=pd.read_csv("../TM/data/en-sentiment/Tweets_EN_sentiment_train.csv")
data_test=pd.read_csv("../TM/data/en-sentiment/Tweets_EN_sentiment_test.csv")
lexicon=pd.read_csv("../TM/data/en/NCR-lexicon.csv")


# In[3]:


data_train = data_train[data_train['text'].notna()]
data_test = data_test[data_test['text'].notna()]
data_train.reset_index(inplace=True)
data_test.reset_index(inplace=True)
dados=data_train.append(data_test)


# # Data Preprocessing

# In[4]:


def tag_words(string):
    transformed = re.sub(r'\b(?:not|never|no|nothing|noone|none|cannot|noo|nop)\b[\w\s]+[^\w\s]', 
                         lambda match: re.sub(r'(\s+)(\w+)', r'\1NEG_\2', match.group(0)), string,flags=re.IGNORECASE)
    return(transformed)


# In[5]:


# POS_TAGGER_FUNCTION : TYPE 1
def simplify_tag(nltk_tag):
    if nltk_tag.startswith('J'):
        return wordnet.ADJ
    elif nltk_tag.startswith('V'):
        return wordnet.VERB
    elif nltk_tag.startswith('N'):
        return wordnet.NOUN
    elif nltk_tag.startswith('R'):
        return wordnet.ADV
    else:          
        return None


# In[6]:


def preprocessing(data):
    
    tokenization=[] 
    lemmatized=[]
    lemmatized_neg=[]
    tokenization_neg=[]
    lemmatizer=WordNetLemmatizer()
    data_neg=data.copy(deep=True)
    
    for i, row in enumerate(data["text"]):
  
        #colocar tudo em minúsculas:
        row=row.lower()

        #remover URLs
        # Retirado de: https://www.regextester.com/94502
        row = re.sub(r"^(?:http(s)?:\/\/)?[\w.-]+(?:\.[\w\.-]+)+[\w\-\._~:\/?#[\]@!\$&'\(\)\*\+,;=.]+$", " ", row)
        
        # Remover caracteres repetidos mais de 2 vezes
        row = re.sub(r'(.)\1{2,}', r'\1', row)
       
        #expandir as contrações
        row=contractions.fix(row)
        row = re.sub(r"cannot", "can not", row)
        
        #remover todos os números
        row= re.sub(r'\d+', '', row)
        
        #tratamento da negação:
        row=row+'.'   #É necessária a adição de pontuação no final dos tweets
        row_neg=tag_words(row)
        
        
        #remover todos os caracteres especiais (tudo o que não seja: [a-zA-Z0-9_])
        row_neg=re.sub(r'[^\w\']', ' ', row_neg)
        
        row= re.sub(r'[^\w\']', ' ', row)
        #situações a resolver : "j/k"; "m&m" (não é necessário resolver neste caso, a menos que se use )
    
        data.loc[i, "text"]=row
        data_neg.loc[i, "text"]=row_neg
        
        #TOKENIZAÇÃO:
        tok=nltk.word_tokenize(row)
        tok_neg=nltk.word_tokenize(row_neg)
        
        tokenization.append([tok, data.loc[i, "class"]])
        tokenization_neg.append([tok_neg, data_neg.loc[i, "class"]])

        #Para efetuar melhorar a lemmatização iremos utilizar a tag correspondente ao part of speech
        pos_tagged = nltk.pos_tag(tok)
        
        
        #criação de uma lista onde cada palavra está associada à sua tag (part of speech)
        wordnet_tagged = list(map(lambda x: (x[0], simplify_tag(x[1])), pos_tagged))
        
        #LEMATIZAÇÃO
        lemmatized_sentence = []
        lemmatized_sentence_neg=[]
        i=0

        for word, tag in wordnet_tagged:

            if tag is None:
                #se não tiver tag adiciona o token como está
                lem=word
                lemmatized_sentence.append(word)
            else:        
                # caso contrário utiliza a tag para a lematização
                lem=lemmatizer.lemmatize(word, tag)
                lemmatized_sentence.append(lem)
            
            #caso a paravra em questão contenha "NEG_" (tratamento da negação) é necessário retirar para obter a palavra lematizada 
            
            if "NEG_" in tok_neg[i]:
                word_neg=tok_neg[i].replace('NEG_', '')

                if tag is None:
                #se não tiver tag adiciona o token como está
                    lemmatized_sentence_neg.append("NEG_"+word)
                else:        
                # caso contrário utiliza a tag para a lematização
                    lemmatized_sentence_neg.append("NEG_"+lemmatizer.lemmatize(word, tag))
                    
            else:
                    lemmatized_sentence_neg.append(lem)
            i=i+1
            

        lemmatized.append([lemmatized_sentence, data.loc[i, "class"]])
        lemmatized_neg.append([lemmatized_sentence_neg, data.loc[i, "class"]])
     
    #print("data: ", data[0:10])
    #print("tokenization:\n", tokenization[0:5], "\n")
    #print("lemmatized:\n", lemmatized[0:5], "\n\n")
    #print("tokenization (NEG):\n", tokenization_neg[0:5], "\n")
    #print("lemmatized (NEG):\n", lemmatized_neg[0:5])
    
    return data, tokenization, lemmatized,data_neg, tokenization_neg, lemmatized_neg


# In[7]:


#Remoção das palavras desnecessárias: através de idf ou stop_words
#use_negation_terms == True para IDF
#use_negation_terms == False para stop words

def remove_unnecessary_words(rows, voc_length = 11200, option_idf=True, use_negation_terms=False):

    if option_idf==True:
        lemmatized_idf = []
        lemma_words_freq = []
        
        if use_negation_terms == True:
            # representar os tweets com frequência de palavras por tweet 
            for t in rows:
                doc = collections.Counter()
                for w in t[0]:
                    doc[w] += 1
                lemma_words_freq.append(doc)

            #USAR IDF: MANTER OS TERMOS MAIS USADOS
            # criar o dicionário de palavras mais usadas
            tf = collections.Counter()
            df = collections.Counter()

            for d in lemma_words_freq:
                for w in d:
                    tf[w] += d[w]
                    df[w] += 1

            idfs = {}
            for w in tf:
                if tf[w] > 1:
                    idfs[w] = np.log(len(lemma_words_freq)/df[w])

            voc = sorted(idfs, key=idfs.get, reverse=True)[:voc_length]
            print("Len(idfs): {}".format(len(idfs.keys()))) # ver quantas palavras existem no corpus de tweets lematizados
            print(len(voc))

            #retirar as palavras que não estão no dicionário das mais usadas
            for d in rows:
                sentence_idf = []
                for w in d[0]:
                    if w in voc:
                        sentence_idf.append(w)
                lemmatized_idf.append([sentence_idf, d[1]])

            return lemmatized_idf
        else:
            # representar os tweets com frequência de palavras por tweet 
            for t in rows:
                doc = collections.Counter()
                for w in t[0]:
                    if "NEG_" in w:
                        doc[w.replace("NEG_", '')] += 1
                    else:
                        doc[w] += 1
                lemma_words_freq.append(doc)

            #USAR IDF: MANTER OS TERMOS MAIS USADOS
            # criar o dicionário de palavras mais usadas
            tf = collections.Counter()
            df = collections.Counter()

            for d in lemma_words_freq:
                for w in d:
                    tf[w] += d[w]
                    df[w] += 1

            idfs = {}
            for w in tf:
                if tf[w] > 1:
                    idfs[w] = np.log(len(lemma_words_freq)/df[w])

            voc = sorted(idfs, key=idfs.get, reverse=True)[:voc_length]
            print("Len(idfs): {}".format(len(idfs.keys()))) # ver quantas palavras existem no corpus de tweets lematizados
            print(len(voc))

            #retirar as palavras que não estão no dicionário das mais usadas
            for d in rows:
                sentence_idf = []
                for w in d[0]:
                    if "NEG_" not in w:
                        if w in voc:
                            sentence_idf.append(w)
                    else:
                        if w.replace("NEG_",'') in voc:
                            sentence_idf.append(w)
                lemmatized_idf.append([sentence_idf, d[1]])

            return lemmatized_idf
            

    #REMOVER AS STOP WORDS:
    else:
        stopw = stopwords.words('english')
        stopw.remove("not")
        
        lemmatized_no_stopwords = []

        for row in rows:
            filtered_sentence=[]
            for word in row[0]:
                if "NEG_" not in word:
                    if word not in stopw: 
                        filtered_sentence.append(word)
                        
                else:
                    if word.replace("NEG_", '') not in stopw:
                        filtered_sentence.append(word)
                        
            lemmatized_no_stopwords.append([filtered_sentence, row[1]])    

        return  lemmatized_no_stopwords 

