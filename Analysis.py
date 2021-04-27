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
from wordcloud import WordCloud, STOPWORDS, ImageColorGenerator


# In[2]:


get_ipython().run_line_magic('run', '"./Preprocessing.ipynb"')


# In[3]:


data_train=pd.read_csv("../TM/data/en-sentiment/Tweets_EN_sentiment_train.csv")
data_test=pd.read_csv("../TM/data/en-sentiment/Tweets_EN_sentiment_test.csv")
lexicon=pd.read_csv("../TM/data/en/NCR-lexicon.csv")

#totalidade dos dados
dados=data_train.append(data_test)
dados = dados[dados['text'].notna()]
dados.reset_index(inplace=True)


# In[4]:


# Análise descritiva
# Pode se observar que a coluna "text" tem valores nulos
print("Train data (shape): ",data_train.shape, "\n")
print("Train data (info): ")
print(data_train.info())
print("\n\n")
print("Test data (shape): ",data_test.shape, "\n")
print("Test data (info): ")
print(data_test.info())


# In[5]:


# identificar os tweets cujos textos são valores nulos
null_dict = {}
for i in range(len(data_train["text"])):
    if str(data_train.iloc[i,1]) == "nan":
        null_dict[i] = [str(data_train.iloc[i,1]) , data_train.iloc[i,2]]
print(len(null_dict))
# Podemos observar que estes tweets nulos estão presentes em ambas as classes pos e neg.
null_dict


# In[6]:


# Vamos realizar uma análise descritiva às classes, no conjunto de treino e de teste, para observar a frequência de cada 
# classe por conjunto
get_ipython().run_line_magic('matplotlib', 'inline')
fig, ax = plt.subplots(1,2, figsize = (10,5))
ax[0].hist(data_train["class"], bins = 3, label = "train")
ax[1].hist(data_train["class"], bins = 3, label = "test")


# In[7]:


# Colocar a coluna 'class' como index
data_train.set_index("class", inplace = True)
data_test.set_index("class", inplace = True)


# In[8]:


pos_train = len(data_train.loc["pos"])
neg_train = len(data_train.loc["neg"])
pos_test = len(data_test.loc["pos"])
neg_test = len(data_test.loc["neg"])
print("Train set --> Total: {} | Pos: {} | Neg: {}".format(len(data_train), pos_train, neg_train))
print("Test set --> Total: {} | Pos: {} | Neg: {}".format(len(data_test), pos_test, neg_test))


# In[9]:


#tweets com sentimento positivo
pos=dados.loc[dados["class"]=="pos"]
pos.reset_index(inplace=True)

#tweets com sentimento negativo
neg=dados.loc[dados["class"]=="neg"]
neg.reset_index(inplace=True)


# In[10]:


#Preprocessmento:
#sentimento positivo:
print("POS_class")
data_preprocessed_pos, tokenization_pos, lemmatized_pos, data_neg_pos, tokenization_neg_pos, lemmatized_neg_pos=preprocessing(pos)

# Sem tratamento da negação
lemmatized_idf_pos=remove_unnecessary_words(lemmatized_pos, voc_length = 11200, option_idf=True)


#sentimento negativo:
print("NEG_class")
data_preprocessed_neg, tokenization_neg, lemmatized_neg, data_neg_neg, tokenization_neg_neg, lemmatized_neg_neg=preprocessing(neg)

# Sem tratamento da negação
lemmatized_idf_neg=remove_unnecessary_words(lemmatized_neg, voc_length = 11200, option_idf=True)


# In[12]:


#transformar as listas de textos em texto seguido
text_all_pos=''
for i in lemmatized_idf_pos:
    for x in i:
        text = " ".join(i[0])
        text_all_pos=text_all_pos+' '+text
print(text_all)


text_all_neg=''
for i in lemmatized_idf_neg:
    for x in i:
        text = " ".join(i[0])
        text_all_neg=text_all_neg+' '+text
#print(text_all)


# In[13]:


#Nuvem de palavras com sentimento positivo
wordcloud = WordCloud(max_font_size=40, max_words=40, background_color="white").generate(text_all_pos)
plt.figure()
plt.imshow(wordcloud, interpolation="bilinear")
plt.axis("off")
plt.show()


# In[14]:


#Nuvem de palavras com sentimento negativo
wordcloud = WordCloud(max_font_size=50, max_words=40, background_color="white").generate(text_all_neg)
plt.figure()
plt.imshow(wordcloud, interpolation="bilinear")
plt.axis("off")
plt.show()


# In[ ]:




