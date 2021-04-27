#!/usr/bin/env python
# coding: utf-8

# In[1]:


# Trabalho de grupo - Tweet Sentiment Analysis
# Text Blob
import csv
import json
import pandas as pd
import matplotlib.pyplot as plt
from textblob import TextBlob
import numpy as np


# In[2]:


# Extrair corpus de tweets
df_train = pd.read_csv("../TM/data/en-sentiment/Tweets_EN_sentiment_train.csv", encoding="utf-8")
df_test = pd.read_csv("../TM/data/en-sentiment/Tweets_EN_sentiment_test.csv", encoding="utf-8")

# Correr Preprocessing.ipynb - antes de correr a linha abaixo, os blocos de código desnecessários em Preprocessing.ipynb têm de ser comentados
get_ipython().run_line_magic('run', '"./Preprocessing.ipynb"')
# Certificar que todos os tweets são interpretados como strings
for i, text in enumerate(df_test["text"]):
    df_test.loc[i,"text"] = str(text)

data, tokenization, lemmatized,data_neg, tokenization_neg, lemmatized_neg = preprocessing(data=df_test)
lemmatized_idf = remove_unnecessary_words(lemmatized, voc_length = 4600, option_idf=True)
lemmatized_idf_neg = remove_unnecessary_words(lemmatized_neg, voc_length = 5100, option_idf=True, use_negation_terms=True) 

# Observar quantas listas vazias o preprocessamento origina
empty_lists = 0
for i in lemmatized_idf:
    if len(i[0]) == 0:
        empty_lists += 1
print(empty_lists)


# In[3]:


# Análise descritiva
# Pode se observar que a coluna "text" tem valores nulos
print(df_train.shape)
print(df_test.shape)
print(df_train.info())
print(df_test.info())


# In[4]:


# identificar os tweets cujos textos são valores nulos
null_dict = {}
for i in range(len(df_train["text"])):
    if str(df_train.iloc[i,1]) == "nan":
        null_dict[i] = [str(df_train.iloc[i,1]) , df_train.iloc[i,2]]
print(len(null_dict))
# Podemos observar que estes tweets nulos estão presentes em ambas as classes pos e neg.
null_dict


# In[5]:


# Vamos realizar uma análise descritiva às classes, no conjunto de treino e de teste, para observar a frequência de cada 
# classe por conjunto
get_ipython().run_line_magic('matplotlib', 'inline')
fig, ax = plt.subplots(1,2, figsize = (10,5))
ax[0].hist(df_train["class"], bins = 3, label = "train")
ax[1].hist(df_test["class"], bins = 3, label = "test")


# In[6]:


# Colocar a coluna 'class' como index
df_train.set_index("class", inplace = True)
df_test.set_index("class", inplace = True)


# In[7]:


pos_train = len(df_train.loc["pos"])
neg_train = len(df_train.loc["neg"])
pos_test = len(df_test.loc["pos"])
neg_test = len(df_test.loc["neg"])
print("Train set --> Total: {} | Pos: {} | Neg: {}".format(len(df_train), pos_train, neg_train))
print("Test set --> Total: {} | Pos: {} | Neg: {}".format(len(df_test), pos_test, neg_test))

# Reset Index
df_train = df_train.reset_index()
df_test = df_test.reset_index()
print(df_train.head(5), '\n', df_test.head(5))


# In[8]:


# Usar o TextBlob para fazer análise de sentimento no conjunto de teste e analisar a sua performance
tb_result = {}
for i, text in enumerate(df_test["text"]):
    tb_result[str(text)] = TextBlob(str(text)).sentiment.polarity
tb_result


# In[9]:


# Tranformar os valores numéricos do TextBlob em 'pos' ou 'neg'
for i in tb_result:
    if tb_result[i] < 0:
        tb_result[i] = "neg"
    else:
        tb_result[i] = "pos"
tb_result


# In[10]:


# Tabela resumo dos resultados previstos vs resultados reais
summary_lst = [0,0,0]
for i in tb_result.keys():
    for j, text in enumerate(df_test["text"]):
        if str(i) == str(text):
            summary_lst = np.vstack((summary_lst,[str(i), tb_result[i], df_test.iloc[j,0]]))

summary_lst = summary_lst[1:,]
summary_lst


# In[11]:


print(len(summary_lst))
print(len(df_test))
summary_df = pd.DataFrame(summary_lst, columns = ["text", "predicted", "real"])
summary_df.head(10)


# In[12]:


# Transformar 'pos' em 1 e 'neg' em 0
for i in range(len(summary_df)):
    if summary_df.iloc[i,1] == 'pos':
        summary_df.iloc[i,1] = 1
    else:
        summary_df.iloc[i,1] = 0
    if summary_df.iloc[i,2] == 'pos':
        summary_df.iloc[i,2] = 1
    else:
        summary_df.iloc[i,2] = 0
summary_df.head(10)


# In[13]:


def evaluation(pred, real):
    '''
    pred, real --> list of numbers, where 0 = False and 1 = True
    Returns the accuracy, recall, precision and f1 score of the classification matrix
    '''
    true_pos = 0
    false_pos = 0
    true_neg = 0
    false_neg = 0
    for i in pred:
        for j in real:
            if i == 1 and j == 1:
                true_pos += 1
            elif i == 1 and j == 0:
                false_pos += 1
            elif i == 0 and j == 1:
                false_neg += 1
            elif i == 0 and j == 0:
                true_neg += 1
    accuracy = (true_pos + true_neg)/(true_pos + true_neg + false_pos + false_neg)
    recall = true_pos/(true_pos + false_neg)
    precision = true_pos/(true_pos + false_pos)
    f1 = (2*precision*recall)/(precision + recall)
    return accuracy, recall, precision, f1


# In[14]:


acc, rec, prec, f1_score = evaluation(summary_df["predicted"], summary_df["real"])
print("accuracy: {} \nrecall: {} \nprecision: {} \nf1: {}".format(acc, rec, prec, f1_score))


# In[15]:


# TextBlob com préprocessamento
tb_result2 = {}
for d in lemmatized_idf:
    tweet = " ".join(d[0])
    tb_result2[tweet] = TextBlob(tweet).sentiment.polarity
tb_result2


# In[16]:


# Tranformar os valores numéricos do TextBlob em 'pos' ou 'neg'
for i in tb_result2:
    if tb_result2[i] < 0:
        tb_result2[i] = "neg"
    else:
        tb_result2[i] = "pos"


# In[17]:


# Tabela resumo dos resultados previstos vs resultados reais
summary_lst2 = [0,0,0]
for d1 in tb_result2.keys():
    for d2 in lemmatized_idf:
        tweet = " ".join(d2[0])
        if d1 == tweet:
            summary_lst2 = np.vstack((summary_lst2,[d1, tb_result2[d1], d2[1]]))

summary_lst2 = summary_lst2[1:,]
summary_lst2


# In[18]:


summary_df2 = pd.DataFrame(summary_lst2, columns = ["text", "predicted", "real"])
summary_df2.head(10)


# In[19]:


# Transformar 'pos' em 1 e 'neg' em 0
for i in range(len(summary_df2)):
    if summary_df2.iloc[i,1] == 'pos':
        summary_df2.iloc[i,1] = 1
    else:
        summary_df2.iloc[i,1] = 0
    if summary_df2.iloc[i,2] == 'pos':
        summary_df2.iloc[i,2] = 1
    else:
        summary_df2.iloc[i,2] = 0
summary_df2.head(10)


# In[20]:


# Avaliar performance do TextBlob com préprocessamento
acc, rec, prec, f1_score = evaluation(summary_df2["predicted"], summary_df2["real"])
print("accuracy: {} \nrecall: {} \nprecision: {} \nf1: {}".format(acc, rec, prec, f1_score))


# In[21]:


# TextBlob com préprocessamento (com tratamento de negação)
tb_result3 = {}
for d in lemmatized_idf_neg:
    tweet = " ".join(d[0])
    tb_result3[tweet] = TextBlob(tweet).sentiment.polarity
tb_result3


# In[22]:


# Tranformar os valores numéricos do TextBlob em 'pos' ou 'neg'
for i in tb_result3:
    if tb_result3[i] < 0:
        tb_result3[i] = "neg"
    else:
        tb_result3[i] = "pos"


# In[24]:


# Tabela resumo dos resultados previstos vs resultados reais
summary_lst3 = [0,0,0]
for d1 in tb_result3.keys():
    for d2 in lemmatized_idf_neg:
        tweet = " ".join(d2[0])
        if d1 == tweet:
            summary_lst3 = np.vstack((summary_lst3,[d1, tb_result3[d1], d2[1]]))

summary_lst3 = summary_lst3[1:,]
summary_lst3


# In[25]:


summary_df3 = pd.DataFrame(summary_lst3, columns = ["text", "predicted", "real"])
summary_df3.head(10)


# In[26]:


# Transformar 'pos' em 1 e 'neg' em 0
for i in range(len(summary_df3)):
    if summary_df3.iloc[i,1] == 'pos':
        summary_df3.iloc[i,1] = 1
    else:
        summary_df3.iloc[i,1] = 0
    if summary_df3.iloc[i,2] == 'pos':
        summary_df3.iloc[i,2] = 1
    else:
        summary_df3.iloc[i,2] = 0
summary_df3.head(10)


# In[27]:


# Avaliar performance do TextBlob com préprocessamento
acc, rec, prec, f1_score = evaluation(summary_df3["predicted"], summary_df3["real"])
print("accuracy: {} \nrecall: {} \nprecision: {} \nf1: {}".format(acc, rec, prec, f1_score))


# In[ ]:




