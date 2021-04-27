#!/usr/bin/env python
# coding: utf-8

# In[1]:


# Random Forest e Máxima Entropia


# In[1]:


import pandas as pd
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import GridSearchCV
import sklearn.metrics as metrics
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfVectorizer
import nltk


# In[2]:


# Extrair corpus de tweets
print('Upload corpur...')
df_train = pd.read_csv("../TM/data/en-sentiment/Tweets_EN_sentiment_train.csv", encoding="utf-8")
df_test = pd.read_csv("../TM/data/en-sentiment/Tweets_EN_sentiment_test.csv", encoding="utf-8")
print('corpus upload: done')
# Certificar que todos os tweets são interpretados como strings
for i, text in enumerate(df_train["text"]):
    df_train.loc[i,"text"] = str(text)
for i, text in enumerate(df_test["text"]):
    df_test.loc[i,"text"] = str(text)

# Correr Preprocessing.ipynb
get_ipython().run_line_magic('run', '"./Preprocessing.ipynb"')
    
#TRAIN SET:
print('\npreprocessing training data...')
data_preprocessed, tokenization, lemmatized, data_neg, tokenization_neg, lemmatized_neg=preprocessing(df_train)
# Com tratamento da negação e sem usar os termos negados no idf
lemmatized_neg_idf1=remove_unnecessary_words(lemmatized_neg, voc_length = 11200, option_idf=True, use_negation_terms=False)
# Com tratamento da negação e usando os termos negados no idf
lemmatized_neg_idf2=remove_unnecessary_words(lemmatized_neg, voc_length = 12650, option_idf=True, use_negation_terms=True)
# Sem tratamento da negação
lemmatized_idf=remove_unnecessary_words(lemmatized, voc_length = 11200, option_idf=True)
print('done')

#TEXT SET:
print('\npreprocessing test data...')
data_preprocessed_test, tokenization_test, lemmatized_test, data_neg_test, tokenization_neg_test, lemmatized_neg_test=preprocessing(df_test)
# Com tratamento da negação e sem usar os termos negados no idf
lemmatized_neg_idf1_test=remove_unnecessary_words(lemmatized_neg_test, voc_length = 11200, option_idf=True, use_negation_terms=False)
# Com tratamento da negação e usando os termos negados no idf
lemmatized_neg_idf2_test=remove_unnecessary_words(lemmatized_neg_test, voc_length = 12650, option_idf=True, use_negation_terms=True)
# Sem tratamento da negação
lemmatized_idf_test=remove_unnecessary_words(lemmatized_test, voc_length = 11200, option_idf=True)
print('done')


# In[3]:


# POS
def feature_pos(data):
    # Colocar o POS depois das palavras
    text_pos = []
    vectorizer = CountVectorizer()
    for text in data:
        if len(text[0]) > 0:
            if "NEG_" not in ' '.join(text[0]):
                features = ' '.join("{}_{}".format(w,p) for w,p in nltk.pos_tag(text[0]))
            else:
                sentence = text[0]
                sentence_treated = []
                for w in sentence:
                    sentence_treated.append(w.replace("NEG_", '')) # para poder analisar o pos da palavra sem o "NEG_"
                    
                features_list = []
                for i, w in enumerate(sentence_treated):
                    f1 = nltk.pos_tag(w)[0][1]
                    features_list.append("{}_{}".format(sentence[i],f1))
                features = ' '.join(features_list)
        text_pos.append((features, text[1]))
    return text_pos


# In[4]:


# Random Forest
def train_test_RF(train_data, train_tags, test_data, test_tags):
    """
    Train and evaluate Random Forest Model with hiperparameter tunning and cross validation
    Return: Accuracy, precision, recall and f1 score
    """
    param = {'n_estimators':[200], 'min_samples_leaf':[3,5]}
    rfc = RandomForestClassifier()
    rfmodel = GridSearchCV(rfc, param, cv=5)
    rfmodel.fit(train_data, train_tags)
    pred = rfmodel.predict(test_data)
    accuracy = metrics.accuracy_score(test_tags, pred)
    precision = metrics.precision_score(test_tags, pred, average='macro', zero_division=1)
    recall = metrics.recall_score(test_tags, pred, average='macro', zero_division=1)
    f1 = metrics.f1_score(test_tags, pred, average='macro', zero_division=1)
    print('Random Forest Model Evaluation')
    print('accuracy: {} | precision: {} \nrecall: {} | f1_score: {}'.format(accuracy, precision, recall, f1))
    return accuracy, precision, recall, f1


# In[5]:


# Mámixa Entropia (Regressão Logística)
def train_test_ME(train_data, train_tags, test_data, test_tags):
    """
    Train and evaluate Maximum Entropy Model with cross validation
    Return: Accuracy, precision, recall and f1 score
    """
    me = LogisticRegression()
    param = {'solver':['sag'], 'max_iter':[1000]}
    memodel = GridSearchCV(me, param, cv=5)
    memodel.fit(train_data, train_tags)
    pred = memodel.predict(test_data)
    accuracy = metrics.accuracy_score(test_tags, pred)
    precision = metrics.precision_score(test_tags, pred, average='macro', zero_division=1)
    recall = metrics.recall_score(test_tags, pred, average='macro', zero_division=1)
    f1 = metrics.f1_score(test_tags, pred, average='macro', zero_division=1)
    print('Maximum Entropy Model Evaluation')
    print('accuracy: {} | precision: {} \nrecall: {} | f1_score: {}'.format(accuracy, precision, recall, f1))
    return accuracy, precision, recall, f1

def tags_to_numeric(tags_lst):
    lst = []
    for tag in tags_lst:
        if tag == 'pos':
            lst.append(1)
        else:
            lst.append(0)
    print(lst[:3])
    return lst


# In[6]:


def text_tags(data):
    #display(data)
    text_set = []
    tags_set = []
    for i in data:
        text_set.append(i[0])
        tags_set.append(i[1])
    return text_set, tags_set

def text_tags_with_join(data):
    # matrix TF-IDF
    text_set = []
    tags_set = []
    for i in data:
        text_set.append(' '.join(i[0]))
        tags_set.append(i[1])
    
    return text_set, tags_set


# In[9]:


print('\nIniciar Feature Extraction...')
print("POS - Sem tratamento de negação")
vectorizer = CountVectorizer()
train_pos, train_tags_pos = text_tags(feature_pos(lemmatized_idf))
test_pos, test_tags_pos = text_tags(feature_pos(lemmatized_idf_test))
train_pos = vectorizer.fit_transform(train_pos)
test_pos = vectorizer.transform(test_pos)
train_tags_pos = tags_to_numeric(train_tags_pos)
test_tags_pos = tags_to_numeric(test_tags_pos)
print("POS - Com tratamento de negação")
train_pos_neg, train_tags_pos_neg = text_tags(feature_pos(lemmatized_neg_idf1))
test_pos_neg, test_tags_pos_neg = text_tags(feature_pos(lemmatized_neg_idf1_test))
print(train_pos_neg[:30])
train_pos_neg = vectorizer.fit_transform(train_pos_neg)
test_pos_neg = vectorizer.transform(test_pos_neg)
train_tags_pos_neg = tags_to_numeric(train_tags_pos_neg)
test_tags_pos_neg = tags_to_numeric(test_tags_pos_neg)

print("TF-IDF - Sem tratamento de negação")
vectorizer = TfidfVectorizer()
train_tfidf, train_tags_tfidf = text_tags_with_join(lemmatized_idf)
test_tfidf, test_tags_tfidf = text_tags_with_join(lemmatized_idf_test)
train_tfidf = vectorizer.fit_transform(train_tfidf)
test_tfidf = vectorizer.transform(test_tfidf)
train_tags_tfidf = tags_to_numeric(train_tags_tfidf)
test_tags_tfidf = tags_to_numeric(test_tags_tfidf)
print("TF-IDF - Com tratamento de negação")
train_tfidf_neg, train_tags_tfidf_neg = text_tags_with_join(lemmatized_neg_idf1)
test_tfidf_neg, test_tags_tfidf_neg = text_tags_with_join(lemmatized_neg_idf1_test)
train_tfidf_neg = vectorizer.fit_transform(train_tfidf_neg)
test_tfidf_neg = vectorizer.transform(test_tfidf_neg)
train_tags_tfidf_neg = tags_to_numeric(train_tags_tfidf_neg)
test_tags_tfidf_neg = tags_to_numeric(test_tags_tfidf_neg)
print('done\n')


# In[10]:


print('\nINICIAR TREINO E TESTE DE MODELOS\n')
print('MÁXIMA ENTROPIA')
print('Máxima Entropia - POS - sem tratamento de negação')
me_pos = train_test_ME(train_pos, train_tags_pos, test_pos, test_tags_pos)
me_pos
print('Máxima Entropia - POS - com tratamento de negação')
me_pos_neg = train_test_ME(train_pos_neg, train_tags_pos_neg, test_pos_neg, test_tags_pos_neg)
me_pos_neg
print('Máxima Entropia - TF-IDF - sem tratamento de negação')
me_tfidf = train_test_ME(train_tfidf, train_tags_tfidf, test_tfidf, test_tags_tfidf)
me_tfidf
print('Máxima Entropia - POS - com tratamento de negação')
me_tfidf_neg = train_test_ME(train_tfidf_neg, train_tags_tfidf_neg, test_tfidf_neg, test_tags_tfidf_neg)
me_tfidf_neg

print('\nRANDOM FOREST')
print('Random Forests - POS - sem tratamento de negação')
rf_pos = train_test_RF(train_pos, train_tags_pos, test_pos, test_tags_pos)
rf_pos
print('Random Forest - POS - com tratamento de negação')
rf_pos_neg = train_test_RF(train_pos_neg, train_tags_pos_neg, test_pos_neg, test_tags_pos_neg)
rf_pos_neg
print('Random Forests - TF-IDF - sem tratamento de negação')
rf_tfidf = train_test_RF(train_tfidf, train_tags_tfidf, test_tfidf, test_tags_tfidf)
rf_tfidf
print('Random Forest - TF-IDF - com tratamento de negação')
rf_tfidf_neg = train_test_RF(train_tfidf_neg, train_tags_tfidf_neg, test_tfidf_neg, test_tags_tfidf_neg)
rf_tfidf_neg
print('DONE!')


# In[11]:


# Modelação sem préprocessamento
# tokenização
df_train_normal = []
df_test_normal = []

for i, text in enumerate(df_train['text']):
    t = nltk.word_tokenize(text)
    df_train_normal.append([t, df_train.loc[i, 'class']])
    
for i, text in enumerate(df_test['text']):
    t = nltk.word_tokenize(text)
    df_test_normal.append([t, df_test.loc[i, 'class']])

print(df_train_normal[:5])


# In[12]:


# Feature Extraction
vectorizer = CountVectorizer()
train_text, train_tags = text_tags_with_join(df_train_normal)
test_text, test_tags = text_tags_with_join(df_test_normal)

train_text = vectorizer.fit_transform(train_text)
test_text = vectorizer.transform(test_text)
train_tags = tags_to_numeric(train_tags)
test_tags = tags_to_numeric(test_tags)


# In[13]:


print('MÁXIMA ENTROPIA')
me_norm = train_test_ME(train_text, train_tags, test_text, test_tags)
me_norm

print('\nRANDOM FOREST')
rf_norm = train_test_RF(train_text, train_tags, test_text, test_tags)
rf_norm


# In[ ]:




