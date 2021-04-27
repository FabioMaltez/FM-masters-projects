#!/usr/bin/env python
# coding: utf-8

# In[1]:


#Corre o notebook relativo ao preprocessamento
get_ipython().run_line_magic('run', '"./Preprocessing.ipynb"')

#packages
from sklearn.model_selection import GridSearchCV
from sklearn.linear_model import LogisticRegression


# In[2]:


# Extrair corpus de tweets
df_train = pd.read_csv("../TM/data/en-sentiment/Tweets_EN_sentiment_train.csv", encoding="utf-8")
df_test = pd.read_csv("../TM/data/en-sentiment/Tweets_EN_sentiment_test.csv", encoding="utf-8")
df_train = df_train[df_train['text'].notna()]
df_test = df_test[df_test['text'].notna()]
df_train.reset_index(inplace=True)
df_test.reset_index(inplace=True)



# Certificar que todos os tweets são interpretados como strings
for i, text in enumerate(df_train["text"]):
    df_train.loc[i,"text"] = str(text)
for i, text in enumerate(df_test["text"]):
    df_test.loc[i,"text"] = str(text)

    
#TRAIN SET:
data_preprocessed, tokenization, lemmatized, data_neg, tokenization_neg, lemmatized_neg=preprocessing(df_train)
# Com tratamento da negação e sem usar os termos negados no idf
lemmatized_neg_idf1=remove_unnecessary_words(lemmatized_neg, voc_length = 11200, option_idf=True, use_negation_terms=False)
# Com tratamento da negação e usando os termos negados no idf
lemmatized_neg_idf2=remove_unnecessary_words(lemmatized_neg, voc_length = 12650, option_idf=True, use_negation_terms=True)
# Sem tratamento da negação
lemmatized_idf=remove_unnecessary_words(lemmatized, voc_length = 11200, option_idf=True)



#TEXT SET:
data_preprocessed_test, tokenization_test, lemmatized_test, data_neg_test, tokenization_neg_test, lemmatized_neg_test=preprocessing(df_test)
# Com tratamento da negação e sem usar os termos negados no idf
lemmatized_neg_idf1_test=remove_unnecessary_words(lemmatized_neg_test, voc_length = 11200, option_idf=True, use_negation_terms=False)
# Com tratamento da negação e usando os termos negados no idf
lemmatized_neg_idf2_test=remove_unnecessary_words(lemmatized_neg_test, voc_length = 12650, option_idf=True, use_negation_terms=True)
# Sem tratamento da negação
lemmatized_idf_test=remove_unnecessary_words(lemmatized_test, voc_length = 11200, option_idf=True)


# In[3]:


# POS
def feature_pos(data):
    # Colocar o POS depois das palavras
    text_pos = []

    for text in data:
        if len(text[0]) > 0:
            #Se a palavra não contiver "NEG_"
            if "NEG_" not in ' '.join(text[0]):
                features = ' '.join("{}_{}".format(w,p) for w,p in nltk.pos_tag(text[0]))
           #caso contenha "NEG_"
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
        
    #print(text_pos[:10])  
    
    return text_pos


# In[4]:


#cria o text_set e tags_set para os modelos
def text_tags(data):
    #display(data)
    text_set = []
    tags_set = []
    for i in data:
        text_set.append(i[0])
        tags_set.append(i[1])
    return text_set, tags_set


# In[5]:


# TF-IDF
def text_tags_with_join(data):
    # matrix TF-IDF
    text_set = []
    tags_set = []
    for i in data:
        text_set.append(' '.join(i[0]))
        tags_set.append(i[1])
    
    return text_set, tags_set



# In[6]:


#Naive Bayes
def NB(train_X, train_tags,test_X,test_tags):
    print("Naive Bayes: ")
    param={'alpha':[1, 1e-1, 1e-2]}
    
    classifier = MultinomialNB()
    model = GridSearchCV(classifier, param, cv=5)
    model.fit(train_X, train_tags)
    
    print("Best parameters: ",model.best_estimator_)
    y_pred = model.predict(test_X)
    
    #print(np.unique(y_pred))
    score = sklearn.metrics.accuracy_score(test_tags, y_pred)
    print("Mislabeled points: %d out of %d"% ((test_tags!=y_pred).sum(), test_X.shape[0]))
    print("Accuracy: ", metrics.accuracy_score(test_tags, y_pred))
    print("Precision: ", metrics.precision_score(test_tags, y_pred, average="binary"))
    print("Recall: ", metrics.recall_score(test_tags, y_pred, average="binary"))
    print("F1-measure: ", metrics.f1_score(test_tags, y_pred, average="binary"))
    return score


# In[7]:


def SVM(train_X, train_tags,test_X,test_tags):
    print("SVM: ")
    kernel= ['rbf', 'poly', 'sigmoid', "linear"]
    param = {'C': [1], 'gamma': [1]} #0.1,10, 100
    for i in kernel:
        print("Kernel: ", i)
        
        clf = svm.SVC(kernel=f'{i}')
        model = GridSearchCV(clf,param, verbose=1, cv=5)
        clf.fit(train_X, train_tags)
        
        print("Best parameters: ",model.best_estimator_)
        y_pred = clf.predict(test_X)

        print("Mislabeled points: %d out of %d"% ((test_tags!=y_pred).sum(), test_X.shape[0]))
        print("Accuracy: ", metrics.accuracy_score(test_tags, y_pred))
        print("\n")
        print("Precision: ", metrics.precision_score(test_tags, y_pred, average="binary"))
        print("Recall: ", metrics.recall_score(test_tags, y_pred, average="binary"))
        print("F1-measure: ", metrics.f1_score(test_tags, y_pred, average="binary"))


# In[ ]:


#TRAIN SET
vectorizer = CountVectorizer()
print("POS - Sem tratamento de negação")
train_pos = feature_pos(lemmatized_idf)
test_pos= feature_pos(lemmatized_idf_test)

train_set, train_tags=text_tags(train_pos)
test_set, test_tags=text_tags(test_pos)

train_X_pos = vectorizer.fit_transform(train_set)
test_X_pos= vectorizer.transform(test_set)

#modelos:
score=NB(train_X_pos, train_tags,test_X_pos,test_tags)
print("\n\n")
SVM(train_X_pos, train_tags, test_X_pos, test_tags)

print("\n\n\n")

print('\nPOS - Com tratamento de negação')
train_pos = feature_pos(lemmatized_neg_idf1)
test_pos= feature_pos(lemmatized_neg_idf1_test)

train_set, train_tags=text_tags(train_pos)
test_set, test_tags=text_tags(test_pos)

train_X_pos_neg = vectorizer.fit_transform(train_set)
test_X_pos_neg= vectorizer.transform(test_set)

#modelos:
score=NB(train_X_pos_neg, train_tags,test_X_pos_neg,test_tags)
print("\n\n")
SVM(train_X_pos_neg, train_tags, test_X_pos_neg, test_tags)
      
      
print("\n\n\n\n")      
vectorizer = TfidfVectorizer()

#TRAIN SET
print("TF-IDF - IDF - Sem tratamento de negação: Done")
train_set, train_tags=text_tags_with_join(lemmatized_idf)
test_set, test_tags=text_tags_with_join(lemmatized_idf_test)

train_X_tfidf = vectorizer.fit_transform(train_set)
test_X_tfidf= vectorizer.transform(test_set)
      
#modelos:    
score=NB(train_X_tfidf, train_tags, test_X_tfidf, test_tags)
print("\n\n")
SVM(train_X_tfidf, train_tags, test_X_tfidf, test_tags)

print("\n\n\n")

print("\nTF-IDF - IDF - Com tratamento de negação: Done")
train_set, train_tags=text_tags_with_join(lemmatized_neg_idf1)
test_set, test_tags=text_tags_with_join(lemmatized_neg_idf1_test)

train_X_tfidf_neg = vectorizer.fit_transform(train_set)
test_X_tfidf_neg= vectorizer.transform(test_set)
      
#modelos:
score=NB(train_X_tfidf_neg, train_tags, test_X_tfidf_neg, test_tags)
print("\n\n")
SVM(train_X_tfidf_neg, train_tags, test_X_tfidf_neg, test_tags)
      
      


# In[8]:


# Modelação sem pre-processamento
# tokenização
df_train_normal = []
df_test_normal = []

for i, text in enumerate(df_train['text']):
    t = nltk.word_tokenize(text)
    df_train_normal.append([t, df_train.loc[i, 'class']])
    
for i, text in enumerate(df_test['text']):
    t = nltk.word_tokenize(text)
    df_test_normal.append([t, df_test.loc[i, 'class']])

#print(df_train_normal[:5])

vectorizer = CountVectorizer()

print("SEM PREPROCESSAMENTO:")
train_text, train_tags = text_tags_with_join(df_train_normal)
test_text, test_tags = text_tags_with_join(df_test_normal)

train_text = vectorizer.fit_transform(train_text)
test_text = vectorizer.transform(test_text)

#modelos:
score=NB(train_text, train_tags, test_text, test_tags)
print("\n\n")
SVM(train_text, train_tags, test_text, test_tags)



# In[ ]:





# In[ ]:




