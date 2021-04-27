#!/usr/bin/env python
# coding: utf-8

# In[1]:


get_ipython().run_line_magic('run', '"./Preprocessing.ipynb"')


# In[2]:


#Load dos dados
data_train=pd.read_csv("../TM/data/en-sentiment/Tweets_EN_sentiment_train.csv")
data_test=pd.read_csv("../TM/data/en-sentiment/Tweets_EN_sentiment_test.csv")
lexicon=pd.read_csv("../TM/data/en/NCR-lexicon.csv")

#remover os NA's
data_train = data_train[data_train['text'].notna()]
data_test = data_test[data_test['text'].notna()]

data_train.reset_index(inplace=True)
data_test.reset_index(inplace=True)


# In[3]:


#Adicionar coluna "total" ao léxico de sentimentos 
lexicon["Total"]=lexicon["Positive"]-lexicon["Negative"]
lexicon= lexicon[lexicon['Total'] != 0]
lexicon.reset_index(inplace=True)


# In[4]:


# Léxico
#ANÁLISE DE SENTIMENTOS ATRAVÉS DE LEXICO DE SENTIMENTOS
def lex(data, lexicon):
    sentiment=np.zeros(len(data))
    
    for i, row in enumerate(data):
        words_counted=0
        for word in row[0]:
            if "NEG_" in word:
                word=word.replace("NEG_", '')
                for n, word_lex in enumerate(lexicon["English"]):
                    if word==word_lex:
                        words_counted+=1
                        sentiment[i]+=((lexicon.loc[n, "Total"])*(-1))
            else:
                for n, word_lex in enumerate(lexicon["English"]):
                    if word==word_lex:
                        words_counted+=1
                        sentiment[i]+=lexicon.loc[n, "Total"]
        #print("row:", i, " | number_words: ", len(row[0]), " | number_words_counted ", words_counted, " | sentiment: ", sentiment[i])´#Já não faz muito sentido uma vez que eliminei todas as palavras de sentimento=0 no lexicon
    return sentiment


# In[5]:


#Definição do sentimento associado consuante a soma dos sentimentos das palavras em questão 
def insert_sentiment(data, sentiment):
    data["lexicon"]=sentiment
    for i in range (0,len(data)):
        if data.loc[i,"lexicon"]>0:
            data.loc[i,"sentiment_lexicon"]="pos"
        elif data.loc[i,"lexicon"]<0:
            data.loc[i,"sentiment_lexicon"]="neg"
        else:
            data.loc[i,"sentiment_lexicon"]="neutro"
    return data


# In[6]:


def results(data):
    #display(data)
    results=data.copy(deep=True)
    
    #Colocando todos os "neutros" como positivos:
    print("NEUTRO AS POSITIVE:")
    #display(neutro_as_positive)
    results.loc[results["sentiment_lexicon"]=="neutro", "sentiment_lexicon"]="pos"
                               
    results.loc[results['sentiment_lexicon'] == 'pos','sentiment_lexicon']=1
    results.loc[results['sentiment_lexicon'] == 'neg','sentiment_lexicon']=0
    results.loc[results['class'] == 'pos','class']=1
    results.loc[results['class'] == 'neg','class']=0

    y=results["class"].astype(int)
    y_pred=results["sentiment_lexicon"].astype(int)
    
    #display(neutro_as_positive)
    display(metrics.confusion_matrix(y, y_pred))
    print("Accuracy: ", metrics.accuracy_score(y, y_pred))
    print("Precision: ", metrics.precision_score(y,y_pred, average="macro"))
    print("Recall: ", metrics.recall_score(y, y_pred, average="macro"))
    print("F1-measure: ", metrics.f1_score(y, y_pred, average="macro"))


# In[7]:


tokenization=[] 
for i, row in enumerate(data_test["text"]):
    tok=nltk.word_tokenize(row)
    tokenization.append([tok, data_test.loc[i, "class"]])


# In[8]:


print("Sem preprocessamento (apenas tokenização)")
test_sentiment=lex(tokenization, lexicon)
test_lexicon=insert_sentiment(data_test, test_sentiment)

results(test_lexicon)


# In[9]:


#TEST_SET:
inicio = time.time()
print("TEST SET:")
data_test_preprocessed, test_tokenization, test_lemmatized, data_test_neg, test_tokenization_neg, test_lemmatized_sentence_neg=preprocessing(data_test)
print("..Preprocessing done..\n\n")
print("REMOVER AS PALAVRAS DESNECESSÁRIAS:\n ")
print("Com tratamento da negação:")
test_lemmatized_neg_no_stopwords=remove_unnecessary_words(test_lemmatized_sentence_neg, voc_length = 11200, option_idf=False)
print(test_lemmatized_neg_no_stopwords[0:10])
print("\n")
print("Sem tratamento da negação:")
test_lemmatized_no_stopwords=remove_unnecessary_words(test_lemmatized, voc_length = 11200, option_idf=False)
print(test_lemmatized_no_stopwords[0:10])
print("\n\n")



print("ANÁLISE DE SENTIMENTOS ATRAVÉS DE LEXICO DE SENTIMENTOS:")
print("Com tratamento da negação:")
test_neg_sentiment=lex(test_lemmatized_neg_no_stopwords, lexicon)
test_neg_lexicon=insert_sentiment(data_test, test_neg_sentiment)
results(test_neg_lexicon)
print("\n")
print("Sem tratamento da negação")
test_sentiment=lex(test_lemmatized_no_stopwords, lexicon)
test_lexicon=insert_sentiment(data_test, test_sentiment)
results(test_lexicon)
fim = time.time()
print("time :", fim - inicio)


# In[10]:


# https://stackoverflow.com/questions/23384351/how-to-add-tags-to-negated-words-in-strings-that-follow-not-no-and-never?rq=1

