#!/usr/bin/env python
# coding: utf-8

# In[1]:


get_ipython().run_line_magic('run', '"./Preprocessing.ipynb"')
from keras.models import Sequential
from keras import layers
import tensorflow as tf
from keras.backend import clear_session
from keras.preprocessing.text import Tokenizer
from keras.preprocessing.sequence import pad_sequences
from keras.models import Sequential
from keras import layers
from keras.wrappers.scikit_learn import KerasClassifier
from sklearn.model_selection import RandomizedSearchCV


# In[33]:


# Extrair corpus de tweets
df_train = pd.read_csv("../TM/data/en-sentiment/Tweets_EN_sentiment_train.csv", encoding="utf-8")
df_test = pd.read_csv("../TM/data/en-sentiment/Tweets_EN_sentiment_test.csv", encoding="utf-8")

df_train = df_train[df_train['text'].notna()]
df_test = df_test[df_test['text'].notna()]

df_train.reset_index(inplace=True)
df_test.reset_index(inplace=True)

#Transformar as tags em números
df_train.loc[df_train['class'] == 'pos','class']=1
df_train.loc[df_train['class'] == 'neg','class']=0
df_test.loc[df_test['class'] == 'pos','class']=1
df_test.loc[df_test['class'] == 'neg','class']=0


# In[34]:


sentences_train=df_train["text"]
sentences_test=df_test["text"]
y_train=df_train["class"]
y_test=df_test["class"]


# In[35]:


input_dim = df_train.shape[1]


# In[36]:


tokenizer = Tokenizer(num_words=5000)
tokenizer.fit_on_texts(sentences_train)

X_train = tokenizer.texts_to_sequences(sentences_train)
X_test = tokenizer.texts_to_sequences(sentences_test)

vocab_size = len(tokenizer.word_index) + 1  # Adding 1 because of reserved 0 index

print(sentences_train[0])
print(X_train[0])


# In[37]:


#Para resolver o problema de haver tamanhos de sequencias diferentes usamos o pad_sequence
maxlen = 100

X_train = pad_sequences(X_train, padding='post', maxlen=maxlen)
X_test = pad_sequences(X_test, padding='post', maxlen=maxlen)

print(X_train[0, :])


# In[47]:


#Embedding layer pega nos valores calculados anteriormente e mapeia-os para um vector the embeddings
embedding_dim = 50

model = Sequential()
model.add(layers.Embedding(input_dim=vocab_size, 
                           output_dim=embedding_dim, 
                           input_length=maxlen))
#model.add(layers.Conv1D(128, 5, activation='relu'))
model.add(layers.GlobalMaxPool1D())  #forma de reduzir o tamanho dos vetores 
model.add(layers.Flatten())  #camada Flatten prepara os outputs da camada embeddings para entrar na camada Dense
model.add(layers.Dense(10, activation='relu'))
model.add(layers.Dense(1, activation='sigmoid'))
model.compile(optimizer='adam',
              loss='binary_crossentropy',
              metrics=['accuracy'])
model.summary()


# In[48]:


y_train = np.asarray(y_train).astype('float32')
y_test = np.asarray(y_test).astype('float32')


# In[49]:


history = model.fit(X_train, y_train,
                    epochs=10,
                    verbose=2,
                    validation_data=(X_test, y_test),batch_size=50)

loss, accuracy = model.evaluate(X_train, y_train, verbose=False)
print("Training Accuracy: {:.4f}".format(accuracy))
loss, accuracy = model.evaluate(X_test, y_test, verbose=False)
print("Testing Accuracy:  {:.4f}".format(accuracy))

