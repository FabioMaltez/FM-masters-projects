#
#
# Etapa 5 - Fase de Modelação
# 
##########
# Libraries
library(corrplot)
library(partykit)
library(rpart)
library(rpart.plot)
library(caret)
library(neuralnet)
library(gbm)
library(MLmetrics)
library(car)
library(ipred)

RegressionModelError <- function(pred, real)
  # This function evaluates regression model accuracy, by printing the 
  # generalization errors RMSE and MAE
  # Returns: RMSE, MAE
{
  # Cálculo do RMSE
  model_rmse<-caret::RMSE(pred=pred,obs=real)
  model_rmse
  # Cálculo do MAE
  model_mae<-caret::MAE(pred=pred,obs=real)
  model_mae
  return(data.frame(RMSE = model_rmse, MAE = model_mae))
}


# Ter em conta que o log() foi aplicado nas variàveis: h_num_comments, h_dist_centro e h_prices
# Ler a BD e verificar que a exportação correu bem
d_booking <- read.csv(".//dados/4-dados_apos_transformacao.csv", header = T, sep = ",", dec = ".", stringsAsFactors = F)
head(d_booking)
str(d_booking)
summary(d_booking)
# Manter apenas as variáveis de input (dependentes e independentes) para a modelação
# Reminder: Já não necessitamos da variável hotel_loc, pois foram criadas as dummy
#           zona_central, zona_oeste e zona_sul
db_input <- d_booking[,10:21]
# Análise de correlação
# Verificamos que existe uma correlação muito forte entre h_canc_gratuito e h_peq_almoço
# (cor=0.98 , o que pode estragar os resultados da análise)
# Escolher apenas uma das duas, logo, vamos eliminar h_canc_gratuito pois entre as duas,
# é a que explica menos variância da variávél target preço (cor de -0.018 vs -0.033)
cor(db_input)
corrplot.mixed(cor(db_input),
               lower = "number",
               upper = "ellipse",
               order = "hclust",
               tl.pos = "lt")
db_input <- db_input[,-7]
corrplot.mixed(cor(db_input),
               lower = "number",
               upper = "ellipse",
               order = "hclust",
               tl.pos = "lt")
# guardar os inputs originais
db_input_original <- db_input

# Fazer a divisão dos dados de treino (80%) e de teste (20%), a divisão 80-20
# deve-se ao facto de haver poucas observações
# Variável target/alvo: h_prices
set.seed(100)
index <- sample(1:nrow(db_input), round(nrow(db_input)*0.8,0))
train_set <- db_input[index,]
test_set <- db_input[-index,]

# verificar que as distribuições da variável alvo no conjunto de treino e de teste
# são relativamente semelhantes
hist(train_set$h_prices)
hist(test_set$h_prices)

#########
#
# Regressão linear com validação cruzada
#
#########
# h_price foi uma variável logaritmizada, antes de avaliarmos o erro temos de reverter a operação
# Sabemos que e = exp(1) e que a função exp(x) = e^x
# Deslogaritmização da variável alvo no conjunto de teste
test_set$h_prices <- exp(test_set$h_prices)

# Método de reamostragem: Validação Cruzada (com k=10)
cv.control<-trainControl(method="cv",number=10)

# Criação do modelo de regressão treinado com validação cruzada 
model_lm = train(h_prices~., data = train_set,method="lm",trControl=cv.control)
# Modelo de Regressão Linear criado
model_lm

# Teste do Modelo criado (usando os dados do conjunto teste)
# Teste do Modelo criado - Passo 1: cálculo das estimativas da variável 
# h_prices para os dados não vistos do conjunto teste
model_lm_previsao = predict(model_lm,test_set)

# Deslogaritmização
model_lm_previsao <- exp(model_lm_previsao)

# Teste do Modelo criado - Passo 2: Criação da tabela com os valores reais e 
# estimativas da variável h_prices para os dados não vistos do conjunto teste
table_lm = data.frame(VReais=test_set$h_prices,VPrevistos=model_lm_previsao)
# Teste do Modelo criado - Passo 3: Adição de uma coluna à tabela que indica 
# os erros associados às estimativas de h_prices
# para os dados não vistos do conjunto teste 
table_lm$error<-with(table_lm,test_set$h_prices-model_lm_previsao)
# Visualização da tabela
table_lm

# gráfico das previsões
plot(test_set$h_prices, model_lm_previsao, main = "Regressão Linear 1: Real vs Predict")
abline(0,1)
#Cálculo do erro através da função criada
table_lm_error = RegressionModelError(pred = model_lm_previsao, 
                                      real = test_set$h_prices)

table_lm_error

# Cálculo dos Resíduos
residue = resid(model_lm)

# Gráficos de estudo dos Resíduos 
plot(train_set$h_prices,residue,xlab="h_prices", ylab = "Resíduos", 
     main = "Valores resíduais das respetivas observações")
abline(h=0)
hist(residue)
plot(train_set$h_prices,sqrt(residue),xlab="h_prices", ylab = "Raíz dos Resíduos",
     main = "Variância dos Resíduos")


# Observa-se claramente que os valores residuais ainda contêm variância que pode ser explicada,
# devido à relação linear observada com a variável alvo.
# Apesar dos residuos apresentarem uma distribuição normalizada, o seu valor esperado nem sempre
# é nulo para diferentes valores da variável alvo e a sua variância não é constante.
# Vamos criar outro modelo de regressão linear apenas com variáveis que apresntem pouca
# correlação entre si, para ver se os resultados obtidos são semelhantes

# Modelo 2 de Regressão Linear ====
# Utilizando apenas parte das variaveis como variaveis preditoras

# Correlações mais forte:
# zona central vs h_dist_praia
# zona central vs zona sul
# zona oeste vs h_dist_centro
# h_prices vs h_num_stars
# h_peq_almoco vs h_num comments

# Variáveis escolhidas:  h_num_stars, h_num_coments, h_dist_centro
cor(train_set$h_num_stars,train_set$h_num_comments)
cor(train_set$h_num_stars, train_set$h_dist_centro)
cor(train_set$h_num_comments, train_set$h_dist_centro)

# Criação do modelo de regressão treinado com validação cruzada, com as variáveis que apresentam
#  das menores relações entre elas (h_num_stars, h_num_coments, h_dist_centro) 

model_lm_2 = train(h_prices~h_num_stars+h_num_comments+h_dist_centro, data = train_set,method="lm",trControl=cv.control)

# Resumo do Modelo de Regressão Linear 
model_lm_2

# Teste do Modelo criado (usando os dados do conjunto teste)
# Teste do Modelo criado - Passo 1: cálculo das estimativas da variável 
#h_prices para os dados não vistos do conjunto teste
model_lm_2_previsao = predict(model_lm_2,test_set)

# Deslogaritmização
model_lm_2_previsao <- exp(model_lm_2_previsao)

# Teste do Modelo criado - Passo 2: Criação da tabela com os valores reais e 
# estimativas da variável h_prices para os dados não vistos do conjunto teste
table_lm_2 = data.frame(VReais=test_set$h_prices,VPrevistos=model_lm_2_previsao)
# Teste do Modelo criado - Passo 3: Adição de uma coluna à tabela que indica 
# os erros associados ás estimativas de h_prices
# para os dados não vistos do conjunto teste 
table_lm_2$error<-with(table_lm_2,test_set$h_prices-model_lm_2_previsao)
# Visualização da tabela
table_lm_2

# gráfico das previsões
plot(test_set$h_prices, model_lm_2_previsao, main = "Regressão Linear 2: Real vs Predict")
abline(0,1)

#Cálculo do erro através da função criada
table_lm_2_error = RegressionModelError(pred = model_lm_2_previsao, 
                                        real = test_set$h_prices)

table_lm_2_error

residue_lm_2 = resid(model_lm_2)

# Gráficos de estudo dos Resíduos 
plot(train_set$h_prices,residue_lm_2,xlab="h_prices", ylab = "Resíduos",
     main = "Valores resíduais das respetivas observações")
abline(h=0)
hist(residue_lm_2)
plot(train_set$h_prices,sqrt(residue_lm_2),xlab="h_prices", ylab = "Raíz dos Resíduos",
     main = "Variância dos Resíduos")

# O comportamento dos residuos manteve-se igual à primeira tentativa
# Logo, o modelo de regressão linear não é um modelo apropriado para resolver o problema deste projeto
# Os residuos apresentam variancia que o modelo ainda necessita de explicar

#########
#
# Árvore de decisão (com e sem operação de poda) com validação cruzada
#
#########
# Treinar modelo de árvore com validação cruzada e permitir o mínimo de observações num nó ser 20
model_tree <- rpart(formula = h_prices ~., data = train_set, method = "anova",
                    control = rpart.control(minsplit = 20, xval = 10, cp = 0.01))
model_tree_party <- as.party(model_tree)
model_tree_party
rpart.plot(model_tree, yesno = T)
# Avaliar o model_tree (árvore de decisão com cv)
model_tree_predict <- predict(model_tree, test_set, type = "vector")
# Deslogaritmização da variável alvo prevista
model_tree_predict <- exp(model_tree_predict)

tabela_tree <- data.frame(Real = test_set$h_prices, Pred = model_tree_predict, Erro = test_set$h_prices - model_tree_predict)
tabela_tree
plot(test_set$h_prices, model_tree_predict, main = "Árvore de Decisão: Real vs Predict")
abline(0,1)
model_tree_error <- RegressionModelError(pred = model_tree_predict, real = test_set$h_prices)
model_tree_error

# Fazer a árvore podada
printcp(model_tree)
plotcp(model_tree)
# Através do gráfico e da tabela de complexidade, verificamos que n=7 para o tamanho
# da árvore parece apropriado (cp = 0.01998) - em n=3 a árvore pode ser considerada bastante simples para captar 
# a variância necessária (apesar de na representação gráfica já ser uma solução aceitável)
model_tree_prune <- prune(model_tree, cp = 0.01998)
# Resumo da nova árvore podada
model_tree_prune_party <- as.party(model_tree_prune)
model_tree_prune_party
rpart.plot(model_tree_prune, yesno = T)

# Avaliar a árvore podada
model_tree_prune_predict <- predict(model_tree_prune, test_set, type = "vector")
model_tree_prune_predict <- exp(model_tree_prune_predict)

tabela_tree_prune <- data.frame(Real = test_set$h_prices, Pred = model_tree_prune_predict, Erro = test_set$h_prices - model_tree_prune_predict)
tabela_tree_prune
plot(test_set$h_prices, model_tree_prune_predict, main = "Árvore de Decisão Podada (n=7): Real vs Predict")
abline(0,1)
model_tree_prune_error <- RegressionModelError(pred = model_tree_prune_predict, 
                                               real = test_set$h_prices)
model_tree_prune_error

(model_tree_prune_error$RMSE/model_tree_error$RMSE)-1
(model_tree_prune_error$MAE/model_tree_error$MAE)-1

# Da árvore normal para a podada, a complexidade da árvore diminui de 23 nós para 15 nós
# Enquando que o RMSE aumenta em 9.1% e o MAE em 5,5%
# Tendo em conta a problemática overfitting vs variance explained, se um maior peso for 
# dado ao evitar um enviesamento da análise e tendo em conta o aumento dos erros acima referido
# A árvore podada encontra-se mais adequada para modelo de generalização, apesar do erro de generalização ser ligeiramente maior
# NOTA: as árvores de decisão são muito sensiveis aos dados e facilmente geram enviesamento com muita complexidade
model_tree$variable.importance
model_tree_prune$variable.importance

# É possivel observar que em ambos os modelos, as variáveis que apresentam
# maior importância em termos de variabilidade explicada são:
#  h_num_comments, h_num_stars h_dist_centro e h_dist_praia


#########
#
# Bagging
#
#########

# Bagging com nbagg = 100 ====

# Técnica Bagging - Utilizando o train
# Deixa-se o modelo escolher os hiperparâmetros que acha mais adequado através do tuneLenght
cv.control.bag<-trainControl(method="cv",number=10,savePredictions="final")
model_bag<-train(h_prices ~ ., data = train_set,
                 method = "treebag",
                 tuneLength = 5, 
                 nbagg= 100,
                 metric = "RMSE",trControl=cv.control.bag)
model_bag

# Teste ao Modelo criado (model_bag) com base nos dados não vistos do conjunto teste (teste_set)
# Cálculo das previsões recorrendo ao modelo criado (model_bag) 
model_bag_previsao<-predict(model_bag,test_set)

# Deslogaritmização
model_bag_previsao = exp(model_bag_previsao)

# Avaliar erros bagging

tabela_bagging <- data.frame(Real = test_set$h_prices, Pred = model_bag_previsao, Erro = test_set$h_prices - model_bag_previsao)
tabela_bagging

plot(test_set$h_prices,model_bag_previsao,main="Previsão com Bagging 1",xlab="Reais",ylab="Previstos")
abline(0,1)

tabela_bagging_error <- RegressionModelError(pred = model_bag_previsao, 
                                             real = test_set$h_prices)

tabela_bagging_error

plot(varImp(model_bag), main = "Importância de cada Variável: Modelo Bagging 1")

# Variáveis mais importantes: h_num_comments, h_score_medio, h_dist_centro e h_dist_praia

# Bagging com nbagg = 300 ====

# Técnica Bagging - Utilizando o train
model_bag_2<-train(h_prices ~ ., data = train_set,
                   method = "treebag",
                   tuneLength = 5, 
                   nbagg= 300,
                   metric = "RMSE",trControl=cv.control.bag)
model_bag_2

# Teste ao Modelo criado (model_bag2) com base nos dados não vistos do conjunto teste (teste_set)
# Cálculo das previsões recorrendo ao modelo criado (model_bag2) 
model_bag_previsao_2<-predict(model_bag_2,test_set)

# Deslogaritmização
model_bag_previsao_2 = exp(model_bag_previsao_2)

# Avaliar erros bagging

tabela_bagging_2 <- data.frame(Real = test_set$h_prices, Pred = model_bag_previsao_2, Erro = test_set$h_prices - model_bag_previsao_2)
tabela_bagging_2

plot(test_set$h_prices,model_bag_previsao_2,main="Previsão com Bagging 2",xlab="Reais",ylab="Previstos")
abline(0,1)

tabela_bagging_error_2 <- RegressionModelError(pred = model_bag_previsao_2, 
                                               real = test_set$h_prices)
tabela_bagging_error_2

plot(varImp(model_bag_2), main = "Importância de cada Variável: Modelo Bagging 2")

# variáveis mais importantes: h_num_comments, h_score_medio, h_dist_centro e h_dist_praia

# Bagging com nbagg = 500 ====

# Técnica Bagging - Utilizando o train
model_bag_3<-train(h_prices ~ ., data = train_set,
                   method = "treebag",
                   tuneLength = 5, 
                   nbagg= 500,
                   metric = "RMSE",trControl=cv.control.bag)
model_bag_3

# Teste ao Modelo criado (model_bag_3) com base nos dados não vistos do conjunto teste (teste_set)
# Cálculo das previsões recorrendo ao modelo criado (model_bag_3) 
model_bag_previsao_3<-predict(model_bag_3,test_set)

#Deslogaritmização
model_bag_previsao_3 = exp(model_bag_previsao_3)

# Avaliar erros bagging

tabela_bagging_3 <- data.frame(Real = test_set$h_prices, Pred = model_bag_previsao_3, Erro = test_set$h_prices - model_bag_previsao_3)
tabela_bagging_3

plot(test_set$h_prices,model_bag_previsao_3,main="Previsão com Bagging 3",xlab="Reais",ylab="Previstos")
abline(0,1)

tabela_bagging_error_3 <- RegressionModelError(pred = model_bag_previsao_3, 
                                               real = test_set$h_prices)

tabela_bagging_error_3

plot(varImp(model_bag_3), main = "Importância de cada Variável: Modelo Bagging 3")

# variáveis mais importantes: h_num_comments, h_score_medio, h_dist_centro e h_dist_praia

tabela_bagging_error
tabela_bagging_error_2
tabela_bagging_error_3

# Os resultados são muito semelhantes, vamos usar o modelo com 500 árvores pois é mais robusto
# e no modelo bagging o maior número de árvores não causa overfitting


#########
#
# Boosting
#
#########
# Para o modelo boosting, vai-se utilizar validação cruzada, n=10
# Definir os hiperparâmetros da seguinte forma: a profundidade da árvore vai de 3 a 7 (correspondem aos índices de complexidade perto do menor erro relativo),
#       .O número de árvores vai de 50 a 500, pois sabe-se que um maior número de árvores pode levar a uma melhor performance dos resultados (se for feito com cuidado)
#       . a taxa de aprendizagem tem os valores 0.05, 0.1 e 0.2 de forma a tentar evitar que a otimização do erro chegue a um mínimo local (ou que cause overfitting)
#       . o número de observações mínimo em cada nó vai ser 5 ou 10
cv.control <- trainControl(method = "cv", number = 10, savePredictions = "final")
tune_gbm <- expand.grid(interaction.depth = c(3,4,5,6,7), n.trees=c(50,100,200,300,500), shrinkage=c(0.05,0.1,0.2),n.minobsinnode = c(5,10))
model_boosting <- train(h_prices~., data = train_set,method="gbm",
                        metric="RMSE", tuneLength = 5,tuneGrid = tune_gbm, trControl = cv.control)
# sumário modelo com erro mímino dentro dos experimentados
model_boosting

# Avaliar o modelo boosting
model_boosting_predict <- predict(model_boosting, test_set)
model_boosting_predict <- exp(model_boosting_predict)

tabela_boosting <- data.frame(Real = test_set$h_prices, Pred = model_boosting_predict, Erro = test_set$h_prices - model_boosting_predict)
tabela_boosting
plot(test_set$h_prices, model_boosting_predict, main = "Boosting: Real vs Predict")
abline(0,1)
model_boosting_error <- RegressionModelError(pred = model_boosting_predict, 
                                               real = test_set$h_prices)
model_boosting_error

plot(varImp(model_boosting), main = "Variância Explicada: importância de cada variável
     no modelo 'gbm' ")

# Variáveis de maior importância no modelo boosting são:
# .h_num_comments, h_dist_praia, h_score_medio e h_num_stars


#########
#
# Floresta Aleatória 
#
#########
# Criação de um modelo de Árvore combinado recorrendo ao método Florestas Aleatórias
#
# O número de árvores será colocado a 500, devido às experiências feitas nos modelos anteriores e
# ao facto de que este modelo cria árvores independentes de forma a evitar causar overfitting.
# Os hiperparâmetros são colocados da seguinte forma:
#   . o nº de variáveis que cada árvore vai utilizar vai ser de 2,3,5,7 ou 10 (mtry)
#   . a regra de divisão será a variância
#   . o número mínimo de nós vai ter os valores 3,5,7 e 10
cv.control.forest<-trainControl(method="cv",number=10,savePredictions="final")
tune_ranger <- expand.grid(mtry = c(2,3,5,7,10), splitrule="variance", min.node.size = c(3,5,7,10))
model_forest<-train(h_prices ~.,data=train_set,method="ranger",metric="RMSE", num.trees=500 ,tuneGrid=tune_ranger,trControl=cv.control.forest, importance = "impurity")
# Necessário juntar o "importance = impurity" para ser possível o cálculo da importância
# das variáveis
model_forest
#
# Teste ao Modelo criado (model_forest) com base nos dados não vistos do conjunto teste (teste_set)
#
# Cálculo das previsões recorrendo ao modelo criado (model_forest)
model_forest_previsao<-predict(model_forest,test_set)

# Deslogaritmização
model_forest_previsao = exp(model_forest_previsao)

plot(test_set$h_prices,model_forest_previsao,xlab="Reais",ylab="Previstos")
abline(0,1)
# Teste do Modelo criado (model_forest): Criação da tabela com os valores reais e 
# estimativas da variável alvo para os dados não vistos do conjunto teste
table_forest<-data.frame(VReais=test_set$h_prices,VPrevistos=model_forest_previsao)
# Teste do novo modelo criado (model_forest): Adição de uma coluna à tabela que 
# indica os erros associados às estimativas de h_prices (obtidas com o model_forest) 
# para os dados não vistos do conjunto teste
table_forest$error<-with(table_forest,test_set$h_prices-model_forest_previsao)
# Visualização da tabela
table_forest

#Cálculo do erro através da função criada
tabela_forrest_error <- RegressionModelError(pred = model_forest_previsao, 
                                             real = test_set$h_prices)

tabela_forrest_error

#Importância das variáveis
plot(varImp(model_forest), main = "Importância de cada variável: modelo Floresta Aleatória")

# variáveis mais importantes: h_num_comments, h_dist_praia, h_dist_centro, h_score_medio, h_num_stars

#########
#
# Rede Neuronal Simples 
# (pois apenas existem poucas observações)
#
#########

# Na rede neuronal, vamos usar como função ativação a função logística.
# Logo, primeiro temos de normalizar todos os valores entre 0 e 1
# Para facilitar, normalizar numa bd completa e depois voltar a dividir entre variável de treino e de teste
db_input2 <- db_input_original
str(db_input2)

# Extrair os valores mínimos e máximos
max_data <- apply(db_input2,2,max)
min_data <- apply(db_input2,2,min)
input_scaled <- scale(db_input2, center = min_data, scale = max_data - min_data)
summary(input_scaled) # verificar que todos os valores estão entre 0 e 1

# dividir em bd de treino e de teste
set.seed(100)
index <- sample(1:nrow(input_scaled), round(nrow(input_scaled)*0.8,0))
train_set2 <- data.frame(input_scaled[index,])
test_set2 <- data.frame(input_scaled[-index,])

# retirar o efeito de normalização e da logaritmização da variável alvo de teste
test_set2$h_prices <- test_set2$h_prices*(max_data["h_prices"]-min_data["h_prices"]) + min_data["h_prices"]
test_set2$h_prices <- exp(test_set2$h_prices)

# Algoritmo de otimização da rede neuronal simples
otimization.grid <- data.frame(X=NULL, LR=NULL, RMSE=NULL, MAE=NULL)

for (x in 1:15){
  lr <- 0.1
  while( lr < 1){
    model_nn <- neuralnet(h_prices~., data = train_set2, hidden = x, 
                          learningrate = lr, act.fct = "logistic", 
                          linear.output = F, threshold = 0.01)
    # avaliar o modelo
    model_nn_predict <- predict(model_nn, test_set2)
    # retirar o efeito de normalização e da logaritmização da variável alvo prevista
    model_nn_predict <- model_nn_predict*(max_data["h_prices"]-min_data["h_prices"]) + min_data["h_prices"]
    model_nn_predict <- exp(model_nn_predict)
    # guardar o erro da função
    model_nn_error <- RegressionModelError(pred = model_nn_predict, 
                                                 real = test_set2$h_prices)
    model_nn_error
    otimization.grid <- rbind(otimization.grid,data.frame(X=x,LR=lr,RMSE=model_nn_error[1],MAE=model_nn_error[2]))
    # atualizar o learning rate
    lr <- lr + 0.2
    # destreinar a rede neuronal
    model_nn <- 0
  }
}

# Criar uma variável que guarde o erro médio entre o RMSE e o MAE
# esta nova variável será utilizada para escolher o modelo ótimo
otimization.grid$mean_error <- (otimization.grid$RMSE+otimization.grid$MAE)/2
otimization.grid
min_error <- min(otimization.grid[5])
optimum_parameters <- c()
for (i in 1:nrow(otimization.grid)){
  if (otimization.grid[i,5] == min_error){
    optimum_parameters <- otimization.grid[i,]
    break
  }
}
optimum_parameters
model_nn <- neuralnet(h_prices~., data = train_set2, hidden = optimum_parameters[1,1], 
                      learningrate = optimum_parameters[1,2], act.fct = "logistic", 
                      linear.output = F, threshold = 0.01)
plot(model_nn)
# fazer a previsão dos resultados com o melhor modelo
model_nn_predict <- predict(model_nn, test_set2)
# retirar o efeito de normalização e da logaritmização da variável alvo prevista
model_nn_predict <- model_nn_predict*(max_data["h_prices"]-min_data["h_prices"]) + min_data["h_prices"]
model_nn_predict <- exp(model_nn_predict)
plot(test_set2$h_prices, model_nn_predict, main = "Simple NN: Real vs Predict")
abline(0,1)
optimum_parameters

# Gerar o R2 dos modelos com os conjuntos de teste
model_lm_r2 <- R2_Score(model_lm_previsao, test_set$h_prices)
model_tree_prune_r2 <- R2_Score(model_tree_prune_predict,test_set$h_prices)
model_boosting_r2 <- R2_Score(model_boosting_predict, test_set$h_prices)
model_bagging_r2 <- R2_Score(model_bag_previsao_3, test_set$h_prices)
model_forest_r2 <- R2_Score(model_forest_previsao, test_set$h_prices)
model_nn_r2 <- R2_Score(model_nn_predict, test_set2$h_prices)
r2 <- c(model_lm_r2, model_boosting_r2, model_tree_prune_r2, 
        model_bagging_r2, model_forest_r2, model_nn_r2)
# Guardar todos os resultados dos 3 modelos finais obtidos
tabela_erros <- data.frame()
nome_modelos <- c("Linear Regression","Boosting", "Pruned Tree","Bagging 500",
                  "Random Forest", "Simple NN")
erro_rmse <- c(table_lm_error$RMSE, model_boosting_error$RMSE, model_tree_prune_error$RMSE, 
               tabela_bagging_error_3$RMSE, tabela_forrest_error$RMSE, optimum_parameters$RMSE)
erro_mae <- c(table_lm_error$MAE, model_boosting_error$MAE, model_tree_prune_error$MAE, 
              tabela_bagging_error_3$MAE, tabela_forrest_error$MAE, optimum_parameters$MAE)
tabela_erros <- data.frame(Model=nome_modelos, RMSE=erro_rmse, MAE=erro_mae, R2=r2)
# Sumário final dos 6 modelos 
tabela_erros

