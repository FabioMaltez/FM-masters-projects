# Trabalho final de RP
# Financial Data set, US companies, 2018

################
## Fase de recolha e tratamento dos dados
##
################
library(VIM)
library(psych)
library(corrplot)
library(ggplot2)
library(gpairs)
library (cluster)
library(factoextra)

data <- read.csv("financial_data2018b.csv", sep = ",", dec = ".", header = T)
str(data)
summary(data)
names(data)

# Com os seguintes comandos pode-se observar que Currency, Fiscal.Year e Fiscal.Period têm sempre o mesmo valor
# logo não possuem nenhuma informação para a análise, e podem não ser consideredas
levels(as.factor(data$Currency)) # 4
levels(as.factor(data$Fiscal.Year)) # 41
levels(as.factor(data$Fiscal.Period)) # 42

# Criar uma variavél Spread.Shares, contendo a diferença entre o nº de ações diluidas e o nº de ações ordinárias de uma empresa,
# ficando esta uma variável de profile e as variáveis
# Shares..Basic. e Shares..Diluted. passam a input

# Index das variáveis de perfil: 1, 2, 3, 40, 43, 44, 67 e 68
data_perfil <- data[,c(1,2,3,40,43,44,67,68)]
data_perfil$Spread.Shares <- data$Shares..Diluted. - data$Shares..Basic.

data_input <- data[,-c(2,3,4,40,41,42,43,44,67,68)]
# guardar os valores originais do input
data_input_original <- data_input


# ver distribuições das variáveis (escolher 4 que representem as 3 demonstrações financeiras + nº de ações no mercado)
# consegue-se observar uma clara tendência para a base de dados ter uma grande representação de distribuições exponênciais (enviesamento positivo)

ggplot(data_input, aes(Operating.Income..Loss.)) + geom_histogram()
ggplot(data_input, aes(Net.Cash.from.Operating.Activities)) + geom_histogram()
ggplot(data_input, aes(Total.Assets)) + geom_histogram()
ggplot(data_input, aes(Shares..Basic.)) + geom_histogram()

# verificar se existem outliers
boxplot(data_input, horizontal = T)


str(data_input) # verificar que todas as variáveis são numéricas

# Tratamento dos valores omissos
# Eliminar variavéis cujos valores omissos excedam 50% das observações totais 
# Extrair os índices das variáveis com mais de 50% de valores omissos
index.na <- c()
for (i in 1:length(data_input[1,])){
  na.count <- 0
  for (j in 1:length(data_input[,i])){
    if (is.na(data_input[j,i]) == T){
      na.count = na.count + 1
    } 
  }
  na.portion <- na.count/length(data_input[,i])
  if (na.portion > 0.5){
    index.na <- c(index.na, i)
  }
}
index.na
# retirar as variáveis com mais de 50% de omissos da BD
data_input2 <- data_input[,-index.na]
# sumário da nova base de dados - 1810 x 47
summary(data_input2)

# Tratar variáveis com valores omissos <= 20% do total de observações com a mediana
# Algumas destas variáveis são importantes e variam consoante a índústria da empresa,
# mas devido ao reduzido número de omissos, o modelo não perde muita informação ao usar a mediana
for (i in 1:length(data_input2)) {
  na.count <- 0
  for (j in 1:length(data_input2[,i])){
    if (is.na(data_input2[j,i]) == T){
      na.count = na.count + 1
    } 
  }
  na.portion <- na.count/length(data_input2[,i])
  if ((na.count > 0 && na.portion <= 0.2)){
    data_input2[is.na(data_input2[,i]), i] <- median(data_input2[,i], na.rm = T)
  }
}
summary(data_input2)

# As variáveis que têm entre 20% a 50% de valores omissos podem ainda ser imputadas
# com cuidado. Este cuidado é redobrado quando reparamos que estas são variáveis importantes
# para analisar o estado financeiro de uma empresa relativo à sua indústria, por isso uma simples mediana não ia chegar.
# Estas variáveis são:"Long.Term.Debt", "Short.Term.Debt", "Inventories", 
#                     "Net.Cash.from.Acquisitions...Divestitures", "Abnormal.Gains..Losses."
# Tratar os valores omissos das restantes NA's com o método knn, passando assim a 
# imputar valores de "vizinhos" que possuem uma situação financeira semelhante

# Fazer a imputação Knn
# Impotação baseada na variância da distância de Gower
k_val <- round(sqrt(length(data_input2[,1])))
data_input3 <- kNN(data_input2, variable = c("Long.Term.Debt", "Short.Term.Debt", "Inventories",
                                             "Net.Cash.from.Acquisitions...Divestitures", "Abnormal.Gains..Losses."), k = k_val)
data_input3 <- data_input3[,1:47]
summary(data_input3) # verificar que já não existe valores omissos

# Tratamento dos outliers

# Fazer um trim a 2.5%, tanto para valores máximos como mínimos
trim_size <- 0.025
index_trim <- c()
for (i in 2:length(data_input3)){
  new_max <- quantile(data_input3[,i], 1-trim_size)
  new_min <- quantile(data_input3[,i], trim_size)
  for (j in 1:length(data_input3[,i])){
    if ((data_input3[j,i] < new_min || data_input3[j,i] > new_max) && j %in% index_trim == F){
      index_trim <- c(index_trim, j)
    }
  }
}
index_trim
# Reduzimos de 1810 para 1011 observações
data_trimmed <- data_input3[-index_trim,]
boxplot(data_trimmed, horizontal = T)


# Aplicara transformação log()
# Captar o valor mínimo da BD
min_val <- abs(min(data_trimmed[,-1]))
min_val

# As variáveis apresentam distribuições exponênciais e também existem valores negativos
# Usar a transformação log(data + min + 1), para tentar reduzir a dispersão dos dados
data_input_log <- log(data_trimmed[,-1] + min_val + 1)
boxplot(data_input_log, horizontal = T)

summary(data_input_log)
ggplot(data_input_log, aes(Operating.Income..Loss.)) + geom_histogram()
ggplot(data_input_log, aes(Net.Cash.from.Operating.Activities)) + geom_histogram()
ggplot(data_input_log, aes(Total.Assets)) + geom_histogram()
ggplot(data_input_log, aes(Shares..Basic.)) + geom_histogram()

# Com o tratamento de outliers e a diminuição da disperção dos dados realizada,
# podemos proseguir para a próxima fase
financial_data <- cbind(data_trimmed$X, data_input_log)

################
## Análise de Componentes Principais (PCA)
##
################

# Matriz de correlação
# Através dos agrupamentos formados pelas elipses, pode-se observar que
# existem grupos de variáveis fortemente correlacionadas entre si.
# E a base de dados está no geral fortemente correlacionada, o que era de esperar tendo
# em conta a natureza financeira / contabilistica da base de dados
correlation <- cor(financial_data[,-1])
par(oma = c(55, 55, 55, 55)) # space around for text
corrplot.mixed(correlation, 
               order = "hclust", #order of variables
               tl.pos = "n", #text left + top
               upper = "ellipse")

#Bartlett test e KMO
cortest.bartlett(correlation) # p.value = 0
KMO(correlation) # MSA = 0.91

# Mais uma vez, devido à natureza da BD financiera, ela é ao todo fortemente relacionada
# E esta forte correlação pode-se observar nos resultados obtidos no teste de bartlett e KMO

# Extrair as componentes principais (PC's)
# Normalizar os dados (de forma a que todas as variáveis tenham a mesma
# importância na análise)
financial_dataZ <- as.data.frame(scale(financial_data[,-1]))
# Assumir que o nº de componentes é igual ao número de variáveis (não perdendo informação nesta fase inicial)
# Extrair PC's sem rotação
pc46 <- principal(financial_dataZ, nfactors=46, rotate="none", scores=TRUE)  

#Valores próprios - Variância das componentes principais 
round(pc46$values,3)

#Screeplot 
par(oma = c(0,0,0,0))
plot(pc46$values, type = "b",
    xlab = "Number of PC", ylab = "Eigenvalue")

# Vetores próprios - componentes dos loadings
# Cada valor é a correlação entra a variável original e a componente
# Pode ser a contribuição de cada variável original na componente (quando colocada ao quadrado)
pc46$loadings

# Utilizar o screeplot, a variância explicada, o critério de Kaiser e algum conhecimento da área 
# para definir um número aproximadamente ótimo de PC's 
# Screeplot: 2 PC's
# Critério Kaizer: as 8 primeiras PC's tem variância superior ou igual a 1
# Variância explicada:2 primeiras PC's explicam mais de 60% da informação total
#                     4 primeiras PC's explicam 70% da informação total
#                     8 primeiras PC's explicam mais de 80% da informação total
# Para escolher a melhor solução dentro deste intervalo e tendo em consideração
# que o objetivo do problema é o de ajudar um investidor a alocar o seu capital,
# grande atenção tem de ser dada aos seguintes aspetos: interpretabilidade e retenção de informação.
# (queremos reter o máximo de informação possível, sendo esta digerivél pelo cérebro humano)
# Uma solução ótima parece estar entre 2 a 4 PC's

#Extrair 2 PC
pc2 <- principal(financial_dataZ, nfactors=2, rotate="none", scores = T)
pc2$loadings

# Rotação varimax no pc2
pc2r <- principal(financial_dataZ, nfactors = 2, rotate = "varimax")
pc2r$loadings
# variáveis mais explicadas por componente:
# 1 (> 85%): Pretax.Income..Loss...Adj., Pretax.Income..Loss., Income..Loss..from.Continuing.Operations,
#            Net.Income, Net.Income..Common., Net.Income.Starting.Line
# 2 (> 85%): Interest.Expense..Net, Total.Noncurrent.Assets, Long.Term.Debt, Total.Noncurrent.Liabilities

round(pc2r$communality,2) # comunalidades

# Extrair 3 PC's
pc3 <- principal(financial_dataZ, nfactors = 3, rotate = "none", scores = T)
pc3$loadings

# Rotação varimax no pc3
pc3r <- principal(financial_dataZ, nfactors = 3, rotate = "varimax")
pc3r$loadings
# variáveis mais explicadas por componente:
# 1 (>85%): Interest.Expense..Net, Total.Noncurrent.Assets, Total.Noncurrent.Liabilities
# 2 (>85%): Pretax.Income..Loss., Net.Income.Starting.Line, Net.Income..Common.,
#           Net.Income, Income..Loss..from.Continuing.Operations, Pretax.Income..Loss...Adj.
# 3 (>65%): Revenue, Operating.Expenses, Inventories, Total.Current.Assets, Payables...Accruals


round(pc3r$communality,2)

# Extrair 4 PC's
pc4 <- principal(financial_dataZ, nfactors = 4, rotate = "none", scores = T)
pc4$loadings

# Rotação varimax no pc4
pc4r <- principal(financial_dataZ, nfactors = 4, rotate = "varimax")
pc4r$loadings
# variáveis mais explicadas por componente:
# 1 (>85%): Non.Operating.Income..Loss., Interest.Expense..Net, Total.Noncurrent.Assets,
#           Long.Term.Debt, Total.Noncurrent.Liabilities
# 2 (>85%): Pretax.Income..Loss...Adj., Pretax.Income..Loss., Net.Income.Starting.Line, Net.Income..Common.,
#           Net.Income, Income..Loss..from.Continuing.Operations
# 3 (>65%): Revenue, Selling..General...Administrative, Inventories, Total.Current.Assets,
#           Payables...Accruals, Total.Current.Liabilities
# 4 (>60%): Cash.from..Repayment.of..Debt, Net.Cash.from.Financing.Activities,
#           Net.Cash.from.Investing.Activities, Net.Cash.from.Acquisitions...Divestitures

round(pc4r$communality,2)

##
# Escolher uma matriz de PC's e guardar os scores

# Depois de analisar as variadas análises de componentes principais,
# pode-se concluir que a melhor opção são 4 componentes principais.
# Nestas 4, conseguimos ter 71% da variância da base de dados explicada e
# com a rotação varimax é possível perceber que cada PC está relacionada com:
# RC1: Nível de alavancagem financeira em ativos de longo prazo
# RC2: Nível de rendibilidade
# RC3: Nível de Working Capital
# RC4: Nível de Cash Flow de atividades financeiras e de investimento

################
## Análise de Clusters
##
################

# analisar os scores da pc4 e juntar as variáveis de perfil
financial_pc_data <- cbind(X = financial_data[,1],financial_dataZ, pc4$scores)
str(financial_pc_data)
financial_pc_data <- merge(data_perfil ,financial_pc_data, by = "X")
financial_pc_data <- merge(financial_pc_data, data_input_original[c("X","Revenue")], by = "X")
str(financial_pc_data)

data_pc4 <- financial_pc_data[,c(56,57,58,59)]
head(data_pc4)
summary(data_pc4)
var(data_pc4$PC1)
var(data_pc4$PC2)
var(data_pc4$PC3)
var(data_pc4$PC4)

hist(data_pc4$PC1)
hist(data_pc4$PC2)
hist(data_pc4$PC3)
hist(data_pc4$PC4)

# alterar os títulos das variáveis
data_pc4 <- data.frame("Alavancagem Financeira" = data_pc4[,1],
                       "Rendibilidade" = data_pc4[,2],
                       "Working Capital" = data_pc4[,3],
                       "CF Financeiros e de Investimento" = data_pc4[,4])

head(data_pc4)
summary(data_pc4)

## clustering hierárquico

data_dist <- dist(data_pc4)
# Standardized * Euclidian * single
hclust_data <- hclust(data_dist, method = "single")
plot(hclust_data, label = NULL, hang = -1)

# Standardized * Euclidian * complete
hclust_data <- hclust(data_dist, method = "complete")
plot(hclust_data, label = NULL, hang = -1)

# Standerdized * Euclidean * ward.D2
hclust_data <- hclust(data_dist, method = "ward.D2")
plot(hclust_data, label = NULL, hang = -1)

# cortar o dendograma em 5 clusters (k = 5) 
groups.k5 <- cutree(hclust_data, k=5)
rect.hclust(hclust_data, k=5, border="red")
aggregate(data_pc4, list(groups.k5), mean)

#Silhouette
plot(silhouette(groups.k5, data_dist)) # 0.55

## k-means - clustering partitivo
# gráfico da variabilidade dentro dos grupos, para diversos números de clusters
wssplot <- function(xx, nc=15, seed=1234){
  wss <- (nrow(data_pc4)-1)*sum(apply(data_pc4,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(xx, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

# k=5 aparenta ser uma boa escolha, numa análise variância vs complexidade
wssplot(data_pc4)

# construção dos clusters
kmeans.k5 <- kmeans(data_pc4, 5, nstart = 100)
kmeans.k5$centers # Análise dos scores das componentes em cada cluster
kmeans.k5$cluster
kmeans.k5$size

# semelhança entre cluster hierárquico e partitivo 
table("Ward"=groups.k5, "k-means"=kmeans.k5$cluster)

#Silhouette
plot(silhouette(kmeans.k5$cluster, data_dist)) # 0.58


## PAM clustering, k=5
pam.k5 <- pam(data_pc4, 5)
summary(as.factor(pam.k5$clustering))

# semelhança entre pam clustering e hierárquico
table("Ward"=groups.k5, "pam"=pam.k5$clustering)

# semelhança entre pam clustering e k-means
table("k-means"=kmeans.k5$cluster, "pam"=pam.k5$clustering)

#Silhouette
plot(silhouette(pam.k5$clustering, data_dist)) # 0.43

# Em termos da análise de silhueta, observa-se que o 
# clustering k-means é o melhor dos experimentados, com k=5

## Análise do perfil das variáveis por cluster
o <- order(kmeans.k5$cluster)
table_perfil_kmeans <- data.frame("K"=kmeans.k5$cluster[o],
                                  "Ticker"=financial_pc_data$Ticker[o],
                                  "Name"=financial_pc_data$Company.Name[o],
                                  "IndustryID"=financial_pc_data$IndustryId[o],
                                  "Spread Shares"=financial_pc_data$Spread.Shares[o],
                                  "Rep_Date"=financial_pc_data$Report.Date[o],
                                  "Revenue"=financial_pc_data$Revenue.y[o])
head(table_perfil_kmeans,10)
str(table_perfil_kmeans)
View(table_perfil_kmeans)
table_perfil_kmeans$IndustryID <- as.factor(table_perfil_kmeans$IndustryID) 

# Análise por indústria
summary(table_perfil_kmeans$IndustryID) # moda: 101003
summary(table_perfil_kmeans$IndustryID[table_perfil_kmeans$K == 1]) # moda: 110001 -> Basic Materials - Chemicals
summary(table_perfil_kmeans$IndustryID[table_perfil_kmeans$K == 2]) # moda: 105001 -> Utilities - Regulated
summary(table_perfil_kmeans$IndustryID[table_perfil_kmeans$K == 3]) # moda: 101001 -> Technology - Computer Hardware
summary(table_perfil_kmeans$IndustryID[table_perfil_kmeans$K == 4]) # moda: 101003 -> Technology - Application Software
summary(table_perfil_kmeans$IndustryID[table_perfil_kmeans$K == 5]) # moda: 103002 -> Consumer Cyclical - Retail - Apparel & Specialty

# Análise por tamanho da empresa.
# Usamos as vendas como medida de dimensão
mean(table_perfil_kmeans$Revenue[table_perfil_kmeans$K == 1], na.rm = T) # $ 8.859.266.868 
mean(table_perfil_kmeans$Revenue[table_perfil_kmeans$K == 2], na.rm = T) # $ 5.277.877.868 
mean(table_perfil_kmeans$Revenue[table_perfil_kmeans$K == 3], na.rm = T) # $ 4.621.012.071
mean(table_perfil_kmeans$Revenue[table_perfil_kmeans$K == 4], na.rm = T) # $ 1.411.442.308
mean(table_perfil_kmeans$Revenue[table_perfil_kmeans$K == 5], na.rm = T) # $ 13.626.842.788

# Análise em relação à variável Spread.Shares
# Spread.Shares = Shares..Diluted. - Shares..Basic.
mean(table_perfil_kmeans$Spread.Shares[table_perfil_kmeans$K == 1], na.rm = T) # 2.491.781
mean(table_perfil_kmeans$Spread.Shares[table_perfil_kmeans$K == 2], na.rm = T) # 3.871.443
mean(table_perfil_kmeans$Spread.Shares[table_perfil_kmeans$K == 3], na.rm = T) # 4.976.596
mean(table_perfil_kmeans$Spread.Shares[table_perfil_kmeans$K == 4], na.rm = T) # 1.999.939
mean(table_perfil_kmeans$Spread.Shares[table_perfil_kmeans$K == 5], na.rm = T) # -469.990

# Análise dos scores das componentes em cada cluster
aggregate(data_pc4, list(kmeans.k5$cluster), mean)
kmeans.k5$size

# Análise gráfica dos clusters
# PCA e Clustering
fviz_cluster(kmeans.k5, data = data_pc4,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

# Análise dos clusters:
# 1: Empresas de grande dimensão com elevado risco, grande alavancagem financeira e grandes problemas de rentabilidade, sendo a maioria do setor da produção de produtos químicos
# 2: Empresas maduras de risco médio alto com grande alavancagem financeira, elevada rentabilidade e liquidez, e CF não operacionais negativos, sendo a maioria do setor das utilities 
# 3: Empresas de risco médio que ligam a performance da empresa à remuneração dos colaboradores, com baixo nível de alavancagem financeira, 
#    nível médio de rendabilidade e grande nível de CF não operacional positivo, sendo a maioria do setor de produção de hardware
# 4: cluster representativo da população com ênfase no setor tecnológico, reduzidas dimensões, baixos níveis de dívidas de longo prazo,
#    rendibilidade quase nula, liquidez equilibrada e com poucas atividades de financiamento / investimento, sendo a maioria do setor de produção de software
# 5: Empresas de grande dimensão com grande alavancagem financeira, grandes problemas de rendibibilidade e liquidez, e que possívelmente utilizam
#    instrumentos financeiros com potencial anti-dilutivo (o que aumenta os lucros por ação), sendo a maioria do setor do retalho

########
#  


