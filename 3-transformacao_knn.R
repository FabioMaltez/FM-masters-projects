#
# Etapa 3 - Transformação (missing values)
#

library(VIM)

path_to_data <- ".\\dados\\2-dados_transformados_com_zonas.csv"

booking_dados <-read.csv(path_to_data,sep = ",",dec = ".", header = T)

summary(is.na(booking_dados))
#Percebemos as seguintes variaveis com missing values:
# 1-h_dist_praia
# 2-h_num_stars
# 3-h_score_medio
#

# Vemos 1 NA na variável h_prices, porém como esta representa o nosso target, nós excluimos a observação em questão:
booking_dados<-booking_dados[!is.na(booking_dados$h_prices),]

 
# Escolhemos a técnica KNN (k-nearest Neighbour) para complementar os missing values de h_dist_praia, h_num_stars e h_score_medio
# escolhemos k = 20, sendo 20 um valor próximo da raiz quadrada do total de observações (417)
booking_dados_apos_knn <- kNN(booking_dados, variable = c("h_dist_praia","h_num_stars","h_score_medio"), k = 20, imp_var = F)

# recebemos um warning que durante a execucao do knn tivemos perda de precisão na variável 'h_num_stars', o que não afecta a nossa analise
# pois esta variavel somente possui valores inteiros

summary(is.na(booking_dados_apos_knn))

write.csv(booking_dados_apos_knn, file=".\\dados\\3-dados_apos_knn.csv")
