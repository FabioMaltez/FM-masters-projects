#
# Etapa 2 - Análise das variáveis
#
# 
#
#
install.packages("forcats")

library(ggplot2)
library(forcats)
library(dplyr)
link<-".//dados//0-dados_originais_apos_normalizacao.csv"
dados_normal <-read.csv(link,sep = ",",dec = ".", header = T)

# Visualização Inicial do Dados, o qual verificamos:
#  03 Variáveis possuem valores omissos: 
#  * h_dist_praia: 23,02% NA's (96/417)
#  * h_num_stars:  43,16% NA's (180/417)
#  * h_prices: 0,002% NA's (1/417)
# Observamos algumas variáveis binárias: h_peq_almoco,h_canc-gratuito e h_quartos_restantes
# h_num_comments, h_dist_centro e h_prices devem contem outliers devido
#a variância observado entre o 3rd Qu e o seus respectivos valores máximos  

summary(dados_normal)

# Variáveis a analisar: h_num_stars, h_num_comments, h_prices
# h_num_stars
ggplot(dados_normal,aes(h_num_stars)) + geom_histogram(binwidth = 0.5)
boxplot(dados_normal$h_num_stars)
plot(dados_normal$h_num_stars)
plot(dados_normal$h_num_stars, dados_normal$h_prices)
summary(as.factor(dados_normal$h_num_stars))
## média de preços por número de estrelas
mean(dados_normal$h_prices[dados_normal$h_num_stars ==5 & is.na(dados_normal$h_num_stars) == F])
mean(dados_normal$h_prices[dados_normal$h_num_stars ==4 & is.na(dados_normal$h_num_stars) == F])
mean(dados_normal$h_prices[dados_normal$h_num_stars ==3 & is.na(dados_normal$h_num_stars) == F])
mean(dados_normal$h_prices[dados_normal$h_num_stars ==2 & is.na(dados_normal$h_num_stars) == F])
mean(dados_normal$h_prices[dados_normal$h_num_stars ==1 & is.na(dados_normal$h_num_stars) == F])

# Sobre a variável h_num_stars, referente ao número de estrelas de cada hotel, conseguimos verificar o seguinte:
# .Possui 180 valores omissos
# .A maioria dos hoteis têm 3 (106/417) ou 4 (96/417) estrelas
# .Os hotéis de 1 estrela são de reduzida dimensão (3/389) e podem ser considerados outliers
# .A ordem dos indexes está relacionada com a visibilidade do hótel no site Booking 
#   (ex. o hotel de index 1 é o que aparece primeiro, na 1ªa página de pesquisa)
# .Os hoteis relacionados aos primeiros indexes, tem tendencia a terem um elevado número de estrelas
#   (ex. tirando 1 hotel, todos os hoteis de 5 estrelas aparecem nos 100 primeiros resultados de pesquisa)
# .É possivel observar uma relação linear positiva entre o número de estrelas e a variável target (preço)
#  (ex. hoteis de 3 estrelas tem um preço médio de 47??? e os de 5 estrelas de 153???)
# . Existem 2 observações que parecem outliers e têm de ser analisadas com cuidado (1 de 3 estrelas acima dos 400??? e uma de 4 acima dos 500???)

####################
# h_num_comments
ggplot(dados_normal,aes(h_num_comments)) + geom_histogram()
boxplot(dados_normal$h_num_comments,horizontal = T)
plot(dados_normal$h_num_comments)
plot(dados_normal$h_num_comments, dados_normal$h_prices)
plot(dados_normal$h_num_comments, dados_normal$h_in_page)

summary(dados_normal$h_num_comments)
# .A variavél h_num_comments, que representa o número de comentários de cada hotel, demonstra uma distribuição exponencial
# .Existem um grande número de outliers devido ao tipo de destribuição da variável
# (NOTA: Uma transformação logarítmica pode ser aplicada para melhorar os resultados de modelação)
# (NOTA: O tratamento dos outliers vai ser importante)
# .Existem uma relação negativa entre o número de comentários e a posição do hotel
#  Os primeiros hotéis têm mais comentários do que os hotéis nas últimas posições
# .Existe uma relação negativa entre o número de variáveis e o preço
# .Estudo de outliers: os 2 hotéis mais caros (outliers, de 3 e 4 estrelas) têm um número muito reduzido de comentários
# . média >> mediana, devido ao elevado nível de "skewness" positiva dos dados (causado pela distribuição exponencial)

##################
# h_prices (target)
ggplot(dados_normal,aes(h_prices)) + geom_histogram()
boxplot(dados_normal$h_prices, horizontal = T)
plot(dados_normal$h_prices)
summary(dados_normal$h_prices)
# .A variável h_prices, que representa o preço de cada hotel, é a nossa variável alvo 
#  e demonstra uma distribuição exponêncial
# .Mais uma vez, podemos observar que uma transformação logarítmica para reduzir a variabilidade dos dados pode ser útil
# Os preços parecem estar distribuidos de forma regular (variância constante) ao longo do eixo dos índices


# Variavel: h_score_medio
summary(dados_normal$h_score_medio)
summary(as.factor(dados_normal$h_score_medio))

ggplot(dados_normal,mapping = aes(h_score_medio)) + geom_histogram(binwidth = 0.1)
boxplot(dados_normal$h_score_medio, horizontal = TRUE)
# Maior concentração do score entre 8 e 9
# Hoteis com score menor que 6 parecem ser outliers

out <- boxplot.stats(dados_normal$h_score_medio)$out
out_ind <- which(dados_normal$h_score_medio %in% c(out))
dados_normal[out_ind, ]


plot(dados_normal$h_score_medio)
plot(dados_normal$h_score_medio, dados_normal$h_prices)

# Medias
mean(dados_normal$h_prices[dados_normal$h_score_medio >= 9 & is.na(dados_normal$h_score_medio) == F])
mean(dados_normal$h_prices[dados_normal$h_score_medio >= 8 & dados_normal$h_score_medio < 9 & is.na(dados_normal$h_score_medio) == F])
mean(dados_normal$h_prices[dados_normal$h_score_medio >= 7 & dados_normal$h_score_medio < 8 & is.na(dados_normal$h_score_medio) == F])
mean(dados_normal$h_prices[dados_normal$h_score_medio >= 6 & dados_normal$h_score_medio < 7 & is.na(dados_normal$h_score_medio) == F])
mean(dados_normal$h_prices[dados_normal$h_score_medio >= 5 & dados_normal$h_score_medio < 6 & is.na(dados_normal$h_score_medio) == F])
mean(dados_normal$h_prices[dados_normal$h_score_medio < 5 & is.na(dados_normal$h_score_medio) == F])


# Variavel: h_dist_praia
summary(dados_normal$h_dist_praia)
summary(as.factor(dados_normal$h_dist_praia))

ggplot(dados_normal,mapping = aes(h_dist_praia)) + geom_histogram(binwidth = 0.05)
boxplot(dados_normal$h_dist_praia, horizontal = TRUE)

plot(dados_normal$h_dist_praia)
plot(dados_normal$h_dist_praia, dados_normal$h_prices)





### Analise dos Variáveis h_peq_almoco, h_canc-gratuito e h_quartos_restantes

#h_peq_almoco
summary(dados_normal$h_peq_almoco)
summary(as.factor(dados_normal$h_peq_almoco))

#Representação Grafica = h_peq_almoco
ggplot(dados_normal,aes(h_peq_almoco)) + geom_histogram(binwidth = 0.5)
boxplot(dados_normal$h_peq_almoco)

plot(dados_normal$h_peq_almoco, dados_normal$h_prices)
plot(dados_normal$h_peq_almoco, dados_normal$h_num_comments)

#
mean(dados_normal$h_prices[dados_normal$h_peq_almoco ==1 & is.na(dados_normal$h_peq_almoco) == F])
mean(dados_normal$h_prices[dados_normal$h_peq_almoco ==0 & is.na(dados_normal$h_peq_almoco) == F])


#h_canc_gratuito
summary(dados_normal$h_canc_gratuito)
summary(as.factor(dados_normal$h_canc_gratuito))

#Representação Grafica = h_canc_gratuito
ggplot(dados_normal,aes(h_canc_gratuito)) + geom_histogram(binwidth = 0.5)
boxplot(dados_normal$h_canc_gratuito)

plot(dados_normal$h_canc_gratuito, dados_normal$h_prices)
plot(dados_normal$h_canc_gratuito, dados_normal$h_num_comments)
plot(dados_normal$h_canc_gratuito, dados_normal$h_dist_praia )
 

# 44,84% da Amostra oferecem o pequeno almoço gratuitamente (187/417)
# 46,04% da Amostra oferecem a opção de Cancelamento gratis (192/417)
# 49,16% da Amostra oferecem a opção de Quartos Restantes (205/417)
# Hotéis com pequeno almoço e cancelamento gratis tendem receberem mais 
#comentários.
# Hotéis com valores de reservas maiores tende não oferecem essas opções

# h_quartos_restantes
summary(dados_normal$h_quartos_restantes)
summary(as.factor(dados_normal$h_quartos_restantes))

#Representação Grafica = h_quartos_restantes
ggplot(dados_normal,aes(h_quartos_restantes)) + geom_histogram(binwidth = 0.5)
boxplot(dados_normal$h_quartos_restantes)

plot(dados_normal$h_quartos_restantes, dados_normal$h_prices)
plot(dados_normal$h_quartos_restantes, dados_normal$h_num_comments)
plot(dados_normal$h_quartos_restantes, dados_normal$h_dist_praia )



# Variavel: h_score_medio
summary(dados_normal$h_score_medio)
summary(as.factor(dados_normal$h_score_medio))

ggplot(dados_normal,mapping = aes(h_score_medio)) + geom_histogram(binwidth = 0.1)
boxplot(dados_normal$h_score_medio, horizontal = TRUE)
# Maior concentração do score entre 8 e 9
# Hoteis com score menor que 6 parecem ser outliers

out <- boxplot.stats(dados_normal$h_score_medio)$out
out_ind <- which(dados_normal$h_score_medio %in% c(out))
dados_normal[out_ind, ]


plot(dados_normal$h_score_medio)
plot(dados_normal$h_score_medio, dados_normal$h_prices)

# Medias
mean(dados_normal$h_prices[dados_normal$h_score_medio >= 9 & is.na(dados_normal$h_score_medio) == F])
mean(dados_normal$h_prices[dados_normal$h_score_medio >= 8 & dados_normal$h_score_medio < 9 & is.na(dados_normal$h_score_medio) == F])
mean(dados_normal$h_prices[dados_normal$h_score_medio >= 7 & dados_normal$h_score_medio < 8 & is.na(dados_normal$h_score_medio) == F])
mean(dados_normal$h_prices[dados_normal$h_score_medio >= 6 & dados_normal$h_score_medio < 7 & is.na(dados_normal$h_score_medio) == F])
mean(dados_normal$h_prices[dados_normal$h_score_medio >= 5 & dados_normal$h_score_medio < 6 & is.na(dados_normal$h_score_medio) == F])
mean(dados_normal$h_prices[dados_normal$h_score_medio < 5 & is.na(dados_normal$h_score_medio) == F])


# Variavel: h_dist_praia
summary(dados_normal$h_dist_praia)
summary(as.factor(dados_normal$h_dist_praia))

ggplot(dados_normal,mapping = aes(h_dist_praia)) + geom_histogram(binwidth = 0.05)
boxplot(dados_normal$h_dist_praia, horizontal = TRUE)

plot(dados_normal$h_dist_praia)
plot(dados_normal$h_dist_praia, dados_normal$h_prices)


###DESCRICÃO DA H_DIST_CENTRO###

summary(dados_normal$h_dist_centro)
#Média de 13.59 km do centro

#Representação de todos os hoteis e a sua distancia ao centro
plot(dados_normal$h_dist_centro)
#Valores acima de 50 muito raros

#Representação em Histograma do número de hotéis por distância (km)
ggplot(dados_normal,aes(h_dist_centro)) + geom_histogram(binwidth = 3)
#Distribuição logaritmica em termos de counts por distância (distancia de 3 em 3 km)
#Novamente valores acima de 50 muito raros

#Representação em Boxplot dos valores de distancia ao centro
boxplot(dados_normal$h_dist_centro, horizontal = T)+geom_line()
ggplot(dados_normal, aes(h_dist_centro)) + geom_boxplot()
#Maior parte dos dados compreendidos entre 0 e 30 km do centro
#Todos os outliers representados acima do mÃ¡ximo (bigode do mÃ¡ximo)
#SÃ³ 3 outliers abaixo dos 50 km, todos os outros acima dos 50
#Hoteis que aparecem na booking tÃªm tendÃªncia a estar bastante prÃ³ximos do centro
#(1o quartil muito prÃ³ximo do 0)

#Valores que os outliers tomam
boxplot.stats(dados_normal$h_dist_centro)$out

#DeterminaÃ§Ã£o dos registos dos outliers
out <- boxplot.stats(dados_normal$h_dist_centro)$out
out_ind <- which(dados_normal$h_dist_centro %in% c(out))
dados_normal[out_ind, ]
#O que fazer aos outliers?

#RepresentaÃ§Ã£o do preÃ§o deacordo com a distancia ao centro
plot(dados_normal$h_dist_centro, dados_normal$h_prices)
#Muitos hoteis concentrados perto dos 0's, tanto para prices como para distancia ao centro
#Valores mais proximos do centro tÃªm tendÃªnca de ser mais caros, e o oposto tambÃ©m se verifica
#NÃ£o existem hoteis caros longe do centro

#Representação da distância de acordo com o preÃ§o
plot(dados_normal$h_prices, dados_normal$h_dist_centro)

summary(as.factor(dados_normal$h_dist_centro))


###DESCRIÃ‡ÃƒO DA HOTEL_LOC###


head(dados_normal$hotel_loc)
#RepresentaÃ§Ã£o de Hotel_Loc
ggplot(dados_normal) + geom_bar(aes(x = hotel_loc)) 
#NÃ£o estÃ¡ a representar os sÃ­tios que tÃªm caracteres especiais
#Copacabana, Centro do Rio de Janeiro e Bairro da Tijuca sÃ£o as que apresentam mais hoteis

#Ordenados por counts
library(forcats)
library(dplyr)
ggplot(mutate(dados_normal, hotel_loc = fct_infreq(hotel_loc))) + geom_bar(aes(x = hotel_loc)) 
#NOTA: Tentar colocar os counts por cima das bars
ggplot(dados_normal, aes(x=hotel_loc, y=h_prices)) + stat_summary(fun="mean", geom="bar")
#Cosme Velho com maior media de preÃ§os (apenas 2 hoteis)
#Zona Sul mais barata em mÃ©dia (apenas 1 hotel)
#Copaabana mais caro dos 3 com mais hoteis
#Centro do Rio de Janeiro mais barato dos 3 com mais hoteis

