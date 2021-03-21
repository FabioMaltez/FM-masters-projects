#
#
# Etapa 4 - Transformação Logarítmica e tratamento dos outliers
#
#
#
library(ggplot2)

df = read.csv(".//dados/3-dados_apos_knn.csv",header = TRUE, sep = ",", dec = ".")

###ASPETO DOS HISTOGRAMAS ANTES DA TRANSFORMAÇAO LOGARITMICA###
##H_num_comments##
ggplot(df,aes(h_num_comments)) + geom_histogram()
#Entre 0 e perto de 15000#
plot(df$h_num_comments, df$h_prices)
#Quase todos os valores próximos dos 0's#

##H_dist_centro##
ggplot(df,aes(h_dist_centro)) + geom_histogram(binwidth = 3)
#Entre 0 e perto de 150#
plot(df$h_dist_centro, df$h_prices)
#Quase todos os valores próximos dos 0's#

##H_Prices##
ggplot(df,aes(h_prices)) + geom_histogram()
#Entre 0 e perto de 500#

###TRANSFORMAÇÕES LOGARITMICAS###
##Transformação logaritmica do h_num_comments##
df$h_num_comments =   log (df$h_num_comments)

ggplot(df,aes(h_num_comments)) + geom_histogram()
#Bastante mais distribuido, com o pico perto do 7,5#
#Perdem-se 58 rows#

plot(df$h_num_comments, df$h_prices)
#Valores apresentam-se muito mais distribuidos relativamente à variavel alvo#

##Transformação logaritmica do h_dist_centro##
df$h_dist_centro = log (df$h_dist_centro)

ggplot(df,aes(h_dist_centro)) + geom_histogram()
#Valores muito mais distribuidos, com o pico perto no 2#
#Apresenta alguns valores negativos, provavelmente tem de se meter ceiling#

plot(df$h_dist_centro,df$h_prices)
#Valores muito mais distribuidos relativamente à variavel alvo#
#Maior concentração entre 0 e 4#

##Transformação logaritmica do h_prices##
df$h_prices =   log (df$h_prices)

ggplot(df,aes(h_prices)) + geom_histogram()
#Maior distribuição, tendo pico entre 3 e 4#

###PLOTS DEPOIS DA TRANSFORMACAO DOS PRICES###
##H_num_comments vs h_prices##
plot(df$h_num_comments, df$h_prices)
#Bastante distribuido, sem padrão visível#

#H_dist_centro vs h_prices
plot(df$h_dist_centro, df$h_prices)
#Muito mais distribuido, com uma maior distriuição entre 2 e 5 nos prices e o e 2 na dist_centro#


###OUTLIERS###
##H_dist_centro##
ggplot(df, aes(h_dist_centro)) + geom_boxplot()

#Valores que os outliers tomam
boxplot.stats(df$h_dist_centro)$out
#13 outliers

#Ceiling Outliers h_dist_centro#
#Todos os valores que estão abaixo do limite de 0,05 passa para para esse limite#
#Todos os valores que estão acima do limite de 0,95 passa para esse limite#
qnt <- quantile(df$h_dist_centro, probs=c(.25, .75), na.rm = T)
caps <- quantile(df$h_dist_centro, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(df$h_dist_centro, na.rm = T)
df$h_dist_centro[df$h_dist_centro < (qnt[1] - H)] <- caps[1]
df$h_dist_centro[df$h_dist_centro > (qnt[2] + H)] <- caps[2]

ggplot(df, aes(h_dist_centro)) + geom_boxplot()

##H_num_star##
ggplot(df, aes(h_num_stars)) + geom_boxplot()

#Valores que os outliers tomam
boxplot.stats(df$h_num_stars)$out
#3 outliers (todos com valor de 1)#
#Diria para ignorar#

##H_num_comments##
ggplot(df, aes(h_num_comments)) + geom_boxplot()
#Valores que os outliers tomam
boxplot.stats(df$h_num_comments)$out
#58 outliers (todos com valor -inf)#
#Passar todos para 0#
df$h_num_comments[which(!is.finite(df$h_num_comments))] <- 0

ggplot(df, aes(h_num_comments)) + geom_boxplot()

##H_prices##
ggplot(df, aes(h_prices)) + geom_boxplot()
#Valores que os outliers tomam
boxplot.stats(df$h_prices)$out
#10 outliers#
#Fazer ceiling nos outliers#

#Ceiling Outliers h_prices#
qnt <- quantile(df$h_prices, probs=c(.25, .75), na.rm = T)
caps <- quantile(df$h_prices, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(df$h_prices, na.rm = T)
df$h_prices[df$h_prices < (qnt[1] - H)] <- caps[1]
df$h_prices[df$h_prices > (qnt[2] + H)] <- caps[2]

ggplot(df, aes(h_prices)) + geom_boxplot()


##H_score_medio##
ggplot(df, aes(h_score_medio)) + geom_boxplot()
#Valores que os outliers tomam
boxplot.stats(df$h_score_medio)$out
#21 outliers#
#Fazer ceiling#

#Ceiling Outliers h_score_medio#
qnt <- quantile(df$h_score_medio, probs=c(.25, .75), na.rm = T)
caps <- quantile(df$h_score_medio, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(df$h_score_medio, na.rm = T)
df$h_score_medio[df$h_score_medio < (qnt[1] - H)] <- caps[1]
df$h_score_medio[df$h_score_medio > (qnt[2] + H)] <- caps[2]

ggplot(df, aes(h_score_medio)) + geom_boxplot()

write.csv(df, file=".//dados/4-dados_apos_transformacao.csv")     

summary(df)


