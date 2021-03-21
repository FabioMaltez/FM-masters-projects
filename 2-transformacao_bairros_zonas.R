#
# Etapa 2 - Transformação bairros em zonas
#
# Script reponsável por criar 3 colunas dummies de acordo com a variável descritiva $hotel_loc, que apresenta o bairro (freguesia) onde o hotel situa-se
# as variáveis dummies separam as observações de acordo com a região onde ficam os hoteis: (zona_norte, zona_oeste, zona_sul ou zona_central)
# e são binomiais (0/1)
#
# Para reduzir as variáveis incluimos somente tres das quatro zonas, são elas: "zona_central", "zona_oeste" e "zona_sul"
# "zona_norte" foi a excluída (a princípio porque tinha menos observações, não que isto interfira nas observações)
# Por dedução sabemos que aqueles que tem as três variáveis com o valor 0 pertencem à "zona_norte"
#
# Por fim, excluimos a variável $hotel_loc dos dados
#

path_to_data <- ".\\dados\\0-dados_originais_apos_normalizacao.csv"

booking_dados <-read.csv(path_to_data,sep = ",",dec = ".", header = T)

levels(as.factor(booking_dados$hotel_loc))

zona_central <- c("Centro do Rio de Janeiro","Glória","Porto Maravilha","Santa Teresa","Rio de JaneiroMostrar no mapa","Gamboa")

zona_oeste <- c("Barra da Tijuca","Recreio dos Bandeirantes","Jacarepaguá","Joatinga")

zona_sul <- c("Copacabana","São Conrado","Ipanema","Leblon","Leme","Botafogo","Catete","Flamengo","Gávea","Laranjeiras","Vidigal","Cosme Velho","Jardim Botânico","Urca","Zona Sul")

booking_dados$zona_central <- 0
booking_dados$zona_oeste <- 0
booking_dados$zona_sul <- 0


for (i in 1:length(booking_dados$hotel_loc)){
  booking_dados$zona_central[i] = ifelse(trimws(booking_dados$hotel_loc[i]) %in% zona_central,1,0)
  booking_dados$zona_oeste[i]   = ifelse(trimws(booking_dados$hotel_loc[i]) %in% zona_oeste,1,0)
  booking_dados$zona_sul[i]     = ifelse(trimws(booking_dados$hotel_loc[i]) %in% zona_sul,1,0)
}

write.csv(booking_dados, file=".\\dados\\2-dados_transformados_com_zonas.csv")



