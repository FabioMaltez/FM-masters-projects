#
# Etapa 0 - WebScrapping
#
# Para executar o webscrapping execute a function conforme descrito abaixo:
# read_and_normalize_data_from_booking_riodejaneiro()
#

library(rvest)
library(stringr)


clean_html <- function(html_to_be_cleaned) {
  return (gsub("\n","",html_to_be_cleaned))
  
}

read_booking_riodejaneiro <- function() {
  
  # current date
  current_time <- str_extract(Sys.time(),"[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}")
  current_date <- strsplit(current_time,"-")
  day <- current_date[[1]][3]
  month <- current_date[[1]][2]
  year <- current_date[[1]][1]
  # URL da 1ª página
  # Aviso: Link não funciona no último dia de cada mês
  # devido à utilização da função paste()
  
  # inicializar o dataframe
  df_hotel <- data.frame(stringsAsFactors = FALSE,
                   h_hotelid=character(0),
                   h_in_page=character(0),
                   hotel_name=character(0),
                   hotel_loc=character(0),
                   h_dist_centro=character(0),
                   h_dist_praia=character(0),
                   h_num_stars=character(0),
                   h_num_comments=character(0),
                   h_prices=character(0),
                   h_peq_almoco=character(0),
                   h_canc_gratuito=character(0),
                   h_quartos_restantes=character(0),
                   h_score_medio=character(0))
  
  # inicializar as variáveis
  # maximo de paginas do booking que o script irá ler
  total_pages_to_read=100
  
  # repetir webscrapping (usado para tentar minimizar o problema dos hoteis que mudam de pagina durante a execução, 
  # e tambem a variável distancia_praia que não é sempre carregada pelo booking, sem motivo aparente)
  total_repeat = 5
  
  h_in_page <- c()
  hotel_name <- c()
  hotel_loc <- c()
  h_dist_centro <- c()
  h_dist_praia <- c()
  h_num_stars <- c()
  h_num_comments <- c()
  h_prices <- c()
  h_peq_almoco <- c()
  h_canc_gratuito <- c()
  h_quartos_restantes <- c()
  h_score_medio <- c()
  h_hotelid <- c()
  
  for (currentAttempt in 0:total_repeat){# Repetição
  
    for (currentpage in 0:total_pages_to_read){ # Paginação
      
      link_booking <- paste("https://www.booking.com/searchresults.pt-pt.html?label=Hoteis&ss=Rio+de+Janeiro&checkin_year=",
                            year,"&checkin_month=",month,"&checkin_monthday=",day,"&checkout_year=",year,"&checkout_month=",month,
                            "&checkout_monthday=",as.character(as.integer(day)+1),"&group_adults=2&group_children=0&no_rooms=1&rows=25&offset=",currentpage*25,
                            sep = "") 
      
      page <- link_booking %>% read_html()
      Sys.sleep(runif(1,3,5))
      
      # Tenta encontrar algum hotel
      elem.list <- page %>% html_nodes(xpath = '//*[contains(@class,"sr-hotel__name")]')
       
      if(length(elem.list)==0)
      {
        # Abandona o loop, nao há mais pages para ler
        break
      }

      # Se é com pequeno almoço incluído, numero de estrelas, numero de comentarios e score medio
      # Para essas variaveis, primeiro encontramos todos os hoteis, e depois olhamos para os dados (se existirem)
      elem.list <- page %>% html_nodes(xpath = '//div[contains(@class,"sr_item_default")]')
      
      for (i in 1:length(elem.list)){
        
        #id do hotel
        hotel_id <- elem.list[i][1] %>% html_attr("data-hotelid")
        
        #verifica se o hotel já se encontra na lista
        index_hotel <- match(hotel_id, h_hotelid)
        
        if( is.na(index_hotel) )# caso seja a primeira leitura do hotel
        {
          #adiciona o id na lista
          h_hotelid <- c(h_hotelid, hotel_id)
          
          # Armazena o numero da page (apenas para troubleshooting)
          h_in_page <- c(h_in_page,currentpage)
          
          # Nome do Hotel
          elem.hotel_name <- elem.list[i][1] %>% html_nodes(xpath = 'child::*//span[contains(@class,"sr-hotel__name")]')
          hotel_name <- c(hotel_name, paste( clean_html( html_text( elem.hotel_name ) ) ) )
          
          # Localizacao do hotel
          elem.hotel_local <- elem.list[i][1] %>% html_nodes(xpath = 'child::*//div/*[contains(@rel,"noopener")]')
          location <- clean_html( html_text( elem.hotel_local ) )
          location <- strsplit(location, ",")
          
          hotel_loc <- c(hotel_loc, paste( location[[1]][1] ) )
          
          # distacia ao centro da cidade
          dist_centro_cidade <- elem.list[i][1] %>% html_nodes(xpath = 'child::*//*[contains(@data-tooltip-position,"top")]')
          dist_centro_cidade_text <- clean_html(html_text(dist_centro_cidade))
          for (j in 1:length(dist_centro_cidade_text)){
            if(grepl("centro",dist_centro_cidade_text[j],fixed=TRUE) == TRUE){
               h_dist_centro <- c(h_dist_centro,paste(dist_centro_cidade_text[j]))
            }
          }
          
          # Cancelamento gratuito? Y/N
          canc_gratuito <- elem.list[i][1] %>% html_nodes(xpath = 'child::*//div/*[contains(@class,"sr_card_room_policies__container")]')
          canc_gratuito_text <- clean_html(html_text(canc_gratuito))
          for (j in 1:length(canc_gratuito_text)){
            h_canc_gratuito <- c(h_canc_gratuito, ifelse(grepl("Pequeno-almoço incluído",canc_gratuito_text[j],fixed=T) == T,"1","0"))
          }
          
          # # Preço quarto 1 noite, 2 pessoas
          preco_diaria <- elem.list[i][1] %>% html_nodes(xpath = 'descendant::*//div[contains(@class,"bui-price-display__value prco-inline-block-maker-helper")]')
          if ( length(preco_diaria) > 0 )
          {
            preco_diaria_text <- clean_html(html_text(preco_diaria))
            for (j in 1:length(preco_diaria_text)){
              if (preco_diaria_text[j] != "..."){
                h_prices <- c(h_prices,paste(preco_diaria_text[j]))
              }
            }
          }
          
          # Verifico se o hotel em questao tem o pequeno almoco
          elem.peq_almoco_nodes <- elem.list[i][1] %>% html_nodes(xpath = 'child::*//div[contains(@class,"bfast-included-ribbon")]')
          h_peq_almoco <- c(h_peq_almoco, ifelse(length(elem.peq_almoco_nodes) > 0, "1", "0"))
      
          #Total de estrelas
          elem.num_stars <- elem.list[i][1] %>% html_nodes(xpath = 'child::*//*[contains(@aria-label,"out of 5")]')
          
          h_num_stars <- c(h_num_stars, ifelse(length(elem.num_stars) > 0, trimws(str_split(html_attr(elem.num_stars[1],"aria-label"),"out",simplify=TRUE)[,1]), ""))
          
          #Num de comentarios
          elem.comments <- elem.list[i][1] %>% html_nodes(xpath = 'child::*//div/*[contains(@class,"bui-review-score__text")]')
          
          h_num_comments <- c(h_num_comments, ifelse(length(elem.comments) > 0, str_split(trimws(clean_html(html_text(elem.comments[1])))," ",simplify=TRUE)[,1], "0"))
          
          # Score médio do Hotel
          elem.score <- elem.list[i][1] %>% html_nodes(xpath = 'child::*//div/*[contains(@aria-label,"Pontuado com")]')
          
          h_score_medio <- c(h_score_medio, ifelse(length(elem.score) > 0, trimws(clean_html(html_text(elem.score[1]))), ""))
  
          # Distancia da praia
          elem.dist_praia <- elem.list[i][1] %>% html_nodes(xpath = 'descendant::*//div[contains(@class,"beach_team_pilot_distance")]')
  
          h_dist_praia <- c(h_dist_praia, ifelse(length(elem.dist_praia) > 0, trimws(clean_html(html_text(elem.dist_praia[1]))), ""))
  
          # Quartos restantes
          elem.str_quartos_restantes <- elem.list[i][1] %>% html_nodes(xpath = 'child::*//span[contains(@class,"sr_rooms_left_wrap")]')
          
          h_quartos_restantes <- c(h_quartos_restantes, ifelse(length(elem.str_quartos_restantes) > 0, "1", "0"))
        }
        else
        {
          # Distancia da praia (por algum motivo não claro, nem sempre esta variavel é carregada pelo booking, e por isso tentamos carregá-la
          # novamente nas repetições)
          elem.dist_praia <- elem.list[i][1] %>% html_nodes(xpath = 'descendant::*//div[contains(@class,"beach_team_pilot_distance")]')
          
          if (length(elem.dist_praia) > 0){
            h_dist_praia[index_hotel] <- trimws(clean_html(html_text(elem.dist_praia[1])))
          }
        }
      }
    }
  }
  # guardar as variáveis na base de dados
  df_hotel <- rbind(df_hotel,
                    data.frame(stringsAsFactors = FALSE,
                               h_hotelid=h_hotelid,
                               h_in_page=h_in_page,
                               hotel_name=hotel_name,
                               hotel_loc=hotel_loc,
                               h_dist_centro=h_dist_centro,
                               h_dist_praia=h_dist_praia,
                               h_num_stars=h_num_stars,
                               h_num_comments=h_num_comments,
                               h_prices=h_prices,
                               h_peq_almoco=h_peq_almoco,
                               h_canc_gratuito=h_canc_gratuito,
                               h_quartos_restantes=h_quartos_restantes,
                               h_score_medio=h_score_medio))
  
  #write.csv(df_hotel, file="teste.csv")
  # fazer return da base de dados
  return(df_hotel)
}

read_and_normalize_data_from_booking_riodejaneiro <- function() {
  #leitura dos dados
  booking_dados <- read_booking_riodejaneiro()
  write.csv(booking_dados, file=".\\dados\\0-dados_originais.csv")
  
  # Preparar a BD
  # passar h_dist_centro para numérico, em km
  for (i in 1:length(booking_dados$h_dist_centro)){
    dist_centro <- strsplit(as.character(booking_dados$h_dist_centro[i]), " ")[[1]][1]
    medida <- strsplit(as.character(booking_dados$h_dist_centro[i]), " ")[[1]][2]
    # passar todas as vírgulas para pontos
    dist_centro <- gsub(",",".",dist_centro)
    if (medida == "m"){
      dist_centro <- as.numeric(dist_centro)/1000
    }
    booking_dados$h_dist_centro[i] <- paste(dist_centro,collapse = " ")
  }
  booking_dados$h_dist_centro <- as.numeric(booking_dados$h_dist_centro)
  # booking_dados$h_dist_centro

  # passar o h_prices para numérico, em euros
  # dificuldade em eliminar o sinal do euro
  for (i in 1:length(booking_dados$h_prices)){
    booking_dados$h_prices[i] <- paste(strsplit(booking_dados$h_prices[[i]][1], "")[[1]][c(-1,-2)],collapse = "")
  }
  booking_dados$h_prices <- as.numeric(booking_dados$h_prices)
  # booking_dados$h_prices


  # passar h_score_medio para numérico
  for (i in 1:length(booking_dados$h_score_medio)){
    # passar todas as vírgulas para pontos
    if (grepl(",",booking_dados$h_score_medio[i]) == T){
      num <- as.character(booking_dados$h_score_medio[i])
      num <- strsplit(num,"")
      for (j in 1:length(num[[1]])){
        if (num[[1]][j] == ","){
          num[[1]][j] <- "."
        }
      }
      booking_dados$h_score_medio[i] <- paste(num[[1]],collapse = "")
    }
  }
  booking_dados$h_score_medio <- as.numeric(booking_dados$h_score_medio)
  # booking_dados$h_score_medio

  # passar h_num_comments para numérico
  for (i in 1:length(booking_dados$h_num_comments)){
    # Remover espacos
    booking_dados$h_num_comments[i] <- str_replace_all(booking_dados$h_num_comments[i],"\\s","")
  }

  booking_dados$h_num_comments <- as.numeric(booking_dados$h_num_comments)
  
  # passar texto da distancia da praia em numerico
  for (i in 1:length(booking_dados$h_dist_praia)){
    if(!is.na(booking_dados$h_dist_praia[i]) && booking_dados$h_dist_praia[i] != "") {
      if (trimws(booking_dados$h_dist_praia[i],whitespace = "[\\h]") == "Em frente à praia")
      {
        booking_dados$h_dist_praia[i] = "0"
      }
      else
      {
        #aplica regex para extrair o numero e converte ',' em '.', e por fim converte em numero
        num_dist_praia <- as.numeric( str_replace_all(str_extract(booking_dados$h_dist_praia[i], "(\\d+,?)+"),",",".") )
        
        # se não tiver o simbolo km no texto, deduzimos que a medida esta em metros, e deve ser convertida
        if (!grepl("km",booking_dados$h_dist_praia[i],fixed=TRUE))
        {
          num_dist_praia = num_dist_praia/1000
        }
        
        booking_dados$h_dist_praia[i] = num_dist_praia
      }
    }
  }
  booking_dados$h_dist_praia <- as.numeric(booking_dados$h_dist_praia)
  
  #passar os quartos restantes para numerico
  booking_dados$h_quartos_restantes <- as.numeric(booking_dados$h_quartos_restantes)
  
  write.csv(booking_dados, file=".\\dados\\0-dados_originais_apos_normalizacao.csv")
}
