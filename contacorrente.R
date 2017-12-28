###### Balan?o conta corrente ####

closeAllConnections()
rm(list=ls())

library(tidyverse)
library(readr)
library(stringr)
library(rebus)
library(countrycode)
library(XLConnect)
library(lubridate)
library(readxl)

# Carregando fun??es auxiliares
source("salvar_dados.R")

# Determinando o caminho do working directory
setwd("C:/Users/Flavio/OneDrive/R Studio/Balanco")

# Carregar dados do cartao #
nome <- "contacorrente.txt"
caminho <- file.path(getwd(),"Entrada",nome)

# dados <- read_fwf(file = caminho,
#                   col_positions = fwf_widths(widths = c(8, 10, 14)),
#                   skip = 9)

dados <- read_fwf(file = caminho,
                  col_positions = fwf_positions(start = c(1,18,69,80),
                                                  end = c(8,43,78,80)),
                  skip = 9)

# Nomeando as colunas
colnames(dados) <- c("data","oque","valor","sinal")

# Mudando a codificação, o banco fornece em ANSI, o que deu problemas no final
dados$oque <- iconv(dados$oque, to = "UTF-8")

# Filtrando valores da data que contenham "17", quer dizer que esses valores s?o
# os desejados
dados_invertidos <- dados
dados <- dados %>% 
  filter(str_detect(data,"17")) %>% 
  filter(!(str_detect(oque, "S A L D O"))) %>% 
  filter(!(str_detect(oque, "Saldo Anterior")))

# Transformando debito e cr?dito em -1 e 1, para multiplicar pela coluna valor,
# para obter o valor positivo ou negativo.
fun_sinal <- function(x){
  if (x == "C"){ return (-1)}
  else {return (1)}
}
dados$sinal <- sapply(dados$sinal, fun_sinal)

# Trocando ponto por nada (separador de milhar), e virgula por ponto

dados$valor <- str_replace(dados$valor, pattern = "\\.", replacement = "")
dados$valor <- str_replace(dados$valor, pattern = ",", replacement = ".")

# Mudando os tipos das colunas para os tipos corretos
dados$data <- dmy(dados$data)
dados$valor <- as.double(dados$valor)
dados$sinal <- as.integer(dados$sinal)

# Coluna valor multiplica pela de sinal, eliminamos o valor inicial e sinal
dados <- dados %>% 
  mutate(valor1 = valor*sinal) %>% 
  select(-valor, -sinal)

colnames(dados)[3] <- "valor"

# Vamos filtrar a dataframe dados_invertidos para obter a descri??o dos gastos
# que n?o est?o na linha referente ? data do lan?amento
dados_invertidos <- dados_invertidos %>% 
  filter(is.na(data))

# Aqui filtro apenas os retultados com data no formato digito digito/digito digito e o hor?rio
# no come?o, para associar a dados valores da df dados, que s?o os pagamentos por cart?o de d?bito
# e os saques

padrao <- START %R% DGT %R% DGT %R% "/" %R% DGT %R% DGT %R% " " %R% # data
  DGT %R% DGT %R% ":" %R% DGT %R% DGT %R% " " # hor?rio
dados_invertidos <- dados_invertidos %>% 
  filter(str_detect(oque, pattern = padrao))

# Eliminando a data e o hor?rio
dados_invertidos$oque <- str_replace(dados_invertidos$oque, 
                                     pattern = padrao, replacement = "")

dados_comparar <- dados %>%
  filter(str_detect(oque,"Compra com") |
           str_detect(oque, "Saque"))

# Agora podemos inserir a coluna "oque" de dados_invertidos em dados_comparar
dados_comparar$oque2 <- dados_invertidos$oque

# Temos de fazer uma fun??o para substituir os valores da coluna "oque" de dados
# que coincidam com os valores "oque" da 

dados_join <- left_join(dados, dados_comparar, by = c("oque","valor","data"))

# mudando os valores de "oque" em dados para "oque2" de dados_join que n?o sejam NA
indices <- !is.na(dados_join$oque2)
dados$oque[indices] <- dados_join$oque2[indices]

# Agrupando pela coluna "oque" e fazendo o total de cada lan?amento distinto
dadosiik <- dados
dados <- dados %>%
  group_by(oque) %>% 
  summarize(total = sum(valor)) %>% 
  filter(oque != "Aplicação Poupança") %>% 
  filter(oque != "Pagto cartão crédito")

salvar_dados(dados, "contacorrente_lancamentos")

########################


# Carregando o arquivo de classifica??o
classificacao <- read_xlsx (file.path(getwd(),"Entrada","classificacao_corrente.xlsx")) %>%
  select(-total)

# Agora basta dar o left_join
dados <- left_join(dados, classificacao, by = "oque")
# 
# Agrupando por classificacao
dados_final <- dados %>%
  group_by(classificacao) %>%
  summarize(total = sum(total))
# 
# dados_final$total <- dados_final$total/2+32.54
