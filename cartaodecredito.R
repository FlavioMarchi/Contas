###### Balanço cartão de crédito ####

closeAllConnections()
rm(list=ls())

library(tidyverse)
library(readr)
library(stringr)
library(rebus)
library(countrycode)
library(XLConnect)
library(readxl)
source("salvar_dados.R")

setwd("C:/Users/Flavio/OneDrive/R Studio/Balanco")

# Carregar dados do cartao #
nome <- "cartao.txt"
caminho <- file.path(getwd(),"Entrada",nome)


dados <- read_tsv(caminho, col_names = "raw",
                    skip = 50)

dados <- dados[!is.na(dados),]

# Eliminando linhas indesejadas
dados <- dados %>% 
  filter(str_detect(raw,"2017")) %>% 
  filter(!str_detect(raw,"acumulada"))


# Agora será feita uma tibble com as seguintes colunas:
#dia  #oque   #local(país)   #valor em R$   #valor em US$
dia <- sapply(dados[,1], str_sub, start = 1, end = 10)[,1]
oque <- sapply(dados[,1], str_sub, start = 11, end = 33)
oque <- str_trim(oque)
pais <- str_trim(sapply(dados, str_sub, start = 48, end = 49))



limpar_numeros <- function(x, start, end){
  x <- x %>% 
    sapply(str_sub, start = start, end = end) %>% 
    str_trim() %>% 
    str_replace(pattern = "\\.", replacement = "") %>% 
    str_replace(pattern = ",", replacement = ".") %>% 
    as.double()
  return(x)
}

taxa_dolar <- function(arquivo){
  con = file(arquivo, "r")
  bandeira <- FALSE
  bandeira2 <- FALSE
  while ( TRUE ) {
    linha = readLines(con, n = 1)
    if ( length(linha) == 0 ) {
      break
    }else if(bandeira == FALSE & bandeira2 == FALSE){
      pattern <- "conversão"
      bandeira <- str_detect(linha, pattern)
    }else if(bandeira == TRUE & bandeira2 == FALSE){
        pattern <- "--------------------------------------------------------------------------------"
        bandeira2 <- str_detect(linha, pattern)
        bandeira <- FALSE
    }else if(bandeira == FALSE & bandeira2 == TRUE){
      conversao <- str_sub(linha, start = 58, end = 63)
      bandeira2 <- FALSE
    }
    
    }
  close(con)
  return (conversao)
}

# Valor em reais
real <- sapply(dados, str_sub, start = 58, end = 68)
real <- str_trim(real)
real <- str_replace(real, pattern = "\\.", replacement = "")
real <- str_replace(real, pattern = ",", replacement = ".")
real <- as.double(real)

# Valor em dólares
dolar <- limpar_numeros(x = dados, start = 70, end =90 )
# dolar <- sapply(dados, str_sub, start = 69, end = 90)

dados <- data_frame(dia = dia,
                    item = oque,
                    pais = pais,
                    real = real,
                    dolar = dolar)

# Eliminando IOF
dados <- dados %>% 
  filter(!str_detect(item, "IOF"))

# Encontrando o câmbio do mês
cambio <- as.double(str_replace(taxa_dolar(arquivo = caminho), pattern = ",", replacement = "."))

# Convertendo dólares para reais, e somando as duas colunas "real" e "dolar" para "valor"
dados$dolar <- dados$dolar*cambio
dados <- dados %>% 
  mutate(valor = dolar + real) %>% 
  select(-dolar,-real)

# Agrupando os gastos
dados <- dados %>% 
  group_by(item) %>% 
  summarize(total = sum(valor))

# Salvando o resultado obtido até agora para o excel, para classificar manualmente
salvar_dados(dados, "preliminar")

# Renomeei o arquivo para "classificacao", para não sobrescrever a tabela feita quando
# rodar o algoritmo novamente e coloquei na pasta "Entrada"

# Carregando o arquivo de classificação
classificacao <- read_xlsx (file.path(getwd(),"Entrada","classificacao.xlsx")) %>% 
  select(-total)

# Agora basta dar o left_join
dados <- left_join(dados, classificacao, by = "item")

# Agrupando por classificacao
dados_final <- dados %>% 
  group_by(classificacao) %>% 
  summarize(total = sum(total))

