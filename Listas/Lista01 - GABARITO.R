# Lista 1

library(tidyverse)
library(magrittr)

# 1. ----
# 1a. Atribua valor 22 à etiqueta (objeto) chamada 'pl_MDB_2017'
# representando uma quantidade de pl's encaminhados por
# deputados do MDB em 2017 (dado fictício)
pl_MDB_2017 <- 22

# 1b. Qual é a classe do seu objeto 'pl_MDB_2017'?
class(pl_MDB_2017)
#[1] "numeric"

# 1c. Informe (como comentário) qual o tipo dessa variável
## R: Numérica


# 1d. Calcule a média da quantidade de pl's do MDB em 2017 e 2018
# sabendo que, em 2018, foram 33

mean(c(pl_MDB_2017,33))

#[1] 27,5


# 1e. Informe (como comentário) um tipo de gráfico indicado caso eu queira entender a relação entre 
# a quantidade de projetos de lei encaminhados por deputados e seus partidos 
# R: Barras com médias por partido, boxplot por partido

# 2. -----

# Leia o script abaixo e comente com as hashtags o que cada código quer dizer:


# 2a.
senador <- 334
# O objeto senador tem valor atribuído 334

# 2b.
class(senador)
# comando que informa a classe do objeto 'senador'

# 2.c
name <- "Flavio"
# O objeto name tem valor atribuído "Flavio"

# 2.d
class(senador) != class(name)
# teste lógico verificando se as classes dos dois objetos são diferentes


# 3. -----
# 3a. Crie um vetor com o nome dos países da América Latina e chame de AL
AL <- c('Argentina', 'Bolívia', 'Brasil', 'Chile', 'Colômbia', 'Costa Rica',
        'Cuba', 'Equador', 'El Salvador', 'Guatemala', 'Haiti', 'Honduras',
        'México', 'Nicarágua', 'Panamá', 'Paraguai', 'Peru',
        'República Dominicana', 'Uruguai' , 'Venezuela')


# 3b. Utilizando a função nchar(), que calcula o número de caracteres de um valor nominal, crie um vetor com o número de caracteres de cada país do vetor AL. 
# Qual é o maior valor e o menor valor
charAL <- nchar(AL)
max(charAL)
# 20
min(charAL)
# 4

# ps.: vi respostas usando summary, muito bom

# 3c. Quantos países possuem mais de 6 caracteres no nome?
AL[nchar(AL)>6]
length(AL[nchar(AL)>6])
# 13

# 3d. Crie um novo vetor a partir do vetor AL com países que não estão incluídos dentro do MERCOSUL
MERCOSUL <- c('Argentina', 'Brasil','Uruguai','Paraguai','Venezuela')

AL.not.MERCOSUL <- AL[!is.element(AL,MERCOSUL)]

# 3e. Qual é o somatório dos nchar() desse novo vetor
sum(nchar(AL.not.MERCOSUL))

# 119

# 4. ---- 

# Reescreva a expressão abaixo utilizando o %>% (pipes).

round(mean(divide_by(sum(1:10),3)),digits = 1)

1:10 %>%
  sum() %>%
  divide_by(3) %>%
  mean() %>%
  round(digits = 1)


# 5. ----

# 5a. Utilizando a função candidate_fed() do pacote electionsBR
# escreva um objeto com os dados de candidates da eleições nacionais/estaduais
# referentes apenas ao DF no ano de 2018
cand.df_2018 <- electionsBR::candidate_fed(year = 2018,uf="DF")

