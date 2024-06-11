# SETUP ------------------------------------------------------------------------

# limpando o ambiente
# rm(list=ls())

# o codigo acima esta como comentario para evitar 

# instalando o pacote pacman, se necessario
if (!require("pacman")) install.packages("pacman")

# usando o pacote pacman pra instalar e carregar outros pacotes
pacman::p_load(tidyverse,
               rio,
               janitor,
               PNADcIBGE,
               Hmisc,
               gridExtra,
               modelsummary)

# desativando notacoes cientificas
options(scipen = 999)


# OBTENDO DADOS ----------------------------------------------------------------

# esta parte depende de conexao com a internet

# as bases são pesadas, por isso esse script baixa apenas as variaveis 
# necessarias

# baixando pnadc anterior a pandemia
pnad2019 <- get_pnadc(2019,
                      quarter = 1,
                      vars = c("V1023",
                               "V2007",
                               "V2010",
                               "V2009",
                               "V1028",
                               "V1022",
                               "V403312"),
                      labels = TRUE)

# isolando as variaveis de interesse e renomeando
pnad2019 <- pnad2019[["variables"]] %>% 
  select(regiao = V1023,
         sexo = V2007,
         etnia = V2010,
         idade = V2009,
         peso = V1028,
         area = V1022,
         renda = V403312)


# baixando pnadc atual
pnad2024 <- get_pnadc(2024,
                      quarter = 1,
                      vars = c("V1023",
                               "V2007",
                               "V2010",
                               "V2009",
                               "V1028",
                               "V1022",
                               "V403312"),
                      labels = TRUE)

# isolando as variaveis de interesse e renomeando
pnad2024 <- pnad2024[["variables"]] %>% 
  select(regiao = V1023,
         sexo = V2007,
         etnia = V2010,
         idade = V2009,
         peso = V1028,
         area = V1022,
         renda = V403312)


# EXAMINANDO RENDA -------------------------------------------------------------

# checando proporcao de NA em renda 
sum(is.na(pnad2019$renda))/nrow(pnad2019) # 59.9%
sum(is.na(pnad2024$renda))/nrow(pnad2024) # 58.0%


# filtrando pessoas acima de 17 anos e abaixo de 66
pnad2019 <- pnad2019 %>% 
  filter(idade %in% 18:65)

pnad2024 <- pnad2024 %>% 
  filter(idade %in% 18:65)

# checando a proporcao de NA em renda depois de filtrar idade
sum(is.na(pnad2019$renda))/nrow(pnad2019) # 40.7%
sum(is.na(pnad2024$renda))/nrow(pnad2024) # 37.8%


# essa proporcao de valores faltantes pode se dar por desemprego
# ou por simples recusa de resposta
# para evitar maiores complexidades, vamos simplesmente desconsiderar o NA

# dropando NA em renda
pnad2019 <- pnad2019 %>% 
  drop_na(renda)

pnad2024 <- pnad2024 %>% 
  drop_na(renda)


# calculando os decis de renda
wtd.quantile(pnad2019$renda,
             weights = pnad2019$peso,
             probs = seq(0,1, by = .1))

# 0%    10%    20%    30%    40%    50%    60%    70%    80%    90%   100% 
#  4    500    998   1000   1200   1400   1600   2000   2700   4000 250000 

wtd.quantile(pnad2024$renda,
             weights = pnad2024$peso,
             probs = seq(0,1, by = .1))

# 0%    10%    20%    30%    40%    50%    60%    70%    80%    90%   100% 
# 10    800   1400   1412   1600   2000   2250   2900   3700   6000 300000 


# aplicando a divisao de decil pra criar faixa de renda
pnad2019 <- pnad2019 %>% 
  mutate(faixa.renda = cut(renda, 
                           breaks = c(4, 500, 998, 1000, 1200, 1400, 1600, 
                                      2000, 2700, 4000, 250000),
                           include.lowest = FALSE,
                           ordered_result = TRUE,
                           labels = 1:10))


pnad2024 <- pnad2024 %>% 
  mutate(faixa.renda = cut(renda, 
                           breaks = c(10, 800, 1400, 1412, 1600, 2000, 2250, 
                                      2900, 3700, 6000, 300000),
                           include.lowest = FALSE,
                           ordered_result = TRUE,
                           labels = 1:10))

# GRAFICOS ---------------------------------------------------------------------

# criando colunas de ano
pnad2019 <- pnad2019 %>% 
  mutate(ano = 2019)

pnad2024 <- pnad2024 %>% 
  mutate(ano = 2024)

# juntando bases para gerar os graficos
pnad <- rbind(pnad2019, pnad2024)

## Renda por sexo --------------------------------------------------------------
ggplot(pnad, aes(log(renda), sexo)) +
  geom_boxplot(aes(fill = sexo),
               show.legend = FALSE) +
  theme_bw() +
  facet_grid(rows = vars(ano)) +
  labs(title = "Distribuição de Renda por Gênero Antes e Depois da Pandemia",
       caption = "Fonte: PNADC 2019.1 e 2024.1. Elaboração Própria.",
       y = "Gênero",
       x = "Log da Renda")



## Renda por etnia -------------------------------------------------------------
pnad %>% 
  filter(etnia != "Ignorado") %>% 
  ggplot(aes(log(renda), etnia)) +
  geom_boxplot(aes(fill = etnia),
               show.legend = FALSE) +
  facet_grid(rows = vars(ano)) +
  theme_bw() +
  labs(title = "Distribuição de Renda por Etnia Antes e Depois da Pandemia",
       caption = "Fonte: PNADC 2019.1 e 2024.1. Elaboração Própria",
       y = "Etnia",
       x = "Log da Renda")



## Renda por area --------------------------------------------------------------
ggplot(pnad, aes(log(renda), area)) +
  geom_boxplot(aes(fill = area),
               show.legend = FALSE) +
  facet_grid(rows = vars(ano)) +
  theme_bw() +
  labs(title = "Distribuição de Renda por Área Antes e Depois da Pandemia",
       caption = "Fonte: PNADC 2019.1 e 2024.1. Elaboração Própria",
       y = "Área",
       x = "Log da Renda")


## Renda em cada ano -----------------------------------------------------------
ggplot(pnad, aes(log(renda))) +
  geom_boxplot(aes(fill = factor(ano)),
               show.legend = FALSE) +
  facet_grid(rows = vars(ano)) + 
  scale_y_discrete(breaks = NULL) +
  theme_bw() +
  labs(title = "Distribuição de Renda Antes e Depois da Pandemia",
       caption = "Fonte: PNADC 2019.1 e 2024.1. Elaboração Própria.",
       x = "Log da Renda",
       y = "Anos")

# REGRESSAO MULTIPLA -----------------------------------------------------------

# excluindo NA em etnia 
pnad2019.filter <- pnad2019 %>% 
  filter(etnia != "Ignorado")

# essa exclusao se faz necessaria pelo fato de que a etnia "ignorado" consta 
# como uma categoria, de forma que o R nao entende que se trata de um valor
# faltante


# criando um modelo de regressao multipla
reg.2019 <- lm(renda ~ sexo + etnia + idade + area,
          weights = peso,
          data = pnad2019.filter)

# checando resultados
summary(reg.2019)


#                 Estimate Std. Error t value            Pr(>|t|)    
# (Intercept)    1863.2381    26.2037   71.11 <0.0000000000000002 ***
# sexoMulher     -597.6087    14.1508  -42.23 <0.0000000000000002 ***
# etniaPreta    -1176.1194    24.3067  -48.39 <0.0000000000000002 ***
# etniaAmarela    862.5558    83.3182   10.35 <0.0000000000000002 ***
# etniaParda    -1105.8515    14.8417  -74.51 <0.0000000000000002 ***
# etniaIndígena -1278.8645   118.9781  -10.75 <0.0000000000000002 ***
# idade            34.2997     0.5907   58.07 <0.0000000000000002 ***
# areaRural     -1128.9062    22.7207  -49.69 <0.0000000000000002 ***

# todos sao significantes para prever a renda
# mulheres ganham em media 600 reais a menos do que homens
# negros e pargos recebem 1100 a 1200 a menos do que brancos
# indigenas recebem 1300 a menos do que brancos
# moradores de areas rurais recebem 1130 a menos do que urbanos
# o efeito de idade serviu para um maior controle
# 6.5% da renda foi explicada



# repetindo para 2024
pnad2024.filter <- pnad2024 %>% 
  filter(etnia != "Ignorado")

reg.2024 <- lm(renda ~ sexo + etnia + idade + area,
               weights = peso,
               data = pnad2024.filter)

summary(reg.2024)


#                  Estimate Std. Error t value             Pr(>|t|)    
#  (Intercept)    2928.2651    36.6421  79.915 < 0.0000000000000002 ***
#  sexoMulher     -782.9041    19.7098 -39.722 < 0.0000000000000002 ***
#  etniaPreta    -1552.2935    32.0277 -48.467 < 0.0000000000000002 ***
#  etniaAmarela    390.3189   113.1448   3.450             0.000561 ***
#  etniaParda    -1468.4526    20.8722 -70.355 < 0.0000000000000002 ***
#  etniaIndígena -1516.5937   154.2400  -9.833 < 0.0000000000000002 ***
#  idade            36.1402     0.8142  44.387 < 0.0000000000000002 ***
#  areaRural     -1469.1427    33.7551 -43.524 < 0.0000000000000002 ***

# todos os resultados sao estatisticamente significantes
# a desigualdade entre homens e mulheres aumentou
# todas as etnias recebem menos em relacao as brancos
# a diferenca entre area rural e urbana aumentou

# colocando graficamente
modelsummary(list(Antes = reg.2019, Depois = reg.2024),
             stars = TRUE,
             estimate = "{estimate} ({std.error}){stars}",
             statistic = NULL,
             gof_omit = "IC")
