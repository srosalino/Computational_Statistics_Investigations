# Trabalho realizado por: Luís Pereira nº 98398 e Sebastião Rosalino nº 98437
# Turma CDB1

# Carregar biblioteca prob ???????

#####install.packages("prob")
#####library(prob)

library(readxl)
dados_equipas <- read_excel("C:/Universidade_ISCTE/2ºAno/1º_Semestre/Estatística Computacional/Variáveis aleatórias e distribuições importantes/2021-2022-EstComp-Ficha3-dados_equipas.xlsx")

header = colnames(dados_equipas)
colnames(dados_equipas) = c("IPC", "Equipas")
dados_equipas = rbind(header, dados_equipas)
dados_equipas$`IPC` = as.numeric(dados_equipas$`IPC`)
View(dados_equipas)


##############
# Grupo A.   #
##############

# 1 - Determine os Intervalos de Confiança concretos para o parâmetro em causa (a média), 
#     tendo como base a amostra existente no ficheiro dados_equipas.xlsx. Num comentário, 
#     interprete os intervalos de confiança obtidos (um para cada equipa, separadamente).


dados_equipas <- data.frame(dados_equipas)

equipa_1 <- subset(dados_equipas, dados_equipas$Equipa == "Equipa1")

equipa_2 <- subset(dados_equipas, dados_equipas$Equipa == "Equipa2")

confidence_level <- 0.95

n1 <- length(equipa_1$IPC)

n2 <- length(equipa_2$IPC)

sd_1 <- sd(equipa_1$IPC)

sd_2 <- sd(equipa_2$IPC)

mean_1 <- mean(equipa_1$IPC)

mean_2 <- mean(equipa_2$IPC)

t2 <- qt((1-confidence_level) / 2, n1 - 1, lower.tail = FALSE)
  
margin_error_1 <- t2* sd_1 / sqrt(n1)

margin_error_2 <- t2* sd_2 / sqrt(n2)

lower_bound_1 <- mean_1 - margin_error_1

upper_bound_1 <- mean_1 + margin_error_1

lower_bound_2 <- mean_2 - margin_error_2

upper_bound_2 <- mean_2 + margin_error_2
 
## Resposta:

cat("o IC a 95% para a produtividade média da equipa A é ]", lower_bound_1, upper_bound_1, "[")

cat("o IC a 95% para a produtividade média da equipa A é ]", lower_bound_2, upper_bound_2, "[")


# 2 - Indique o valor da Margem de Erro (Erro Amostral) para os intervalos em causa

## Resposta:
  
margin_error_1
margin_error_2

# 3 - A Direção exige uma confiança maior na previsão efetuada. Por isso, decidiu solicitar 
#     que os Intervalos de Confiança em causa fossem recalculados para uma confiança de 99%. 
#     Calcule os novos intervalos de confiança, usando a amostra existente no ficheiro 
#     (dados_equipas.xlsx). Num breve comentário, comente qual o efeito desse aumento do 
#     nível de confiança nos intervalos encontrados.


new_confidence_level <- 0.99

new_t2 <- qt((1-confidence_level) / 2, n1 - 1, lower.tail = FALSE)

new_margin_error_1 <- new_t2* sd_1 / sqrt(n1)

new_margin_error_2 <- new_t2* sd_2 / sqrt(n2)

new_lower_bound_1 <- mean_1 - new_margin_error_1

new_upper_bound_1 <- mean_1 + new_margin_error_1

new_lower_bound_2 <- mean_2 - new_margin_error_2

new_upper_bound_2 <- mean_2 + new_margin_error_2

cat("o IC a 95% para a produtividade média da equipa A é ]", new_lower_bound_1, new_upper_bound_1, "[")

cat("o IC a 95% para a produtividade média da equipa A é ]", new_lower_bound_2, new_upper_bound_2, "[")


# 4 - Exercício Teórico. Admitindo que a variância populacional do Indicador de 
#     Produtividade da Equipa 1 é 0.36, e que a distribuição desse Indicador pode ser considerada 
#     Normal, qual a dimensão da amostra a considerar para garantir um erro máximo de 0.2 na 
#     estimação por intervalos da respetiva média, a 95% de confiança?


new_varianca_equipa_1 <- 0.36

erro <- 0.2

dimensao_da_amostra <- ((1.96 * sqrt(new_varianca_equipa_1)/erro)^2)

cat("A dimensão da amostra é", dimensao_da_amostra)


##############
# Grupo B.   #
##############

# a) Tendo em conta a opinião do chefe da equipa 2, se se pretender fazer um teste de 
#    hipóteses sobre o valor médio do indicador em causa, formule/indique as hipóteses a serem 
#    consideradas no respetivo Teste de Hipóteses. 


cat("A hipótese nula H0 é o chefe da equipa 2 não ter razão na sua afirmação ou seja a produtividade média da sua equipa ser inferior ou igual a 7 (<= 7). Já a hipótese H1 é o ser chefe razão na sua afirmação ou seja a produtividade média ser maior que 7 (> 7)")




# b) Indique se o chefe da equipa 2 tem razão. Para o efeito, efetue um teste de hipóteses 
#    para o parâmetro em causa, utilizando o nível de significância de ???? = 0.05 e utilizando os 
#    dados amostrais recolhidos no ficheiro dados_equipas.xlsx. Interprete os resultados obtidos.



#    O chefe da equipa 2 prosseguiu a reunião dizendo:
#    Chefe equipa 2: "Posso afirmar que não existe diferença entre a produtividade média 
#    da minha equipa e a produtividade média da equipa 1." 





# c) Tendo em conta a opinião do chefe da equipa 2, se se pretender fazer um teste de 
#    hipóteses sobre o valor médio do indicador em causa, formule/indique as hipóteses a serem 
#    consideradas no respetivo Teste de Hipóteses. 






# d) Indique se o chefe da equipa 2 tem razão. Para o efeito, efetue um teste de hipóteses 
#    para o parâmetro em causa, utilizando o nível de significância de ???? = 0.05 e utilizando os 
#    dados amostrais recolhidos no ficheiro dados_equipas.xlsx. Interprete os resultados obtidos.





# e) Num breve comentário, indique qual o tipo de erro em que poderá estar a incorrer, com 
#    as conclusões que retirou no ponto d). 





# f) O Chefe da equipa 1 afirma que a produtividade média da sua excede em mais de 1 ponto 
#    (na escala de 0 a 10) a produtividade média da equipa 2. Terá razão? 

#    i) Formule as hipóteses, 
#   ii) realize o teste e 
#   iii) interprete os resultados obtidos







