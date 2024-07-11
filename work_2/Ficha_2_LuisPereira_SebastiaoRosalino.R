# Trabalho realizado por: Luís Pereira nº 98398 e Sebastião Rosalino nº 98437
# Turma CDB1

# Carregar biblioteca prob

install.packages("prob")
library(prob)


##############
# Grupo A.   #
##############

# 1) Num comentário em R, defina a variável em causa, indicando o respetivo modelo 
#    de distribuição de probabilidades (nome e parâmetros).

## Resposta:A variável em causa [Número de potenciais eleitores contactados por dia] é uma 
##          variável aleatória discreta, infinita, com distribuição de Poisson e com
##          pârametros de média [E[X]] e variância [Var [X]] igual a 20. 


# 2) Tendo em conta a distribuição em causa, crie o respetivo gráfico da função de
#    probabilidades e da função de distribuição. 


# Como o suporte da Poisson é infinito, embora numerável,
# vamos representar graficamente os valores necessários para a acumulação de 
# 0.99999 ("desprezamos" 0.00001)


## Resposta:

  ## Função Probabilidade (pf: executar o código abaixo para visualizar o gráfico da função de 
  ## probabilidades)

media <- 20
top<-qpois(0.99999,media) # Dada a probabilidade 0.99999, determina o primeiro valor de x em que esse acumulado é atingido.
plot(dpois(c(0:top),media),type="h",lwd=4,col="blue",
     xlab="x", ylab="f(x)",main="Função de probabilidade de Po(20)")


  ## Função Distribuição (pf: executar o código abaixo para visualizar o gráfico da função de 
  ## distribuição)

plot.stepfun(stepfun(c(0:top),c(0,ppois(c(0:top),media)),right=FALSE),
             verticals=FALSE,main="Função Distribuição",xlab="Número de potenciais eleitores",ylab="Probabilidade")


# 3) Qual a probabilidade de em determinado dia, o chefe ficar satisfeito?

# O chefe fica satisfeito satisfeito se forem contactadas pelo menos 15 pessoas

prob_chefe_ficar_satisfeito <- ppois(14, 20, lower.tail = FALSE) # P[X > 14] «=» P[X >= 15]

cat("A probabilidade de num certo dia o chefe ficar satisfeito é", prob_chefe_ficar_satisfeito)

## Resposta: A probabilidade de num certo dia o chefe ficar satisfeito é 0.8951357, ou seja, de aproximadamente 89,5%.


# 4) Qual deverá ser o número de contactos mínimo por dia, para que seja, pelo menos 0.95 a 
#    probabilidade do chefe ficar satisfeito?

# Para saber o mínimo de contactos (K) para que a probabilidade do chefe ficar satisfeito seja pelo menos 0.95,
# o que pode ser traduzido em P[X >= k] >= 0.95 ou P[X < k] <= 0.05. Assim, teremos de calcular o k usando qpois(0.05, 20)

# Começa-se por calcular a probabilidade para o quantil 0.05:
qpois(0.05, 20) # o resultado é de 13.

# Calcula-se a probabilidade para 13 eleitores
ppois(13, 20, lower.tail = FALSE) # e o resultado é de 0.9338724

# A probabilidade de, com 13 eleitores inquiridos, o chefe ficar satisfeito é 0.9338724, o que é inferior a
# 0.95, pelo que teremos de fazer o cálculo para 12 eleitores

# Verificando a probabilidade de, para 12 eleitores inquiridos, o chefe ficar satisfeito:

ppois(12,20, lower.tail = FALSE) # resultado é 0.960988

# Para 12 eleitores a condição é satisfeita, porque o resultado é de 0.960988

## Resposta: Deve haver um número mínimo de 12 pessoas contactadas para que o chefe esteja satisfeito.


# 5) Qual a probabilidade de num determinado mês (30 dias), serem inquiridas no máximo 550 pessoas?

# Utilizamos a probabilidade da Poisson para determinar o valor do p na Binomial
# Neste caso, multiplicamos o valor esperado diário (20) pelos 30 dias indicados para cálculo da probalidade pedida,
# usando a função distribuição (ppois).

cat("A probabilidade de serem inquiridas no máximo 550 em 30 dias é", ppois(550, 600))
    
## Resposta: A probabilidade de serem inquiridas no máximo 550 em 30 dias é 0.02054038. 


# 6) Crie uma amostra aleatória de 365 observações (1 ano) com base na distribuição do número 
#    de contactos diários. Represente graficamente a informação recolhida. Calcule e interprete 
#    a média amostral, no contexto dado.

# Geração da amostra:

amostra_365 <- rpois(365, 20)

# Representação gráfica da amostra:

hist(amostra_365, xlim = c(0, 40), ylim = c(0,80), xlab = "Número de potenciais eleitores", ylab = "Número de dias verificados", main = "Representação gráfica da amostra")

# Cálculo da média amostral:

media_amostral <- mean(amostra_365)

cat("A média amostral é", media_amostral)

## Resposta: A média amostral é  20.07123 (valor observado numa execução), ou seja, relativamente à amostra em causa, podemos concluir que num período de
##           365 dias são contactados, em média, 20 indivíduos por dia.


# 7) Qual a probabilidade de em 15 dias, existirem 10, onde o chefe fica satisfeito?

# Usa-se primeiro o dbinom para obter a probalidade num ponto (10), sabendo que o segundo parâmetro (size: dias) é igual a 15,
# sendo que o terceiro parâmetro (probabilidade) é o valor da probabilidade do chefe ficar satisfeito num certo dia (exerc.3).

cat("A probabilidade de em 15 dias, existirem 10, onde o chefe fica satisfeito é", dbinom(10, 15, prob = ppois(14, 20, lower.tail = FALSE)))

## Resposta: A probabilidade de em 15 dias, existirem 10, onde o chefe fica satisfeito é 0.01257715


# 8) Qual a probabilidade de em 15 dias, existirem 13, onde o chefe fica satisfeito? Compare o 
#    resultado com a alínea anterior e num breve comentário em R, comente o porquê da diferença. 

# Usa-se primeiro o dbinom para obter a probalidade num ponto (13), sabendo que o segundo parâmetro (size: dias) é igual a 15,
# sendo que o terceiro parâmetro (probabilidade) é o valor da probabilidade do chefe ficar satisfeito num certo dia (exerc.3).

cat("A probabilidade de em 15 dias, existirem 13, onde o chefe fica satisfeito é", dbinom(13, 15, prob = ppois(14, 20, lower.tail = FALSE)))

## Resposta: A probabilidade de em 15 dias, existirem 13, onde o chefe fica satisfeito é 0.2735269

## A razão pela qual a probabilidade de em 15 dias, existirem 13, onde o chefe fica satisfeito ser superior à probabilidade 
## de em 15 dias, existirem 10, onde o chefe fica satisfeito, deve-se ao facto da satisfação ou insatisfação num 
## determinado dia não ser influenciada pelos resultados obtidos nos restantes dias, ou seja, é independente, logo, como a  
## probabilidade em geral (exerc. 3) do chefe ficar satisfeito é 0.8951357 e a probabilidade do chefe ficar insatisfeito (à contrário) 
## é de 0.1048643, resulta que a probabilidade obtida da alínea 8 (mais dias satisfeito) seja superior à da alínea 7 (menos dias satisfeito).


##############
# Grupo B.   #
##############


# 9)  Calcule as seguintes probabilidades:

# 1. P ( X = 5)

## Resposta: Como se trata de uma distribuição contínua, a probabilidade num certo ponto é sempre 0.

    
# 2. A probabilidade do custo de manutenção da equipa ser superior a 6 mil euros.

# Replicação da função densidade probabilidade em R

fx <- function(x){
  ifelse((2 < x & x < 10), (1/496)*x^2 + 1/24,0)}

# Cálculo da probabilidade da área pedida usando um integral com limite inferior 6 e limite superior 10 

prob_custo_superior_6_mil <- integrate(fx, 6, 10, abs.tol = TRUE)

cat("A probabilidade do custo de manutenção da equipa ser superior a 6 mil euros é", prob_custo_superior_6_mil$value)

## Resposta: A probabilidade do custo de manutenção da equipa ser superior a 6 mil euros é 0.6935484


# 10) Represente o respetivo gráfico da função densidade de probabilidade.

## Resposta:

## Função densidade de probabilidade (pf: executar o código abaixo para visualizar o gráfico da função de 
## desensidade de probabilidade)

plot(fx, 0, 12, xlim=c(0, 5), n=1001, xlab = "Custos da equipa em milhares de euros", ylab = "fdp", main = "Representação gráfica da f.d.p")


# 11) Calcule o valor esperado de X

# Queremos saber os custos mensais esperados, que são dados por E(X).
# Temos que criar uma nova função, dado que para calcular o valor esperado, 
# precisamos de ter o x a multiplicar pela função:

xfx <- function(x){
  ifelse((2 < x & x < 10), x*((1/496)*x^2 + 1/24), 0)}

#Depois disto podemos calcular o integral da nova função para ter o valor esperado:

E_X=integrate(xfx, 2, 10)

# Para termos os custos mensais esperados teremos que multiplicar por mil,
# já que os custos são dados em milhares de euros

cat("Os custos mensais esperados são:", round(1000 * E_X$value, digits=2),"euros")

## Resposta: Os custos mensais esperados são: 7032.26 euros.


##############
# Grupo C.   #
##############


# 12) Sabendo que a partir de 200 horas, a equipa começa a receber horas extra, qual a 
#     probabilidade de de existirem horas extra pagas num determinado trabalho?

media_normal <- 180
desvio_padrao <- 36

# Construção do gráfico para ver como se comporta a função 

top_norm <- qnorm(0.9999999, media_normal, desvio_padrao)
plot(dnorm(c(0:top_norm),media_normal, desvio_padrao),type="l",lwd=4,col="blue",
     xlab="x", ylab="f(x)",main="Função densidade probabilidade")

# Cálculo da probabilidade da equipa receber horas extra num determinado trabalho (apartir das 200 horas)
 
prob_horas_extra_pagas <- pnorm(200, media_normal, desvio_padrao, lower.tail = FALSE)

cat("A probabilidade de existirem horas extra pagas num determinado trabalho é", prob_horas_extra_pagas)

## Resposta: A probabilidade de existirem horas extra pagas num determinado trabalho é 0.2892574



# 13) Qual a probabilidade de serem necessárias menos de 350 horas para terminar o projeto?

# Equipa interna:
media_interna <- 180
desvio_padrao_interno <- 36

# Equipa externa
media_externa <- 100
desvio_padrao_externa <- 49

# Aplicação das propriedades da aditividade da normal à média e ao desvio-padrão
nova_media <- media_interna + media_externa

novo_desvio_padrao <- sqrt(desvio_padrao_interno^2 + desvio_padrao_externa^2)

# Cálculo da probabilidade de serem necessárias menos de 350 horas para terminar o projeto,
# sabendo que a média da nova distribuição normal é 280 e o desvio-padrão é 85. Utiliza-se para este efeito
# o pnorm, que obtem a probabilidade acumulada até ao ponto em questão.

prob_menos_350 <- pnorm(350, nova_media, novo_desvio_padrao)

cat("A probabilidade de serem necessárias menos de 350 horas para terminar o projeto é", prob_menos_350)

## Resposta: A probabilidade de serem necessárias menos de 350 horas para terminar o projeto é 0.8751873. 


# 14) Qual a duração máxima para esse projeto que garante ser a capacidade de trabalho do
#     conjunto das duas equipas suficiente para o término do projeto em 90% dos casos? 

# Utiliza-se o qnorm para determinar o quantil necessário para que, nesta nova distribuição normal, seja 90%
# a probabilidade de em conjunto, as duas equipas terminarem o projeto.

cat("A duração máxima do projeto para que se terminem 90% dos trabalhos é", qnorm(0.9, nova_media, novo_desvio_padrao), "horas")

## Resposta: A duração máxima do projeto para que se terminem 90% dos trabalhos é 357.9221 horas


# 15) Num breve comentário em R, indique se concorda com o facto da empresa projetar um 
#     orçamento para o seu cliente onde são orçamentadas 230 horas para concluir este trabalho
#     de grande envergadura.

# Cálculo da probabilidade de 230 horas serem suficientes para terminar o projeto de grande evergadura.
pnorm(230, nova_media, novo_desvio_padrao)

## Resposta:

## A probabilidade de serem necessárias menos de 230 horas de trabalho para terminar o projeto é de 20.5%, pelo que não se 
## justifica que sejam orçamentadas apenas 230 horas para a conclusão do projeto. Na mesma linha de raciocío, existe uma elevada
## probabilidade (79.5%) de serem necessárias mais de 230 para fazer este grande projeto, pelo que a prudência aconselha à
## previsão de um número superior de horas.


# 16) Efetue uma simulação para 1000 trabalhos onde é utilizada uma equipa interna e uma 
#     equipa externa. Simule separadamente as horas afetas à equipa interna e à equipa externa, 
#     calculando o número de horas totais afetas ao projeto através da soma. Crie o respetivo 
#     gráfico que permite observar os resultados da simulação.

## Resposta:

## Simulação para a equipa interna
simulacao_1000_trabalhos_equipa_interna <- sample(rnorm(1000, media_interna, desvio_padrao_interno))

## Simulação para a equipa Externa
simulacao_1000_trabalhos_equipa_externa <- sample(rnorm(1000, media_externa, desvio_padrao_externa))

## Calculo do número de horas totais afetas ao projeto
simulacao_1000_trabalhos <- simulacao_1000_trabalhos_equipa_interna + simulacao_1000_trabalhos_equipa_externa

## Gráfico do resultado da simulação
hist(simulacao_1000_trabalhos, xlab = "Horas necessárias", ylab = "Frequência", main = "Resultados da simulação")


# 17)  Verifique em quantos desses 1000 trabalhos não se ultrapassaram as 350 horas totais de 
#      trabalho. Compare com o valor que esperava obter a partir dos resultados de 13.

# Número de casos onde se ultrapassam as 350 horas totais de trabalho
sum(simulacao_1000_trabalhos <= 350)

# Proporção de casos que ultrapassam as 350 horas totais de trabalho
mean(simulacao_1000_trabalhos <= 350)

## Resposta:

## A partir do exercício 13 concluimos que a probabilidade de serem necessárias menos de 350 horas
## para terminar o projeto é 0.8751873. Nesta nova simulação a proporção de trabalhos concluidos a verificarem
## esta condição foi de 0.862 (um valor próximo). À medida que o tamanho da amostra aumenta, esta proporção
## deverá convergir para 0.8751873. 















































































































