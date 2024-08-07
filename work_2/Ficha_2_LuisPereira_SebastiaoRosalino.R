# Trabalho realizado por: Lu�s Pereira n� 98398 e Sebasti�o Rosalino n� 98437
# Turma CDB1

# Carregar biblioteca prob

install.packages("prob")
library(prob)


##############
# Grupo A.   #
##############

# 1) Num coment�rio em R, defina a vari�vel em causa, indicando o respetivo modelo 
#    de distribui��o de probabilidades (nome e par�metros).

## Resposta:A vari�vel em causa [N�mero de potenciais eleitores contactados por dia] � uma 
##          vari�vel aleat�ria discreta, infinita, com distribui��o de Poisson e com
##          p�rametros de m�dia [E[X]] e vari�ncia [Var [X]] igual a 20. 


# 2) Tendo em conta a distribui��o em causa, crie o respetivo gr�fico da fun��o de
#    probabilidades e da fun��o de distribui��o. 


# Como o suporte da Poisson � infinito, embora numer�vel,
# vamos representar graficamente os valores necess�rios para a acumula��o de 
# 0.99999 ("desprezamos" 0.00001)


## Resposta:

  ## Fun��o Probabilidade (pf: executar o c�digo abaixo para visualizar o gr�fico da fun��o de 
  ## probabilidades)

media <- 20
top<-qpois(0.99999,media) # Dada a probabilidade 0.99999, determina o primeiro valor de x em que esse acumulado � atingido.
plot(dpois(c(0:top),media),type="h",lwd=4,col="blue",
     xlab="x", ylab="f(x)",main="Fun��o de probabilidade de Po(20)")


  ## Fun��o Distribui��o (pf: executar o c�digo abaixo para visualizar o gr�fico da fun��o de 
  ## distribui��o)

plot.stepfun(stepfun(c(0:top),c(0,ppois(c(0:top),media)),right=FALSE),
             verticals=FALSE,main="Fun��o Distribui��o",xlab="N�mero de potenciais eleitores",ylab="Probabilidade")


# 3) Qual a probabilidade de em determinado dia, o chefe ficar satisfeito?

# O chefe fica satisfeito satisfeito se forem contactadas pelo menos 15 pessoas

prob_chefe_ficar_satisfeito <- ppois(14, 20, lower.tail = FALSE) # P[X > 14] �=� P[X >= 15]

cat("A probabilidade de num certo dia o chefe ficar satisfeito �", prob_chefe_ficar_satisfeito)

## Resposta: A probabilidade de num certo dia o chefe ficar satisfeito � 0.8951357, ou seja, de aproximadamente 89,5%.


# 4) Qual dever� ser o n�mero de contactos m�nimo por dia, para que seja, pelo menos 0.95 a 
#    probabilidade do chefe ficar satisfeito?

# Para saber o m�nimo de contactos (K) para que a probabilidade do chefe ficar satisfeito seja pelo menos 0.95,
# o que pode ser traduzido em P[X >= k] >= 0.95 ou P[X < k] <= 0.05. Assim, teremos de calcular o k usando qpois(0.05, 20)

# Come�a-se por calcular a probabilidade para o quantil 0.05:
qpois(0.05, 20) # o resultado � de 13.

# Calcula-se a probabilidade para 13 eleitores
ppois(13, 20, lower.tail = FALSE) # e o resultado � de 0.9338724

# A probabilidade de, com 13 eleitores inquiridos, o chefe ficar satisfeito � 0.9338724, o que � inferior a
# 0.95, pelo que teremos de fazer o c�lculo para 12 eleitores

# Verificando a probabilidade de, para 12 eleitores inquiridos, o chefe ficar satisfeito:

ppois(12,20, lower.tail = FALSE) # resultado � 0.960988

# Para 12 eleitores a condi��o � satisfeita, porque o resultado � de 0.960988

## Resposta: Deve haver um n�mero m�nimo de 12 pessoas contactadas para que o chefe esteja satisfeito.


# 5) Qual a probabilidade de num determinado m�s (30 dias), serem inquiridas no m�ximo 550 pessoas?

# Utilizamos a probabilidade da Poisson para determinar o valor do p na Binomial
# Neste caso, multiplicamos o valor esperado di�rio (20) pelos 30 dias indicados para c�lculo da probalidade pedida,
# usando a fun��o distribui��o (ppois).

cat("A probabilidade de serem inquiridas no m�ximo 550 em 30 dias �", ppois(550, 600))
    
## Resposta: A probabilidade de serem inquiridas no m�ximo 550 em 30 dias � 0.02054038. 


# 6) Crie uma amostra aleat�ria de 365 observa��es (1 ano) com base na distribui��o do n�mero 
#    de contactos di�rios. Represente graficamente a informa��o recolhida. Calcule e interprete 
#    a m�dia amostral, no contexto dado.

# Gera��o da amostra:

amostra_365 <- rpois(365, 20)

# Representa��o gr�fica da amostra:

hist(amostra_365, xlim = c(0, 40), ylim = c(0,80), xlab = "N�mero de potenciais eleitores", ylab = "N�mero de dias verificados", main = "Representa��o gr�fica da amostra")

# C�lculo da m�dia amostral:

media_amostral <- mean(amostra_365)

cat("A m�dia amostral �", media_amostral)

## Resposta: A m�dia amostral �  20.07123 (valor observado numa execu��o), ou seja, relativamente � amostra em causa, podemos concluir que num per�odo de
##           365 dias s�o contactados, em m�dia, 20 indiv�duos por dia.


# 7) Qual a probabilidade de em 15 dias, existirem 10, onde o chefe fica satisfeito?

# Usa-se primeiro o dbinom para obter a probalidade num ponto (10), sabendo que o segundo par�metro (size: dias) � igual a 15,
# sendo que o terceiro par�metro (probabilidade) � o valor da probabilidade do chefe ficar satisfeito num certo dia (exerc.3).

cat("A probabilidade de em 15 dias, existirem 10, onde o chefe fica satisfeito �", dbinom(10, 15, prob = ppois(14, 20, lower.tail = FALSE)))

## Resposta: A probabilidade de em 15 dias, existirem 10, onde o chefe fica satisfeito � 0.01257715


# 8) Qual a probabilidade de em 15 dias, existirem 13, onde o chefe fica satisfeito? Compare o 
#    resultado com a al�nea anterior e num breve coment�rio em R, comente o porqu� da diferen�a. 

# Usa-se primeiro o dbinom para obter a probalidade num ponto (13), sabendo que o segundo par�metro (size: dias) � igual a 15,
# sendo que o terceiro par�metro (probabilidade) � o valor da probabilidade do chefe ficar satisfeito num certo dia (exerc.3).

cat("A probabilidade de em 15 dias, existirem 13, onde o chefe fica satisfeito �", dbinom(13, 15, prob = ppois(14, 20, lower.tail = FALSE)))

## Resposta: A probabilidade de em 15 dias, existirem 13, onde o chefe fica satisfeito � 0.2735269

## A raz�o pela qual a probabilidade de em 15 dias, existirem 13, onde o chefe fica satisfeito ser superior � probabilidade 
## de em 15 dias, existirem 10, onde o chefe fica satisfeito, deve-se ao facto da satisfa��o ou insatisfa��o num 
## determinado dia n�o ser influenciada pelos resultados obtidos nos restantes dias, ou seja, � independente, logo, como a  
## probabilidade em geral (exerc. 3) do chefe ficar satisfeito � 0.8951357 e a probabilidade do chefe ficar insatisfeito (� contr�rio) 
## � de 0.1048643, resulta que a probabilidade obtida da al�nea 8 (mais dias satisfeito) seja superior � da al�nea 7 (menos dias satisfeito).


##############
# Grupo B.   #
##############


# 9)  Calcule as seguintes probabilidades:

# 1. P ( X = 5)

## Resposta: Como se trata de uma distribui��o cont�nua, a probabilidade num certo ponto � sempre 0.

    
# 2. A probabilidade do custo de manuten��o da equipa ser superior a 6 mil euros.

# Replica��o da fun��o densidade probabilidade em R

fx <- function(x){
  ifelse((2 < x & x < 10), (1/496)*x^2 + 1/24,0)}

# C�lculo da probabilidade da �rea pedida usando um integral com limite inferior 6 e limite superior 10 

prob_custo_superior_6_mil <- integrate(fx, 6, 10, abs.tol = TRUE)

cat("A probabilidade do custo de manuten��o da equipa ser superior a 6 mil euros �", prob_custo_superior_6_mil$value)

## Resposta: A probabilidade do custo de manuten��o da equipa ser superior a 6 mil euros � 0.6935484


# 10) Represente o respetivo gr�fico da fun��o densidade de probabilidade.

## Resposta:

## Fun��o densidade de probabilidade (pf: executar o c�digo abaixo para visualizar o gr�fico da fun��o de 
## desensidade de probabilidade)

plot(fx, 0, 12, xlim=c(0, 5), n=1001, xlab = "Custos da equipa em milhares de euros", ylab = "fdp", main = "Representa��o gr�fica da f.d.p")


# 11) Calcule o valor esperado de X

# Queremos saber os custos mensais esperados, que s�o dados por E(X).
# Temos que criar uma nova fun��o, dado que para calcular o valor esperado, 
# precisamos de ter o x a multiplicar pela fun��o:

xfx <- function(x){
  ifelse((2 < x & x < 10), x*((1/496)*x^2 + 1/24), 0)}

#Depois disto podemos calcular o integral da nova fun��o para ter o valor esperado:

E_X=integrate(xfx, 2, 10)

# Para termos os custos mensais esperados teremos que multiplicar por mil,
# j� que os custos s�o dados em milhares de euros

cat("Os custos mensais esperados s�o:", round(1000 * E_X$value, digits=2),"euros")

## Resposta: Os custos mensais esperados s�o: 7032.26 euros.


##############
# Grupo C.   #
##############


# 12) Sabendo que a partir de 200 horas, a equipa come�a a receber horas extra, qual a 
#     probabilidade de de existirem horas extra pagas num determinado trabalho?

media_normal <- 180
desvio_padrao <- 36

# Constru��o do gr�fico para ver como se comporta a fun��o 

top_norm <- qnorm(0.9999999, media_normal, desvio_padrao)
plot(dnorm(c(0:top_norm),media_normal, desvio_padrao),type="l",lwd=4,col="blue",
     xlab="x", ylab="f(x)",main="Fun��o densidade probabilidade")

# C�lculo da probabilidade da equipa receber horas extra num determinado trabalho (apartir das 200 horas)
 
prob_horas_extra_pagas <- pnorm(200, media_normal, desvio_padrao, lower.tail = FALSE)

cat("A probabilidade de existirem horas extra pagas num determinado trabalho �", prob_horas_extra_pagas)

## Resposta: A probabilidade de existirem horas extra pagas num determinado trabalho � 0.2892574



# 13) Qual a probabilidade de serem necess�rias menos de 350 horas para terminar o projeto?

# Equipa interna:
media_interna <- 180
desvio_padrao_interno <- 36

# Equipa externa
media_externa <- 100
desvio_padrao_externa <- 49

# Aplica��o das propriedades da aditividade da normal � m�dia e ao desvio-padr�o
nova_media <- media_interna + media_externa

novo_desvio_padrao <- sqrt(desvio_padrao_interno^2 + desvio_padrao_externa^2)

# C�lculo da probabilidade de serem necess�rias menos de 350 horas para terminar o projeto,
# sabendo que a m�dia da nova distribui��o normal � 280 e o desvio-padr�o � 85. Utiliza-se para este efeito
# o pnorm, que obtem a probabilidade acumulada at� ao ponto em quest�o.

prob_menos_350 <- pnorm(350, nova_media, novo_desvio_padrao)

cat("A probabilidade de serem necess�rias menos de 350 horas para terminar o projeto �", prob_menos_350)

## Resposta: A probabilidade de serem necess�rias menos de 350 horas para terminar o projeto � 0.8751873. 


# 14) Qual a dura��o m�xima para esse projeto que garante ser a capacidade de trabalho do
#     conjunto das duas equipas suficiente para o t�rmino do projeto em 90% dos casos? 

# Utiliza-se o qnorm para determinar o quantil necess�rio para que, nesta nova distribui��o normal, seja 90%
# a probabilidade de em conjunto, as duas equipas terminarem o projeto.

cat("A dura��o m�xima do projeto para que se terminem 90% dos trabalhos �", qnorm(0.9, nova_media, novo_desvio_padrao), "horas")

## Resposta: A dura��o m�xima do projeto para que se terminem 90% dos trabalhos � 357.9221 horas


# 15) Num breve coment�rio em R, indique se concorda com o facto da empresa projetar um 
#     or�amento para o seu cliente onde s�o or�amentadas 230 horas para concluir este trabalho
#     de grande envergadura.

# C�lculo da probabilidade de 230 horas serem suficientes para terminar o projeto de grande evergadura.
pnorm(230, nova_media, novo_desvio_padrao)

## Resposta:

## A probabilidade de serem necess�rias menos de 230 horas de trabalho para terminar o projeto � de 20.5%, pelo que n�o se 
## justifica que sejam or�amentadas apenas 230 horas para a conclus�o do projeto. Na mesma linha de racioc�o, existe uma elevada
## probabilidade (79.5%) de serem necess�rias mais de 230 para fazer este grande projeto, pelo que a prud�ncia aconselha �
## previs�o de um n�mero superior de horas.


# 16) Efetue uma simula��o para 1000 trabalhos onde � utilizada uma equipa interna e uma 
#     equipa externa. Simule separadamente as horas afetas � equipa interna e � equipa externa, 
#     calculando o n�mero de horas totais afetas ao projeto atrav�s da soma. Crie o respetivo 
#     gr�fico que permite observar os resultados da simula��o.

## Resposta:

## Simula��o para a equipa interna
simulacao_1000_trabalhos_equipa_interna <- sample(rnorm(1000, media_interna, desvio_padrao_interno))

## Simula��o para a equipa Externa
simulacao_1000_trabalhos_equipa_externa <- sample(rnorm(1000, media_externa, desvio_padrao_externa))

## Calculo do n�mero de horas totais afetas ao projeto
simulacao_1000_trabalhos <- simulacao_1000_trabalhos_equipa_interna + simulacao_1000_trabalhos_equipa_externa

## Gr�fico do resultado da simula��o
hist(simulacao_1000_trabalhos, xlab = "Horas necess�rias", ylab = "Frequ�ncia", main = "Resultados da simula��o")


# 17)  Verifique em quantos desses 1000 trabalhos n�o se ultrapassaram as 350 horas totais de 
#      trabalho. Compare com o valor que esperava obter a partir dos resultados de 13.

# N�mero de casos onde se ultrapassam as 350 horas totais de trabalho
sum(simulacao_1000_trabalhos <= 350)

# Propor��o de casos que ultrapassam as 350 horas totais de trabalho
mean(simulacao_1000_trabalhos <= 350)

## Resposta:

## A partir do exerc�cio 13 concluimos que a probabilidade de serem necess�rias menos de 350 horas
## para terminar o projeto � 0.8751873. Nesta nova simula��o a propor��o de trabalhos concluidos a verificarem
## esta condi��o foi de 0.862 (um valor pr�ximo). � medida que o tamanho da amostra aumenta, esta propor��o
## dever� convergir para 0.8751873. 















































































































