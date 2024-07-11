          ###
         ## ##
        ##   ##
       ##     ##
      ##       ##
     #############
    ###############
   ##             ##
  ##               ##
 ##                 ##
##                   ##

   
#Três empresas de sondagens fazem a sua atividade de forma independente no mercado nacional
#de sondagens. A probabilidade dos resultados serem precisos, nas projeção do resultado de
#eleições, é de:
# Empresa 1 = 0.5
# Empresa2 = 0.6
# Empresa 3 = 0.4

library(prob)
library(stringr)
library(tidyr)
##############
# Pergunta 1 #
##############


# Definição do espaço de resultados

# Vetor que contém os dois outcomes possíveis: A empresa ter sucesso ou insucesso na previsão dos resultados da eleição
resultados = c("Acertou", "Errou")

resultado1 = resultados
resultado2 = resultados
resultado3 = resultados

# Vetores com as probabilidades de cada empresa ter sucesso ou errar na sondagem
prob1 = c(0.5, 0.5)
prob2 = c(0.6, 0.4)
prob3 = c(0.4, 0.6)

# Tabela com os possíveis acontecimentos
Acontecimentos = expand.grid('Empresa1'=resultado1,'Empresa2'=resultado2,'Empresa3'=resultado3)

# Tabela com as probabilidades dos acontecimentos da tabela anterior
Probabilidades = expand.grid('Empresa1_prob'=prob1,'Empresa2_prob'=prob2,'Empresa3_prob'=prob3)

Probabilidades

espaco = probspace(Acontecimentos, probs = Probabilidades)

espaco

##############
# Pergunta 2 #
##############


# Adicionar a probabilidade de ocorrência de cada acontecimento elementar à tabela Probabilidades
Probabilidades$Prob_Ocorrer = Probabilidades$Empresa1_prob * Probabilidades$Empresa2_prob * Probabilidades$Empresa3_prob

# Adicionar a probabilidade de ocorrência de cada acontecimento elementar à tabela Acontecimentos
Acontecimentos$Prob_Ocorrer = Probabilidades$Empresa1_prob * Probabilidades$Empresa2_prob * Probabilidades$Empresa3_prob

Acontecimentos

##############
# Pergunta 3 #
##############


Acontecimentos$Prob_Ocorrer[Acontecimentos$Empresa1=="Acertou" & Acontecimentos$Empresa2=="Acertou" & Acontecimentos$Empresa3=="Acertou"]
print("Por observação da tabela podemos verificar que a probabilidade das 3 empresas acertarem nas provisões é 0.12")


##############
# Pergunta 4 #
##############


Acontecimentos$Prob_Ocorrer[Acontecimentos$Empresa1=="Errou" & Acontecimentos$Empresa2=="Errou" & Acontecimentos$Empresa3=="Errou"]
# Por observação da tabela podemos verificar que a probabilidade das 3 empresas acertarem nas provisões é 0.12


##############
# Pergunta 5 #
##############


# Por observação da tabela podemos verificar que os acontecimentos que verificam a condição de pelo menos duas
# empresas acertarem as projeções são:
#
# - Acertaram todas as empresas (prob = 0.12)
# - Acertaram as empresas 1 e 2 (prob = 0.18)
# - Acertaram as empresas 2 e 3 (prob = 0.12)
# - Acertaram as empresas 1 e 3 (prob = 0.08)
#
# Posto isto, podemos concluir que a probabilidade de pelo menos duas empresas acertarem as projeções consiste
# na soma das probabilidades dos acontecimentos descritos anteriormente se verificarem, logo:
#
# R: 0.12 + 0.18 + 0.12 + 0.08 = 0,5


##############
# Pergunta 6 #
##############

# Criar o subsespaço onde a empresa 1 acerta as projeções
empresa1acerta = subset(espaco, espaco$Empresa1 == "Acertou")

# Criar o subsespaço onde a empresa 2 acerta as projeções
empresa2acerta = subset(espaco, espaco$Empresa2 == "Acertou")

# Sabendo que, numa determinada eleição, a segunda empresa acerta na respetiva projeção, qual a probabilidade 
# de nessa mesma eleição a primeira empresa acertar na sua projeção?
  
Prob(empresa1acerta, given = empresa2acerta)


#########
##       ##
##        ##
##        ##
##       ##
#########
##       ##
##        ##
##        ##
##       ##
#########


##############
# Pergunta 7 #
##############


# Dataframe criado com os acontecimentos possiveis
novoempresas = data.frame(Acontecimento = paste(Acontecimentos[,1],Acontecimentos[,2],Acontecimentos[,3]))
# Probabilidades de cada acontecimento
probsempresas = c(Acontecimentos$Prob_Ocorrer)
# Simulação de uma eleição 1x
simulacao = sample(novoempresas$Acontecimento, size=1, replace = TRUE, prob = probsempresas)


##############
# Pergunta 8 #
##############

# Simulação de uma eleição 10x
size = 10

sim10raw = sample(novoempresas$Acontecimento, size, replace = TRUE, prob = probsempresas)
simulacao10 = data.frame(Acontecimento = sim10raw)
sim10pog = separate(simulacao10, "Acontecimento", c("E1", "E2", "E3"))

View(simulacao10)


# a) Calcule a proporção de vezes onde as 3 empresas acertam nas respetivas projeções

# Vezes onde as 3 empresas acertam nas respetivas projeções
sum(rowSums(sim10pog=="Acertou")==3)

# Respetiva proporção
mean(rowSums(sim10pog=="Acertou")==3)


# b) Calcule a proporção de vezes onde pelo menos duas empresas acertam nas respetivas projeções

# Vezes onde pelo menos duas empresas acertam nas respetivas projeções
sum(rowSums(sim10pog=="Acertou")>=2)

# Respetiva proporção
mean(rowSums(sim10pog=="Acertou")>=2)


##############
# Pergunta 9 #
##############


# Simulação de uma eleição 15000x
Sim_15000rep = data.frame(Acontecimento = replicate(15000, sample(novoempresas$Acontecimento, 1, replace = TRUE, prob = probsempresas)))
sim15000pog = separate(Sim_15000rep, "Acontecimento", c("E1", "E2", "E3"))

# a) Calcule a proporção de vezes onde as 3 empresas acertam nas respetivas projeções

# Vezes onde as 3 empresas acertam nas respetivas projeções
sum(rowSums(sim15000pog=="Acertou")==3)

# Respetiva proporção
mean(rowSums(sim15000pog=="Acertou")==3)

# b) Calcule a proporção de vezes onde pelo menos duas empresas acertam nas respetivas projeções

# Vezes onde pelo menos duas empresas acertam nas respetivas projeções
sum(rowSums(sim15000pog=="Acertou")>=2)

# Respetiva proporção
mean(rowSums(sim15000pog=="Acertou")>=2)

# COMPARAR E COMENTAR COM OS RESULTADOS DO 3 E 5 ATENÇÃO
#


    ###############
  #################
 ####
###
###
###
###
###
 ####
  #################
    ###############


##############
# Pergunta 1 #
##############

# Qual a probabilidade desta empresa internacional ser precisa na projeção de uma determinada eleição?


# Vetor com as probabilidades de cada equipa ser selecionada para realizar a projeção (Equipa 1, 2 e 3 respetivamente)
probequipas = c(0.6, 0.3, 0.1)

# Vetor com as probabilidades da projeção ser precisa sabendo que equipa a realizou (Equipa 1, 2 e 3 respetivamente)
probacertarsabendo = c(0.6, 0.7, 0.8)

# Probabilidade da empresa internacional ser precisa na projeção de uma determinada eleição
# Aplicação do teorema da probabilidade total

serprecisa = sum(probequipas * probacertarsabendo)

##############
# Pergunta 2 #
##############

# Escolheria a empresa estrangeira visto que a probabilidade da mesma realizar uma projeção precisa, 0.65, é supeior
# à probabilidade das empresas 1, 2 e 3 com as respetivas probabilidades de 0.5, 0.6 e 0.4 serem precisas.

##############
# Pergunta 3 #
##############

# Vetor com as probabilidades de cada equipa ser selecionada e ser a que realizou a projeção (Equipa 1, 2 e 3 respetivamente)

intersecao = c(probequipas * probacertarsabendo)

# Probabilidade da Equipa 1 ter realizado a projeção sabendo que foi precisa
probcondequipa1 = intersecao[1] / serprecisa

# Probabilidade da Equipa 2 ter realizado a projeção sabendo que foi precisa
probcondequipa2 = intersecao[2] / serprecisa

# Probabilidade da Equipa 3 ter realizado a projeção sabendo que foi precisa
probcondequipa3 = intersecao[3] / serprecisa

