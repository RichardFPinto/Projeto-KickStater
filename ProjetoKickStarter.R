#carregando os pacotes
library(stringr)
library(tibble)
library(dplyr)
library(chron)
library(ggplot2)
library(tidyverse)
library(stringi)

# importando os dados
KSO = read.csv("G:/Github/20200211 KickStart/1 - Dados Originais/ks-projects-201612.csv",head=TRUE,sep=",")

# transformando em tibble para melhor visualização
KS = as_tibble(KSO)


# primeira visão dos dados
KS

# fazendo split no deadline e launched, e removendo as colunas originais

Date_deadline = str_sub(KSO[,6],start = 1,end = 10)
KS = cbind(KS,Date_deadline)
Time_deadline = str_sub(KSO[,6],start = 12,end = 50)
KS = cbind(KS, Time_deadline)
KS = KS[,-6]

Date_launched = str_sub(KSO[,8],start = 1,end = 10)
KS = cbind(KS,Date_launched)
Time_launched = str_sub(KSO[,8],start = 12,end = 50)
KS = cbind(KS, Time_launched)
KS = KS[,-7]
KS = as_tibble(KS)
KS

# limpando as variaveis de apoio
rm(Date_deadline)
rm(Date_launched)
rm(Time_deadline)
rm(Time_launched)

# agora com as tabelas com as colunas só precisamos arrumar os tipos de dados 
# primeiro colocando para formato data
KS$Date_deadline = as.Date(KS$Date_deadline, format = "%Y-%m-%d")
KS$Date_launched = as.Date(KS$Date_launched, format = "%Y-%m-%d")

# convertendo agora para horas
KS$Time_deadline = chron(times=KS$Time_deadline)
KS$Time_launched = chron(times=KS$Time_launched)
#KS$goal = as.numeric(KS$goal)
#KS$pledged = as.numeric(KS$pledged)
#KS$backers = as.integer(KS$backers)
#KS$usd.pledged = as.numeric(KS$usd.pledged)
# agora com a tabela arruma podemos começar a criar graficos para começaramos a analisar os dados
# pegando os ultimos onde estão os failed, canceled, live e sucesssful, suspended e undefined e criando um grafico de barras
# foi ordenado em ordem descrescente para pegarmos os que mais aparecem
KS_state = data.frame(sort.default(table(KS$state), decreasing = TRUE))
# fazendo o grafico com os 6 maiores 
ggplot(head(KS_state), aes(x = Var1 , y = Freq)) + geom_bar(stat = "identity")
# plotando graficos para main_category, Category e country
KS_MainC = data.frame(sort.default(table(KS$main_category), decreasing = TRUE))
KS_Category = data.frame(sort.default(table(KS$category), decreasing = TRUE))
KS_Country = data.frame(sort.default(table(KS$country), decreasing = TRUE))
# olhando cada um dos dataframes criados e fazendo os plots para analisarmos 
#MainC
dim(KS_MainC)
head(KS_MainC)
# esse não podemos descartar nada, então vamos criar o grafico de barras com os 15 maiores pois todos ficaria muito ruim de visualizar, pois são 120 diferentes
ggplot(head(KS_MainC,15), aes(x = Var1 , y = Freq)) + geom_bar(stat = "identity")
#Category
dim(KS_Category)
head(KS_Category)
# esse faremos o grafico com os 15 maiores tambem, pois tambem tem muitos para ficar em um plot
ggplot(head(KS_Category,15), aes(x = Var1 , y = Freq)) + geom_bar(stat = "identity")
#Country
dim(KS_Country)
head(KS_Country)         
# esse faremos o grafico com os 15 maiores tambem, pois tambem tem muitos para ficar em um plot
ggplot(head(KS_Country,15), aes(x = Var1 , y = Freq)) + geom_bar(stat = "identity")

# adicionar o tempo do projeto do inicio até o final
KS_F = KS %>% filter(state == "successful" | state == "failed" | state == "canceled" | state == "live" | state == "suspended")
Date_Interval = KS_F$Date_deadline - KS_F$Date_launched
KS_F = cbind(KS_F, Date_Interval)
KS_F = as_tibble(KS_F)
KS_F
# agora vamos colocar canceled, suspended e falied = 0 e live e sucessful = 1 para ficar mais facil a classificação do algortimo

KS_F$state = stri_replace_all_fixed(KS_F$state , pattern = c("canceled", "suspended"), replacement = "failed", vectorize_all = FALSE)
KS_F$state = stri_replace_all_fixed(KS_F$state , pattern = "live", replacement = "successful", vectorize_all = FALSE)

KS_F$state = as.factor(KS_F$state)
table(KS_F$state)
KS_F <- KS_F[,c(1:7,9:20,8)]
# Feito agora as tabelas podemos começar a analisar e comparar quais as melhores variaveis para tentar prever se vai dar certo o kickstarted
# separando em treino e teste os dois
amostra = sample(2,dim(KS_F)[1], replace = T, prob = c(0.7,0.3))
KSTreino = KS_F[amostra == 1,]
KSTeste = KS_F[amostra == 2,]
rm(amostra)
# exportar a tabela final do KS_F
KS_F
write.csv(KS_F,"G:/Github/20200211 KickStart/2 - Dados Preparados/2020-02-21 KickStarter.csv", row.names = FALSE)


# testando os modelos de classificação
# arvore de decisão
library(rpart)
# fazendo com todos as variaveis fica em torno de 80% a taxa de acerto
rpM = rpart(state ~ ., data=KSTreino,  method="class")

predrp = predict(rpM,newdata= KSTeste)

predrp

KS_Pred = cbind(KSTeste,predrp)

head(KS_Pred)

KS_Pred['Result'] = ifelse(KS_Pred$failed>=0.5,"failed","successful")

confusao = table(KS_Pred$state,KS_Pred$Result)
taxaacerto = (confusao[1] + confusao[4]) / sum(confusao)
taxaacerto
taxaerro = (confusao[2] + confusao[3]) / sum(confusao)
taxaerro

# retirando algumas colunas no treinandomento para tentar aumentar a % de acerto, foi o modelo o qual conseguiu o melhores % de acerto.
rpM2 = rpart(state ~ main_category + currency + goal+ backers + country + usd.pledged + Date_Interval, data=KSTreino,  method="class")

predrp2 = predict(rpM2,newdata= KSTeste)


predrp2

KS_Pred2 = cbind(KSTeste,predrp2)

head(KS_Pred2)

KS_Pred2['Result'] = ifelse(KS_Pred2$failed>=0.5,"failed","successful")

confusao2 = table(KS_Pred2$state,KS_Pred2$Result)
taxaacerto2 = (confusao2[1] + confusao2[4]) / sum(confusao2)
taxaacerto2
taxaerro2 = (confusao2[2] + confusao2[3]) / sum(confusao2)
taxaerro2

# uma terceira tentativa de melhorar o modelo e depois aplicar eles no outros conjunto de dados que temos de kickstarter
rpM3 = rpart(state ~ main_category + currency + backers + country + usd.pledged + Date_Interval, data=KSTreino,  method="class")

predrp3 = predict(rpM3,newdata= KSTeste)

predrp3

KS_Pred3 = cbind(KSTeste,predrp3)

head(KS_Pred3)

KS_Pred3['Result'] = ifelse(KS_Pred3$failed>=0.5,"failed","successful")

confusao3 = table(KS_Pred3$state,KS_Pred3$Result)
taxaacerto3 = (confusao3[1] + confusao3[4]) / sum(confusao3)
taxaacerto3
taxaerro3 = (confusao3[2] + confusao3[3]) / sum(confusao3)
taxaerro3
