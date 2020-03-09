### PROJETO 1 - Detecção de Fraudes no Tráfego de Cliques em Propagandas de Aplicações Mobile

############################### SELECIONANDO O DIRETORIO DE TRABALHO #############################

setwd("D:/ohbto/Documents/FormacaoCientistaDados/BigDataRAzure/Projeto/Projeto1")

################################# CARREGANDO AS BIBLIOTECAS DE INTERESSE ##########################

library(dplyr)
library(data.table)
library(explore)
library(ggplot2)
library(caret) #biblioteca que auxilia no treinamento de casos desbalanceado
                # pesquisa feita em https://www.kaggle.com/kailex/talkingdata-eda-and-class-imbalance

################################# CARREGANDO OS ARQUIVOS DE DADOS #################################

amostra <- read.csv("train_sample.csv", stringsAsFactors = F)
#amostra <- fread(file = "train.csv", sep = ",", header = T, stringsAsFactors = F)

teste <- fread(file = 'test.csv',sep = ",",header = T,stringsAsFactors = F)
 
#describe(amostra)
# Através do método describe do pacote explore, é possível ver que a quantidade de valores únicos,
# os valores NA (que não existem), os valores mínimos, a média e os valores máximos, para cada variável.
# Nota-se que os valores únicos são muitos e considerar que tais variáveis são fator seria intratável, pois um
# range de valores não tem muito sentido para o problema em questão. Dessa forma, como existem poucas variáveis
# vamos criar algumas manualmente através de data munging, a partir da contagem da quantidade de algumas combinações
# de variáveis
# Mas antes, vamos fazer uma análise exploratória dos dados, para entender como estão distribuidos os dados


################################## ANÁLISE EXPLORATÓRIA ##########################################


source("Tools.R")

# Como se trata de um problema de classificação, a variável preditora será colocada como fator 
amostra$is_attributed <- factor(amostra$is_attributed)
levels(amostra$is_attributed) <- c("falso","verdadeiro")

amostra%>%group_by(is_attributed)%>%summarise(Qtde = n()) %>%mutate(Percent = Qtde*100 / sum(Qtde))
# A quantidade entre as amostras 0 e 1 é gritante para os dados de treino. Dessa forma, usaremos duas abordagens: uma sem
# e outra com SMOTE ****************

# Vejamos como as variáveis que não são data estão distribuidas
lapply(1:5, function(x) grafico_histograma(amostra,"is_attributed",names(amostra)[x]))
# Não foi possível verificar, pelo volume, como os dados classificados como 1 estão distribuidos.
# Dessa forma, façamos um filtro para verificar como eles estão

lapply(1:5, function(x) grafico_histograma(amostra %>% filter(is_attributed == 1),"is_attributed",names(amostra)[x]))
# A distribuição é bem similar aos dados com classificação 0


################################# DATA MUNGING VARIAVEIS ###################################

#Vamos unir as variáveis de treino e de teste para que as variáveis criadas para o treino sejam também criadas juntamente
#para o teste e não tenhamos que repetir o processo
#Utilizaremos data.table para fazer a manipulação por conta do seu melhor desempenho perante ao dplyr

combinacao_treino_teste <- rbind(amostra,teste,fill=T)
rm(amostra,teste) #liberando espaço na memoria
#combinacao_treino_teste <- data.table(amostra)
combinacao_treino_teste[,click_time := as.POSIXct(click_time),
                        ][,AnoMesDia := year(click_time)*10000 + month(click_time)*100 + mday(click_time),
                        ][,hora := hour(click_time),
                        ][,AnoMesDiaHora := AnoMesDia*100+hora,
                        ][,ip_N := .N, by = ip,
                        ][,app_N := .N, by = app,
                        ][,device_N := .N, by = device,
                        ][,os_N := .N, by = os,
                        ][,channel_N := .N, by = channel,
                        ][,ip_app_N := .N, by = .(ip,app),
                        ][,ip_device_N := .N, by = .(ip , device),
                        ][,ip_os_N := .N,by = .(ip,os),
                        ][,ip_channel_N := .N, by = .(ip,channel),
                        ][,app_device_N := .N, by = .(app,device),
                        ][,app_os_N := .N, by = .(app,os),
                        ][,app_channel_N := .N, by = .(app,channel),
                        ][,device_os_N := .N, by = .(device,os),
                        ][,device_channel_N := .N, by = .(device,channel),
                        ][,os_channel_N := .N, by = .(os,channel),
                        ][,ip_app_device_N := .N, by = .(ip,app,device),
                        ][,ip_app_os_N := .N, by = .(ip,app,os),
                        ][,ip_app_channel_N := .N, by = .(ip,app,channel),
                        ][,ip_device_os_N := .N, by = .(ip,device,os),
                        ][,ip_device_channel_N := .N, by = .(ip,device,channel),
                        ][,ip_os_channel_N := .N, by = .(ip,os,channel),
                        ][,app_device_os_N := .N, by = .(app,device,os),
                        ][,app_device_channel_N := .N, by = .(app,device,channel),
                        ][,app_channel_os_N := .N, by = .(app,channel,os),
                        ][,channel_device_os_N := .N, by = .(channel,device,os),
                        ][,app_device_os_channel_N := .N, by = .(app,device,os,channel),
                        ][,ip_device_os_channel_N := .N, by = .(ip,device,os,channel),
                        ][,ip := NULL, #a ideia aqui é não depender do numero de ip, apenas do comportamento
                        ][,AnoMesDia := NULL,
                        ][,attributed_time := NULL,
                        ][,click_time := NULL
                          ]
#                        ][,periodoDia := ifelse(hora < 6, "madrugada",ifelse(hora<12,"manha",
#                                                                              ifelse(hora<18,"tarde","noite"))),  
#                        ][,periodoDia := factor(periodoDia, levels = 
#                                                     c("madrugada","manha","tarde","noite"), ordered = T),
#                        ][, hora := factor(hora,ordered = T)]

#Salvar e ler o backup do arquivo
fwrite(combinacao_treino_teste, "backup_combinacao.csv")
#combinacao_treino_teste <- fread(file = 'backup_combinacao.csv',sep = ",",header = T,stringsAsFactors = F)

                                                     
#Vamos analisar como se comportam as novas variáveis

lista_classes <- lapply(combinacao_treino_teste[0], class)
colunas_inteiro <- grep("integer",lista_classes)
colunas_fator <- grep("factor",lista_classes)
colunas_inteiro <- append(colunas_inteiro,grep("numeric",lista_classes))

lapply(names(combinacao_treino_teste)[colunas_fator], 
       function(x) graficos_barra(combinacao_treino_teste, "is_attributed",x))

lapply(names(combinacao_treino_teste)[colunas_inteiro], 
       function(x) graficos_boxplot(combinacao_treino_teste, "is_attributed",x))

lapply(names(combinacao_treino_teste)[colunas_inteiro], 
       function(x) grafico_histograma(combinacao_treino_teste, "is_attributed",x))


############################################ NORMALIZAÇÃO #################################################

## Para as variáveis inteiras, vamos realizar a normalização de cada uma delas, deixando a média nula
## e o desvio padrão zero

colunas_normalizacao <- colunas_inteiro[6:length(colunas_inteiro)]

combinacao_treino_teste[,paste(names(combinacao_treino_teste)[colunas_normalizacao],"_norm",sep = "") :=
                        lapply(.SD,function(x){((x-mean(x))/sd(x))}), 
                        .SDcols = colunas_normalizacao]               

combinacao_treino_teste[,names(combinacao_treino_teste)[colunas_normalizacao] := 
                          lapply(.SD, function(x){x = NULL}),
                        .SDcols = colunas_normalizacao]

###################################### FEATURE SELECTION ##############################################
#Para o feature selection, vamos usar a função train do pacote caret, utilizando como método xgbTree
featureSelection <- train(is_attributed ~ .,
                          data = combinacao_treino_teste[!is.na(is_attributed),!"click_id"], 
                          method = "xgbTree",
                          metric = "ROC",
                          trControl = trainControl(classProbs = T, 
                                                   summaryFunction = twoClassSummary,
                                                   method = "cv",
                                                   number = 10),
                          tuneGrid = expand.grid(nrounds = 300,
                                                 max_depth = 5,
                                                 eta = 0.3,
                                                 gamma = 0,
                                                 colsample_bytree = 1,
                                                 min_child_weight = 1, 
                                                 subsample = 1),
                          nthread = 4)

ggplot(data.frame(Variavel = row.names(varImp(featureSelection)$importance),
                  varImp(featureSelection)$importance),aes(x=reorder(Variavel,Overall),y=Overall))+geom_bar(stat="identity")+coord_flip()

variaveis_importantes <- c("app_N_norm","channel","ip_N_norm","hora_norm",
                           "device_channel_N_norm","app_channel_N_norm","channel_N_norm",
                           "ip_device_N_norm ","app_device_channel_N_norm")

###################################### DIVISAO DE VARIAVEIS DE TREINO E TESTE ########################

index_treino <- sample(1:nrow(combinacao_treino_teste[!is.na(is_attributed)]),
                       round(0.7*nrow(combinacao_treino_teste[!is.na(is_attributed)])))

###################################### CRIANDO O MODELO #############################################


#como a diferença entre classes é muito grande, iremos utilizar o método SMOTE dentro do treino
#para reduzir essa diferença
formula <- as.formula(paste("is_attributed ~", paste(variaveis_importantes,collapse = " + ")))
modelo1 <- train(formula,
                data = combinacao_treino_teste[!is.na(is_attributed),
                                               ][index_treino],
                method = "xgbTree",
                metric = "ROC",
                trControl = trainControl(classProbs = T, 
                                         summaryFunction = twoClassSummary,
                                         method = "cv",
                                         number = 10,
                                         sampling = "smote"),
                tuneGrid = expand.grid(nrounds = 300,
                                       max_depth = 5,
                                       eta = 0.3,
                                       gamma = 0,
                                       colsample_bytree = 1,
                                       min_child_weight = 1, 
                                       subsample = 1),
                nthread = 4)


###################################### ANALISANDO O MODELO #########################################

confusionMatrix(predict(modelo1,combinacao_treino_teste[!is.na(is_attributed),
                                                        ][!index_treino]),combinacao_treino_teste[!is.na(is_attributed),
                                                                                                  ][!index_treino,is_attributed])
#Os resultados sao excelentes. dessa forma, vamos aplicar aos dados de teste
predict(modelo1,combinacao_treino_teste[is.na(is_attributed)])

