# Projeto: Resolução de Caso Concreto - Instituto de Estudos para Políticas de Saúde
# Autor: Iago de Carvalho Nunes - contato: github.com/iagocnunes iagodcn@gmail.com
rm(list = ls())

# libraries utilizadas
library(tidyverse) # para ETL
library(data.table) # trabalhar com dt
library(lubridate) # trabalhar com datas
library(hablar) # funcao s() para simplificar operacoes com N/As
library(Hmisc) # para utilizar a funcao describe()
library(plotly) # graficos interativos
library(ggplot2) # graficos
library(lsmeans) # extrair medias de anova

# desativar notacao cientifica
options(scipen=999)

# diretorio de trabalho da sessao
setwd("C:/Exemplo/Exemplo")

# carregando bases
# virgulas separam valores e pontos separam decimos
sih <- read.csv("aih_teste.csv", header=T)
cnes <- read.csv("cnes_teste.csv", header=T)

# montando objeto RDS, que carrega mais rapido
saveRDS(sih,'sih_raw')
saveRDS(cnes,'cnes_raw')
rm(list = ls())
sih <-readRDS('sih_raw')
cnes <-readRDS('cnes_raw')

#############################################
############## ETL dos dados ################
#############################################

# N, NAs e N distintos
describe(sih)
describe(cnes)

## Estudando duplicidades/prorrogacoes na variavel N_AIH

# selecionando linhas duplicadas, desde a primeira
dup_sih <- sih[duplicated(sih[, 4]) | duplicated(sih[, 4], fromLast=T),]
describe(dup_sih$N_AIH) # 15.695 AIH com 66.885 internacoes

# numerando duplicidades
dup_sih <- dup_sih %>% group_by(N_AIH) %>% mutate(id = row_number()) %>% 
  ungroup()
describe(dup_sih$id) # uma mesma internacao pode repetir de duas a 13 vezes

# compreendendo duplicacoes no mes de janeiro
x1b2 <- dup_sih %>% filter(MES_CMPT==1) 
describe(x1b2$id)
# 4784 internacoes duplicadas em 2023 estao presentes em janeiro
# 4724 AIH; 52 repetem duas ou tres vezes em janeiro; demais, repetem no resto do ano

# base SIH sem duplicados
NOdup_sih <- sih %>%  filter(!N_AIH %in% dup_sih$N_AIH)

# selecionando apenas a primeira entrada
dup_sih <- dup_sih[order(dup_sih$N_AIH, decreasing=F),]
dup_sih <- dup_sih[!duplicated(dup_sih$N_AIH),]

# removendo coluna utilizada
dup_sih <- dup_sih %>% select(-id)

# unindo internacoes, 2.620.155 observacoes
sih_internacoes <- rbind(NOdup_sih,dup_sih)

# limpando area
rm(dup_sih,NOdup_sih,x1b2)

# unindo informacoes do CNES ao SIH total
sih <- sih %>% left_join(cnes, by = join_by(CNES == cnes))
describe(sih$porte) # 34.070 sem informacao de porte

# selecionando AIH com informacoes de porte
sih <- sih %>% 
  filter(is.na(porte)!=T) %>%  # 2.637.275 rows
  select(-n_leito_total)

# unindo informacoes do CNES ao SIH internacoes
sih_internacoes <- sih_internacoes %>% left_join(cnes, by = join_by(CNES == cnes))
describe(sih_internacoes$porte) # 16.133 internacoes sem informacao de porte

# selecionando internacoes com informacao de porte
sih_internacoes <- sih_internacoes %>% 
  filter(is.na(porte)!=T) %>%  # 2.604.022 rows
  select(-n_leito_total)

# base sih para calculo da tx de ocupacao
sih_txocup <- sih %>% select(CNES,DIAS_PERM) %>%
  mutate(DIAS_PERM1 = ifelse(DIAS_PERM==0, 0.5, DIAS_PERM)) %>%
  group_by(CNES) %>%
  summarise(DIAS_PERM1 = sum(s(DIAS_PERM1))) %>% 
  ungroup()
sum(sih_txocup$DIAS_PERM1) # 13.655.893

# base cnes para calculo da tx de ocupacao
cnes_txocup <- cnes %>% left_join(sih_txocup, by = join_by(cnes == CNES))
cnes_txocup <- subset(cnes_txocup, is.na(cnes_txocup$DIAS_PERM1)!=T)
cnes_txocup$leito_ano <- cnes_txocup$n_leito_total*365
sum(cnes_txocup$leito_ano) # 25.149.230
rm(sih_txocup)

# label para grafico
sih$label <- ifelse(sih$porte=="0", "Outros",
                    ifelse(sih$porte=="pequeno", "Pequeno",
                           ifelse(sih$porte=="medio", "Médio",
                                  ifelse(sih$porte=="grande", "Grande",
                                         ifelse(sih$porte=="especial", "Especial", NA)))))

# amostra
set.seed(123)
sih2000 <- sih[sample(rownames(sih), 2000), ]
describe(sih2000)


######################################
####### TOTAL DE INTERNACOES #########
######################################

# total de internacoes por porte
sih_tot <- sih_internacoes %>% 
  mutate(inter=1) %>% 
  select(porte,inter) %>% 
  group_by(porte) %>%
  summarise(inter = sum(inter)) %>% 
  ungroup()

# grafico total de internacoes
sih_tot$label <- c("Outros","Especial","Grande","Médio","Pequeno")
sih_totfig <- plot_ly(sih_tot, x = ~label, y = ~inter, type = 'bar', 
                      text = ~format(inter, big.mark=".", decimal.mark=","), textposition = 'outside',
                      marker = list(color = c('rgba(204,204,204,1)','rgba(204,204,204,1)',
                                              'rgba(204,204,204,1)','rgba(204,204,204,1)',
                                              'rgb(252,93,0)')))
sih_totfig <- sih_totfig %>% layout(xaxis = list(title=list(text='Porte',
                                                            font=list(size = 19, color='rgb(0,1,3)')),
                                                 tickfont = list(size = 19),
                                                 color='rgb(0,1,3)'),
                                    yaxis = list(title=list(text='Internações',
                                                            font=list(size = 17, color='rgb(0,1,3)')),
                                                 color='rgb(0,1,3)'),
                                    uniformtext= list(minsize=19, mode='show'),
                                    font= list(color = 'rgb(0,1,3)'),
                                    margin = list(t = 90))
sih_totfig
rm(sih_totfig)

########################################
###### TX DE OCUPAÇÃO DE LEITOS ########
########################################

# taxa de ocupacao por porte
cnes_txocup <- cnes_txocup %>% select(porte, leito_ano, DIAS_PERM1) %>% group_by(porte) %>%
  summarise(sumlei= sum(s(leito_ano)),
            suminter = sum(s(DIAS_PERM1)),
            txocup = (suminter/sumlei)*100) %>% 
  ungroup()

# grafico tx ocupacao
cnes_txocup$label <- c("Outros","Especial","Grande","Médio","Pequeno")
cnes_txocup <- arrange(cnes_txocup, label)

cnes_txocupfig <- plot_ly(cnes_txocup, x = ~as.numeric(factor(label)), y = ~txocup, type = 'bar', 
                          text = ~format(paste0(round(txocup,2), "%"), big.mark=".", decimal.mark=","), textposition = 'outside',
                          marker = list(color = c('rgba(204,204,204,1)','rgba(204,204,204,1)',
                                                  'rgba(204,204,204,1)','rgba(204,204,204,1)',
                                                  'rgb(252,93,0)')),
                          name = "Taxa de ocupação")
cnes_txocupfig <- cnes_txocupfig %>% add_segments(x = .6, y = 75, xend = 5.4, yend = 75,
                                                  type='scatter',
                                                  mode='lines-markers',
                                                  line = list(color = 'black'),
                                                  name = "Nível mínimo<br>recomendado: 75%")
cnes_txocupfig <- cnes_txocupfig %>% layout(xaxis = list(title=list(text='Porte',
                                                                    font=list(size = 19, color='rgb(0,1,3)')),
                                                         color='rgb(0,1,3)',
                                                         tickfont = list(size = 19),
                                                         ticktext = cnes_txocup$label,
                                                         tickvals = seq_along(cnes_txocup$label)),
                                            yaxis = list(title=list(text='%',
                                                                    font=list(size = 17, color='rgb(0,1,3)')),
                                                         color='rgb(0,1,3)'),
                                            uniformtext= list(minsize=19, mode='show'),
                                            font= list(color = 'rgb(0,1,3)'),
                                            margin = list(t = 90),
                                            legend = list(font = list(size = 19)))
cnes_txocupfig
rm(cnes_txocupfig)

#####################################
###### INTERNACOES SENSIVEIS ########
#####################################

# percentual de internacoes sensiveis
sih_csap <- sih_internacoes %>% 
  mutate(inter=1) %>% 
  select(porte,csap,inter) %>% 
  group_by(porte,csap) %>%
  summarise(inter = sum(inter)) %>% 
  ungroup() %>% 
  group_by(porte) %>% 
  mutate(sum = sum(inter), tx_csap = (inter/sum)*100) %>%
  select(-sum) %>% 
  ungroup()

# grafico internacoes sensiveis
sih_csap$label <- c("Outros","Outros","Especial","Especial","Grande","Grande",
                    "Médio","Médio","Pequeno","Pequeno")
sih_csapfig <- plot_ly(sih_csap, x = ~label[csap==T], y = ~tx_csap[csap==T], type = 'bar', 
                       text = ~format(paste0(round(tx_csap[csap==T],2), "%"), big.mark=".", decimal.mark=","), textposition = 'outside',
                       marker = list(color = c('rgba(204,204,204,1)','rgba(204,204,204,1)',
                                               'rgba(204,204,204,1)','rgba(204,204,204,1)',
                                               'rgb(252,93,0)')))
sih_csapfig <- sih_csapfig %>% layout(xaxis = list(title=list(text='Porte',
                                                              font=list(size = 19, color='rgb(0,1,3)')),
                                                   tickfont = list(size = 19),
                                                   color='rgb(0,1,3)'),
                                      yaxis = list(title=list(text='%',
                                                              font=list(size = 17, color='rgb(0,1,3)')),
                                                   color='rgb(0,1,3)'),
                                      uniformtext= list(minsize=19, mode='show'),
                                      font= list(color = 'rgb(0,1,3)'),
                                      margin = list(t = 90))
sih_csapfig
rm(sih_csapfig)

##########################################
###### TEMPO MEDIO DE PERMANENCIA ########
##########################################

# tempo medio de permanencia
sih_temp <- sih2000 %>% 
  mutate(inter=1,
         DIAS_PERM1 = ifelse(DIAS_PERM==0, 0.5, DIAS_PERM),
         dia_alta = lubridate::ymd(paste0(ANO_CMPT,"-",MES_CMPT,"-01")) + lubridate::days(DIAS_PERM),
         dia_alta = ifelse(substr(dia_alta, 1, 4)=="2023", 1, 0)) %>%
  select(porte,DIAS_PERM1,dia_alta) %>% 
  group_by(porte) %>%
  summarise(median = median(DIAS_PERM1, na.rm=T),
            DIAS_PERM1 = sum(s(DIAS_PERM1)),
            dia_alta = sum(s(dia_alta)),
            tempmed = DIAS_PERM1/dia_alta) %>% 
  ungroup()

# grafico
means <- aggregate(ifelse(DIAS_PERM==0, 0.5, DIAS_PERM) ~ label, sih2000, mean)
means$DIAS_PERM1 <- round(means[,2], 1)

sih_templot <- ggplot(sih2000, aes(x=label, y=ifelse(DIAS_PERM==0, 0.5, DIAS_PERM), fill=label)) + 
  geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun=mean, colour="darkred", geom="point",
               shape=18, size=3, show.legend=F) + 
  geom_text(data = means, aes(label = DIAS_PERM1, y = DIAS_PERM1 + 0.8, x = as.numeric(factor(label)) + 0.03,
                              size = 6.65)) +
  coord_cartesian(ylim = c(0,25)) + labs(x = "Porte", y = "Dias") +
  scale_fill_manual(values=c("grey", "grey", "grey", "grey", "orange2")) +
  theme(text = element_text(size = 19), legend.position = "none")
sih_templot
rm(sih_templot)

# teste de medias

sih_aov <- aov(ifelse(DIAS_PERM==0, 0.5, DIAS_PERM) ~ porte, data = sih2000)
summary(sih_aov)
TukeyHSD(sih_aov)
lsmc1<- as.data.frame(lsmeans(sih_aov, ~ porte))
rm(means, sih_aov)

##########################################
###### CUSTO MEDIO DAS INTERNACOES #######
##########################################

# custo medio das internacoes
sih_cost <- sih2000 %>% 
  mutate(inter=1) %>%
  select(porte,VAL_TOT,inter) %>% 
  group_by(porte) %>%
  summarise(median = median(VAL_TOT, na.rm=T),
            VAL_TOT = sum(s(VAL_TOT)),
            costmed = VAL_TOT/sum(s(inter))) %>% 
  ungroup()

# grafico
means <- aggregate(VAL_TOT ~ label, sih2000, mean)
means$VAL_TOT <- round(means$VAL_TOT, 2)

sih_costplot <- ggplot(sih2000, aes(x=label, y=VAL_TOT, fill=label)) + 
  geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun=mean, colour="darkred", geom="point",
               shape=18, size=3, show.legend=F) + 
  geom_text(data = means, aes(label = VAL_TOT, y = VAL_TOT + 200, x = as.numeric(factor(label)) + 0.1,
                              size = 6.65)) +
  coord_cartesian(ylim = c(0,6000)) + labs(x = "Porte", y = "R$") +
  scale_fill_manual(values=c("grey", "grey", "grey", "grey", "orange2")) +
  theme(text = element_text(size = 19), legend.position = "none")
sih_costplot
rm(sih_costplot)

# teste de medias
sih_aov <- aov(VAL_TOT ~ porte, data = sih2000)
summary(sih_aov)
TukeyHSD(sih_aov)
lsmc2<- as.data.frame(lsmeans(sih_aov, ~ porte))

rm(list = ls())

