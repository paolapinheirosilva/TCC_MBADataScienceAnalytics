##################################################################################
#                  INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
##################################################################################
#Pacotes utilizados
pacotes <- c( "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
              #evitar sobreposição de textos
              "PerformanceAnalytics", #função 'chart.Correlation' para plotagem
              "Hmisc", # matriz de correlações com p-valor
              "reshape2", #função 'melt'
              "neuralnet", #Biblioteca de rede neural
              "rpart", # Biblioteca de árvores
              "tidyverse", #carregar outros pacotes do R
              "plotly",#plataforma gráfica
              "amap", #funções 'matlogic' para matrizes binária
              "sjPlot",#elaboração de tabelas de contingência
              "knitr", "kableExtra", #formatação de tabelas
              "fastDummies",#gerar variáveis dummies
              "jtools",
              "MASS", #stepwise para rede neural
              "caret",#treinamento modelos
              "pROC", #curva ROC
              "rpart.plot", #arvore de decisao
              "ade4")#bliblioteca para ACM  

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

set.seed(0)

#Carregando os dados
previous <- read.csv("previous_app.csv",encoding = "UTF-8")

#Olhando os dados
str(previous)
head(previous)
summary(previous)

#Criando base de dados para teste no step wise:

db_avaliacao_variaveis <- previous[c(3:8, 16:19,21:26, 28:30)]
str(db_avaliacao_variaveis)

#verificando tipos de contrato
contrato <- unique(db_avaliacao_variaveis$NAME_CONTRACT_TYPE)
length(contrato) #ha 4 tipos de contrato: emprestimo pessoal, capital de giro, 
#rotativo e N/A
#substituindo os espacos por _
db_avaliacao_variaveis$NAME_CONTRACT_TYPE <- gsub("\\s+",".",
                                                  db_avaliacao_variaveis$NAME_CONTRACT_TYPE)
db_avaliacao_variaveis$NAME_CONTRACT_TYPE <- gsub("XNA","Not.Declared",
                                                      db_avaliacao_variaveis$NAME_CONTRACT_TYPE)

print(contrato)

#motivo da solicitação de empréstimo
motivo <- unique(db_avaliacao_variaveis$NAME_CASH_LOAN_PURPOSE)
length(motivo)#ha 25 motivos
print(motivo)
db_avaliacao_variaveis$NAME_CASH_LOAN_PURPOSE <- gsub("/","",
                                                      db_avaliacao_variaveis$NAME_CASH_LOAN_PURPOSE)
db_avaliacao_variaveis$NAME_CASH_LOAN_PURPOSE <- gsub("\\s+",".",
                                                      db_avaliacao_variaveis$NAME_CASH_LOAN_PURPOSE)
print(motivo)
#agrupando variável XNA e XAP com não declarado
db_avaliacao_variaveis$NAME_CASH_LOAN_PURPOSE <- gsub("XAP","Not.Declared",
                                                      db_avaliacao_variaveis$NAME_CASH_LOAN_PURPOSE)
db_avaliacao_variaveis$NAME_CASH_LOAN_PURPOSE <- gsub("XNA","Not.Declared",
                                                      db_avaliacao_variaveis$NAME_CASH_LOAN_PURPOSE)
db_avaliacao_variaveis$NAME_CASH_LOAN_PURPOSE %>% str()


#Verificando tipos de status
status <- unique(db_avaliacao_variaveis$NAME_CONTRACT_STATUS)
length(status) #ha 4 tipos de status: aprovado, reprovado, cancelado e não utilizado
print(status)
#como essa será a variável explicativa, vamos reduzir para duas categorias, Aprovado e Recusado

db_avaliacao_variaveis$NAME_CONTRACT_STATUS [
  db_avaliacao_variaveis$NAME_CONTRACT_STATUS == "Unused offer"] <- "Approved"

#o status cancelado não será utilizado, pois o processo não seguiu para frente

db_avaliacao_variaveis <- db_avaliacao_variaveis %>% filter(
  NAME_CONTRACT_STATUS != "Canceled")

#verificando tipos de pagamentos
tipo_pag <- unique(db_avaliacao_variaveis$NAME_PAYMENT_TYPE)
length(tipo_pag) #ha 4 tipos de pagamentos: cash through the bank (pagamento 
#efetivado pelo banco), non-cash from your bank (item depositado em uma 
#conta, mas não creditado até que seja compensado), cashless from the 
#account of the employer (conta sem saldo, pagamento não efetivado) 
db_avaliacao_variaveis$NAME_PAYMENT_TYPE <- gsub("\\s+",".",
                                                 db_avaliacao_variaveis$NAME_PAYMENT_TYPE)
db_avaliacao_variaveis$NAME_PAYMENT_TYPE <- gsub("XNA","Not.Declared",
                                                 db_avaliacao_variaveis$NAME_PAYMENT_TYPE)
print(tipo_pag)

#verificando tipos de relacionamentos
tipo_relac <- unique(db_avaliacao_variaveis$NAME_TYPE_SUITE)
length(tipo_relac) # 
db_avaliacao_variaveis$NAME_TYPE_SUITE <- gsub(",","",
                                               db_avaliacao_variaveis$NAME_TYPE_SUITE)
db_avaliacao_variaveis$NAME_TYPE_SUITE <- gsub("\\s+",".",
                                               db_avaliacao_variaveis$NAME_TYPE_SUITE)
print(tipo_relac)
db_avaliacao_variaveis <- db_avaliacao_variaveis %>% filter(NAME_TYPE_SUITE != "")
db_avaliacao_variaveis$NAME_TYPE_SUITE <- gsub("Other_B","Other",
                                               db_avaliacao_variaveis$NAME_TYPE_SUITE)
db_avaliacao_variaveis$NAME_TYPE_SUITE <- gsub("Other_A","Other",
                                               db_avaliacao_variaveis$NAME_TYPE_SUITE)

db_avaliacao_variaveis$NAME_TYPE_SUITE %>% str()
print(tipo_relac)

#verificando tipos de clientes
cliente <- unique(db_avaliacao_variaveis$NAME_CLIENT_TYPE)
length(cliente) #ha 4 tipos de clientes: repetido, novo, atualizado e outros
print(cliente)
db_avaliacao_variaveis$NAME_CLIENT_TYPE <- gsub("XNA","Not.Declared",
                                                db_avaliacao_variaveis$NAME_CLIENT_TYPE)


#verificando tipos de consumo
consumo <- unique(db_avaliacao_variaveis$NAME_GOODS_CATEGORY)
length(consumo) #28 tipos
db_avaliacao_variaveis$NAME_GOODS_CATEGORY <- gsub("\\s+",".",db_avaliacao_variaveis$NAME_GOODS_CATEGORY)
db_avaliacao_variaveis$NAME_GOODS_CATEGORY <- gsub("./","",db_avaliacao_variaveis$NAME_GOODS_CATEGORY)
db_avaliacao_variaveis$NAME_GOODS_CATEGORY <- gsub("XNA","Not.Declared",
                                                db_avaliacao_variaveis$NAME_GOODS_CATEGORY)
print(consumo)

#verificando forma de pagamento
forma_pag <- unique(db_avaliacao_variaveis$NAME_PORTFOLIO)
length(forma_pag) #5 tipos 
#resolvendo erro digitação
db_avaliacao_variaveis$NAME_PORTFOLIO [db_avaliacao_variaveis$NAME_PORTFOLIO == "Cars"] <- "Cards"
db_avaliacao_variaveis$NAME_PORTFOLIO <- gsub("XNA","Not.Declared",
                                            db_avaliacao_variaveis$NAME_PORTFOLIO)


print(forma_pag)

#verificando tipos de produtos
produto <- unique(db_avaliacao_variaveis$NAME_PRODUCT_TYPE)
length(produto) #há 3 tipos de produtos: adicional, novo e outros 
db_avaliacao_variaveis$NAME_PRODUCT_TYPE <- gsub("-",".",db_avaliacao_variaveis$NAME_PRODUCT_TYPE)
db_avaliacao_variaveis$NAME_PRODUCT_TYPE <- gsub("XNA","Not.Declared",
                                              db_avaliacao_variaveis$NAME_PRODUCT_TYPE)
print(produto)



#tipo de canal de atendimento
tipo_canal <- unique(db_avaliacao_variaveis$CHANNEL_TYPE)
length(tipo_canal) #8 tipos 

db_avaliacao_variaveis$CHANNEL_TYPE <- gsub("\\s+",".",db_avaliacao_variaveis$CHANNEL_TYPE)
db_avaliacao_variaveis$CHANNEL_TYPE <- gsub("./","",db_avaliacao_variaveis$CHANNEL_TYPE)
db_avaliacao_variaveis$CHANNEL_TYPE <- gsub("-",".",db_avaliacao_variaveis$CHANNEL_TYPE)
print(tipo_canal)


#tipo de industria
tipo_ind <- unique(db_avaliacao_variaveis$NAME_SELLER_INDUSTRY)
length(tipo_ind) #11 tipos 
db_avaliacao_variaveis$NAME_SELLER_INDUSTRY <- gsub("\\s+",".",db_avaliacao_variaveis$NAME_SELLER_INDUSTRY)
db_avaliacao_variaveis$NAME_SELLER_INDUSTRY <- gsub("XNA","Not.Declared",
                                                 db_avaliacao_variaveis$NAME_SELLER_INDUSTRY)
print(tipo_ind)

#frequencia de transação
freq_trans <- unique(db_avaliacao_variaveis$NAME_YIELD_GROUP)
length(freq_trans) #5 tipos 
db_avaliacao_variaveis$NAME_YIELD_GROUP <- gsub("_",".",db_avaliacao_variaveis$NAME_YIELD_GROUP)
db_avaliacao_variaveis$NAME_YIELD_GROUP <- gsub("XNA","Not.Declared",
                                                    db_avaliacao_variaveis$NAME_YIELD_GROUP)
print(freq_trans)

###############################################################################
### Preparando a base para análise ###

db_avaliacao_variaveis %>% str()

#Realocando a variável de decisão para a primeira coluna:
db_avaliacao_variaveis <- db_avaliacao_variaveis %>% relocate (NAME_CONTRACT_STATUS, .before = NAME_CONTRACT_TYPE)

db_avaliacao_variaveis %>% str()

db_avaliacao_variaveis %>% summary()

########################################################################
#retirando miss values
db_avaliacao_variaveis_na <- na.omit (db_avaliacao_variaveis)

db_avaliacao_variaveis_na %>% summary()

#verificando NAs
miss_value <- is.na(db_avaliacao_variaveis_na)
verificar_valor <- unique(miss_value) #filtrando resultados diferente encontrados
print(verificar_valor) #somente false - logo base completa

########################################################################

#realizar dummies das variáveis categóricas

dummies <- dummyVars(~NAME_CONTRACT_TYPE+
                       NAME_CASH_LOAN_PURPOSE+
                       NAME_PAYMENT_TYPE+
                       NAME_TYPE_SUITE+
                       NAME_CLIENT_TYPE+
                       NAME_GOODS_CATEGORY+
                       NAME_PORTFOLIO+
                       NAME_PRODUCT_TYPE+
                       CHANNEL_TYPE+
                       NAME_SELLER_INDUSTRY+
                       NAME_YIELD_GROUP,
                     data = db_avaliacao_variaveis_na)

db_dummies <- predict(dummies, newdata = db_avaliacao_variaveis_na)

newprevious <- db_avaliacao_variaveis_na[,c("AMT_ANNUITY",
                                            "AMT_APPLICATION",
                                            "AMT_CREDIT",
                                            "AMT_GOODS_PRICE",
                                            "AMT_DOWN_PAYMENT",
                                            "DAYS_DECISION",
                                            "CNT_PAYMENT"
                                            )]

###agrupando variáveis dummies

newprevious <- cbind.data.frame(newprevious,db_dummies)

###############################################################################

#Estatísitca Descritiva

# Estatísticas descritivas univariadas


#vetores para variáveis qualitativas e quantitativas
quant <- c(1:7)
qual <- as.data.frame(unclass(db_avaliacao_variaveis_na[,c("NAME_CONTRACT_TYPE",
                                                           "NAME_CASH_LOAN_PURPOSE",
                                                           "NAME_PAYMENT_TYPE",
                                                           "NAME_TYPE_SUITE",
                                                           "NAME_CLIENT_TYPE",
                                                           "NAME_GOODS_CATEGORY",
                                                           "NAME_PORTFOLIO",
                                                           "NAME_PRODUCT_TYPE",
                                                           "CHANNEL_TYPE",
                                                           "NAME_SELLER_INDUSTRY",
                                                           "NAME_YIELD_GROUP")]),stringsAsFactors=TRUE)

summary(newprevious[,quant])

summary (newprevious[,quant]) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)


# Coeficientes de correlação de Pearson para cada par de variáveis quantitativas
rho <- rcorr(as.matrix(newprevious[,quant]), type="pearson")

corr_coef <- rho$r # Matriz de correlações
corr_sig <- round(rho$P, 5) # Matriz com p-valor dos coeficientes

# Elaboração de um mapa de calor das correlações de Pearson entre as variáveis


ggplotly(newprevious[,quant] %>% 
           cor() %>% 
           melt() %>% 
           dplyr::rename(Correlação = value) %>%
           ggplot() +
           geom_tile(aes(x = Var1, y = Var2, fill = Correlação)) +
           geom_text(aes(x = Var1, y = Var2, label = format(Correlação, digits = 2)),
                     size = 3) +
           scale_fill_gradient2(low = "dodgerblue4", 
                                mid = "white", 
                                high = "orange",
                                midpoint = 0) +
           labs(x = NULL, y = NULL) +
           theme(panel.background = element_rect("white"),
                 panel.grid = element_line("grey95"),
                 panel.border = element_rect(NA),
                 legend.position = "bottom"))


## analisando as variáveis qualititativas
summary(qual)

summary (qual) %>%
        kable() %>%
        kable_styling(bootstrap_options = "striped", 
                      full_width = TRUE, 
                      font_size = 12)

######################################################################

#aplicando método de mínimos e máximos para melhorar performance ANN

max_data <- apply(newprevious, 2, max)

min_data <- apply(newprevious, 2, min)

scaled <- data.frame (scale(newprevious,
                            center = min_data,
                            scale = max_data - min_data))

#preparar variável explicativa
# 1 se crédito aprovado / 0 se reprovado

aprovado <- ifelse(db_avaliacao_variaveis_na$NAME_CONTRACT_STATUS == "Approved", 1, 0)

#criar dataset com todas variáveis normalizadas e dummizadas 
#, sendo que a primeira variável será a variável de decisão aprovado

db_rede_neuro <- data.frame()
db_rede_neuro <- cbind.data.frame(aprovado,scaled)


db_rede_neuro %>% str()

# Coeficientes de correlação de Pearson para cada par de variáveis
rho <- rcorr(as.matrix(db_rede_neuro), type="pearson")

corr_coef <- rho$r # Matriz de correlações
corr_sig <- round(rho$P, 5) # Matriz com p-valor dos coeficientes

# Elaboração de um mapa de calor das correlações de Pearson entre as variáveis
ggplotly(
  db_rede_neuro[,2:7] %>%
    cor() %>%
    melt() %>%
    rename(Correlação = value) %>%
    ggplot() +
    geom_tile(aes(x = Var1, y = Var2, fill = Correlação)) +
    geom_text(aes(x = Var1, y = Var2, label = format(round(Correlação,3))),
              size = 3) +
    scale_fill_viridis_b() +
    labs(x = NULL, y = NULL) +
    theme_bw(base_size = 6))

summary(db_rede_neuro)

##
#dividindo base teste e treino da ANN
#reduzindo amostra
set.seed(0)
reducao = sample(1:nrow(db_rede_neuro),round(0.04*nrow(db_rede_neuro)))
db_reduzido <- as.data.frame(db_rede_neuro[reducao,])

#gerando treino e teste
index = sample(1:nrow(db_reduzido),round(0.8*nrow(db_reduzido)))
index2 = sample(1:nrow(db_reduzido),round(0.2*nrow(db_reduzido)))
train_data <- as.data.frame(db_reduzido[index,])
test_data <- as.data.frame(db_reduzido[index2,])

#Utiliza o neuralnet
#set.seed(0)
#n <- names(train_data)
#f <- as.formula(paste("aprovado ~", paste(n[!n %in% "aprovado"], collapse = " + ")))

#rodando e contando tempo execução


learning_rate <- function(epoca) {
  initial_lr <- 0.1  # taxa de aprendizado inicial
  decay_rate <- 0.001 # taxa de decaimento
  lr <- initial_lr / (1 + decay_rate * epoca)
  return(lr)
}

start_time <- Sys.time()
nn <- neuralnet(aprovado ~.,
                data=train_data,
                hidden=c(4),
                #threshold = 0.1,
                stepmax = 300000,
                learningrate = learning_rate,
                #algorithm = "backprop",
                #err.fct="sse",
                #rep = 100,
                linear.output=F) 
end_time <- Sys.time()
tempo_nn <- end_time - start_time
tempo_nn
plot(nn)

pr.nn <- neuralnet::compute(nn,test_data[,2:93])

#como temos 0 ou 1, vamos arredondar com 0 digitos. Assim verificamos o que passou
#ou não

crit <- function(valor) {
  ifelse(valor > 0.80, 1, 0) #taxa de aprovação de 20% (CUTOFF)
} 

pr.nn$yhate <- sapply(pr.nn$net.result,crit)

#pr.nn$yhate <- sapply(pr.nn$net.result,round,digits=0)## CUTOFF DE 50% 


tab_net <- table(test_data$aprovado,pr.nn$yhate)

acc_net <- (tab_net[1,1]+tab_net[2,2])/nrow(test_data)

print(acc_net)

#logo a acurancidade da rede neural está em 89.00%

##########################CURVA ROC###########################

teste_prob <- pr.nn$net.result

# calcular curva ROC para a base de treino

pr.nn_treino <- neuralnet::compute(nn,train_data[,2:93])

treino_prob <- pr.nn_treino$net.result

train_roc <- roc(train_data$aprovado,treino_prob)

# calcular curva ROC para a base de teste

test_roc <- roc(test_data$aprovado, teste_prob)


#plotar curvas ROC para a base de treino e de teste

plot(1-train_roc$specificities, train_roc$sensitivities, col = "blue" , 
     lwd = 2,
     main = "Curva ROC - Treino vs. Teste Rede Neural", 
     xlab = "1 - Especificidade", ylab = "Sensitividade")
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
     col = "lightgray", border = NA)
abline(v = seq(par("usr")[1], par("usr")[2], length.out = 6), 
       h = seq(par("usr")[3], par("usr")[4], length.out = 6), 
       col = "white", lty = 1, lwd = 0.5)
lines(1-train_roc$specificities, train_roc$sensitivities, col = "blue" , 
      lwd = 2)
lines(1-test_roc$specificities, test_roc$sensitivities, col = "red",
      lwd = 2)
lines(x = c(0,1), y = c(0,1), col = "black", lty = 2, lwd=1)
legend("bottomright", legend = c("Treino", "Teste"), 
       col = c("blue", "red"), lty = 1, lwd = 2)
text(x = 0.93, y = 0.28, 
     labels = paste0("AUC Teste = ", round(auc(test_roc), digits = 2)), 
     col = "red", cex = 1.2)
text(x = 0.93, y = 0.35, 
     labels = paste0("AUC Treino = ", round(auc(train_roc), digits = 2)), 
     col = "blue", cex = 1.2)
   


##############Comparando com Árvore Decisão####################################

#Train-Test Split

#train_test_split_index <- 0.8 * nrow(newprevious)
#train <- data.frame(newprevious[1:train_test_split_index,])
#test <- data.frame(newprevious[(train_test_split_index+1): nrow(newprevious),])

#train %>% str()
#test %>% str()


# árvore
fit_tree <- rpart(aprovado ~.,
                  parms = list(split = 'gini'),
                  method="class", data=train_data)

tree_predict <- predict(fit_tree,test_data)
c_test=factor(ifelse(tree_predict[,2] > 0.8,"y","n"))

tab_tree <- table(c_test, test_data$aprovado)

acc_tree <- (tab_tree[1,1]+tab_tree[2,2])/nrow(test_data)

print(acc_tree)
## 88.26% de acuracia

# Visualizando a árvore #

# Definindo uma paleta de cores
paleta = scales::viridis_pal(begin=.75, end=1)(20)
# Plotando a árvore
rpart.plot(fit_tree,box.palette = paleta) # Paleta de cores
