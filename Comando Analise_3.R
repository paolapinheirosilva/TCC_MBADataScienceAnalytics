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

#Criando base de dados
newprevious <- previous[c(3:6, 17:19, 22, 23, 25)]

#Verificando Missing Values
newprevious [is.na(newprevious) == TRUE] #temos muitos messsing values

#Limpando miss value
newprevious <- drop_na(newprevious) #Com a retirada o número de observações caiu
                                    # de 1.67M para 1.29M. Ainda uma base considerável
newprevious [is.na(newprevious) == TRUE]
head (newprevious)

#Analise descritiva banco de dados
#Verificando diferentes variaveis categoricas por variavel

#Verificando tipos de status
status <- unique(newprevious$NAME_CONTRACT_STATUS)
length(status) #ha 4 tipos de status: aprovado, reprovado, cancelado e não utilizado
print(status)
#como essa será a variável explicativa, vamos reduzir para duas categorias, Aprovado e Recusado

newprevious$NAME_CONTRACT_STATUS [newprevious$NAME_CONTRACT_STATUS == "Unused offer"] <- "Approved"

#o status cancelado não será utilizado, pois o processo não seguiu para frente

newprevious <- newprevious %>% filter(NAME_CONTRACT_STATUS != "Canceled")

#verificando tipos de contrato
contrato <- unique(newprevious$NAME_CONTRACT_TYPE)
length(contrato) #ha 3 tipos de contrato: emprestimo pessoal, capital de giro, 
      #rotativo
print(contrato)

#verificando tipos de clientes
cliente <- unique(newprevious$NAME_CLIENT_TYPE)
length(cliente) #ha 4 tipos de clientes: repetido, novo, atualizado e outros
print(cliente)

#como temos valores faltantes que não foram identificados antes, e para não 
#reduzir a base, vamos considerar como outros
newprevious$NAME_CLIENT_TYPE [newprevious$NAME_CLIENT_TYPE == "XNA"] <- "Other_client"

#verificando tipos de pagamentos
pagamento <- unique(newprevious$NAME_PAYMENT_TYPE)
length(pagamento) #ha 4 tipos de pagamentos: cash through the bank (pagamento 
      #efetivado pelo banco), non-cash from your bank (item depositado em uma 
      #conta, mas não creditado até que seja compensado), cashless from the 
      #account of the employer (conta sem saldo, pagamento não efetivado) 
print(pagamento)

#como temos valores faltantes que não foram identificados antes, e para não 
#reduzir a base, vamos considerar como outros
newprevious$NAME_PAYMENT_TYPE [newprevious$NAME_PAYMENT_TYPE == "XNA"] <- "Other_payment"

#newprevious <- newprevious %>% filter(NAME_PAYMENT_TYPE != "XNA")

#verificando categoria de utilização do crédito
utilizacao <- unique(newprevious$NAME_GOODS_CATEGORY)
length(utilizacao) #ha 28 tipos de produtos
print(utilizacao)

#como temos valores faltantes que não foram identificados antes, e para não 
#reduzir a base, vamos considerar como outros

newprevious$NAME_GOODS_CATEGORY [newprevious$NAME_GOODS_CATEGORY == "XNA"] <- "Other"

#verificando tipos de produtos
produto <- unique(newprevious$NAME_PRODUCT_TYPE)
length(produto) #há 3 tipos de proodutos: adicional, novo e outros 
print(produto)

#como temos valores faltantes que não foram identificados antes, e para não 
#reduzir a base, vamos considerar como outros

newprevious$NAME_PRODUCT_TYPE [newprevious$NAME_PRODUCT_TYPE == "XNA"] <- "Other_product"

###############################################################################
### Preparando a base para análise ###

newprevious %>% str()

#transformando variáveis categóricas em fatores

newprevious <- as.data.frame(unclass(newprevious), stringsAsFactors=TRUE)

newprevious %>% str()

#Realocando a variável de decisão para a primeira coluna:
newprevious <- newprevious %>% relocate (NAME_CONTRACT_STATUS, .before = NAME_CONTRACT_TYPE)

newprevious %>% str()

########################################################################
#Estatísitca Descritiva

# Estatísticas descritivas univariadas


#vetores para variáveis qualitativas e quantitativas
quant <- c(3:6)
qual <- c(1,2,7:10)

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
summary(newprevious[,qual])

summary (newprevious[,qual]) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)



#########################################################################################
### Iniciando Análise através de modelos supervisionados em ML


#Neural Network
set.seed(0)

#preparar variável explicativa
# 1 se crédito aprovado / 0 se reprovado

aprovado <- ifelse(newprevious$NAME_CONTRACT_STATUS == "Approved", 1, 0)

#vamos agrupar as variáveis categóricas utilizando análise de correspondência
#múltipla

#primeiro vamos substituir espaço por _
newprevious$NAME_CONTRACT_TYPE <- gsub(" ","_",newprevious$NAME_CONTRACT_TYPE)
newprevious$NAME_CLIENT_TYPE <- gsub(" ","_",newprevious$NAME_CLIENT_TYPE)
newprevious$NAME_GOODS_CATEGORY <- gsub(" ","_",newprevious$NAME_GOODS_CATEGORY)
newprevious$NAME_PRODUCT_TYPE <- gsub(" ","_",newprevious$NAME_PRODUCT_TYPE)
newprevious$NAME_PAYMENT_TYPE <- gsub(" ","_",newprevious$NAME_PAYMENT_TYPE)

newprevious <- as.data.frame(unclass(newprevious), stringsAsFactors=TRUE)

newprevious %>% str()

###Análise correspondência multipla
var_quali <- newprevious[,c(2,7:10)]

# A função para a criação da ACM pede que sejam utilizados "fatores"
var_quali <- as.data.frame(unclass(var_quali), stringsAsFactors=TRUE)

# Estatísticas descritivas
summary(var_quali)

# Iniciando a Análise de Correspondência Múltipla nas variáveis qualitativas

# Tabelas de contingência
sjt.xtab(var.row = var_quali$NAME_CONTRACT_TYPE,
         var.col = var_quali$NAME_PAYMENT_TYPE,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$NAME_CONTRACT_TYPE,
         var.col = var_quali$NAME_CLIENT_TYPE,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$NAME_CONTRACT_TYPE,
         var.col = var_quali$NAME_GOODS_CATEGORY,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$NAME_CONTRACT_TYPE,
         var.col = var_quali$NAME_PRODUCT_TYPE,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

#como todos p values são iguais a zero, podemos seguir com a análise ACM, pois
#todas as variaveis apresentam significancia estatística com pelo menos uma das
#variáveis

# Análise de Correspondência Múltipla
ACM <- dudi.acm(var_quali, scannf = FALSE) #pegando somente 2 primeiras dimensoes
#devido a limitacoes computacionais para a análise completa

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACM$eig / sum(ACM$eig)) * 100
perc_variancia

# Quantidade de categorias por variável
quant_categorias <- apply(var_quali,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACM$c1, Variável = rep(names(quant_categorias),
                                            quant_categorias))


# Obtendo as coordenadas das observações
coord_obs <- ACM$li

#aplicando método de mínimos e máximos para melhorar performance ANN

max_data <- apply(newprevious[,quant], 2, max)

min_data <- apply(newprevious[,quant], 2, min)

scaled <- data.frame (scale(newprevious[,quant],
                            center = min_data,
                            scale = max_data - min_data))

#criar dataset com todas variáveis criadas na ACM e normalização das variaveis 
#quantitativas, sendo que a primeira variável será a variável de decisão aprovado

db_rede_neuro <- data.frame()
db_rede_neuro <- cbind.data.frame(aprovado,scaled)
db_rede_neuro <- cbind.data.frame(db_rede_neuro,coord_obs)

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

##
#dividindo base teste e treino da ANN
#reduzindo amostra
set.seed(0)
reducao = sample(1:nrow(db_rede_neuro),round(0.1*nrow(db_rede_neuro)))
db_reduzido <- as.data.frame(db_rede_neuro[reducao,])

#gerando treino e teste
index = sample(1:nrow(db_reduzido),round(0.8*nrow(db_reduzido)))
index2 = sample(1:nrow(db_reduzido),round(0.2*nrow(db_reduzido)))
train_data <- as.data.frame(db_reduzido[index,])
test_data <- as.data.frame(db_reduzido[index2,])

#Utiliza o neuralnet
set.seed(0)
n <- names(train_data)
f <- as.formula(paste("aprovado ~", paste(n[!n %in% "aprovado"], collapse = " + ")))

#rodando e contando tempo execução
start_time <- Sys.time()
nn <- neuralnet(f,
                data=train_data,
                hidden=c(1),
                #threshold = 0.1,
                #stepmax = 10000, 
                linear.output=F) 
end_time <- Sys.time()
end_time - start_time

plot(nn)

pr.nn <- neuralnet::compute(nn,test_data[,2:7])

#como temos 0 ou 1, vamos arredondar com 0 digitos. Assim verificamos o que passou
#ou não
pr.nn$net.result <- sapply(pr.nn$net.result,round,digits=0)
pr.nn$net.result

tab_net <- table(test_data$aprovado,pr.nn$net.result)

acc_net <- (tab_net[1,1]+tab_net[2,2])/nrow(test_data)

print(acc_net)
#logo a acurancidade da rede neural está em 82.34%

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
c_test=factor(ifelse(tree_predict[,2] > 0.5,"y","n"))

tab_tree <- table(c_test, test_data$aprovado)

acc_tree <- (tab_tree[1,1]+tab_tree[2,2])/nrow(test_data)

print(acc_tree)
