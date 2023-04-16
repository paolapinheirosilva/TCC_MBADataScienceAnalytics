
## criando modelo GLM para verificação significancia estatística das variáveis
#uma vez que não existe processo stepwise para redes neurais, sendo mais importante
#alterar os hiperparâmetros. Contudo, como já foram realizados modificações e não
#houve melhoa no tamanha da amostra, iremos partir para o GLM para realizarmos 
#análise de significância

#reduzindo amostra para verificar convergencia de valores
set.seed(0)
reducao = sample(1:nrow(db_rede_neuro),round(1*nrow(db_rede_neuro)))
db_glm_reduzido <- as.data.frame(db_rede_neuro[reducao,])
db_glm_reduzido$aprovado <- as.factor(db_glm_reduzido$aprovado)

#utilizando modelo GLM Binomial para verificar variáveis estatísticamente significantes
modelo_teste_glm <- glm(formula = aprovado ~ . ,
                         data = db_glm_reduzido,
                         family = "binomial")
summary(modelo_teste_glm)


#Pelo GLM Binomial, considerando uma significância de 95% temos que as seguintes 
#variáveis deverão ser consideradas:
#AMT_ANNUITY
#AMT_APPLICATION
#AMT_CREDIT
#AMT_DOWN_PAYMENT
#DAYS_DECISION
#CNT_PAYMENT
#NAME_CASH_LOAN_PURPOSEBuying.a.used.car
#NAME_CASH_LOAN_PURPOSENot.Declared
#NAME_CASH_LOAN_PURPOSEPayments.on.other.loans 
#NAME_PAYMENT_TYPECash.through.the.bank 
#NAME_TYPE_SUITEChildren                  
#NAME_TYPE_SUITEFamily                                      
#NAME_TYPE_SUITEGroup.of.people                             
#NAME_TYPE_SUITEOther                                        
#NAME_TYPE_SUITESpouse.partner                               
#NAME_CLIENT_TYPENew                                        
#NAME_CLIENT_TYPERefreshed                                  
#NAME_GOODS_CATEGORYNot.Declared                            
#NAME_PRODUCT_TYPEwalk.in                                   
#CHANNEL_TYPECountry.wide                                  
#CHANNEL_TYPECredit.and.cash.offices                          
#NAME_SELLER_INDUSTRYClothing                                
#NAME_SELLER_INDUSTRYConnectivity                          
#NAME_SELLER_INDUSTRYConstruction                           
#NAME_SELLER_INDUSTRYConsumer.electronics                   
#NAME_SELLER_INDUSTRYFurniture                             
#NAME_SELLER_INDUSTRYIndustry                               
#NAME_SELLER_INDUSTRYJewelry                                
#NAME_SELLER_INDUSTRYMLM.partners                           
#NAME_YIELD_GROUPhigh                                       
#NAME_YIELD_GROUPlow.action                                 
#NAME_YIELD_GROUPlow.normal                                 

#Gerando nova base, considerando somente as variáveis significantes

db_stepwise <- db_glm_reduzido[,c("AMT_ANNUITY",
                                  "AMT_APPLICATION",
                                  "AMT_CREDIT",
                                  "AMT_DOWN_PAYMENT",
                                  "DAYS_DECISION",
                                  "CNT_PAYMENT",
                                  "NAME_CASH_LOAN_PURPOSEBuying.a.used.car",
                                  "NAME_CASH_LOAN_PURPOSENot.Declared",
                                  "NAME_CASH_LOAN_PURPOSEPayments.on.other.loans",
                                  "NAME_PAYMENT_TYPECash.through.the.bank",
                                  "NAME_TYPE_SUITEChildren",
                                  "NAME_TYPE_SUITEFamily",
                                  "NAME_TYPE_SUITEGroup.of.people",
                                  "NAME_TYPE_SUITEOther",
                                  "NAME_TYPE_SUITESpouse.partner",
                                  "NAME_CLIENT_TYPENew",
                                  "NAME_CLIENT_TYPERefreshed",
                                  "NAME_GOODS_CATEGORYNot.Declared",
                                  "NAME_PRODUCT_TYPEwalk.in",
                                  "CHANNEL_TYPECountry.wide",
                                  "CHANNEL_TYPECredit.and.cash.offices",
                                  "NAME_SELLER_INDUSTRYClothing",
                                  "NAME_SELLER_INDUSTRYConnectivity",
                                  "NAME_SELLER_INDUSTRYConstruction",
                                  "NAME_SELLER_INDUSTRYConsumer.electronics",
                                  "NAME_SELLER_INDUSTRYFurniture",
                                  "NAME_SELLER_INDUSTRYIndustry",
                                  "NAME_SELLER_INDUSTRYJewelry",
                                  "NAME_SELLER_INDUSTRYMLM.partners",
                                  "NAME_YIELD_GROUPhigh",
                                  "NAME_YIELD_GROUPlow.action",
                                  "NAME_YIELD_GROUPlow.normal")]
db_stepwise <- cbind.data.frame(db_rede_neuro$aprovado,db_stepwise)

db_stepwise <- db_stepwise %>% rename(aprovado = `db_rede_neuro$aprovado`) 

summary(db_stepwise)

#Iniciando processo rede neural

#reduzindo amostra para verificar convergencia de valores
set.seed(0)
reducao = sample(1:nrow(db_stepwise),round(0.04*nrow(db_stepwise)))
db_reduzido_step_wise <- as.data.frame(db_stepwise[reducao,])

#gerando treino e teste
set.seed(0)

index = sample(1:nrow(db_reduzido_step_wise),round(0.8*nrow(db_reduzido_step_wise)))
index2 = sample(1:nrow(db_reduzido_step_wise),round(0.2*nrow(db_reduzido_step_wise)))
train_data_SW <- as.data.frame(db_reduzido_step_wise[index,])
test_data_SW <- as.data.frame(db_reduzido_step_wise[index2,])


learning_rate <- function(epoca) {
  initial_lr <- 0.1  # taxa de aprendizado inicial
  decay_rate <- 0.001 # taxa de decaimento
  lr <- initial_lr / (1 + decay_rate * epoca)
  return(lr)
}

start_time <- Sys.time()
nn_sw <- neuralnet(aprovado ~.,
                data=train_data_SW,
                hidden=c(4),
                #threshold = 0.1,
                stepmax = 300000,
                learningrate = learning_rate,
                #algorithm = "backprop",
                #err.fct="sse",
                #rep = 100,
                linear.output=F) 
end_time <- Sys.time()
tempo_step_wise <- end_time - start_time
tempo_step_wise
plot(nn_sw)


pr.nn_sw <- neuralnet::compute(nn_sw,test_data_SW[,2:33])

#como temos 0 ou 1, vamos arredondar com 0 digitos. Assim verificamos o que passou
#ou não

crit <- function(valor) {
  ifelse(valor > 0.80, 1, 0) #taxa de aprovação de 20% (CUTOFF)
} 

pr.nn_sw$yhate <- sapply(pr.nn_sw$net.result,crit)

#pr.nn$yhate <- sapply(pr.nn$net.result,round,digits=0)## CUTOFF DE 50% 


tab_net_sw <- table(test_data_SW$aprovado,pr.nn_sw$yhate)

acc_net_sw <- (tab_net[1,1]+tab_net[2,2])/nrow(test_data_SW)

print(acc_net_sw)

#logo a acurancidade da rede neural está em 89.00%

##########################CURVA ROC###########################

teste_prob_sw <- pr.nn_sw$net.result

# calcular curva ROC para a base de treino

pr.nn_treino_sw <- neuralnet::compute(nn_sw,train_data_SW[,2:33])

treino_prob_sw <- pr.nn_treino_sw$net.result

train_roc_sw <- roc(train_data_SW$aprovado,treino_prob_sw)

# calcular curva ROC para a base de teste

test_roc_sw <- roc(test_data_SW$aprovado, teste_prob_sw)


#plotar curvas ROC para a base de treino e de teste

plot(1-train_roc_sw$specificities, train_roc_sw$sensitivities, col = "blue" , 
     lwd = 2,
     main = "Curva ROC - Treino vs. Teste Rede Neural com Step Wise", 
     xlab = "1 - Especificidade", ylab = "Sensitividade")
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
     col = "lightgray", border = NA)
abline(v = seq(par("usr")[1], par("usr")[2], length.out = 6), 
       h = seq(par("usr")[3], par("usr")[4], length.out = 6), 
       col = "white", lty = 1, lwd = 0.5)
lines(1-train_roc_sw$specificities, train_roc_sw$sensitivities, col = "blue" , 
      lwd = 2)
lines(1-test_roc_sw$specificities, test_roc_sw$sensitivities, col = "red",
      lwd = 2)
lines(x = c(0,1), y = c(0,1), col = "black", lty = 2, lwd=1)
legend("bottomright", legend = c("Treino", "Teste"), 
       col = c("blue", "red"), lty = 1, lwd = 2)
text(x = 0.93, y = 0.28, 
     labels = paste0("AUC Teste = ", round(auc(test_roc_sw), digits = 2)), 
     col = "red", cex = 1.2)
text(x = 0.93, y = 0.35, 
     labels = paste0("AUC Treino = ", round(auc(train_roc_sw), digits = 2)), 
     col = "blue", cex = 1.2)



##############Comparando com Árvore Decisão####################################

#Train-Test Split

#train_test_split_index <- 0.8 * nrow(newprevious)
#train <- data.frame(newprevious[1:train_test_split_index,])
#test <- data.frame(newprevious[(train_test_split_index+1): nrow(newprevious),])

#train %>% str()
#test %>% str()


# árvore
fit_tree_sw <- rpart(aprovado ~.,
                  parms = list(split = 'gini'),
                  method="class", data=train_data_SW)

tree_predict_sw <- predict(fit_tree_sw,test_data_SW)
c_test_sw=factor(ifelse(tree_predict_sw[,2] > 0.8,"y","n"))

tab_tree_sw <- table(c_test_sw, test_data_SW$aprovado)

acc_tree_sw <- (tab_tree_sw[1,1]+tab_tree_sw[2,2])/nrow(test_data_SW)

print(acc_tree_sw)
## 88.26% de acuracia