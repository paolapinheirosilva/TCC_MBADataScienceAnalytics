db_td <- db_avaliacao_variaveis_na[,c("AMT_ANNUITY",
                                            "AMT_APPLICATION",
                                            "AMT_CREDIT",
                                            "AMT_GOODS_PRICE",
                                            "AMT_DOWN_PAYMENT",
                                            "DAYS_DECISION",
                                            "CNT_PAYMENT"
)]

qualitativa <- db_avaliacao_variaveis_na[,c(
                                        "NAME_CONTRACT_TYPE",
                                        "NAME_CASH_LOAN_PURPOSE",
                                        "NAME_PAYMENT_TYPE",
                                        "NAME_TYPE_SUITE",
                                        "NAME_CLIENT_TYPE",
                                        "NAME_GOODS_CATEGORY",
                                        "NAME_PORTFOLIO",
                                        "NAME_PRODUCT_TYPE",
                                        "CHANNEL_TYPE",
                                        "NAME_SELLER_INDUSTRY",
                                        "NAME_YIELD_GROUP")]

db_td <- cbind.data.frame(db_td,qualitativa)

db_td <- cbind.data.frame(aprovado,db_td)


set.seed(0)
reducao_td = sample(1:nrow(db_td),round(0.04*nrow(db_td)))
db_reduzido <- as.data.frame(db_td[reducao,])

#Train-Test Split

train_test_split_index <- 0.8 * nrow(db_reduzido)
train <- data.frame(db_reduzido[1:train_test_split_index,])
test <- data.frame(db_reduzido[(train_test_split_index+1): nrow(db_reduzido),])

#train %>% str()
#test %>% str()


# árvore
fit_tree <- rpart(aprovado ~.,
                  parms = list(split = 'gini'),
                  method="class", 
                  data=db_reduzido,
                  control = rpart.control(minsplit = 0, cp = 0.003384 ))

tree_predict <- predict(fit_tree,test)
c_test=factor(ifelse(tree_predict[,2] > 0.8,"y","n"))

tab_tree <- table(c_test, test$aprovado)

acc_tree <- (tab_tree[1,1]+tab_tree[2,2])/nrow(test_data)

print(acc_tree)
## 88.26% de acuracia

# Visualizando a árvore #

# Definindo uma paleta de cores
paleta = scales::viridis_pal(begin=.75, end=1)(20)
# Plotando a árvore
rpart.plot(fit_tree,box.palette = paleta, under = T, fallen.leaves = T, varlen = -10) # Paleta de cores

