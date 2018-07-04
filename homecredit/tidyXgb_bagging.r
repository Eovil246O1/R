library(tidyverse)
library(xgboost)
library(magrittr)

data_dir = "C:\\Users\\Eovil\\Desktop\\Kaggle_Homecredit competition"

m_list = list()
m_result = list()
#for (i in 1:4) {
#set.seed(42+i)
set.seed(42)
#---------------------------
cat("Loading data...\n")
tr <- read_csv(file.path(data_dir, "application_train.csv"))
te <- read_csv(file.path(data_dir, "application_test.csv"))

bureau <- read_csv(file.path(data_dir, "bureau.csv")) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer())) 

cred_card_bal <-  read_csv(file.path(data_dir, "credit_card_balance.csv")) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer())) 

pos_cash_bal <- read_csv(file.path(data_dir, "POS_CASH_balance.csv")) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer())) 

prev <- read_csv(file.path(data_dir, "previous_application.csv")) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer()))

#---------------------------
cat("Preprocessing...\n")

to_exclude = c('SK_ID_CURR', 'SK_ID_BUREAU')

# AVG metrics calculation
avg_bureau <- bureau %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>%
  rename_at(vars(-one_of(to_exclude)), ~ paste0(., '_avg')) %>%
  mutate(buro_count = bureau %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

avg_cred_card_bal <- cred_card_bal %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_avg')) %>%
  mutate(card_count = cred_card_bal %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

avg_pos_cash_bal <- pos_cash_bal %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_avg')) %>%
  mutate(pos_count = pos_cash_bal %>%  
           group_by(SK_ID_PREV, SK_ID_CURR) %>% 
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

avg_prev <- prev %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_avg')) %>%
  mutate(nb_app = prev %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

# MAX metrics calculation

max_bureau <- bureau %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(max(., na.rm = TRUE))) %>%
  rename_at(vars(-one_of(to_exclude)), ~ paste0(., '_max'))

max_cred_card_bal <- cred_card_bal %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(max(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_max'))

max_pos_cash_bal <- pos_cash_bal %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(max(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_max'))

max_prev <- prev %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(max(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_max'))

# MIN metrics calculation

min_bureau <- bureau %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(min(., na.rm = TRUE))) %>%
  rename_at(vars(-one_of(to_exclude)), ~ paste0(., '_min'))

min_cred_card_bal <- cred_card_bal %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(min(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_min'))

min_pos_cash_bal <- pos_cash_bal %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(min(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_min'))

min_prev <- prev %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(min(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_min'))

# SUM metrics calcualtion

sum_bureau <- bureau %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(sum(., na.rm = TRUE))) %>%
  rename_at(vars(-one_of(to_exclude)), ~ paste0(., '_sum'))

sum_cred_card_bal <- cred_card_bal %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(sum(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_sum'))

sum_pos_cash_bal <- pos_cash_bal %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(sum(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_sum'))

sum_prev <- prev %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(sum(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_sum'))

# STD.DEV metrics calcualtion

sd_bureau <- bureau %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(sd(., na.rm = TRUE))) %>%
  rename_at(vars(-one_of(to_exclude)), ~ paste0(., '_sd'))

sd_cred_card_bal <- cred_card_bal %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(sd(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_sd'))

sd_pos_cash_bal <- pos_cash_bal %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(sd(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_sd'))

sd_prev <- prev %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(sd(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_sd'))

tri <- 1:nrow(tr)
y <- tr$TARGET

tr_te <- tr %>% 
  select(-TARGET) %>% 
  bind_rows(te) %>%
  left_join(avg_bureau, by = "SK_ID_CURR") %>% 
  left_join(avg_cred_card_bal, by = "SK_ID_CURR") %>% 
  left_join(avg_pos_cash_bal, by = "SK_ID_CURR") %>% 
  left_join(avg_prev, by = "SK_ID_CURR") %>%
  left_join(max_bureau, by = "SK_ID_CURR") %>% 
  left_join(max_cred_card_bal, by = "SK_ID_CURR") %>% 
  left_join(max_pos_cash_bal, by = "SK_ID_CURR") %>% 
  left_join(max_prev, by = "SK_ID_CURR") %>%
  left_join(min_bureau, by = "SK_ID_CURR") %>% 
  left_join(min_cred_card_bal, by = "SK_ID_CURR") %>% 
  left_join(min_pos_cash_bal, by = "SK_ID_CURR") %>% 
  left_join(min_prev, by = "SK_ID_CURR") %>%
  left_join(sum_bureau, by = "SK_ID_CURR") %>% 
  left_join(sum_cred_card_bal, by = "SK_ID_CURR") %>% 
  left_join(sum_pos_cash_bal, by = "SK_ID_CURR") %>% 
  left_join(sum_prev, by = "SK_ID_CURR") %>%
  left_join(sd_bureau, by = "SK_ID_CURR") %>% 
  left_join(sd_cred_card_bal, by = "SK_ID_CURR") %>% 
  left_join(sd_pos_cash_bal, by = "SK_ID_CURR") %>% 
  left_join(sd_prev, by = "SK_ID_CURR") %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  data.matrix()

rm(tr, te, prev, avg_prev, bureau, avg_bureau, 
   cred_card_bal, avg_cred_card_bal, pos_cash_bal, avg_pos_cash_bal,
   max_prev, max_bureau, max_cred_card_bal, max_pos_cash_bal, 
   min_prev, min_bureau, min_cred_card_bal, min_pos_cash_bal, 
   sum_prev, sum_bureau, sum_cred_card_bal, sum_pos_cash_bal, 
   sd_prev, sd_bureau, sd_cred_card_bal, sd_pos_cash_bal); gc()

#---------------------------
cat("Preparing data...\n")
dtest <- xgb.DMatrix(data = tr_te[-tri, ])
tr_te <- tr_te[tri, ]

tri <- caret::createDataPartition(y, p = 0.9, list = F) %>% c()
dtrain <- xgb.DMatrix(data = tr_te[tri, ], label = y[tri])
dval <- xgb.DMatrix(data = tr_te[-tri, ], label = y[-tri])
cols <- colnames(tr_te)

rm(tr_te, y, tri); gc()

#---------------------------
cat("Training model...\n")
p <- list(objective = "binary:logistic",
          booster = "gbtree",
          eval_metric = "auc",
          nthread = 8,
          eta = 0.01,
          max_depth = 25,
          min_child_weight = 19,
          gamma = 0,
          subsample = 0.5,
          colsample_bytree = 0.9,
          alpha = 0,
          lambda = 0.05,
          nrounds = 5000)

m_xgb <- xgb.train(p, dtrain, p$nrounds, list(val = dval), print_every_n = 50, early_stopping_rounds = 200)
#m_list[[i]] = m_xgb
#m_result[[i]] = predict(m_list[[i]], dtest)

xgb.importance(cols, model=m_xgb) %>% 
  xgb.plot.importance(top_n = 30)
#}

avg_xgb = Reduce(`+`, m_result)
avg_xgb = avg_xgb/i

#---------------------------
read_csv(file.path(data_dir, "sample_submission.csv")) %>%  
  mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
         TARGET = avg_xgb) %>%
  write_csv(file.path(data_dir, paste0("xgb_bagging", ".csv")))