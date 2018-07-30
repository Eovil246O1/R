library(tidyverse)
library(xgboost)
library(magrittr)
library(smbinning)
library(data.table)
data_dir = "C:\\Users\\Viacheslav_Pyrohov\\Desktop\\Kaggle_Homecredit competition"
#---------------------------
cat("Loading data...\n")

bbalance <- read_csv(file.path(data_dir, "bureau_balance.csv"))
bureau <- read_csv(file.path(data_dir, "bureau.csv"))
cc_balance <- read_csv(file.path(data_dir, "credit_card_balance.csv"))
payments <- read_csv(file.path(data_dir, "installments_payments.csv"))
pc_balance <- read_csv(file.path(data_dir, "POS_CASH_balance.csv"))
prev <- read_csv(file.path(data_dir, "previous_application.csv"))
tr <- read_csv(file.path(data_dir, "application_train.csv"))
te <- read_csv(file.path(data_dir, "application_test.csv"))

#---------------------------
cat("Preprocessing...\n")

fn <- funs(mean, sd, min, max, sum, n_distinct, .args = list(na.rm = TRUE))

sum_bbalance <- bbalance %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_BUREAU) %>% 
  summarise_all(fn) 
rm(bbalance); gc()

sum_bureau <- bureau %>% 
  left_join(sum_bbalance, by = "SK_ID_BUREAU") %>% 
  select(-SK_ID_BUREAU) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn)
rm(bureau, sum_bbalance); gc()

sum_cc_balance <- cc_balance %>% 
  select(-SK_ID_PREV) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn)
rm(cc_balance); gc()

sum_payments <- payments %>% 
  select(-SK_ID_PREV) %>% 
  mutate(PAYMENT_PERC = AMT_PAYMENT / AMT_INSTALMENT,
         PAYMENT_DIFF = AMT_INSTALMENT - AMT_PAYMENT,
         DPD = DAYS_ENTRY_PAYMENT - DAYS_INSTALMENT,
         DBD = DAYS_INSTALMENT - DAYS_ENTRY_PAYMENT,
         DPD = ifelse(DPD > 0, DPD, 0),
         DBD = ifelse(DBD > 0, DBD, 0)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn) 
rm(payments); gc()

sum_pc_balance <- pc_balance %>% 
  select(-SK_ID_PREV) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn)
rm(pc_balance); gc()

sum_prev <- prev %>%
  select(-SK_ID_PREV) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  mutate(DAYS_FIRST_DRAWING = ifelse(DAYS_FIRST_DRAWING == 365243, NA, DAYS_FIRST_DRAWING),
         DAYS_FIRST_DUE = ifelse(DAYS_FIRST_DUE == 365243, NA, DAYS_FIRST_DUE),
         DAYS_LAST_DUE_1ST_VERSION = ifelse(DAYS_LAST_DUE_1ST_VERSION == 365243, NA, DAYS_LAST_DUE_1ST_VERSION),
         DAYS_LAST_DUE = ifelse(DAYS_LAST_DUE == 365243, NA, DAYS_LAST_DUE),
         DAYS_TERMINATION = ifelse(DAYS_TERMINATION == 365243, NA, DAYS_TERMINATION),
         APP_CREDIT_PERC = AMT_APPLICATION / AMT_CREDIT) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn) 
rm(prev); gc()

tri <- 1:nrow(tr)
y <- tr$TARGET

tr_te <- tr %>% 
  select(-TARGET) %>% 
  bind_rows(te) %>%
  left_join(sum_bureau, by = "SK_ID_CURR") %>% 
  left_join(sum_cc_balance, by = "SK_ID_CURR") %>% 
  left_join(sum_payments, by = "SK_ID_CURR") %>% 
  left_join(sum_pc_balance, by = "SK_ID_CURR") %>% 
  left_join(sum_prev, by = "SK_ID_CURR") %>% 
  select(-SK_ID_CURR) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  mutate(na = apply(., 1, function(x) sum(is.na(x))),
         DAYS_EMPLOYED = ifelse(DAYS_EMPLOYED == 365243, NA, DAYS_EMPLOYED),
         DAYS_EMPLOYED_PERC = sqrt(DAYS_EMPLOYED / DAYS_BIRTH),
         INCOME_CREDIT_PERC = AMT_INCOME_TOTAL / AMT_CREDIT,
         INCOME_PER_PERSON = log1p(AMT_INCOME_TOTAL / CNT_FAM_MEMBERS),
         ANNUITY_INCOME_PERC = sqrt(AMT_ANNUITY / (1 + AMT_INCOME_TOTAL)),
         LOAN_INCOME_RATIO = AMT_CREDIT / AMT_INCOME_TOTAL,
         ANNUITY_LENGTH = AMT_CREDIT / AMT_ANNUITY,
         CHILDREN_RATIO = CNT_CHILDREN / CNT_FAM_MEMBERS, 
         CREDIT_TO_GOODS_RATIO = AMT_CREDIT / AMT_GOODS_PRICE,
         INC_PER_CHLD = AMT_INCOME_TOTAL / (1 + CNT_CHILDREN),
         SOURCES_PROD = EXT_SOURCE_1 * EXT_SOURCE_2 * EXT_SOURCE_3,
         CAR_TO_BIRTH_RATIO = OWN_CAR_AGE / DAYS_BIRTH,
         CAR_TO_EMPLOY_RATIO = OWN_CAR_AGE / DAYS_EMPLOYED,
         PHONE_TO_BIRTH_RATIO = DAYS_LAST_PHONE_CHANGE / DAYS_BIRTH,
         PHONE_TO_EMPLOY_RATIO = DAYS_LAST_PHONE_CHANGE / DAYS_EMPLOYED) 

docs <- str_subset(names(tr), "FLAG_DOC")
live <- str_subset(names(tr), "(?!NFLAG_)(?!FLAG_DOC)(?!_FLAG_)FLAG_")
inc_by_org <- tr_te %>% 
  group_by(ORGANIZATION_TYPE) %>% 
  summarise(m = median(AMT_INCOME_TOTAL)) %$% 
  setNames(as.list(m), ORGANIZATION_TYPE)

# part for empty EXT
empt_ext <- te %>%
  select(SK_ID_CURR, EXT_SOURCE_1, EXT_SOURCE_2, EXT_SOURCE_3) %>%
  mutate(IS_EMPT = if_else(apply(., 1, function(x) any(is.na(x))), 1, 0)) %>%
  select(SK_ID_CURR, IS_EMPT)

rm(tr, te, fn, sum_bureau, sum_cc_balance, 
   sum_payments, sum_pc_balance, sum_prev); gc()

tr_te %<>% 
  mutate(DOC_IND_KURT = apply(tr_te[, docs], 1, moments::kurtosis),
         LIVE_IND_SUM = apply(tr_te[, live], 1, sum),
         NEW_INC_BY_ORG = recode(tr_te$ORGANIZATION_TYPE, !!!inc_by_org),
         NEW_EXT_SOURCES_MEAN = apply(tr_te[, c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3")], 1, mean),
         NEW_SCORES_STD = apply(tr_te[, c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3")], 1, sd))%>%
  mutate_all(funs(ifelse(is.nan(.), NA, .))) %>% 
  mutate_all(funs(ifelse(is.infinite(.), NA, .))) %>% 
  data.matrix()

#---------------------------
cat("Save & load long dataset...\n")
saveRDS(tr_te, file = paste0(data_dir, "//Calculation//input_bigmatrix.rds"))
saveRDS(tri, file = paste0(data_dir, "//Calculation//input_tri.rds"))
saveRDS(y, file = paste0(data_dir, "//Calculation//input_y.rds"))
gc()

#---------------------------
xgb_feat = data.table(Feature = character(), Gain = numeric(), Cover = numeric(), Frequency = numeric())
xgb_pred_list = list()
xgb_pred_empt_list = list()
xgb_score = vector()
cat("Start bagging...\n")
for (i in 1:5) {
  cat("Load data...\n")
  tr_te = readRDS(paste0(data_dir, "//Calculation//input_bigmatrix.rds"))
  tri = readRDS(paste0(data_dir, "//Calculation//input_tri.rds"))
  y = readRDS(paste0(data_dir, "//Calculation//input_y.rds"))
  
  #---------------------------
  cat("Create test sample...\n")
  dtest <- xgb.DMatrix(data = tr_te[-tri, ])
  tr_val <- tr_te[tri, ]
  #---------------------------
  cat("Create train and validation sample...\n")
  df_test = data.frame(tr_val[,c('EXT_SOURCE_1', 'EXT_SOURCE_2', 'EXT_SOURCE_3')], y, id = 1:nrow(tr_val))
  
  df_bin = smbinning(df_test, y = 'y', x = 'EXT_SOURCE_1', p = .1)
  df_test = smbinning.gen(df_test, df_bin, chrname = 'EXT_SOURCE_1_BIN')
  
  df_bin = smbinning(df_test, y = 'y', x = 'EXT_SOURCE_2', p = .1)
  df_test = smbinning.gen(df_test, df_bin, chrname = 'EXT_SOURCE_2_BIN')
  
  df_bin = smbinning(df_test, y = 'y', x = 'EXT_SOURCE_3', p = .1)
  df_test = smbinning.gen(df_test, df_bin, chrname = 'EXT_SOURCE_3_BIN')
  
  df_test = as.data.table(df_test)
  df_test[is.na(EXT_SOURCE_2), 
          `:=` (EXT_SOURCE_1_BIN = 'EXT_SOURCE_2_NA', 
                EXT_SOURCE_2_BIN = 'EXT_SOURCE_2_NA', 
                EXT_SOURCE_3_BIN = 'EXT_SOURCE_2_NA')]
  df_test = df_test[,.(EXT_SOURCE_1_BIN, EXT_SOURCE_2_BIN, EXT_SOURCE_3_BIN, y, id)]
  
  df_fin <- df_test %>%
    group_by(EXT_SOURCE_1_BIN, EXT_SOURCE_2_BIN, EXT_SOURCE_3_BIN, y) %>%
    sample_frac(., 0.9) #%>%
  
  val_id = df_fin$id
  
  dtrain <- xgb.DMatrix(data = tr_val[val_id, ], label = y[val_id])
  dval <- xgb.DMatrix(data = tr_val[-val_id, ], label = y[-val_id])
  #cols <- colnames(tr_val)
  
  #rm(tr_te, y, tri); gc()
  gc()
  
  p <- list(objective = "binary:logistic",
            booster = "gbtree",
            eval_metric = "auc",
            nthread = 4,
            eta = 0.05,
            max_depth = 6,
            min_child_weight = 30,
            gamma = 0,
            subsample = 0.85,
            colsample_bytree = 0.7,
            colsample_bylevel = 0.632,
            alpha = 0,
            lambda = 0,
            nrounds = 2000)
  #---------------------------
  cat("Training ordinary model...\n")
  m_xgb <- xgb.train(p, dtrain, p$nrounds, list(val = dval), print_every_n = 50, early_stopping_rounds = 300)
  xgb_pred_list[[i]] = rank(predict(m_xgb, dtest), ties.method = 'first')
  #---------------------------
  cat("Training empty EXT sources model...\n")
  tr_te = tr_te[,!colnames(tr_te) %in% c('EXT_SOURCE_1', 'EXT_SOURCE_2', 'EXT_SOURCE_3', 'SOURCES_PROD', 'NEW_EXT_SOURCES_MEAN', 'NEW_SCORES_STD')]
  dtest <- xgb.DMatrix(data = tr_te[-tri, ])
  tr_val <- tr_te[tri, ]
  dtrain <- xgb.DMatrix(data = tr_val[val_id, ], label = y[val_id])
  dval <- xgb.DMatrix(data = tr_val[-val_id, ], label = y[-val_id])
  cols <- colnames(tr_val)
  
  m_xgb_empt <- xgb.train(p, dtrain, p$nrounds, list(val = dval), print_every_n = 50, early_stopping_rounds = 300)
  xgb_pred_empt_list[[i]] = rank(predict(m_xgb_empt, dtest), ties.method = 'first')

  xgb_feat = rbindlist(list(xgb_feat, xgb.importance(cols, model = m_xgb_empt)))
  xgb_score[i] = m_xgb$best_score
}

#---------------------------
xgb_pred_avg = Reduce(`+`, xgb_pred_list)
xgb_pred_avg = xgb_pred_avg/i
xgb_pred_avg = xgb_pred_avg/max(xgb_pred_avg)

xgb_pred_empt_avg = Reduce(`+`, xgb_pred_empt_list)
xgb_pred_empt_avg = xgb_pred_empt_avg/i
xgb_pred_empt_avg = xgb_pred_empt_avg/max(xgb_pred_empt_avg)

xgb_feat_avg = xgb_feat %>% group_by(Feature) %>%
  summarize(gain_avg = mean(Gain),
            cover_avg = mean(Cover),
            frequency_avg = mean(Frequency))

xgb_score_avg = mean(xgb_score)

#---------------------------
read_csv(file.path(data_dir, "//Models//sample_submission.csv")) %>%
  left_join(empt_ext, by = "SK_ID_CURR") %>%
  mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
         TARGET_ORD = xgb_pred_avg,
         TARGET_EMPT_EXT = xgb_pred_empt_avg,
         TARGET = if_else(IS_EMPT == 0, TARGET_ORD, 0.6*TARGET_ORD + 0.4*TARGET_EMPT_EXT)) %>%
  #select(-c(IsEmptBureau)) %>%
  write_csv(file.path(data_dir, paste0("//Models//new_mod_", round(xgb_score_avg, 5), ".csv")))

# write file with characteristic parameters
write_csv(xgb_feat_avg, file.path(data_dir, paste0("//Results//new_mod_", round(xgb_score_avg, 5), "_importance.csv")))
