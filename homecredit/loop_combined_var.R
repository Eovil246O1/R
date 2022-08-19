rm(list = ls()); gc()

data_dir = "C:\\Users\\Eovil\\Desktop\\Kaggle_Homecredit competition"

load(file = paste0(data_dir, "//Calculation//input_bigmatrix_short.RData"), .GlobalEnv)
load(file = paste0(data_dir, "//Calculation//input_tri.RData"), .GlobalEnv)
load(file = paste0(data_dir, "//Calculation//input_y.RData"), .GlobalEnv)

dt = as.data.table(tr_te[tri, ])
search_depth = 5 #unused by now
res_fin = data.frame(formula = character(),
                     corr_1 = double(),
                     corr_2 = double(),
                     corr_pair = double())
list_oper = c('-', '*') #to do: add c('+', '-', '*', '/')
for (g in 1:(ncol(dt)-1)) {
  cat(paste0("Start calc correlations for variable ", colnames(dt)[g], ", ¹", g, " from ", ncol(dt)-1, "...\n"))
  time = Sys.time()
  col_g_name = colnames(dt)[g]
  if (nrow(dt[is.nan(eval(as.name(col_g_name))) | is.na(eval(as.name(col_g_name))), ])/nrow(dt) > 0.75) next
  for (j in (g+1):ncol(dt)) {
    col_j_name = colnames(dt)[j]
    if (nrow(dt[is.nan(eval(as.name(col_j_name))) | is.na(eval(as.name(col_j_name))), ])/nrow(dt) > 0.75) next
    for (op in 1:length(list_oper)) {
      list_params = vector()
      list_params[1] = col_g_name
      list_params[2] = col_j_name
      list_transf = c('ordinary', 'ordinary')
      list_const = c(1, 1) #apply constant = 1
      type_oper = list_oper[op]
      fla = ""
      # create formula for calculations
      for (i in 1:length(list_params)) {
        if (i > 1) fla = paste0(fla, ' ', type_oper, ' ') #for all elements after first add operation
        #apply transformation for each column in subset
        if (list_transf[i] == 'power') {
          fla = paste0(fla, list_const[i], '*', list_params[i], '^', round(runif(1, lim, 4-lim), 1))
        } else if (list_transf[i] == 'log') {
          fla = paste0(fla, list_const[i], '*', 'log(', list_params[i], ', ', round(runif(1, lim, 4-lim), 1), ')')
        } else {
          fla = paste0(fla, list_const[i], '*', list_params[i])
        }
      }
      #parse list
      args <- paste(list_params, collapse=", ")
      func = eval(parse(text = paste('f <- function(', args, ') { return(' , fla , ')}', sep='')))
      params = c('func', list_params) # create vector with fla and params
      dt_temp = dt[, list_params, with = FALSE]
      dt_temp[, cor_col := do.call(mapply, as.list(get(params)))]
      res_temp = data.frame(formula = fla, 
                            corr_1 = as.vector(cor(y, dt_temp[,1], use = "pairwise.complete.obs")), 
                            corr_2 = as.vector(cor(y, dt_temp[,2], use = "pairwise.complete.obs")), 
                            corr_pair = cor(y, dt_temp$cor_col, use = "pairwise.complete.obs"))
      res_fin = rbind(res_fin, res_temp)
    }
  }
  cat(paste0("Done for ", Sys.time() - time, " seconds...\n"))
  gc()
}

View(res_fin)
write_csv(res_fin, file.path(data_dir, paste0("//Results//add2arg_corr.csv")))

#------------------------