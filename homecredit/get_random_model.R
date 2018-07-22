data_dir = "C:\\Users\\Eovil\\Desktop\\Kaggle_Homecredit competition"
load(file = paste0(data_dir, "//Calculation//input_y.Rdata"))

tr <- read_csv(file.path(data_dir, "application_train.csv"))
te <- read_csv(file.path(data_dir, "application_test.csv"))

get_random_model = function(dt, count_random_vars = 100, predictor = y) {
  col_num = ncol(dt)
  for (cura in 1:count_random_vars) {
    tryCatch({
      ind = sample(1:col_num, 1)
      name = colnames(dt)[ind] #calculate first column
      lim = 1E-6
      list_transformations = c('power', 'log', rep('n', 10))
      #list_operators = c('+', '-', '*', '/')
      list_params = vector()
      list_params[1] = name
      list_transf = vector()
      #list_const = vector()
      #list_oper = vector()
      list_transf[1] = sample(list_transformations, 1) #get transformation to apply to new argument
      #list_const[1] = round(runif(1, lim, 2-lim), 1) #get random number to apply to new argument
      i = 1
      k = 1
      while (k >= runif(1, 0, 1)) { #calculate loop for random transformations
        list_transf[i+1] = sample(list_transformations, 1) #get transformation to apply to new argument
        #list_const[i+1] = round(runif(1, lim, 2-lim), 1) #get random number to apply to new argument
        #list_oper[i] = sample(list_operators, 1) #get operator to apply to new argument
        ind = sample(1:col_num, 1) #get new parameter index
        name = colnames(dt)[ind]
        list_params[i+1] = name
        i = i + 1
        k = k - runif(1, 0, 1)
      }
      list_params
      list_transf
      #list_const
      #list_oper
      fla = "predictor ~ 0 + "
      i = 1
      # create formula for calculations
      for (i in 1:length(list_params)) {
        list_params[i]
        if (i > 1) fla = paste0(fla, ' + ') #for all elements after first add operation
        #apply transformation for each column in subset
        if (list_transf[i] == 'power') {
          fla = paste0(fla, 'I(', list_params[i], '^', round(runif(1, lim, 4-lim), 1), ')')
        } else if (list_transf[i] == 'log') {
          fla = paste0(fla, 'log(', list_params[i], ', ', round(runif(1, lim, 4-lim), 1), ')')
        } else {
          fla = paste0(fla, list_params[i])
        }
      }
      list_params = unique(list_params)
      # apply model
      dt_mod = as.data.table(cbind(predictor, dt[1:length(predictor), list_params, with = FALSE]))
      mod = glm(data=dt_mod, formula=as.formula(fla)) #to do: add random model here
      dt[, paste0('newcol','_', sub('predictor ~ 0 \\+ ', '', fla)) := predict.glm(mod, dt[, list_params, with = FALSE], na.action = na.pass)]
      # to do: add coefficient description here
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  return(dt[,(col_num+1):ncol(dt)])
}
