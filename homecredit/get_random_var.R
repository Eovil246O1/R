data_dir = "C:\\Users\\Eovil\\Desktop\\Kaggle_Homecredit competition"

get_random_feature = function(dt, count_random_vars = 100) {
  col_num = ncol(dt)
  for (cura in 1:count_random_vars) {
    ind = sample(1:col_num, 1)
    name = colnames(dt)[ind] #calculate first column
    lim = 1E-6
    list_transformations = c('power', 'log', rep('n', 10))
    list_operators = c('+', '-', '*', '/')
    list_params = vector()
    list_params[1] = name
    list_transf = vector()
    list_const = vector()
    list_oper = vector()
    list_transf[1] = sample(list_transformations, 1) #get transformation to apply to new argument
    list_const[1] = round(runif(1, lim, 2-lim), 1) #get random number to apply to new argument
    i = 1
    k = 1
    while (k >= runif(1, 0, 1)) { #calculate loop for random transformations
      list_transf[i+1] = sample(list_transformations, 1) #get transformation to apply to new argument
      list_const[i+1] = round(runif(1, lim, 2-lim), 1) #get random number to apply to new argument
      list_oper[i] = sample(list_operators, 1) #get operator to apply to new argument
      ind = sample(1:col_num, 1) #get new parameter index
      name = colnames(dt)[ind]
      list_params[i+1] = name
      i = i + 1
      k = k - runif(1, 0, 1)
    }
    list_params
    list_transf
    list_const
    list_oper
  
    fla = ""
    i = 1
    # create formula for calculations
    for (i in 1:length(list_params)) {
      list_params[i]
      if (i > 1) fla = paste0(fla, ' ', list_oper[i-1], ' ') #for all elements after first add operation
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
    list_params = unique(list_params)
    args <- paste(list_params, collapse=", ")
    func = eval(parse(text = paste('f <- function(', args, ') { return(' , fla , ')}', sep='')))
    params = c('func', list_params) # create vector with fla and params
    dt[, paste0('newcol','_', fla) := do.call(mapply, as.list(get(params)))]
    #dt[, gsub(pattern = "[[:space:]]", "", paste0('newcol','_',gsub(pattern = "[[:punct:]]", "_", fla))) := do.call(mapply, as.list(get(params)))]
  }
  return(dt[,(col_num+1):ncol(dt)])
}

warnings()
dt$`newcol_0.5*AMT_CREDIT_SUM_OVERDUE * 0.3*SK_ID_BUREAU * 1.6*SK_ID_CURR`

func


list_operators

as.function(alist(a + b))


power_applied = function (a) a ^ round(runif(1,0,2), 1)
log_applied = function (a) log(a, base = round(runif(1,0,2), 1))

dt = data.table(x1 = c(1,2,3,4), x2 = c(3,4,5,2), x3 = c(3,4,5,2))
body <- "(x1 + x2) * x3"
args <- "x1, x2, x3"
fla = eval(parse(text = paste('f <- function(', args, ') { return(' , body , ')}', sep='')))
res_dt = dt[, k := mapply(fla, x1, x2, x3)]

res_dt

p = c('fla', 'x1','x2','x3')
res_dt = dt[, k_fin := do.call(mapply, as.list(get(p)))]

dt = data.table(x1 = c(1,2,3,4), x2 = c(3,4,5,2), x3 = c(3,4,5,2))
add_field = function(dt){
  dt[, test_1 := x1 * x2]
  return(dt)
}
some_func = function(dt){
  dt = add_field(dt)
  dt[, test_2 := x1 + x2]
  #return(dt)
}
dt_test = some_func(dt)
dt_test
