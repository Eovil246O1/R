packages = c('corrgram', 'manipulate', 'iotools', 'modeest')
install.packages(packages)

library(scoredevr)
library(data.table)
warnings()

scoredevr:::binColumn()

#load some test data
test_data = data.table(iris)
#mtcars

#test functions scoredevr
a = selectVars(test_data, c('Species'), c('Species', 'Sepal.Width', 'Petal.Length'))
a

? selectVars

#test My functions
test_data = data.table(iris)

test_data
test_data[Sepal.Width <=3 , Sepal.Width := Inf]
test_data
a = create_nan_vars(test_data)
a

install.packages('roxygen2')
library(roxygen2)
? create_nan_vars

str(a)

#TODO
#No documentation
