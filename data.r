credit.dat <- read.csv('credit.txt')
pima.dat <- read.csv('pima.txt')
covtype.dat <- read.csv('covtype_shrinked.data')

create_training_test_data = function()
{
    index <- 1:nrow(covtype.dat)
    trainindex <- sample(index, trunc(length(index) * 7 / 10))
    trainingset <<- covtype.dat[trainindex, ]
    testset <<- covtype.dat[-trainindex, ]

    class_col <- 13
    training_x <<- trainingset[, c(1:(class_col - 1))]#, (class_col + 1):ncol(trainingset))]
    training_y <<- trainingset[, class_col]
    test_x <<- testset[, c(1:(class_col - 1))]#, (class_col + 1):ncol(trainingset))]
    test_y <<- testset[, class_col]
}

get_class_labels = function()
{
    for(i in 11:55)
    {
        print(i)
        print(max(trainingset[,i]))
        print(sum(trainingset[,i]))
    }
}
