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
    training_x <<- trainingset[, 1:10]
    training_y <<- trainingset[, class_col]
    test_x <<- testset[, 1:10]
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
