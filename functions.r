# NOTE:
# The pima testcase confusion matrix differs a bit from the answer in the assignment description
# This is due to the way classification is done when there is no strict majority class
# i.e. when the number of 1's equals the number of 0's in a leaf


# Util
assert = function(bool) if (!bool) stop('Assertion error.')


# mode(x: vector): a single value from x
#
# Compute the mode (most frequent value) of given vector
mode = function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# intermediate(x: vector): vector
#
# Compute intermediate values of list
intermediate = function(x) diff(x) / 2 + head(x, -1)


# p(y: vector of bits): real number
#
# Compute p(1|y)
# Exploit the fact that y only contains 1's and 0's
p = function(y) sum(y) / length(y)


# impurity(y: vector of bits): real number
#
# Compute gini index
impurity = function (y) p(y) * (1 - p(y))


# tree(rows: matrix, classlabels: vector): tree object
#
# Constructor for tree objects where the root node has given rows and classlabels as data.
# Because the tree is actually an environment under the hood, this allows us to
# set properties "in place", which is normally not possible in R. This is very convenient
# when building up a tree.
tree = function(rows, classlabels)
{
    this = new.env(parent = globalenv())
    class(this) <- "tree"
    this$rows <- rows
    this$classlabels <- classlabels
    this$majorityclass <- mode(classlabels)
    return(this)
}


# set_values(object: tree object, leftchild: tree object, rightchild: tree object,
#            splitattribute: integer, splitvalue: real number): nothing
#
# Setter function for tree properties.
set_values = function(object, leftchild, rightchild, splitattribute, splitvalue)
{
    if (!is(object, "tree"))
        stop(" 'object' argument must be of class 'tree' .")
    object$leftchild = leftchild
    object$rightchild = rightchild
    object$splitvalue = splitvalue
    object$splitattribute = splitattribute
    return(NULL)
}


# bestsplitvalue(x: vector of real numbers, y: vector of bits): real number
#
# Computes the best split value w.r.t. the numeric attribute x
# Doesn't use segment borders
bestsplitvalue = function (x, y, minleaf)
{
    assert(length(x) == length(y))
    assert(length(x) > 1)

    # Construct all candidate splits values
    attribute_values <- sort(unique(x))
    if (length(attribute_values) < 2)
        return(NULL)
    splitvalues <- intermediate(attribute_values)

    # Make sure only to consider splits which meet the minleaf constraint
    allowed_splits = list()
    for (splitvalue in splitvalues)
        if (length(y[x < splitvalue]) >= minleaf && length(y[x >= splitvalue]) >= minleaf)
            allowed_splits <- c(allowed_splits, splitvalue)

    if (length(allowed_splits) == 0)
        return(NULL)

    # Function for computing impurity reduction
    impurity_reduction <- function(splitvalue)
    {
        y1 <- y[x < splitvalue]
        y2 <- y[x >= splitvalue]
        reduction <- (impurity(y) - (length(y1) / length(y) * impurity(y1) +
                                     length(y2) / length(y) * impurity(y2)))
        # This is not always true due to rounding errors
        # assert(reduction >= 0)
        return(reduction)
    }

    # Assess split values by impurity reduction and pick the best
    # which.min only returns 1 index
    reductions <- lapply(allowed_splits, impurity_reduction)
    best <- which.max(reductions)

    splitvalue <- allowed_splits[[best]]
    reduction <- reductions[[best]]

    return(list(splitvalue = splitvalue, reduction = reduction))
}


# bestsplit(x: matrix of real numbers, y: vector of bits): integer and real number
#
# Computes the best split attribute and value w.r.t. the numeric attributes in x
bestsplit = function (x, y, minleaf)
{
    max_reduction <- 0
    splitvalue <- NULL
    splitattribute <- NULL
    for (attribute in 1:NCOL(x))
    {
        result <- bestsplitvalue(x[,attribute], y, minleaf)
        if (is.null(result)) next

        if (result$reduction > max_reduction)
        {
            max_reduction <- result$reduction
            splitvalue <- result$splitvalue
            splitattribute <- attribute
        }
    }

    if (is.null(splitattribute))
        return(NULL)
    return(list(splitattribute = splitattribute, splitvalue = splitvalue))
}


# tree.grow(x: matrix of real numbers, y: vector of bits,
#           nmin: integer, minleaf: integer): tree object
#
# Grow a classification tree from given training data.
# Argument nmin is the number of observations that a node must contain at least,
# for it to be allowed to be split.
# Argument minleaf is the minimum number of observations required for a leaf node.
tree.grow = function(x, y, nmin, minleaf)
{
    root <- tree(x, y)
    nodelist <- list(root)
    while(length(nodelist) > 0)
    {
        node <- nodelist[[1]]
        nodelist[[1]] <- NULL

        assert(length(node$classlabels) > 0)

        if (impurity(node$classlabels) > 0 && length(node$classlabels) >= nmin)
        {
            # Compute the best split attribute-value pair
            result <- bestsplit(node$rows, node$classlabels, minleaf)
            if (is.null(result)) next
            splitvalue <- result$splitvalue
            splitattribute <- result$splitattribute

            # Construct the tree children resulting from the split
            leftpart <- node$rows[, splitattribute] < splitvalue
            rightpart <- node$rows[, splitattribute] >= splitvalue

            leftchild <- tree(node$rows[leftpart,], node$classlabels[leftpart])
            rightchild <- tree(node$rows[rightpart,], node$classlabels[rightpart])

            assert(length(leftchild$classlabels) > 0)
            assert(length(rightchild$classlabels) > 0)

            set_values(node, leftchild, rightchild, splitattribute, splitvalue)

            # Because trees are lists we must wrap left and right in a list
            # to perform a correct concatenation
            nodelist <- c(nodelist, list(leftchild, rightchild))
        }
    }
    return(root)
}


# tree.print(tr: tree object): nothing
#
# Prints a flat representation of the given tree object to give an indication of what
# the tree looks like.
tree.print = function(tr)
{
    nodelist <- list(tr)
    while(length(nodelist) > 0)
    {
        node <- nodelist[[1]]
        nodelist[[1]] <- NULL

        # If we are at a leaf
        if (is.null(node$leftchild))
            print('Leaf')
        else
        {
            print(paste('Splitattribute:', toString(node$splitattribute)))
            print(paste('Splitvalue:', toString(node$splitvalue)))

            nodelist <- c(nodelist, list(node$leftchild, node$rightchild))
        }
    }
}


# tree.classify(x: matrix of real numbers, tr: tree object): vector of bits
#
# Compute the classlabels of given data using the given classification tree
tree.classify = function(x, tr)
{
    y <- NULL
    for (row in 1:NROW(x))
    {
        node <- tr
        # While the current node still has children
        while(!is.null(node$leftchild))
        {
            assert(!is.null(node$rightchild))
            if (x[row, node$splitattribute] < node$splitvalue)
                node <- node$leftchild
            else
                node <- node$rightchild
        }
        y[row] <- node$majorityclass
    }
    return(y)
}


# create_training_test_data(size: integer): nothing
#
# Generate trainingset and testset of given size and put them into global variables
create_training_test_data = function(size)
{
    # Sample random data from the data set
    data <- covtype.dat[sample(1:nrow(covtype.dat), size), ]

    # Divide in trainingset and testset
    index <- 1:nrow(data)
    trainindex <- sample(index, trunc(length(index) * 7 / 10))
    trainingset <<- data[trainindex, ]
    testset <<- data[-trainindex, ]

    class_col <- 13
    training_x <<- trainingset[, 1:10]
    training_y <<- trainingset[, class_col]
    test_x <<- testset[, 1:10]
    test_y <<- testset[, class_col]
}


# confusion_matrix(nmin: integer, minleaf: integer): nothing
#
# Generate tree from trainingset and evaluate testset.
# Then print the resulting confusion matrix and error rate.
confusion_matrix = function(nmin, minleaf)
{
    tr <- tree.grow(training_x, training_y, nmin, minleaf)
    prediction <- tree.classify(test_x, tr)

    prediction_0 <- prediction[test_y == 0]
    prediction_1 <- prediction[test_y == 1]

    errors <- sum(prediction != test_y)
    rate <- errors / length(prediction)

    print(matrix(c(length(prediction_0) - sum(prediction_0), sum(prediction_0),
                    length(prediction_1) - sum(prediction_1), sum(prediction_1)),
                  nrow = 2, ncol = 2))
    print(rate)
}


# error_rate(nmin: integer, minleaf: integer): real number
#
# Generate tree from trainingset and evaluate testset.
# Then return the resulting error rate.
error_rate = function(nmin, minleaf)
{
    tr <- tree.grow(training_x, training_y, nmin, minleaf)
    prediction <- tree.classify(test_x, tr)
    expected <- test_y
    errors <- sum(prediction != expected)
    rate <- errors / length(prediction)
    return(rate)
}


# generate_nmin_minleaf_plot(): nothing
#
# For many values of nmin and minleaf generate tree from trainingset and evaluate testset.
# Then print these together with the resulting error rate.
# This function was used to generate data for the 3d plots.
generate_nmin_minleaf_plot = function()
{
    result <- ""
    values <- seq(from = 0, to = 100, by = 5)
    #values <- c(seq(from = 0, to = 100, by = 10),
                #seq(from = 200, to = 1000, by = 100))
    for(nmin in values)
        for(minleaf in values)
        {
            result <- cbind(result, paste(toString(nmin), ', ',
                            toString(minleaf), ', ',
                            toString(error_rate(nmin, minleaf))))
            print(paste(nmin, minleaf))
        }

    write(result, file = 'output.txt')
}
