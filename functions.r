# TODO:
# - implement segment borders
# - rewrite loops to apply

# NOTE:
# The pima testcase confusion matrix differs a bit from the answer in the assignment
# This is due to the way classification is done when there is no strict majority class


# Compute the mode of given vector
mode = function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Compute intermediate values of list
intermediate = function(l) diff(l) / 2 + head(l, -1)

# Util
assert = function(bool) if (!bool) stop('Assertion error.')

# p(1|y)
# Exploit the fact that y only contains 1's and 0's
p = function(y) sum(y) / length(y)

# Gini index
# y is a vector of bits indicating the class label
impurity = function (y) p(y) * (1 - p(y))

# Constructor for tree
tree = function(rows, classlabels)
{
    this = new.env(parent = globalenv())
    class(this) <- "tree"
    this$rows <- rows
    this$classlabels <- classlabels
    this$majorityclass <- mode(classlabels)
    return(this)
}

# Setter for tree
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

# Bestsplitvalue
# x is a vector of numeric attribute
# y is a vector of bits indicating the class label
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

# Bestsplit
# x is a matrix of numeric attributes
# y is a vector of bits indicating the class label
# Computes the best split w.r.t. the numeric attributes x
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


confusion_matrix = function(x, y, nmin, minleaf)
{
    tr <- tree.grow(x, y, nmin, minleaf)
    prediction <- tree.classify(x, tr)

    #tree.print(tr)

    prediction_0 <- prediction[y == 0]
    prediction_1 <- prediction[y == 1]

    return(matrix(c(length(prediction_0) - sum(prediction_0), sum(prediction_0),
                    length(prediction_1) - sum(prediction_1), sum(prediction_1)),
                  nrow = 2, ncol = 2))
}
