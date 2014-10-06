# Compute intermediate values of list
intermediate <- function(l) (diff(l) / 2 + head(l, -1))

# pi
# Exploit the fact that y only contains 1's and 0's
p <- function(y) (sum(y) / length(y))

# Util
assert <- function(bool) if (!bool) stop('Assertion error.')

# Gini index
# y is a vector of bits indicating the class label
# Returns 0 if y is empty
impurity <- function (y)
{
    #if (length(y) == 0) return(0)
    assert(length(y) > 0)

    # Exploit the fact that y only contains 0's and 1's
    p_1 <- p(y)
    assert(p_1 <= 1)
    return(p_1 * (1 - p_1))
}

# Bestsplit
# x is a vector of numeric attribute
# y is a vector of bits indicating the class label
# Computes the best split w.r.t. the numeric attribute x
# Doesn't use segment borders
# TODO: consider changing split with splitvalues; removes the need for compute split
bestsplit <- function (x, y, minleaf)
{
    assert(length(x) == length(y))
    assert(length(x) > 1)

    # Construct all candidate splits values
    attribute_values <- sort(unique(x))
    if (length(attribute_values) <= 1)
        return(NULL)
    splitvalues <- intermediate(attribute_values)

    allowed_splits = list()
    for (splitvalue in splitvalues)
        if (length(y[x < splitvalue]) >= minleaf & length(y[x >= splitvalue]) >= minleaf)
            allowed_splits <- c(allowed_splits, splitvalue)

    if (length(allowed_splits) == 0)
        return(NULL)

    # Assess the potential impurity of given split
    impurity_reduction <- function(splitvalue)
    {
        y1 <- y[x < splitvalue]
        y2 <- y[x >= splitvalue]
        return(impurity(y) - (p(y1) * impurity(y1) + p(y2) * impurity(y2)))
    }

    # Assess the split values and pick the purest
    # which.min only returns 1 index
    reductions <- lapply(allowed_splits, impurity_reduction)
    purest <- which.max(reductions)

    splitvalue <- allowed_splits[[purest]]
    reduction <- reductions[[purest]]

    return(splitvalue)
    return(list(splitvalue = splitvalue, reduction = reduction))
}

newPointer = function(inputValue)
{
    object = new.env(parent = globalenv())  
    object$value = inputValue  
    class(object) = 'pointer'

    return(object)  
}

updatePointerValue = function (object, ...) { # create S3 generic
    UseMethod("updatePointerValue")
}
updatePointerValue.pointer = function(object, newValue){ # create S3 method
    if (!is(object, "pointer"))
        stop(" 'object' argument must be of class 'pointer' .")
    object$value = newValue
    return(NULL)
}

# Constructor for tree
tree2 <- function(rows, classlabels)
{
    this = new.env(parent = globalenv())  
    class(this) <- "tree"
    this$rows <- rows
    this$classlabels <- classlabels
    this$splitvalue <- NULL
    this$leftchild <- NULL
    this$rightchild <- NULL
    return(this)
}
set_values = function (object, ...) { # create S3 generic
    UseMethod("set_children")
}
set_values = function(object, leftchild, rightchild, splitvalue){ # create S3 method
    if (!is(object, "tree"))
        stop(" 'object' argument must be of class 'pointer' .")
    object$leftchild = leftchild
    object$rightchild = rightchild
    object$splitvalue = splitvalue
    return(NULL)
}

tree.grow <- function(x, y, nmin, minleaf)
{
    root <- tree2(x, y)
    nodelist <- list(root)
    while(length(nodelist) > 0)
    {
        node <- nodelist[[1]]
        nodelist[[1]] <- NULL

        assert(length(node$classlabels) > 0)
        if (impurity(node$classlabels) > 0 & length(node$classlabels > nmin))
        {
            assert(length(node$classlabels) > 1)
            #for (attribute in dim(x)[1]) {
            #}
            splitvalue <- bestsplit(node$rows[, 1], node$classlabels, minleaf)
            if (is.null(splitvalue))
                next

            print(1)

            left <- node$rows[, 1] < splitvalue
            right <- node$rows[, 1] >= splitvalue
            leftchildrows <- node$rows[left,]
            rightchildrows <- node$rows[right,]
            leftchildclasslabels <- node$classlabels[left]
            rightchildclasslabels <- node$classlabels[right]

            leftchild <- tree2(leftchildrows, leftchildclasslabels)
            rightchild <- tree2(rightchildrows, rightchildclasslabels)

            assert(length(leftchild$classlabels) > 0)
            assert(length(rightchild$classlabels) > 0)

            set_values(node, leftchild, rightchild, splitvalue)

            # Because trees are lists we must wrap left and right in a list
            # to perform a correct concatenation
            nodelist <- c(nodelist, list(leftchild, rightchild))
        }
    }
    return(root)
}
