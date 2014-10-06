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

# Constructor for tree
tree <- function(rows=NULL, classlabels=NULL, splitvalue=NULL, leftchild=NULL, rightchild=NULL)
{
    this <- list()
    class(this) <- "tree"
    this$rows <- rows
    this$classlabels <- classlabels
    this$splitvalue <- splitvalue
    this$leftchild <- leftchild
    this$rightchild <- rightchild
    return(this)
}

tree.grow <- function(x, y, nmin, minleaf)
{
    root <- tree(x, y)
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

            if (!is.null(splitvalue))
            {
                leftchild <- tree()
                rightchild <- tree()

                left <- node$rows[, 1] < splitvalue
                right <- node$rows[, 1] >= splitvalue

                leftchild$rows <- node$rows[left,]
                rightchild$rows <- node$rows[right,]

                leftchild$classlabels <- node$classlabels[left]
                rightchild$classlabels <- node$classlabels[right]

                assert(length(leftchild$classlabels) > 0)
                assert(length(rightchild$classlabels) > 0)

                # Because trees are lists we must wrap left and right in a list
                # to perform a correct concatenation
                nodelist <- c(nodelist, list(leftchild, rightchild))
            }
        }
    }
    return(root)
}
