# Compute intermediate values of list
intermediate <- function(l) diff(l) / 2 + head(l, -1)

# pi
# Exploit the fact that y only contains 1's and 0's
p <- function(y) sum(y) / length(y)

# Util
assert <- function(bool) if (!bool) stop('Assertion error.')

# Gini index
# y is a vector of bits indicating the class label
# Returns 0 if y is empty
impurity <- function (y) p(y) * (1 - p(y))
#{
    #assert(length(y) > 0)
    #p_1 <- p(y)
    #assert(p_1 <= 1)
    #return(p_1 * (1 - p_1))
#}

# Bestsplit
# x is a vector of numeric attribute
# y is a vector of bits indicating the class label
# Computes the best split w.r.t. the numeric attribute x
# Doesn't use segment borders
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

    return(list(splitvalue = splitvalue, reduction = reduction))
}

# Constructor for tree
tree = function(rows, classlabels)
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
            max_reduction <- 0
            splitvalue = NULL
            splitattribute <- NULL
            for (attribute in dim(x)[2])
            {
                result <- bestsplit(node$rows[, attribute], node$classlabels, minleaf)
                if (is.null(result)) next

                if (result$reduction > max_reduction)
                {
                    max_reduction <- result$reduction
                    splitvalue <- result$splitvalue
                    splitattribute <- attribute
                }
            }

            if (is.null(splitvalue)) next

            #print(splitvalue)
            #print(splitattribute)
            #print(node$rows[splitattribute])

            left <- node$rows[, splitattribute] < splitvalue
            right <- node$rows[, splitattribute] >= splitvalue

            leftchildrows <- node$rows[left,]
            rightchildrows <- node$rows[right,]
            leftchildclasslabels <- node$classlabels[left]
            rightchildclasslabels <- node$classlabels[right]

            leftchild <- tree(leftchildrows, leftchildclasslabels)
            rightchild <- tree(rightchildrows, rightchildclasslabels)

            assert(length(leftchild$classlabels) > 0)
            assert(length(rightchild$classlabels) > 0)

            set_values(node, leftchild, rightchild, splitvalue, splitattribute)

            # Because trees are lists we must wrap left and right in a list
            # to perform a correct concatenation
            nodelist <- c(nodelist, list(leftchild, rightchild))
        }
    }
    return(root)
}

tree.classify <- function(x, tr)
{

}
