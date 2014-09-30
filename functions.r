# Compute values in between values of ordered list
intermediate <- function(l) (diff(l) / 2 + head(l, -1))

# Gini index
impurity <- function (y)
{
    # Exploit the fact that y only contains 0's and 1's
    p_1 <- sum(y) / length(y)
    return(p_1 * (1 - p_1))
}

# Doesn't use segment borders
bestsplit <- function (x, y)
{
    # Assess the potential impurity of given split value
    assess_split <- function(c)
    {
        child1 <- y[x<c]
        child2 <- y[x>=c]
        return(impurity(child1) + impurity(child2))
    }

    # Construct all candidate split values
    values <- sort(unique(x))
    candidates <- intermediate(values)

    # Assess the split values and pick the purest
    assessments <- mapply(assess_split, candidates)
    purest <- which.min(assessments)

    return(candidates[purest])
}

