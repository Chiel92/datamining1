impurity <- function (y) 
{
    p1 <- sum(y) / length(y)
    result <- p1*(1-p1)
    return(result)
}


bestsplit <- function (x, y) 
{
    values <- sort(unique(x))
    candidates <- diff(values) / 2 + head(values, -1)

    assess_split <- function(c)
    {
        child1 <- y[x<c]
        child2 <- y[x>=c]
        return(impurity(child1) + impurity(child2))
    }

    assessments <- mapply(assess_split, candidates)
    return(candidates[which.min(assessments)])
}

