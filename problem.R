data <- tibble(
    constituent = "fluoride",
    level = c(0.1,0.5,1,5,10,25),
    response = c(0.015,0.055,0.09,0.6,1,2.5)
    ) |> 
    mutate(
        weights = 1/(level^2)
    )

# my predicted should match predicted, but it does not.
# this is a problem
data |> 
    nest(regdata = !constituent) |> 
    mutate(
        reg = map(regdata, \(x) lm(level ~ response + 0, x, weights = x$weights)),
        fitted = map(reg, \(x) x$fitted.values),
        predicted = map2(reg, regdata, \(x, y) predict(x, y, weights = 1)),
        slope = map_dbl(reg, \(x) coef(x) |> pluck("response"))
    ) |> 
    unnest(c(regdata, fitted, predicted)) |> 
    mutate(
        my_predicted = response / slope
    )

data

lm(response~level, data) |> 
    broom::tidy()

lm(response ~ level + 0, data, weights) |> 
    predict(
        tibble(level = c(1, 5, 25))
    )

lm(level ~ response + 0, data, weights) |> 
    predict(
        tibble(response = c(1, 2, 2.5))
    )




lm(response ~ level + I(level^2), data) |> 
    broom::tidy() |> 
    select(term, estimate) |> 
    pivot_wider(names_from = term, values_from = estimate)




lm(response ~ level, data, weights) |> predict.lm(data)
