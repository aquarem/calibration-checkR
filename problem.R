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
# the problem was that I was doing the lm backwards!!!!!!!
# now fitted/predicted numbers make sense
# and my calcs obviously don't anymore cuz I'm going backwards
data |> 
    nest(regdata = !constituent) |> 
    mutate(
        reg = map(regdata, \(x) lm(level ~ response + 0, x, weights = x$weights)),
        fitted = map(reg, \(x) x$fitted.values),
        predicted = map2(reg, regdata, \(x, y) predict(x, y, weights = 1))
        # slope = map_dbl(reg, \(x) coef(x) |> pluck("response"))
    ) |> 
    unnest(c(regdata, fitted, predicted))

data

lm(level ~ response + 0, data, weights) |> 
    predict(
        tibble(response = c(1, 2.5))
    )



# final task is figuring out how to extract the formula
# and display it in a nice way
# I guess since we can control/limit the options of model fit
# we can be safe in just doing it manually
# since broom, et al. does not seem to actually have any similar functionality
# to write the formula in the way people ar efmiliar with though we also 
# have to like invert the regression
# so basically do it twice, once to get the actual predicted values and twice to
# get the coefficients that people expect

regress <- lm(level ~ response + I(response^2), data) 
regress |> broom::tidy()

# this would be the most annoying
regress |> coef()


data
# lets try this actually
data |> 
    mutate(
        
    )
map2(list(data), 
    c(
        "level ~ response + 0", 
        "level ~ response", 
        "level ~ response + I(response^2)"
    ),
    \(x, y) lm(as.formula(y), x) |> broom::tidy()
)
    bind_rows() |> 
    mutate(
        constituent =  c(
            "avgRF", 
            rep("linear", 2),
            rep("quad", 3)
        )
    ) |> 
    # problem is 
    # vec[NA] -> NA, length vec
    # vec[NULL] -> numeric(0)
    mutate(
        estimate = signif(estimate, digits = 2),
        term = case_when(
            term == "response" ~ "x",
            term == "(Intercept)" ~ "b",
            term == "I(response^2)" ~ "x2",
            .default = "???"
        ),
        # I am not sure if this is useful or not
        # but basically we need to selectively filter and then
        # 
        pair = case_when(
            term == "x" ~ paste0(estimate, "x"),
            term == "b" ~ paste0("+ ", estimate),
            term == "x2" ~ paste0(estimate, "x^2")
        ),
        order = case_when(
            term == "x" ~ 2,
            term == "b" ~ 3,
            term == "x2" ~ 1
        )
    ) |> 
    arrange(constituent, order) |> 
    mutate(
        .by = constituent,
        expr = paste0(pair, collapse = " ")
    )

# I guess we have to figure out how to type superscripts???



'







'


