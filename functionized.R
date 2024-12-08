# functionizing the calibration nonsense

# last thing will be adding a function to add/remove calibration points
# but idk if I feel like doing that rn


# we will do everything together for now, might be easier!
# I think it will be best to just process all of the arguments like
# first and then do the regression
# avgRF is the hard part since it basically requires no regression
# so maybe we'll just put a big case there and have it be
# just avg. RF
# I forget where we calculate RF RSD but we're going to get that
# and then manually set the weight to 1/x^2 for the avgRF regression

# the problem here was not my RSE calculation

# that actual problem is that the models can't go backwards
# in calibration we use known x and y and pretend the response is the predicted variable,
# but then in real life we use a known y to predict the x
# so for R purposes response should be the x axis, not conncentration
# so we had to swap the formula around and swap the plotting around
# since calculated was actually the response, not the conc.!

# after fixes, chloride RSE == RF RSD, but fluoride and bromide are still slightly different
# with RF RSD being higher for some reason. Who knows why
# still serves the purpose of showing why RF RSD makes no sense for quadratic



make_cal <- function(calData, calTypes) {
    

    # using a swap function since our formula needs to be backwards for calcs
    # but for display we want to show it response ~ level
    str_swap <- function(str, swap1, swap2, int) {
        str |> 
            str_replace_all(swap1, int) |>  # lul using string as replacement
            str_replace_all(swap2, swap1) |>  # 2 goes to 1
            str_replace_all(int, swap2) # intermediate goes to 2
    }
    
    # str_swap("level ~ response + I(response^2)", "response", "level", "rand")
    # # does it work with vectors? yes!!
    # str_swap(
    #     c(
    #         "level ~ response + 0", 
    #         "level ~ response", 
    #         "level ~ response + I(response^2)"
    #     ),
    #     "level", "response", "rand"
    # )
    
    cal_reg <- calData |> 
        left_join(calTypes) |> 
        mutate(
            .by = constituent,
            weight = case_when(
                # this needs to be first
                calibration == "avgRF" |
                    weight == "1/x^2" ~ 1 / (level ^2),
                weight == "none" ~ level,
                weight == "1/x" ~ 1 / level
            ),
            form = case_when(
                calibration == "avgRF" | 
                    (calibration == "linear" & !yIntercept) ~ "level ~ response + 0",
                calibration == "linear" ~ "level ~ response",
                calibration == "quadratic" ~ "level ~ response + I(response^2)"
            ),
            # tricky swap so we can extract the formula
            printform = str_swap(form, "level", "response", "inter"),
            # total, including NA points
            num = length(response),
            n = sum(!is.na(response)),
            df = case_when(
                calibration == "avgRF" | 
                    (calibration == "linear" & !yIntercept) ~ 1,
                calibration == "linear" ~ 2,
                calibration == "quadratic" ~ 3 # I think???
            )
        ) |> 
        drop_na(response) |> 
        nest(regdata = !c(constituent, form, printform)) |> 
        mutate(
            reg = map2(regdata, form, \(x, y) lm(as.formula(y), x, weights = x$weight)),
            printreg = map2(regdata, printform, \(x, y) lm(as.formula(y), x, weights = x$weight)),
            r2 = map_dbl(reg, \(x) summary(x) |> pluck("adj.r.squared")),
            calculated = map2(regdata, reg, \(x, y) {
                predict(y, x)
            }),
            expression = map(printreg, \(x) {
                x |> 
                    broom::tidy() |> 
                    mutate(
                        estimate = signif(estimate, digits = 2),
                        term = case_when(
                            term == "level" ~ "x",
                            term == "(Intercept)" ~ "b",
                            term == "I(level^2)" ~ "x2",
                            .default = "???"
                        ),
                        pair = case_when(
                            term == "x" ~ paste0(estimate, "x"),
                            term == "b" ~ paste0("+ ", estimate),
                            term == "x2" ~ paste0(estimate, "\u00B2 +")
                        ),
                        order = case_when(
                            term == "x2" ~ 1,
                            term == "x" ~ 2,
                            term == "b" ~ 3
                        )
                    ) |> 
                    arrange(order) |> 
                    summarize(
                        expr = paste0(pair, collapse = " ")
                    )
            })
        ) |> 
        unnest(c(regdata, calculated, expression)) |> # what shape will this be???
        mutate(
            accuracy = calculated / level * 100,
            se = ( ( ( calculated - level ) / level )^2 ) / (n - df),
            rf = response / level
        ) |>
        # still not perfectly identical even though they should be - why????????
        mutate(
            .by = constituent,
            rse = (sum(se, na.rm = TRUE) |> sqrt()) * 100,
            rf_rsd = ( sd(rf) / mean(rf, na.rm = TRUE) ) * 100,
        )
    
    
    # now it would be good to get the formula sorted somewhere in here
    # would be repeated a lot
}


# I think we need to plot the calculated curve with higher resolution
# than just the 5 points or whatever
plot_cal <- function(cal_reg) {
    cal_curve <- cal_reg |> 
        reframe(
            .by = constituent,
            response = seq(0, max(response), 0.1),
            reg = unique(reg)
        ) |> 
        nest(regdata = !c(constituent, reg)) |> 
        mutate(
            .by = constituent,
            calculated = map2(regdata, reg, \(x, y) {
                predict(y, x)
            })        
        ) |> 
        unnest(c(regdata, calculated))
    
    cal_reg |> 
        ggplot(aes(level, response)) +
        geom_point() +
        geom_line(aes(calculated, response), cal_curve) +
        geom_text(
            data = cal_reg |> 
                summarize(
                    .by = constituent,
                    left = min(level, na.rm = TRUE),
                    top = max(response, na.rm = TRUE),
                    r2 = unique(r2),
                    rse = unique(rse),
                    rf_rsd = unique(rf_rsd),
                    n = length(level),
                    num = unique(num),
                    expr = unique(expr)
                ),
            aes(
                x = left,
                y = top,
                label = paste(
                    expr,
                    paste0(n, " of ", num, " levels"),
                    paste0("R2: ", round(r2, 4)),
                    paste0("RSE: ", round(rse, 1)),
                    paste0("RF RSD: ", round(rf_rsd, 1)),
                    sep = "\n"
                )
            ),
            hjust = 0,
            vjust = 1,
            size = 3
        ) +
        facet_wrap(vars(constituent), scales = "free") +
        theme_bw()
}
