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
# it is that both fitted.values and predict() use the weights twice
# it is not logical to apply "training" data weights to "test" data
# i.e., not make sense to use weights to fit then use them also to predict
# however, R does this anyways
# so our manual linreg calcs came out correct (and rf rsd), but anything
# else would come out wrong
# so, we have to write a function that takes those coefficients
# and applies them in the correct way without the weights

# I guess then we need to write a like prediction function
# that applies the correct formula and does not use weights


make_cal <- function(calData, calTypes) {
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
            # maybe this is backwards?????
            # was response ~ level
            # which I thought meant response predicts level
            # so when we use response we calculate level
            # but this seems to be backwards or something
            form = case_when(
                calibration == "avgRF" | 
                    (calibration == "linear" & !yIntercept) ~ "level ~ response + 0",
                calibration == "linear" ~ "level ~ response",
                calibration == "quadratic" ~ "level ~ response + I(response^2)"
            ),
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
        nest(regdata = !c(constituent, form)) |> 
        mutate(
            reg = map2(regdata, form, \(x, y) lm(as.formula(y), x, weights = x$weight)),
            r2 = map_dbl(reg, \(x) summary(x) |> pluck("adj.r.squared")),
            calculated = map2(regdata, reg, \(x, y) {
                predict(y, x)
            })
        ) |>
        unnest(c(regdata, calculated)) |>
        mutate(
            accuracy = calculated / level * 100,
            se = ( ( ( calculated - level ) / level )^2 ) / (n - 1),
            rf = response / level
        ) |>
        # still not perfectly identical even though they should be - why????????
        mutate(
            .by = constituent,
            rse = (sum(se, na.rm = TRUE) |> sqrt()) * 100,
            rf_rsd = ( sd(rf) / mean(rf, na.rm = TRUE) ) * 100
        )
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
        unnest(c(regdata, calculated)) |> 
        print()
    
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
                    # slope = unique(slope) |> signif(3),
                    # intercept = unique(intercept) |> signif(3)
                ),
            aes(
                x = left,
                y = top,
                label = paste(
                    # paste0("y = ", slope, "x + ", intercept),
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
