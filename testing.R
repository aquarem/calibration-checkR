library(tidyverse)
library(here)

# pretending person selected correct tabs

level <- here('sample.xlsx') |> 
    readxl::read_excel(sheet = "standards")

response <- here('sample.xlsx') |> 
    readxl::read_excel('areas')

level

response

# testing behavior of missing bromide lowest level
cal <- level |> 
    pivot_longer(!1) |> 
    mutate(type = "level") |> 
    bind_rows(
        response |> 
            pivot_longer(!1) |> 
            mutate(type = "response")
    ) |> 
    { \(x) rename(
        x, 
        standard = names(x)[1],
        constituent = name
    ) }() |> 
    pivot_wider(
        names_from = type,
        values_from = value
    )



# idk how best to do so since broom kind of give us nonsense
# broom::tidy
# but weights is super easy!
# so that's good

# do response ~ level + 0 for no y int, but we'll need
# to write a separate function since we can't like unwrite that
# at least I think??? Maybe lm takes char arguments instead?
# but since it's in tidy lingo then no
# so maybe just do it separately
# and then we'll need to fix the intercept to be 0 in that case
# since it doesn't return 0 of course

reg <- cal |> 
    mutate(
        weights = 1 / (level^2)
    ) |> 
    mutate(
        .by = constituent,
        # this will have to be a function
        # reg = list(lm(response ~ level, weights = weights)),
        reg = list(lm(response ~ level + 0, weights = weights)),
        intercept = map_dbl(reg, \(x) x |> 
            coef() |> 
            pluck("(Intercept)") |> 
            coalesce(0)
        ),
        slope = map_dbl(reg, \(x) coef(x) |> pluck("level")),
        r2 = map_dbl(reg, \(x) summary(x) |> pluck("adj.r.squared")),
        n = length(response |> discard(is.na))
    )




cal |> 
    mutate(calibration = "avgRF", yIntercept = FALSE, weight = "none") |> 
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
                (calibration == "linear" & !yIntercept) ~ "response ~ level + 0",
            calibration == "linear" ~ "response ~ level",
            calibration == "quadratic" ~ "response ~ level + I(level^2)"
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
        calculated = map2(regdata, reg, \(x, y) predict(y, x))
    ) |> 
    unnest(c(regdata, calculated)) |> 
    mutate(
        accuracy = calculated / level * 100
    ) |> 
    select(calculated, level, accuracy)





# maybe we should do the accuracy and RSE calculations now
# rather than do them separately
# for rse, n - 2 would be n - 1 ONLY if y-int is forced through 0
# if quadratic then it'd be n = 3 I think, but idc about that rn

cal_reg <- cal |> 
    left_join(reg) |> 
    mutate(
        .by = constituent,
        calculated = (response - intercept) / slope,
        accuracy = calculated / level * 100,
        rse = (
            ( (calculated - level) / level )^2  / (n - 1) # 2 if y-int enable
        ) |> 
            sum(na.rm = TRUE) |> 
            sqrt() * 100,
        
        rf_rsd = { \(res, lev) {
            rfs <- res / lev
            sd(rfs, na.rm = TRUE) / mean(rfs, na.rm = TRUE) * 100
        } }(response, level)
    )

cal |> 
    left_join(reg) |> 
    mutate(
        .by = constituent,
        calculated = (response - intercept) / slope,
        accuracy = calculated / level * 100,
        rse = (
            ( (calculated - level) / level )^2  / (n - 1) # 2 if y-int enable
        ) |> 
            sum(na.rm = TRUE) |> 
            sqrt() * 100,
        
        rf_rsd = { \(res, lev) {
            rfs <- res / lev
            sd(rfs, na.rm = TRUE) / mean(rfs, na.rm = TRUE) * 100
        } }(response, level)
    ) |> 
    select(constituent,level, calculated, accuracy, rse, rf_rsd) |> 
    arrange(constituent) |> 
    unique()
# so now the question is how would we display other stuff?
# and/or let things be disabled?
# I guess we could put a bonus thing on there
# and then maybe just do a table if we can't figure out how to get
# something like R^2
# in this example, the RSE and RF RSD
# MUST match if y-int is forced to 0 and weighting is 1/x^2
# and it is! this is good

# however, plotly doesn't use geom_label yet
# dumb
# does geom_text work instead?

cal_plots <- cal_reg |> 
    ggplot(aes(level, response)) +
    geom_point() +
    geom_abline(aes(slope = slope, intercept = intercept)) +
    geom_text(
        data = cal_reg |> 
            summarize(
                .by = constituent,
                left = min(level, na.rm = TRUE),
                top = max(response, na.rm = TRUE),
                r2 = unique(r2),
                rse = unique(rse),
                rf_rsd = unique(rf_rsd),
                n = unique(n),
                num = length(level),
                slope = unique(slope) |> signif(3),
                intercept = unique(intercept) |> signif(3)
            ),
        aes(
            x = left,
            y = top,
            label = paste(
                paste0("y = ", slope, "x + ", intercept),
                paste0(n, " of ", num, " points"),
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

cal_plots








