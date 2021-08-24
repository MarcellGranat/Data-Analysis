A termékenységi ráta kapcsolata az egy főre eső bruttó kibocsátással és
a munkanélküliséggel
================
Granát Marcell
2020-10-01

# A magyar születésszám az elmúlt 60 évben

``` r
LiveBirthAndFertility %>%
  mutate_at(-1, function(x) x / x[1]) %>%
  pivot_longer(-1) %>%
  mutate(
    name = factor(case_when(
      name == "LiveBirthTotal" ~ "Születésszám",
      name == "LiveBirthTo1000" ~ "Ezer főre eső születésszám",
      T ~ "Teljes termékenységi arányszám"
    ), levels = c("Teljes termékenységi arányszám", "Ezer főre eső születésszám", "Születésszám"))
  ) %>%
  ggplot() +
  geom_hline(aes(
    yintercept = 2.1 / LiveBirthAndFertility$TotalFertility[1],
    linetype = "Reprodukciót biztosító TTA érték"
  ), color = "black", size = 1) +
  geom_line(aes(x = Year, y = value, color = name), size = 2) +
  scale_color_grey() +
  scale_linetype_manual(values = c("Reprodukciót biztosító TTA érték" = "dotted")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Év", y = "Százalék (1960 = 100%)", color = NULL, linetype = NULL, title = "Születési mutatók bázisindexe (1960=100%)") +
  theme(legend.box = "vertical")
```

![](ujdemografiaiprogram_files/figure-gfm/unnamed-chunk-2-1.svg)<!-- -->

# Kointegrációs teszt az OECD által közölt termékenységi arányszámokon

``` r
oecd_fertility %>%
  filter(location %in% c("HUN", "POL")) %>%
  mutate(
    tfr_d = c(NA, diff(tfr)),
    tfr_d = ifelse(time == 1960, NA, tfr_d)
  ) %>%
  pivot_longer(-c(1, 2)) %>%
  rbind(
    data.frame(location = "HUN", time = 1960:2018, name = "res", value = oecd_fertility %>%
      filter(location %in% c("HUN", "POL")) %>%
      pivot_wider(names_from = location, values_from = tfr) %>%
      lm(formula = HUN ~ POL) %>% .$residuals)
  ) %>%
  mutate(name = factor(name, levels = c("tfr", "tfr_d", "res"))) %>%
  ggplot(aes(x = time, y = value, color = location)) +
  geom_line(size = 1.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_color_grey() +
  facet_wrap(~name,
    ncol = 1, scales = "free",
    labeller = as_labeller(c(
      "tfr" = "Transzformálatlan idősorok",
      "tfr_d" = "Differenciázott idősorok",
      "res" = "OLS maradéktagja"
    ))
  ) +
  labs(x = "Év", y = NULL, color = NULL, title = "A magyar és lengyel TTA idősorok között fennálló kointegráció")
```

![](ujdemografiaiprogram_files/figure-gfm/unnamed-chunk-3-1.svg)<!-- -->

``` r
nodes <- CountryData %>%
  filter(code3 %in% names(NeighbourCountry)) %>%
  dplyr::select(code3, longitude, latitude) %>%
  set_names(c("name", "lon", "lat"))

ggplot(nodes) +
  geom_polygon(aes(x = long, y = lat, group = group),
    data = map_data("world"),
    fill = "#CECECE", color = "black",
    size = 0.15
  ) +
  geom_curve(aes(
    x = x, y = y, xend = xend, yend = yend,
    color = "Szomszédosnak tekintettek"
  ),
  size = 1.2,
  data = NeighbourCountry %>% mutate(x = names(NeighbourCountry)) %>%
    pivot_longer(-x, names_to = "y") %>%
    na.exclude() %>% dplyr::select(-value) %>%
    merge(nodes, by.x = "x", by.y = "name") %>%
    set_names(c("name", "key", "x", "y")) %>%
    merge(nodes, by.x = "key", by.y = "name") %>%
    set_names(c("name", "key", "x", "y", "xend", "yend")) %>%
    filter(name < key), curvature = 0.33,
  alpha = 0.7
  ) +
  geom_text(aes(x = lon, y = lat, label = name),
    hjust = 0.7, nudge_x = 0, nudge_y = 0,
    size = 2, color = "black", fontface = "bold"
  ) +
  labs(color = "", title = "Számítások során egymás szomszédjának tekintett országok hálója") +
  coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80)) +
  scale_color_manual(values = "grey20") +
  theme_void() +
  theme(legend.position = "bottom")
```

![](ujdemografiaiprogram_files/figure-gfm/unnamed-chunk-4-1.svg)<!-- -->

``` r
df <- NeighbourCountry %>%
  mutate(x = names(NeighbourCountry)) %>%
  pivot_longer(-x, names_to = "y", values_to = "neighbour") %>%
  mutate(
    neighbour = ifelse(is.na(neighbour), "Nem határosak", "Határosak"),
    neighbour = ifelse(x == y, NA, neighbour)
  ) %>%
  merge(
    oecd_fertility %>% group_by(location) %>%
      summarise(d = ndiffs(tfr, test = "kpss", type = "level", alpha = .05)) %>%
      set_names("y", "dy")
  ) %>%
  merge(
    oecd_fertility %>% group_by(location) %>%
      summarise(d = ndiffs(tfr, test = "kpss", type = "level", alpha = .05)) %>%
      set_names("x", "dx")
  )

v <- vector() # collector vector
for (i in 1:nrow(df)) {
  if (df$dx[i] == df$dy[i] & df$x[i] != df$y[i]) {
    res <- oecd_fertility %>%
      pivot_wider(names_from = "location", values_from = "tfr") %>%
      dplyr::select(df$x[i], df$y[i]) %>%
      set_names(c("x", "y")) %>%
      na.exclude() %>%
      lm(formula = y ~ x) %>%
      .$residuals

    if (ndiffs(res, test = "kpss", type = "level", alpha = .05) < df$dx[i]) {
      v[i] <- "Kointegráltak"
    } else {
      v[i] <- "Nem kointegráltak"
    }
  } else {
    v[i] <- "A teszt nem elvégezhető"
  }
}

df$coint <- v

df <- merge(df, oecd_fertility %>%
  pivot_wider(names_from = "location", values_from = "tfr") %>%
  dplyr::select(-1) %>% cor(use = "pairwise.complete.obs") %>%
  data.frame() %>% rownames_to_column(var = "x") %>%
  pivot_longer(-1, names_to = "y", values_to = "cor"))
```

``` r
ggplot(data = df) +
  geom_tile(aes(x = x, y = y, fill = coint), color = "black") +
  labs(x = "", y = "", fill = "", title = "Kointegrációs tesztek eredményei") +
  scale_fill_manual(values = c("white", "black", "grey")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.45))
```

![](ujdemografiaiprogram_files/figure-gfm/unnamed-chunk-6-1.svg)<!-- -->

``` r
df %>%
  group_by(neighbour, coint) %>%
  summarise(n = n()) %>%
  filter(!is.na(neighbour)) %>%
  pivot_wider(names_from = neighbour, values_from = n) %>%
  mutate_at(-1, function(x) scales::percent(x / sum(x), decimal.mark = ",")) %>%
  .[c(1, 3, 2), ] %>%
  knitr::kable(
    col.names = c("", "Határosak", "Nem határosak"), align = c("l", "c", "c"),
    caption =
      "Tesztek eredményeinek arányai egymással határos és nem határos országok esetében"
  )
```

|                         | Határosak | Nem határosak |
| ----------------------- | :-------: | :-----------: |
| A teszt nem elvégezhető |   32,2%   |     49,4%     |
| Nem kointegráltak       |   27,6%   |     27,9%     |
| Kointegráltak           |   40,2%   |     22,7%     |

Tesztek eredményeinek arányai egymással határos és nem határos országok
esetében

``` r
df %>%
  group_by(neighbour) %>%
  summarise(mean(cor)) # mean of correlation
```

``` 
# A tibble: 3 x 2
  neighbour     `mean(cor)`
  <chr>               <dbl>
1 Határosak           0.829
2 Nem határosak       0.721
3 <NA>                1    
```

# Magyarországi idősoros adatok elemzése vektor autoregresszív modellel

``` r
irf_plot <- function(model, n.ahead = 10, ci = .95, plot_filter = NULL, n.col = NULL) {
  irf <- model %>%
    vars::irf(n.ahead = n.ahead, ci = ci)

  point <- irf %>%
    .$irf %>%
    do.call(what = rbind) %>%
    data.frame()

  point <- point %>%
    mutate(
      shock = rep(names(point), each = nrow(point) / length(point)),
      t = rep(0:(nrow(point) / length(point) - 1), times = length(point))
    ) %>%
    pivot_longer(cols = seq_along(point)) %>%
    transmute(
      t = t,
      variable = str_c(shock, " › ", name),
      point = value
    )

  lower <- irf %>%
    .$Lower %>%
    do.call(what = rbind) %>%
    data.frame()

  lower <- lower %>%
    mutate(
      shock = rep(names(lower), each = nrow(lower) / length(lower)),
      t = rep(0:(nrow(lower) / length(lower) - 1), times = length(lower))
    ) %>%
    pivot_longer(cols = seq_along(lower)) %>%
    transmute(
      t = t,
      variable = str_c(shock, " › ", name),
      lower = value
    )

  upper <- irf %>%
    .$Upper %>%
    do.call(what = rbind) %>%
    data.frame()

  upper <- upper %>%
    mutate(
      shock = rep(names(upper), each = nrow(upper) / length(upper)),
      t = rep(0:(nrow(upper) / length(upper) - 1), times = length(upper))
    ) %>%
    pivot_longer(cols = seq_along(upper)) %>%
    transmute(
      t = t,
      variable = str_c(shock, " › ", name),
      upper = value
    )

  df <- merge(point, lower, by = c("t", "variable")) %>%
    merge(upper, by = c("t", "variable")) %>%
    arrange(variable)

  if (!is.null(plot_filter)) {
    df <- df %>% filter(variable %in% unique(variable)[plot_filter])
  }

  if (is.null(n.col)) {
    n.col <- (n_distinct(df$variable)^0.5 %/% 1)
  }

  df %>% ggplot() +
    geom_hline(yintercept = 0, linetype = "dotted", size = 1) +
    geom_ribbon(aes(min = lower, max = upper, x = t, fill = paste(scales::percent(ci), "konfidencia intervallum")), alpha = .2) +
    geom_line(aes(x = t, y = point, color = "irf"), size = 1.2) +
    facet_wrap(vars(variable), scales = "free", ncol = n.col) +
    scale_color_manual(values = c("irf" = "black")) +
    scale_fill_manual(values = "grey50") +
    scale_x_continuous(breaks = 0:n.ahead, labels = 0:n.ahead, expand = c(0, 0)) +
    labs(
      x = "", y = "", fill = NULL, color = NULL
    ) +
    theme(
      legend.position = "bottom"
    )
}
```

## Csak egy főre jutó GDP-t és TTA-t tartalmazó modell

``` r
model <- socioeconomic_indicators %>%
  merge(LiveBirthAndFertility) %>%
  dplyr::select(TotalFertility, GDPCAP1960) %>%
  set_names(c("TTA", "GDP")) %>%
  mutate_at(1:2, function(x) {
    d <- ndiffs(x, test = "kpss", type = "level")
    if (d > 0) x <- c(rep(NA, d), diff(x))
    x
  }) %>%
  na.exclude() %>%
  ts() %>%
  vars::VAR(ic = "AIC")
```

``` r
model %>% vars::roots()
```

    [1] 0.4350131 0.4350131

``` r
model %>% summary()
```

``` 

VAR Estimation Results:
========================= 
Endogenous variables: TTA, GDP 
Deterministic variables: const 
Sample size: 56 
Log Likelihood: -106.189 
Roots of the characteristic polynomial:
0.435 0.435
Call:
vars::VAR(y = ., ic = "AIC")


Estimation results for equation TTA: 
==================================== 
TTA = TTA.l1 + GDP.l1 + const 

        Estimate Std. Error t value Pr(>|t|)   
TTA.l1  0.334521   0.122613   2.728  0.00862 **
GDP.l1  0.002822   0.001362   2.071  0.04322 * 
const  -0.014359   0.010446  -1.375  0.17504   
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


Residual standard error: 0.06893 on 53 degrees of freedom
Multiple R-Squared: 0.2015, Adjusted R-squared: 0.1714 
F-statistic: 6.687 on 2 and 53 DF,  p-value: 0.002572 


Estimation results for equation GDP: 
==================================== 
GDP = TTA.l1 + GDP.l1 + const 

       Estimate Std. Error t value Pr(>|t|)    
TTA.l1  -4.3149    10.6453  -0.405   0.6869    
GDP.l1   0.5293     0.1183   4.475 4.08e-05 ***
const    1.9014     0.9069   2.097   0.0408 *  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


Residual standard error: 5.984 on 53 degrees of freedom
Multiple R-Squared: 0.2745, Adjusted R-squared: 0.2471 
F-statistic: 10.03 on 2 and 53 DF,  p-value: 0.0002028 



Covariance matrix of residuals:
          TTA      GDP
TTA  0.004751 -0.01842
GDP -0.018421 35.81064

Correlation matrix of residuals:
         TTA      GDP
TTA  1.00000 -0.04466
GDP -0.04466  1.00000
```

``` r
vars::causality(model, cause = "TTA")
```

    $Granger
    
        Granger causality H0: TTA do not Granger-cause GDP
    
    data:  VAR object model
    F-Test = 0.1643, df1 = 1, df2 = 106, p-value = 0.686
    
    
    $Instant
    
        H0: No instantaneous causality between: TTA and GDP
    
    data:  VAR object model
    Chi-squared = 0.11147, df = 1, p-value = 0.7385

``` r
vars::causality(model, cause = "GDP")
```

    $Granger
    
        Granger causality H0: GDP do not Granger-cause TTA
    
    data:  VAR object model
    F-Test = 4.2899, df1 = 1, df2 = 106, p-value = 0.04077
    
    
    $Instant
    
        H0: No instantaneous causality between: GDP and TTA
    
    data:  VAR object model
    Chi-squared = 0.11147, df = 1, p-value = 0.7385

``` r
model %>% irf_plot(plot_filter = c(1, 2), n.col = 1, n.ahead = 7) +
  labs(title = "GDP/fő és TTA-t tartalmazó VAR modellből számított impulzus válaszfüggvények")
```

![](ujdemografiaiprogram_files/figure-gfm/unnamed-chunk-12-1.svg)<!-- -->

``` r
model %>%
  vars::fevd() %>%
  .$TTA
```

``` 
            TTA        GDP
 [1,] 1.0000000 0.00000000
 [2,] 0.9485717 0.05142830
 [3,] 0.9140946 0.08590538
 [4,] 0.9003777 0.09962226
 [5,] 0.8959989 0.10400111
 [6,] 0.8947696 0.10523041
 [7,] 0.8944536 0.10554639
 [8,] 0.8943776 0.10562244
 [9,] 0.8943602 0.10563982
[10,] 0.8943564 0.10564362
```

## Az egy főre jutó GDP-t, munkanélküliségi rátát és TTA-t tartalmazó modell

``` r
model <- socioeconomic_indicators %>%
  merge(LiveBirthAndFertility) %>%
  dplyr::select(TotalFertility, UnemploymentT, GDPCAP1960) %>%
  set_names(c("TTA", "Munkanélküliség", "GDP")) %>%
  mutate_at(1:3, function(x) {
    d <- ndiffs(x, test = "kpss", type = "level")
    if (d > 0) x <- c(rep(NA, d), diff(x))
    x
  }) %>%
  na.exclude() %>%
  ts() %>%
  vars::VAR(ic = "AIC")
```

``` r
model %>% vars::roots()
```

    [1] 0.8589995 0.2316852 0.2316852

``` r
model %>% summary()
```

``` 

VAR Estimation Results:
========================= 
Endogenous variables: TTA, Munkanélküliség, GDP 
Deterministic variables: const 
Sample size: 18 
Log Likelihood: -40.693 
Roots of the characteristic polynomial:
0.859 0.2317 0.2317
Call:
vars::VAR(y = ., ic = "AIC")


Estimation results for equation TTA: 
==================================== 
TTA = TTA.l1 + Munkanélküliség.l1 + GDP.l1 + const 

                    Estimate Std. Error t value Pr(>|t|)   
TTA.l1             -0.060635   0.200079  -0.303  0.76630   
Munkanélküliség.l1  0.012837   0.005349   2.400  0.03087 * 
GDP.l1              0.004412   0.001475   2.990  0.00974 **
const              -0.113110   0.047905  -2.361  0.03325 * 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


Residual standard error: 0.03554 on 14 degrees of freedom
Multiple R-Squared: 0.4052, Adjusted R-squared: 0.2777 
F-statistic: 3.178 on 3 and 14 DF,  p-value: 0.05722 


Estimation results for equation Munkanélküliség: 
================================================ 
Munkanélküliség = TTA.l1 + Munkanélküliség.l1 + GDP.l1 + const 

                   Estimate Std. Error t value Pr(>|t|)    
TTA.l1             -4.91229    5.87641  -0.836   0.4172    
Munkanélküliség.l1  0.78325    0.15710   4.986   0.0002 ***
GDP.l1             -0.06977    0.04333  -1.610   0.1297    
const               1.97827    1.40699   1.406   0.1815    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


Residual standard error: 1.044 on 14 degrees of freedom
Multiple R-Squared: 0.8117, Adjusted R-squared: 0.7714 
F-statistic: 20.12 on 3 and 14 DF,  p-value: 2.408e-05 


Estimation results for equation GDP: 
==================================== 
GDP = TTA.l1 + Munkanélküliség.l1 + GDP.l1 + const 

                   Estimate Std. Error t value Pr(>|t|)
TTA.l1              -4.2122    42.5279  -0.099    0.923
Munkanélküliség.l1  -0.9274     1.1370  -0.816    0.428
GDP.l1               0.2414     0.3136   0.770    0.454
const               12.0531    10.1824   1.184    0.256


Residual standard error: 7.553 on 14 degrees of freedom
Multiple R-Squared: 0.1828, Adjusted R-squared: 0.007687 
F-statistic: 1.044 on 3 and 14 DF,  p-value: 0.4037 



Covariance matrix of residuals:
                      TTA Munkanélküliség      GDP
TTA              0.001263        -0.01175  0.03661
Munkanélküliség -0.011753         1.08928 -5.21863
GDP              0.036613        -5.21863 57.05124

Correlation matrix of residuals:
                    TTA Munkanélküliség     GDP
TTA              1.0000         -0.3169  0.1364
Munkanélküliség -0.3169          1.0000 -0.6620
GDP              0.1364         -0.6620  1.0000
```

A kauzalitás vizsgálatok gretl-ben történtek.

``` r
model %>% irf_plot() + labs(title = "GDP/fő, munkanélküliségi rátát és TTA-t tartalmazó VAR modellből számított impulzus válaszfüggvények")
```

![Impulzus
válaszfüggvények](ujdemografiaiprogram_files/figure-gfm/unnamed-chunk-16-1.svg)

``` r
model %>%
  vars::fevd() %>%
  .$TTA %>%
  data.frame() %>%
  mutate(
    t = seq(from = 0, to = nrow(.) - 1)
  ) %>%
  gather(key = "variable", value = "value", -t) %>%
  mutate(variable = factor(case_when(
    variable == "Munkanélküliség" ~ "Munkanélküliség",
    variable == "GDP" ~ "GDP/fő",
    T ~ "TTA"
  ), levels = c("Munkanélküliség", "GDP/fő", "TTA"))) %>%
  ggplot() +
  geom_area(aes(x = t, y = value, fill = variable), color = "black") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) +
  scale_fill_grey() +
  scale_x_continuous(labels = 0:5, breaks = 0:5, expand = c(0, 0), limits = c(0, 5)) +
  labs(x = "Év", y = "TTA varianciájának magyarázata", fill = NULL, title = "GDP/fő, munkanélküliségi rátát és TTA-t tartalmazó VAR modellből számított varianciadekompozíció")
```

![](ujdemografiaiprogram_files/figure-gfm/unnamed-chunk-17-1.svg)<!-- -->

# Magyarországi panel adatok

``` r
c.panel.extended <- c.panel %>%
  transmute(
    county, year, tfr, GDPcap,
    GDPcap2 = GDPcap^2,
    GDPcap_l = ifelse(year == 2005, NA, dplyr::lag(GDPcap)),
    GDPcap_l2 = ifelse(year == 2005, NA, dplyr::lag(GDPcap2)),
    GDPcap_ll = ifelse(year == 2005, NA, dplyr::lag(GDPcap_l)),
    GDPcap_ll2 = ifelse(year == 2005, NA, dplyr::lag(GDPcap_l2)),
    GDPcap_lll = ifelse(year == 2005, NA, dplyr::lag(GDPcap_ll)),
    GDPcap_lll2 = ifelse(year == 2005, NA, dplyr::lag(GDPcap_ll2)),
    unr,
    unr2 = unr^2,
    unr_l = ifelse(year == 2005, NA, dplyr::lag(unr)),
    unr_l2 = ifelse(year == 2005, NA, dplyr::lag(unr2)),
    unr_ll = ifelse(year == 2005, NA, dplyr::lag(unr_l)),
    unr_ll2 = ifelse(year == 2005, NA, dplyr::lag(unr_l2)),
    unr_lll = ifelse(year == 2005, NA, dplyr::lag(unr_ll)),
    unr_lll2 = ifelse(year == 2005, NA, dplyr::lag(unr_ll2)),
  )

terms <- list(
  c("GDPcap", "unr"),
  c(
    "GDPcap", "unr", "unr2", "GDPcap2", "unr_l", "unr_l2", "GDPcap_l",
    "GDPcap_l2", "unr_ll", "unr_ll2", "GDPcap_ll", "GDPcap_ll2", "unr_lll", "unr_lll2",
    "GDPcap_lll", "GDPcap_lll2"
  ),
  c(
    "GDPcap", "GDPcap2", "GDPcap_l",
    "GDPcap_l2", "GDPcap_ll", "GDPcap_ll2"
  ),
  c("GDPcap_l", "GDPcap_l2", "GDPcap_ll")
)
panel.tbl <- data.frame(term = c(names(c.panel.extended)[-c(1, 2, 3)], "Chow-teszt", "Hausman-teszt", "Korrigált R^2"))

for (i in 1:length(terms)) {
  formula <- as.expression(paste("formula = tfr ~", paste(terms[[i]], collapse = " + ")))

  c.panel.pooling.extended <- plm(eval(formula), data = c.panel.extended, model = "pooling")
  c.panel.within.extended <- plm(eval(formula), data = c.panel.extended, model = "within")
  c.panel.random.extended <- plm(eval(formula), data = c.panel.extended, model = "random")

  panel.tbl <- panel.tbl %>% merge(c.panel.within.extended %>% broom::tidy() %>% transmute(
    term,
    estimate = paste0(as.character(format(estimate, digits = 3, nsmall = 3, decimal.mark = ",")), case_when(
      p.value < .01 ~ "***",
      p.value < .05 ~ "**",
      p.value < .1 ~ "*",
      T ~ ""
    ))
  ) %>% rbind(
    data.frame(
      term = c("Chow-teszt", "Hausman-teszt", "Korrigált R^2"),
      estimate = c(
        pooltest(c.panel.pooling.extended, c.panel.within.extended)$p.value %>% scales::percent(accuracy = .01, decimal.mark = ","),
        phtest(c.panel.within.extended, c.panel.random.extended)$p.value %>% scales::percent(accuracy = .01, decimal.mark = ","),
        c.panel.within.extended %>% plm::r.squared(dfcor = T) %>% scales::percent(accuracy = .01, decimal.mark = ",")
      )
    )
  ) %>% set_names(c("term", i)),
  all = T
  )
}
panel.tbl %>%
  setNames(c("variable", as.roman(1:(ncol(.) - 1)))) %>%
  mutate(variable = factor(variable, levels = c(names(c.panel.extended), "Chow-teszt", "Hausman-teszt", "Korrigált R^2"))) %>%
  arrange(variable) %>%
  mutate(
    variable = as.character(variable),
    variable = str_replace(variable, "2", "^2"),
    variable = ifelse(variable == "Korrigált R^^2", "Korrigált R^2", variable),
    variable = str_replace(variable, "GDPcap", "GDP/fő"),
    variable = str_replace(variable, "unr", "Munkanélküliségi ráta"),
    variable = str_replace(variable, "_lll", " (l=3)"),
    variable = str_replace(variable, "_ll", " (l=2)"),
    variable = str_replace(variable, "_l", " (l=1)")
  ) %>%
  mutate_all(function(x) ifelse(is.na(x), "", x)) %>%
  knitr::kable(
    col.names = c("változó", "I.", "II.", "III.", "IV."),
    align = c("l", rep("c", ncol(.) - 1)), caption = "Becsült longitudinális modellek a TTA-ra"
  )
```

| változó                       |        I.        |      II.       |      III.      |       IV.        |
| :---------------------------- | :--------------: | :------------: | :------------: | :--------------: |
| GDP/fő                        |  6,34e-05\*\*\*  |   \-9,61e-05   |    8,53e-05    |                  |
| GDP/fő^2                      |                  |    1,15e-08    |   \-1,33e-09   |                  |
| GDP/fő (l=1)                  |                  | 3,54e-04\*\*\* | 4,23e-04\*\*\* |  4,63e-04\*\*\*  |
| GDP/fő (l=1)^2                |                  |  \-3,24e-08\*  | \-3,77e-08\*\* | \-3,15e-08\*\*\* |
| GDP/fő (l=2)                  |                  |   2,00e-04\*   |  \-1,48e-04\*  |  \-8,16e-05\*\*  |
| GDP/fő (l=2)^2                |                  |   \-2,06e-08   |    9,40e-09    |                  |
| GDP/fő (l=3)                  |                  |   \-7,65e-05   |                |                  |
| GDP/fő (l=3)^2                |                  |    8,49e-09    |                |                  |
| Munkanélküliségi ráta         | \-2,05e-02\*\*\* |   \-2,39e-03   |                |                  |
| Munkanélküliségi ráta^2       |                  |   \-2,66e-04   |                |                  |
| Munkanélküliségi ráta (l=1)   |                  |    9,95e-04    |                |                  |
| Munkanélküliségi ráta (l=1)^2 |                  |   \-2,73e-04   |                |                  |
| Munkanélküliségi ráta (l=2)   |                  |    3,21e-03    |                |                  |
| Munkanélküliségi ráta (l=2)^2 |                  |   \-2,29e-04   |                |                  |
| Munkanélküliségi ráta (l=3)   |                  |    1,23e-02    |                |                  |
| Munkanélküliségi ráta (l=3)^2 |                  |   \-1,44e-04   |                |                  |
| Chow-teszt                    |      0,00%       |     0,00%      |     0,00%      |      0,00%       |
| Hausman-teszt                 |      0,00%       |     0,07%      |     0,00%      |      0,00%       |
| Korrigált R^2                 |      68,07%      |     82,75%     |     71,41%     |      71,02%      |

Becsült longitudinális modellek a TTA-ra

``` r
# Hungarian map draw function -----------
hun_map_plot <- function(df, na.value = "white", low = "white", high = "black") {
  hunsf %>%
    merge(set_names(df, c("NAME", "value"))) %>%
    ggplot() +
    geom_sf(aes(fill = value)) +
    ggthemes::theme_map() +
    scale_fill_gradient(
      na.value = na.value, low = low, high = high,
      guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
    )
}
```

``` r
c.panel.within.extended %>%
  plm::fixef() %>%
  data.frame() %>%
  rownames_to_column() %>%
  hun_map_plot() + labs(fill = "TTA", title = "A IV. longitudinális modell egyedi konstansai") + theme(legend.position = "right")
```

![](ujdemografiaiprogram_files/figure-gfm/unnamed-chunk-20-1.svg)<!-- -->

``` r
c.panel.within.extended %>%
  broom::augment() %>%
  dplyr::select(.rownames, .fitted) %>%
  merge(c.panel %>% mutate(".rownames" = seq(nrow(c.panel)))) %>%
  transmute(Becsült = .fitted, Valós = tfr, county = str_remove_all(county, paste(c(" megye", "-Csanád"), collapse = "|")), year = year) %>%
  pivot_longer(1:2) %>%
  ggplot(aes(x = as.numeric(year), y = value, color = name)) +
  geom_line(size = 1.2) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(2008, 2016, 4)) +
  scale_color_grey() +
  facet_wrap(~county, ncol = 4) +
  labs(x = "Év", y = "TTA", color = NULL, title = "TTA becslése megyénként a fix modellel")
```

![](ujdemografiaiprogram_files/figure-gfm/unnamed-chunk-21-1.svg)<!-- -->

``` r
ggplot(data.frame(x = c(0, 10000)), aes(x = x)) +
  stat_function(fun = function(x) x * c.panel.within.extended$coefficients["GDPcap_l"] + x^2 * c.panel.within.extended$coefficients["GDPcap_l2"], size = 2) +
  geom_vline(xintercept = c.panel %>% filter(year == 2018) %>% pull(GDPcap) %>% .[-1], linetype = "dashed") +
  geom_vline(aes(linetype = "Magyar megyék egy főre eső GDP értékei", xintercept = (c.panel %>% filter(year == 2018) %>% pull(GDPcap) %>% .[1]))) +
  scale_linetype_manual(values = "dashed") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) +
  labs(x = "GDP/fő (ezer Ft-ban) (l = 1)", y = "Egyedi konstanson felüli TTA érték", linetype = NULL, title = "A GDP/fő hatása a TTA-ra a IV. panel modell alapján") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
```

![](ujdemografiaiprogram_files/figure-gfm/unnamed-chunk-22-1.svg)<!-- -->
