##Question #2 - T-Test 

### Run a t-test comparing 'gave' between treatment and control groups
treatment_group <- df$gave[df$treatment == 1]
control_group <- df$gave[df$control == 1]

### T-test
t_test_result <- t.test(treatment_group, control_group)

### Print results
print(t_test_result)

##Question #2 - Bivariate Linear Regression

### Run the regression
model <- lm(gave ~ treatment, data = df)

### View the summary
summary(model)

##Question #2 - Bivariate Linear Regression

# Probit regression (link = "probit")
probit_model_treatment <- glm(gave ~ treatment, data = df, family = binomial(link = "probit"))

# View the summary
summary(probit_model_treatment)

# Probit regression (link = "probit")
probit_model_control <- glm(gave ~ control, data = df, family = binomial(link = "probit"))

# View the summary
summary(probit_model_control)

## Marketing Analytics HW#1

#### Question #1 - T-Test - "dormant" 


```{r fig.width = 7, fig.height = 4.31, dpi = 96}
result <- compare_means(
  karlan_list_2007_rl, 
  var1 = "treatment_factor", 
  var2 = "dormant"
)
summary(result, show = TRUE)
plot(result, plots = "scatter", custom = FALSE)
```

#### Question #1 - Linear Regression - "dormant"



```{r fig.width = 7, fig.height = 2.69, dpi = 96}
result <- regress(
  karlan_list_2007_rl, 
  rvar = "dormant", 
  evar = "treatment_factor"
)
summary(result)
plot(result, plots = "scatter", custom = FALSE)
```

#### Question #1 - T-Test - "pblack"


```{r fig.width = 7, fig.height = 4.31, dpi = 96}
result <- compare_means(
  karlan_list_2007_rl, 
  var1 = "treatment_factor", 
  var2 = "pblack"
)
summary(result, show = TRUE)
plot(result, plots = "scatter", custom = FALSE)
```
#### Question #1 - Linear Regression - "pblack"

```{r fig.width = 7, fig.height = 2.69, dpi = 96}
result <- regress(
  karlan_list_2007_rl, 
  rvar = "pblack", 
  evar = "treatment_factor"
)
summary(result)
plot(result, plots = "scatter", nrobs = -1, custom = FALSE)
```



#### Question #1 - T-Test - "median_hhincome"


```{r fig.width = 7, fig.height = 4.31, dpi = 96}
result <- compare_means(
  karlan_list_2007_rl, 
  var1 = "treatment_factor", 
  var2 = "median_hhincome"
)
summary(result, show = TRUE)
plot(result, plots = "scatter", custom = FALSE)

```

#### Question #1 - Linear Regression - "median_hhincome"



```{r fig.width = 7, fig.height = 2.69, dpi = 96}
result <- regress(
  karlan_list_2007_rl, 
  rvar = "median_hhincome", 
  evar = "treatment_factor"
)
summary(result)
plot(result, plots = "scatter", nrobs = -1, custom = FALSE)
```

#### Question #2 - Proportion of People Who Donated by Treatment vs Control Group


```{r fig.width = 7, fig.height = 7, dpi = 96}
visualize(
  karlan_list_2007_rl, 
  xvar = "treatment_factor", 
  yvar = "gave", 
  type = "bar", 
  axes = "flip", 
  labs = list(
    title = "Proportion of People Who Donated by Treatment vs Control Group", 
    x = "Treatment Group", 
    y = "% Who Donated"
  ), 
  custom = FALSE
)
```
#### Question #2 - T-Test - Gave by Treatment vs Control Group


```{r fig.width = 7, fig.height = 4.31, dpi = 96}
result <- compare_means(
  karlan_list_2007_rl, 
  var1 = "treatment_factor", 
  var2 = "gave"
)
summary(result, show = TRUE)
plot(result, plots = "bar", custom = FALSE)

```

#### Question #2 - Regression - Gave by Treatment vs Control Group




```{r fig.width = 7, fig.height = 2.69, dpi = 96}
result <- regress(
  karlan_list_2007_rl, 
  rvar = "gave", 
  evar = "treatment_factor"
)
summary(result)
plot(result, plots = "pred_plot", incl = "treatment_factor", custom = FALSE)
```


