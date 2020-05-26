
### Help

* Upload the model data by clicking 'Browse'.

* Variable names are converted to synctactically valid names using the *make.names* function and '.' will be replaced by '_'.

* The variable types are auto-detected from the data. Use the dropdown against each variable to modify the type.

* Choose a target variable. If multiple variables are chosen, only the first one will be considered. This variable must have the values 0 and 1 only.

* Choose the variables for stratification while creating train and test samples. The good/bad flag will always be used as a stratification variable. Choosing a
numeric or categorical variable with too many distinct values may cause errors
in the Sample tab.

* Choose the variables which you want to bin and consider as predictor variables during model building. The good/bad flag will not be binned. Only categorical,
numeric or integer variables can be binned.

```{r}
include_graphics("www/binning-woe.png")
#knitr::include_graphics("www/binning-woe.png")
```


```
## Error in knitr::include_graphics("www/binning-woe.png"): Cannot find the file(s): "www/binning-woe.png"
```

The IV is calculated at the characteristic level. Higher the IV, the more the predictive power of this characteristic in separating goods from bads. A general rule of thumb is that to be considered in the model the IV must be at least 3%.


```
## Error in knitr::include_graphics("www/binning-iv.png"): Cannot find the file(s): "www/binning-iv.png"
```

The following diagrams show a numeric and a character variable, after they have been binned to obtain a monotonic WoE pattern.


```
## Error in knitr::include_graphics("www/binning-numeric.png"): Cannot find the file(s): "www/binning-numeric.png"
```


```
## Error in knitr::include_graphics("www/binning-character.png"): Cannot find the file(s): "www/binning-character.png"
```
