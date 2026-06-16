pacman::p_load(char = c("dplyr", "flextable", "table1", "lattice", "car"), install = T, update = F)

auto_shapiro_raw <- function (data, flextableformat = FALSE) 
{
    if (!is.data.frame(data)) {
        stop("Data provided must be a data.frame object")
    }
    if (!is.logical(flextableformat)) {
        stop("Argument flextableformat must be a logical operator")
    }
    var_cont <- c(colnames(data %>% select_if(is.numeric)))
    resultados <- list()
    for (var1 in var_cont) {
        if (var1 %in% names(data)) {
            shapiro_p <- stats::shapiro.test((data[[var1]]))$p.value
            resultados[[var1]] <- list(Variable = var1, p_shapiro = ifelse(shapiro_p > 
                0.001, paste(round(shapiro_p, digits = 5)), "<0.001*"), 
                Normality = paste(ifelse(shapiro_p > 0.05, "Normal", 
                  "Non-normal")))
        }
        else {
            warning("\nVariable ", var1, " is not present in provided dataframe\n")
        }
    }
    resultadosdf <- do.call(rbind, lapply(resultados, as.data.frame))
    if (flextableformat == TRUE) {
        return(flextable::flextable(resultadosdf))
    }
    else {
        return(resultadosdf)
    }
}


continuous_2g <- function (data, groupvar, ttest_args = list(), wilcox_args = list(), 
    flextableformat = FALSE) 
{
    if (!is.data.frame(data)) {
        stop("Data must be a data.frame object")
    }
    if (is.character(flextableformat) || !is.logical(flextableformat)) {
        stop("flextableformat argument must be a logical operator")
    }
    if (!(groupvar %in% names(data))) {
        stop(groupvar, " is not in the provided dataframe")
    }
    if ("var.equal" %in% names(ttest_args)) {
        warning("\nThe argument 'var.equal' provided in 'ttest_args' will be ignored. \nThe function will determine 'var.equal' automatically based on the Levene test.")
        ttest_args$var.equal <- NULL
    }
    if (any("paired" %in% names(ttest_args) || "paired" %in% 
        names(wilcox_args))) {
        stop("\nPaired is not supported in this function\nPlease use continuous_2g_pair(data, groupvar) instead")
    }
    valid_alternative <- c("two.sided", "less", "greater")
    if (("alternative" %in% names(ttest_args)) | "alternative" %in% 
        names(wilcox_args)) {
        if (!(ttest_args$alternative %in% valid_alternative)) {
            stop("Invalid alternative. Allowed alternatives are: two.sided, less, greater")
        }
    }
    data[[groupvar]] <- as.factor(data[[groupvar]])
    if (length(levels(data[[groupvar]])) != 2) {
        stop("Grouping variable must have exactly 2 levels")
    }
    variables_continuas <- colnames(data %>% dplyr::select(where(is.numeric)))
    resultados <- list()
    for (var1 in variables_continuas) {
        if (var1 %in% names(data)) {
            valid_data <- data[!is.na(data[[groupvar]]) & !is.na(data[[var1]]), 
                ]
            groupingdata <- valid_data[[groupvar]]
            continuous_data <- valid_data[[var1]]
            variable_lab <- if (!is.null(table1::label(data[[var1]]))) 
                table1::label(data[[var1]])
            else var1
            if (length(unique(groupingdata)) < 2 || length(continuous_data) < 
                2) {
                resultados[[var1]] <- list(Variable = variable_lab, 
                  P_Shapiro_Resid = NA, P_Levene = NA, P_T_Test = NA, 
                  Var_Equal = NA, P_Mann_Whitney = NA, Diff_Means = NA, 
                  CI_Lower = NA, CI_Upper = NA, Significant_test = NA)
                next
            }
            tryCatch({
                lm_model <- lm(continuous_data ~ groupingdata)
                shapiro_res <- stats::shapiro.test(residuals(lm_model))$p.value
                levene_p <- car::leveneTest(continuous_data ~ 
                  groupingdata)$"Pr(>F)"[1]
                var_equal <- levene_p > 0.05
                group_levels <- levels(groupingdata)
                group1 <- continuous_data[groupingdata == group_levels[1]]
                group2 <- continuous_data[groupingdata == group_levels[2]]
                ttest_args <- modifyList(ttest_args, list(x = group1, 
                  y = group2, var.equal = var_equal))
                t_test <- do.call(t.test, ttest_args)
                t_p <- t_test$p.value
                diff_means <- mean(group1, na.rm = TRUE) - mean(group2, 
                  na.rm = TRUE)
                ci_lower <- t_test$conf.int[1]
                ci_upper <- t_test$conf.int[2]
                wilcox_args <- modifyList(wilcox_args, list(x = group1, 
                  y = group2))
                mann_whitney <- do.call(wilcox.test, wilcox_args)
                mann_u_p <- mann_whitney$p.value
                signiftest <- "None"
                if (!is.na(shapiro_res) && shapiro_res > 0.05) {
                  if (!is.na(levene_p) && levene_p > 0.05 && 
                    !is.na(t_p) && t_p < 0.05) {
                    signiftest <- "Student T test"
                  }
                  else if (!is.na(levene_p) && levene_p < 0.05 && 
                    !is.na(t_p) && t_p < 0.05) {
                    signiftest <- "Welch T test"
                  }
                }
                else if (!is.na(shapiro_res) && shapiro_res <= 
                  0.05 && !is.na(mann_u_p) && mann_u_p < 0.05) {
                  signiftest <- "Mann-W-U test"
                }
                resultados[[var1]] <- list(Variable = variable_lab, 
                  P_Shapiro_Resid = ifelse(shapiro_res > 0.001, 
                    round(shapiro_res, 5), "<0.001*"), P_Levene = ifelse(levene_p > 
                    0.001, round(levene_p, 5), "<0.001*"), P_T_Test = ifelse(t_p > 
                    0.001, round(t_p, 5), "<0.001*"), Var_Equal = var_equal, 
                  P_Mann_Whitney = ifelse(mann_u_p > 0.001, round(mann_u_p, 
                    5), "<0.001*"), Diff_Means = round(diff_means, 
                    5), CI_Lower = round(ci_lower, 5), CI_Upper = round(ci_upper, 
                    5), Significant_test = signiftest)
            }, error = function(e) {
                resultados[[var1]] <- list(Variable = variable_lab, 
                  P_Shapiro_Resid = NA, P_Levene = NA, P_T_Test = NA, 
                  Var_Equal = NA, P_Mann_Whitney = NA, Diff_Means = NA, 
                  CI_Lower = NA, CI_Upper = NA, Significant_test = NA)
            })
        }
    }
    resultados_df <- do.call(rbind, lapply(resultados, as.data.frame))
    rownames(resultados_df) <- NULL
    if (flextableformat) {
        return(flextable::flextable(resultados_df))
    }
    else {
        return(resultados_df)
    }
}

encode_factors <- function (data, encode = c("character", "integer"), list_factors = NULL, 
    uselist = FALSE) 
{
    if (!is.data.frame(data)) {
        stop("The 'data' argument must be a data frame")
    }
    if (uselist && is.null(list_factors)) {
        stop("If 'uselist' is TRUE, 'list_factors' cannot be NULL")
    }
    if (is.character(uselist) | !is.logical(uselist)) {
        stop("uselist must be a logical argument")
    }
    if (uselist) {
        if (!all(list_factors %in% names(data))) {
            stop("At least one column in 'list_factors' does not exist in the data")
        }
        data[list_factors] <- lapply(data[list_factors], as.factor)
    }
    else if (encode[1] == "character") {
        data <- data %>% mutate(across(where(is.character), as.factor))
    }
    else if (encode[1] == "integer") {
        data <- data %>% mutate(across(where(is.integer), as.factor))
    }
    else {
        stop("The 'encode' argument must be either 'character' or 'integer'")
    }
    return(data)
}