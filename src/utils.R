#################################
# generative model of income mobility and mortality
# auxiliary functions
# author: sebastian daza
################################


library(texreg)

table = function (...) base::table(..., useNA = 'ifany')
cor = function(...) stats::cor(..., use = "complete.obs")
perc.rank = function(x) trunc(rank(x)) / length(x)

reg = function(kid_income, parent_income, relative = TRUE) {
    m = lm(kid_income ~ parent_income)
    c = coef(m)
    if (relative) { return(c[2])}
    else {
        return (c[1]  + 0.25 * c[2])
    }
}

addDiagonalProbs = function(parent_type, kid_type, index = 1) {
    diag(prop.table(table(parent_type, kid_type), 1))[index]
}

logIncome = function(x, center = TRUE) {
    a = ifelse(x == 0, log(1), log(x))
    a = scale(a, scale = FALSE, center = center)
    return(a)
}

readMultipleFiles = function(pattern, path, remove_files = FALSE, save_rds = TRUE,
    extension = "csv", rds_extension = "rds") {
    files = list.files(path, paste0(pattern, ".+", extension))
    files_rds = list.files(path, paste0(pattern, ".+", rds_extension))
    if (length(files) == 0) {
        message(paste0("No ", extension, " files found! Trying to read RDS"))
        if (length(files_rds) > 0) {
            files_rds = paste0(path, files_rds)
            r = lapply(files_rds, readRDS)
            if (remove_files & save_rds == FALSE) { save_rds = TRUE}
            if (remove_files)  { sapply(files_rds, unlink, recursive = TRUE) }
            m = rbindlist(r)
            if (save_rds) { saveRDS(m, paste0(path, pattern, ".rds")) }
            return(m)
        } else { return(readRDS(paste0(path, pattern, ".rds"))) }
    } else {
        files = paste0(path, files)
        l = lapply(files, fread)
        m =  rbindlist(l)
        if (remove_files & save_rds == FALSE) { save_rds = TRUE}
        if (save_rds) { saveRDS(m, paste0(path, pattern, ".rds")) }
        if (remove_files)  { sapply(files, unlink, recursive = TRUE) }
        return(m)
    }
}


extractColumns = function(data, variable, new_columns) {
    data[, (variable)  := gsub("\\[|\\]", "", get(variable))]
    data[, (new_columns) := tstrsplit(get(variable), ",", fixed = TRUE)]
    data[, (new_columns) := lapply(.SD, as.numeric), .SDcols = new_columns]
    return(data)
}


saveRDSFile = function(dt, path, overwrite = FALSE) {
    if (file.exists(path) & overwrite == FALSE) {
        warning("File already exist!")
    } else {
        saveRDS(dt, path)
    }
}


savepdf = function(file, width = 16, height = 10) {
  fname = paste0(file, ".pdf")
  pdf(fname, width = width / 2.54, height = height / 2.54,
      pointsize = 10)
  par(mgp = c(2.2, 0.45, 0), tcl = -0.4, mar = c(3.3, 3.6, 1.1, 1.1))
}

# metafor functions
# functions
coxModel = function(replicates, data,
    f = formula("Surv(age, status) ~ total_rank_slope_exposure + lincome + county_lincome"),
    predictor = "total_rank_slope_exposure") {
    yi = NULL
    sei = NULL
    for (i in replicates) {
        # model = coxme::coxme(f, data = data[replicate == i])
        model = coxph(f, data = data[replicate == i])
        model = summary(model)
        coeff_names = rownames(model$coefficients)
        yi = c(yi, model$coefficients[which(coeff_names == predictor), 1])
        sei = c(sei, model$coefficients[which(coeff_names == predictor), 3])
        summary(sei)
        #yi = c(yi, model$coefficients[predictor])
        #sei = c(sei, AICcmodavg::extractSE(model)[predictor])
    }
    output = metafor::rma(yi = yi, sei = sei)

    return(output)
}


linearModel = function(replicates, data,
    f = formula("le ~ rank_slope + gini + lincome + lpopulation"),
    predictor = "rank_slope") {
    yi = NULL
    sei = NULL
    for (i in replicates) {
        model = lm(f, data = data[replicate == i])
        model = summary(model)
        coeff_names = rownames(model$coefficients)
        yi = c(yi, model$coefficients[which(coeff_names == predictor), 1])
        sei = c(sei, model$coefficients[which(coeff_names == predictor), 2])
        # yi = c(yi, fixef(model)[predictor])
        # sei = c(sei, AICcmodavg::extractSE(model)[predictor])
    }
    output = metafor::rma(yi = yi, sei = sei)
    return(output)
}


# texreg extract function
extract.metafor = function(model, include.i2 = FALSE) {

    s = summary(model)
    coefficient.names = "yi"
    coefficients = s$b[1,1]
    se = s$se
    ci.low = s$ci.lb
    ci.upper = s$ci.ub

    gof = s$k
    gof.names = "Replicates"
    gof.decimal = FALSE

    if (include.i2) {
        gof = c(gof, s$I2)
        gof.names = c(gof.names, "$I^2$")
        gof.decimal = c(gof.decimal, TRUE)
    }

    tr = texreg::createTexreg(coef.names = coefficient.names,
        coef = coefficients,
        se = se,
        ci.low = ci.low,
        ci.up = ci.upper,
        gof.names = gof.names,
        gof = gof,
        gof.decimal = gof.decimal
    )
    return(tr)
}

setMethod("extract",
          signature = className("rma.uni", "rma"),
          definition = extract.metafor)


select_tab_coeff = function(tab_list, header, bottom) {
    t = list()
    r = NULL
    for (i in seq_along(tab_list)) {
        a = gsub("(.+midrule)(.+midrule)|\\\\bottomrule.+", "\\2", tab_list[[i]])
        r = gsub(".+midrule", "", a)
        a = gsub("\\\\midrule.+", "", a)
        if (i < length(tab_list)) { a = paste0(a, "\\addlinespace[10pt]\n") }
        if (i == length(tab_list)) { a = paste0(a, "\\midrule")  }
        t[[i]] = a
    }
    t[[length(tab_list)+1]] = r
    t[[length(tab_list)+2]] = bottom
    t = c(header, t)
    return(paste(t, collapse = ""))
}