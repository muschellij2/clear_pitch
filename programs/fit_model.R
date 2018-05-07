# Running the models
library(here)
library(dplyr)
library(tidyr)
library(readr)
library(caret)
library(neurobase)
library(ranger)

set.seed(20180227)
# root_dir <- "~/CLEAR_PITCH"
root_dir = here::here()
img_dir = file.path(root_dir, "original")
proc_dir = file.path(root_dir, "processed")
out_dir = file.path(root_dir, "models")
res_dir = file.path(root_dir, "results")
source(file.path(root_dir, "programs", 
  "helper_functions.R"))

groups = c("train")
studies = c("CLEAR", "BOTH")
run_fracs = c(0.1)
eg = expand.grid(n4 = c(FALSE, TRUE),
  run_frac = run_fracs, 
  stratified = c(FALSE, TRUE),
  groups = groups,
  study = c(studies),
  stringsAsFactors = FALSE)
eg = eg %>% 
  mutate(
    app = ifelse(groups == "train",
      "", paste0(groups, "_")),
    outfile = file.path(out_dir, 
    paste0("ranger_", 
      ifelse(n4, "n4_", ""), 
      ifelse(run_frac != 0.1, 
        paste0(run_frac, "_"),  ""),
      ifelse(stratified, "stratified_", ""),
      ifelse(study == "CLEAR", "", "combined_"), 
      app,
    "model.rds")))
eg = eg %>% 
  mutate(
    logistic = sub("ranger_", "logistic_", outfile),
    leekasso = sub("ranger_", "leekasso_", outfile),
    filenames_df = file.path(res_dir, 
        ifelse(study == "CLEAR",
       "filenames_df.rds", "all_filenames_df.rds")
        )
    )


iscen = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(iscen)) {
  iscen = 5
}

outfile = eg$outfile[iscen]
log_outfile = eg$logistic[iscen]
leek_outfile = eg$leekasso[iscen]
n4 = eg$n4[iscen]
run_frac = eg$run_frac[iscen]
stratified = eg$stratified[iscen]
group = eg$group[iscen]
filenames_df = eg$filenames_df[iscen]

if (!all(file.exists(
  c(outfile, log_outfile,
    leek_outfile)))) {

  df = read_rds(filenames_df)

  df = df[ df$train %in% group, ]
  # keep first scan 
  df = df %>% 
    mutate(d2 = as.numeric(date)) %>% 
    arrange(id, d2) %>% 
    group_by(id) %>% 
    dplyr::slice(1) %>% 
    select(-d2)

  df$stub = paste0(df$stub, ifelse(n4, "_n4", ""))

  df$rds = file.path(df$id_proc_dir,
    paste0(df$stub, "_",  "predictor_df.rds"))

  n_ids = nrow(df)
  all_df = vector(mode = "list",
    length = nrow(df))
  names(all_df) = df$scan

  # read in the data
  for (iid in seq(n_ids)) {
  	print(iid)
  	proc_df = read_rds(df$rds[iid])
  	all_df[[iid]] = proc_df
  	rm(proc_df); 
  }

  # make a categorical variable
  full_df = bind_rows(all_df, .id = "scan")
  rm(all_df); gc()
  full_df$y = ifelse(full_df$Y > 0,
    "lesion", "non_lesion")
  full_df$y = factor(full_df$y,
    levels = c("non_lesion", "lesion"))

   
  if (stratified) {
    cn = colnames(full_df)
    cn = setdiff(cn, c("scan", "Y", "y", 
      "mask", "id"))
    quant_vars = full_df[,cn]
    lgls = sapply(quant_vars, function(x) {
      all(x == as.logical(x))
    })
    lgl_df = quant_vars[, lgls]
    lgl_df = lgl_df %>% 
      transmute_all(funs(med = as.logical))
    quant_vars = quant_vars[, !lgls]
    quant_vars = quant_vars %>% 
      transmute_all(funs(med = . > median(.)))

    full_df = bind_cols(
      full_df,
      quant_vars, 
      lgl_df)

    add_cols = c(colnames(quant_vars),
      colnames(lgl_df))
    rm(lgl_df); gc()
    rm(quant_vars); gc()

    groupers = c("scan", "Y", add_cols)

    samp = full_df %>% 
    	group_by_(groupers) %>% 
    	sample_frac(size = run_frac) %>% 
    	ungroup()


    samp = samp %>% 
      select(-ends_with("med"))
    full_df = full_df %>% 
      select(-ends_with("med"))  
  } else {
    samp = full_df %>% 
    group_by(scan, Y) %>% 
    sample_frac(size = run_frac) %>% 
    ungroup()
  }

  print(nrow(samp))

  cn = colnames(full_df)
  keep = !cn %in% c("mask", "scan", "Y")
  xdf = full_df[, !keep]
  full_df = full_df[, keep]

  xsamp = samp[, !keep]
  samp = samp[, keep]

  # mod = ranger(Y ~ ., data = samp)
  # mod = ranger(Y ~ ., data = full_df)
  rm(full_df); gc()
  rm(xdf); gc()
  rm(xsamp); gc(); gc(); gc();
  gc(); gc(); gc();
  # Create custom trainControl: myControl
  myControl <- trainControl(
    method = "cv", number = 5,
    summaryFunction = twoClassSummary,
    classProbs = TRUE, # IMPORTANT!
    verboseIter = TRUE
  )
  y = samp$y
  samp = samp %>% 
    select(-y)
  
  if (!file.exists(outfile)) {
    model <- train(
      x = samp,
      y = y,
      tuneLength = 1,
      method = "ranger",
      trControl = myControl,
      importance = "permutation"
      # save.memory = TRUE
    )
    model = reduce_train_object(model)    
    write_rds(model, path = outfile)
  }

  if (!file.exists(log_outfile)) {
    model <- train(
      x = samp,
      y = y,
      tuneLength = 1,
      method = "glm",
      family="binomial",
      trControl = myControl
      # save.memory = TRUE
    )
    model = reduce_train_object(model)    
    model$finalModel = reduce_glm_mod(
      model$finalModel)    
    write_rds(model, path = log_outfile)
  }  

  if (!file.exists(leek_outfile)) {
    
    cn = colnames(samp)
    mods = data_frame(
      var = cn)
    mods$z = NA


    for (icol in cn) {
      model <- train(
        x = samp[, icol],
        y = y,
        tuneLength = 1,
        method = "glm",
        family="binomial",
        trControl = myControl
        # save.memory = TRUE
      )
      model = reduce_train_object(model)
      smod = summary(model)
      zval = coef(smod)[icol, "z value"]
      mods = mods %>% 
        mutate(z = ifelse(var == icol,
          zval, z)
        )
    }
    run_mods = mods %>% 
      arrange(desc(abs(z))) %>% 
      dplyr::slice(1:10)
    run_cn = run_mods$var
    run_samp = samp[, run_cn]

    model <- train(
        x = run_samp,
        y = y,
        tuneLength = 1,
        method = "glm",
        family="binomial",
        trControl = myControl
        # save.memory = TRUE
      )
    model = reduce_train_object(model)    
    model$finalModel = reduce_glm_mod(
      model$finalModel)
    write_rds(model, path = leek_outfile)
  }  

}
