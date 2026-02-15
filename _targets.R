## Load your packages, e.g. library(targets).
source("./packages.R")
library(conflicted)

## Load your R files
source("R/target_functions.R")

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

tar_option_set(
  # controller = crew_controller_local(workers = 1,
  #                                    seconds_idle = 5),
  iteration = "list",
  memory = "transient",
  error = "continue"
)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(

  tar_target(n_runs, 8),

  tar_target(model_run_labels, paste0("run_", 1:n_runs)),

  tar_target(model_run_labels_2.5_lgneg4.5, paste0(model_run_labels, "_2.5_lgneg4.5")),

  tar_target(model_run_labels_2.5_lgneg3.5, paste0(model_run_labels, "_2.5_lgneg3.5")),

  tar_target(ivae_lat_long_script, "bioclim_ivae_lat_long.R"),

  tar_target(model_runs_2.5_lgneg4.5, run_model_guildai(script = ivae_lat_long_script,
                                               label = model_run_labels_2.5_lgneg4.5,
                                               tag = "ident_lat_lon_weighted_scratch",
                                               flags = list(loggamma_init = -4.5,
                                                            num_epochs = 5000,
                                                            lr = 0.0005,
                                                            res = "2.5")),
             pattern = map(model_run_labels_2.5_lgneg4.5)),

  tar_target(model_runs_2.5_lgneg3.5, run_model_guildai(script = ivae_lat_long_script,
                                               label = model_run_labels_2.5_lgneg3.5,
                                               tag = "ident_lat_lon_weighted_scratch",
                                               flags = list(loggamma_init = -3.5,
                                                            num_epochs = 5000,
                                                            lr = 0.0005,
                                                            res = "2.5")),
             pattern = map(model_run_labels_2.5_lgneg3.5)),

  tar_target(model_runs_2.5, bind_rows(list_rbind(model_runs_2.5_lgneg3.5),
                                       list_rbind(model_runs_2.5_lgneg4.5))),

  tar_target(MCC_2.5_n5, run_mcc_analysis(model_runs_2.5 |>
                                            unnest(cols = c(scalars, flags)) |>
                                            filter(active_dims == 5))),

  tar_target(regions, c("AWT", "NSW", "CAN", "NZ", "SA", "SWI")),

  tar_target(POs, disPo(regions),
             pattern = map(regions)),

  tar_target(BGs, disBg(regions),
             pattern = map(regions)),

  tar_target(borders, disBorder(regions),
             pattern = map(regions)),

  tar_target(CRSs, crs(borders),
             pattern = map(borders)),

  tar_target(pc_file, "data/bioclim_pca_10.tif",
             format = "file"),

  tar_target(pc_file_5m, "data/bioclim_pca_10_5m.tif",
             format = "file"),

  tar_target(bc_file, "data/bioclim_scaled.tif",
             format = "file"),

  tar_target(bc_file_5m, "data/bioclim_scaled_5.tif",
             format = "file"),

  tar_target(bm_file, c("data/bioclim_manifold_1.tif",
                        "data/bioclim_manifold_2.tif",
                        "data/bioclim_manifold_3.tif",
                        "data/bioclim_manifold_4.tif",
                        "data/bioclim_manifold_5.tif"),
             format = "file"),

  tar_target(bm_file_5m, c("data/bioclim_manifold_5m_1.tif",
                           "data/bioclim_manifold_5m_2.tif",
                           "data/bioclim_manifold_5m_3.tif",
                           "data/bioclim_manifold_5m_4.tif",
                           "data/bioclim_manifold_5m_5.tif"),
             format = "file"),

  tar_target(pc_env, crop_envs(pc_file, borders, CRSs),
             pattern = map(borders, CRSs),
             iteration = "list"),

  tar_target(pc_env_5m, crop_envs(pc_file_5m, borders, CRSs),
             pattern = map(borders, CRSs),
             iteration = "list"),

  tar_target(bc_env, crop_envs(bc_file, borders, CRSs),
             pattern = map(borders, CRSs),
             iteration = "list"),

  tar_target(bc_env_5m, crop_envs(bc_file_5m, borders, CRSs),
             pattern = map(borders, CRSs),
             iteration = "list"),

  tar_target(bm_env, crop_envs(bm_file, borders, CRSs),
             pattern = map(borders, CRSs),
             iteration = "list"),

  tar_target(bm_env_5m, crop_envs(bm_file_5m, borders, CRSs),
             pattern = map(borders, CRSs),
             iteration = "list"),

  tar_target(specs, unique(POs$spid),
             pattern = map(POs)),

  tar_target(po_dfs, map(unique(POs$spid),
                         ~ POs %>% dplyr::filter(spid == .x) %>% dplyr::select(lon = x, lat = y)),
             pattern = map(POs),
             iteration = "list"),

  tar_target(all_dat_lists, list(po_df = po_dfs,
                                 border = rep(list(borders), length(po_dfs)),
                                 bg = rep(list(BGs), length(po_dfs)),
                                 crs = rep(list(CRSs), length(po_dfs)),
                                 pc_env = rep(list(pc_env), length(po_dfs)),
                                 bc_env = rep(list(bc_env), length(po_dfs)),
                                 bm_env = rep(list(bm_env), length(po_dfs)),
                                 spec = specs) %>%
               transpose(),
             pattern = map(po_dfs, borders, BGs, CRSs, pc_env, bc_env, bm_env, specs),
             deployment = "main"),

  tar_target(all_dat_lists_5m, list(po_df = po_dfs,
                                    border = rep(list(borders), length(po_dfs)),
                                    bg = rep(list(BGs), length(po_dfs)),
                                    crs = rep(list(CRSs), length(po_dfs)),
                                    pc_env = rep(list(pc_env_5m), length(po_dfs)),
                                    bc_env = rep(list(bc_env_5m), length(po_dfs)),
                                    bm_env = rep(list(bm_env_5m), length(po_dfs)),
                                    spec = specs) %>%
               transpose(),
             pattern = map(po_dfs, borders, BGs, CRSs, pc_env_5m, bc_env_5m, bm_env_5m, specs),
             deployment = "main"),

  tar_target(dat_1, all_dat_lists[[1]],
             deployment = "main"),

  tar_target(dat_2, all_dat_lists[[2]],
             deployment = "main"),

  tar_target(dat_3, all_dat_lists[[3]],
             deployment = "main"),

  tar_target(dat_4, all_dat_lists[[4]],
             deployment = "main"),

  tar_target(dat_5, all_dat_lists[[5]],
             deployment = "main"),

  tar_target(dat_6, all_dat_lists[[6]],
             deployment = "main"),

  tar_target(dat_5m_1, all_dat_lists_5m[[1]],
             deployment = "main"),

  tar_target(dat_5m_2, all_dat_lists_5m[[2]],
             deployment = "main"),

  tar_target(dat_5m_3, all_dat_lists_5m[[3]],
             deployment = "main"),

  tar_target(dat_5m_4, all_dat_lists_5m[[4]],
             deployment = "main"),

  tar_target(dat_5m_5, all_dat_lists_5m[[5]],
             deployment = "main"),

  tar_target(dat_5m_6, all_dat_lists_5m[[6]],
             deployment = "main"),

  tar_target(SDM_1, get_models(dat_1),
             pattern = map(dat_1)),

  tar_target(SDM_2, get_models(dat_2),
             pattern = map(dat_2),
             error = "null"),

  tar_target(SDM_3, get_models(dat_3),
             pattern = map(dat_3)),

  tar_target(SDM_4, get_models(dat_4),
             pattern = map(dat_4)),

  tar_target(SDM_5, get_models(dat_5),
             pattern = map(dat_5)),

  tar_target(SDM_6, get_models(dat_6),
             pattern = map(dat_6)),

  tar_target(SDM_block_1, get_models_block(dat_1),
             pattern = map(dat_1)),

  tar_target(SDM_block_2, get_models_block(dat_2),
             pattern = map(dat_2),
             error = "null"),

  tar_target(SDM_block_3, get_models_block(dat_3),
             pattern = map(dat_3)),

  tar_target(SDM_block_4, get_models_block(dat_4),
             pattern = map(dat_4)),

  tar_target(SDM_block_5, get_models_block(dat_5),
             pattern = map(dat_5)),

  tar_target(SDM_block_6, get_models_block(dat_6),
             pattern = map(dat_6)),

  tar_target(SDM_5m_1, get_models(dat_5m_1),
             pattern = map(dat_5m_1)),

  tar_target(SDM_5m_2, get_models(dat_5m_2),
             pattern = map(dat_5m_2),
             error = "null"),

  tar_target(SDM_5m_3, get_models(dat_5m_3),
             pattern = map(dat_5m_3)),

  tar_target(SDM_5m_4, get_models(dat_5m_4),
             pattern = map(dat_5m_4)),

  tar_target(SDM_5m_5, get_models(dat_5m_5),
             pattern = map(dat_5m_5)),

  tar_target(SDM_5m_6, get_models(dat_5m_6),
             pattern = map(dat_5m_6)),

  tar_target(SDM_block_5m_1, get_models_block(dat_5m_1),
             pattern = map(dat_5m_1)),

  tar_target(SDM_block_5m_2, get_models_block(dat_5m_2),
             pattern = map(dat_5m_2),
             error = "null"),

  tar_target(SDM_block_5m_3, get_models_block(dat_5m_3),
             pattern = map(dat_5m_3)),

  tar_target(SDM_block_5m_4, get_models_block(dat_5m_4),
             pattern = map(dat_5m_4)),

  tar_target(SDM_block_5m_5, get_models_block(dat_5m_5),
             pattern = map(dat_5m_5)),

  tar_target(SDM_block_5m_6, get_models_block(dat_5m_6),
             pattern = map(dat_5m_6)),

  tar_target(TSS_1, map_depth(SDM_1, 4, ~ mean(.x@TPR + .x@TNR - 1)) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "tss", indices_to = "dataset"),
             pattern = map(SDM_1)),

  tar_target(TSS_summ_1, TSS_1 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_tss = tss - tss[env == "bc"]),
             pattern = map(TSS_1)),

  tar_target(TSS_2, map_depth(SDM_2, 4, ~ mean(.x@TPR + .x@TNR - 1)) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "tss", indices_to = "dataset"),
             pattern = map(SDM_2),
             error = "null"),

  tar_target(TSS_summ_2, TSS_2 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_tss = tss - tss[env == "bc"]),
             pattern = map(TSS_2),
             error = "null"),

  tar_target(TSS_3, map_depth(SDM_3, 4, ~ mean(.x@TPR + .x@TNR - 1)) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "tss", indices_to = "dataset"),
             pattern = map(SDM_3)),

  tar_target(TSS_summ_3, TSS_3 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_tss = tss - tss[env == "bc"]),
             pattern = map(TSS_3)),

  tar_target(TSS_4, map_depth(SDM_4, 4, ~ mean(.x@TPR + .x@TNR - 1)) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "tss", indices_to = "dataset"),
             pattern = map(SDM_4)),

  tar_target(TSS_summ_4, TSS_4 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_tss = tss - tss[env == "bc"]),
             pattern = map(TSS_4)),

  tar_target(TSS_5, map_depth(SDM_5, 4, ~ mean(.x@TPR + .x@TNR - 1)) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "tss", indices_to = "dataset"),
             pattern = map(SDM_5)),

  tar_target(TSS_summ_5, TSS_5 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_tss = tss - tss[env == "bc"]),
             pattern = map(TSS_5)),

  tar_target(TSS_6, map_depth(SDM_6, 4, ~ mean(.x@TPR + .x@TNR - 1)) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "tss", indices_to = "dataset"),
             pattern = map(SDM_6)),

  tar_target(TSS_summ_6, TSS_6 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_tss = tss - tss[env == "bc"]),
             pattern = map(TSS_6)),

  tar_target(AUC_1, map_depth(SDM_1, 4, ~.x@auc) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "auc", indices_to = "dataset"),
             pattern = map(SDM_1)),

  tar_target(AUC_summ_1, AUC_1 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_auc = auc - auc[env == "bc"]),
             pattern = map(AUC_1)),

  tar_target(AUC_2, map_depth(SDM_2, 4, ~.x@auc) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "auc", indices_to = "dataset"),
             pattern = map(SDM_2),
             error = "null"),

  tar_target(AUC_summ_2, AUC_2 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_auc = auc - auc[env == "bc"]),
             pattern = map(AUC_2),
             error = "null"),

  tar_target(AUC_3, map_depth(SDM_3, 4, ~.x@auc) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "auc", indices_to = "dataset"),
             pattern = map(SDM_3)),

  tar_target(AUC_summ_3, AUC_3 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_auc = auc - auc[env == "bc"]),
             pattern = map(AUC_3)),

  tar_target(AUC_4, map_depth(SDM_4, 4, ~.x@auc) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "auc", indices_to = "dataset"),
             pattern = map(SDM_4)),

  tar_target(AUC_summ_4, AUC_4 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_auc = auc - auc[env == "bc"]),
             pattern = map(AUC_4)),

  tar_target(AUC_5, map_depth(SDM_5, 4, ~.x@auc) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "auc", indices_to = "dataset"),
             pattern = map(SDM_5)),

  tar_target(AUC_summ_5, AUC_5 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_auc = auc - auc[env == "bc"]),
             pattern = map(AUC_5)),

  tar_target(AUC_6, map_depth(SDM_6, 4, ~.x@auc) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "auc", indices_to = "dataset"),
             pattern = map(SDM_6)),

  tar_target(AUC_summ_6, AUC_6 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_auc = auc - auc[env == "bc"]),
             pattern = map(AUC_6)),

  tar_target(AUC_5m_1, map_depth(SDM_5m_1, 4, ~.x@auc) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "auc", indices_to = "dataset"),
             pattern = map(SDM_5m_1),
             error = "null"),

  tar_target(AUC_5m_summ_1, AUC_5m_1 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_auc = auc - auc[env == "bc"]),
             pattern = map(AUC_5m_1),
             error = "null"),

  tar_target(AUC_5m_2, map_depth(SDM_5m_2, 4, ~.x@auc) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "auc", indices_to = "dataset"),
             pattern = map(SDM_5m_2),
             error = "null"),

  tar_target(AUC_5m_summ_2, AUC_5m_2 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_auc = auc - auc[env == "bc"]),
             pattern = map(AUC_5m_2),
             error = "null"),

  tar_target(AUC_5m_3, map_depth(SDM_5m_3, 4, ~.x@auc) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "auc", indices_to = "dataset"),
             pattern = map(SDM_5m_3),
             error = "null"),

  tar_target(AUC_5m_summ_3, AUC_5m_3 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_auc = auc - auc[env == "bc"]),
             pattern = map(AUC_5m_3),
             error = "null"),

  tar_target(AUC_5m_4, map_depth(SDM_5m_4, 4, ~.x@auc) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "auc", indices_to = "dataset"),
             pattern = map(SDM_5m_4),
             error = "null"),

  tar_target(AUC_5m_summ_4, AUC_5m_4 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_auc = auc - auc[env == "bc"]),
             pattern = map(AUC_5m_4),
             error = "null"),

  tar_target(AUC_5m_5, map_depth(SDM_5m_5, 4, ~.x@auc) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "auc", indices_to = "dataset"),
             pattern = map(SDM_5m_5),
             error = "null"),

  tar_target(AUC_5m_summ_5, AUC_5m_5 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_auc = auc - auc[env == "bc"]),
             pattern = map(AUC_5m_5),
             error = "null"),

  tar_target(AUC_5m_6, map_depth(SDM_5m_6, 4, ~.x@auc) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "auc", indices_to = "dataset"),
             pattern = map(SDM_5m_6),
             error = "null"),

  tar_target(AUC_5m_summ_6, AUC_5m_6 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_auc = auc - auc[env == "bc"]),
             pattern = map(AUC_5m_6),
             error = "null"),


  tar_target(AUC_block_1, map_depth(SDM_block_1, 4, ~.x@auc) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "auc", indices_to = "dataset"),
             pattern = map(SDM_block_1)),

  tar_target(AUC_block_summ_1, AUC_block_1 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_auc = auc - auc[env == "bc"]),
             pattern = map(AUC_block_1)),

  tar_target(AUC_block_2, map_depth(SDM_block_2, 4, ~.x@auc) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "auc", indices_to = "dataset"),
             pattern = map(SDM_block_2),
             error = "null"),

  tar_target(AUC_block_summ_2, AUC_block_2 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_auc = auc - auc[env == "bc"]),
             pattern = map(AUC_block_2),
             error = "null"),

  tar_target(AUC_block_3, map_depth(SDM_block_3, 4, ~.x@auc) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "auc", indices_to = "dataset"),
             pattern = map(SDM_block_3)),

  tar_target(AUC_block_summ_3, AUC_block_3 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_auc = auc - auc[env == "bc"]),
             pattern = map(AUC_block_3)),

  tar_target(AUC_block_4, map_depth(SDM_block_4, 4, ~.x@auc) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "auc", indices_to = "dataset"),
             pattern = map(SDM_block_4)),

  tar_target(AUC_block_summ_4, AUC_block_4 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_auc = auc - auc[env == "bc"]),
             pattern = map(AUC_block_4)),

  tar_target(AUC_block_5, map_depth(SDM_block_5, 4, ~.x@auc) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "auc", indices_to = "dataset"),
             pattern = map(SDM_block_5)),

  tar_target(AUC_block_summ_5, AUC_block_5 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_auc = auc - auc[env == "bc"]),
             pattern = map(AUC_block_5)),

  tar_target(AUC_block_6, map_depth(SDM_block_6, 4, ~.x@auc) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "auc", indices_to = "dataset"),
             pattern = map(SDM_block_6)),

  tar_target(AUC_block_summ_6, AUC_block_6 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_auc = auc - auc[env == "bc"]),
             pattern = map(AUC_block_6)),

  tar_target(AUC_block_5m_1, map_depth(SDM_block_5m_1, 4, ~.x@auc) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "auc", indices_to = "dataset"),
             pattern = map(SDM_block_5m_1),
             error = "null"),

  tar_target(AUC_block_5m_summ_1, AUC_block_5m_1 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_auc = auc - auc[env == "bc"]),
             pattern = map(AUC_block_5m_1),
             error = "null"),

  tar_target(AUC_block_5m_2, map_depth(SDM_block_5m_2, 4, ~.x@auc) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "auc", indices_to = "dataset"),
             pattern = map(SDM_block_5m_2),
             error = "null"),

  tar_target(AUC_block_5m_summ_2, AUC_block_5m_2 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_auc = auc - auc[env == "bc"]),
             pattern = map(AUC_block_5m_2),
             error = "null"),

  tar_target(AUC_block_5m_3, map_depth(SDM_block_5m_3, 4, ~.x@auc) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "auc", indices_to = "dataset"),
             pattern = map(SDM_block_5m_3),
             error = "null"),

  tar_target(AUC_block_5m_summ_3, AUC_block_5m_3 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_auc = auc - auc[env == "bc"]),
             pattern = map(AUC_block_5m_3),
             error = "null"),

  tar_target(AUC_block_5m_4, map_depth(SDM_block_5m_4, 4, ~.x@auc) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "auc", indices_to = "dataset"),
             pattern = map(SDM_block_5m_4),
             error = "null"),

  tar_target(AUC_block_5m_summ_4, AUC_block_5m_4 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_auc = auc - auc[env == "bc"]),
             pattern = map(AUC_block_4),
             error = "null"),

  tar_target(AUC_block_5m_5, map_depth(SDM_block_5m_5, 4, ~.x@auc) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "auc", indices_to = "dataset"),
             pattern = map(SDM_block_5m_5),
             error = "null"),

  tar_target(AUC_block_5m_summ_5, AUC_block_5m_5 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_auc = auc - auc[env == "bc"]),
             pattern = map(AUC_block_5m_5),
             error = "null"),

  tar_target(AUC_block_5m_6, map_depth(SDM_block_5m_6, 4, ~.x@auc) %>%
               list(all = .) %>%
               as_tibble() %>%
               mutate(model = names(all)) %>%
               unnest_longer(all, values_to = "all", indices_to = "rep") %>%
               unnest_longer(all, values_to = "all", indices_to = "env") %>%
               unnest_longer(all, values_to = "auc", indices_to = "dataset"),
             pattern = map(SDM_block_5m_6),
             error = "null"),

  tar_target(AUC_block_5m_summ_6, AUC_block_5m_6 %>%
               group_by(model, dataset, rep) %>%
               mutate(delta_auc = auc - auc[env == "bc"]),
             pattern = map(AUC_block_5m_6),
             error = "null"),


  tar_target(AUC_summ, bind_rows(AUC_summ_1 %>%
                         imap_dfr(~ .x %>% mutate(spec = .y)) %>%
                         mutate(group = 1),
                         AUC_summ_2 %>%
                         imap_dfr(possibly(~ .x %>% mutate(spec = .y))) %>%
                         mutate(group = 2),
                         AUC_summ_3 %>%
                         imap_dfr(~ .x %>% mutate(spec = .y)) %>%
                         mutate(group = 3),
                         AUC_summ_4 %>%
                         imap_dfr(~ .x %>% mutate(spec = .y)) %>%
                         mutate(group = 4),
                         AUC_summ_6 %>%
                         imap_dfr(~ .x %>% mutate(spec = .y)) %>%
                         mutate(group = 6))),

  tar_target(AUC_block_summ, bind_rows(AUC_block_summ_1 %>%
                         imap_dfr(~ .x %>% mutate(spec = .y)) %>%
                         mutate(group = 1),
                         AUC_block_summ_2 %>%
                         imap_dfr(possibly(~ .x %>% mutate(spec = .y))) %>%
                         mutate(group = 2),
                         AUC_block_summ_3 %>%
                         imap_dfr(~ .x %>% mutate(spec = .y)) %>%
                         mutate(group = 3),
                         AUC_block_summ_4 %>%
                         imap_dfr(~ .x %>% mutate(spec = .y)) %>%
                         mutate(group = 4),
                         AUC_block_summ_6 %>%
                         imap_dfr(~ .x %>% mutate(spec = .y)) %>%
                         mutate(group = 6))),

  tar_target(AUC_5m_summ, bind_rows(AUC_5m_summ_1 %>%
                         imap_dfr(possibly(~ .x %>% mutate(spec = .y))) %>%
                         mutate(group = 1),
                         AUC_5m_summ_2 %>%
                         imap_dfr(possibly(~ .x %>% mutate(spec = .y))) %>%
                         mutate(group = 2),
                         AUC_5m_summ_3 %>%
                         imap_dfr(possibly(~ .x %>% mutate(spec = .y))) %>%
                         mutate(group = 3),
                         AUC_5m_summ_4 %>%
                         imap_dfr(possibly(~ .x %>% mutate(spec = .y))) %>%
                         mutate(group = 4),
                         AUC_5m_summ_6 %>%
                         imap_dfr(possibly(~ .x %>% mutate(spec = .y))) %>%
                         mutate(group = 6))),

  tar_target(AUC_block_5m_summ, bind_rows(AUC_block_5m_summ_1 %>%
                         imap_dfr(possibly(~ .x %>% mutate(spec = .y))) %>%
                         mutate(group = 1),
                         AUC_block_5m_summ_2 %>%
                         imap_dfr(possibly(~ .x %>% mutate(spec = .y))) %>%
                         mutate(group = 2),
                         AUC_block_5m_summ_3 %>%
                         imap_dfr(possibly(~ .x %>% mutate(spec = .y))) %>%
                         mutate(group = 3),
                         AUC_block_5m_summ_4 %>%
                         imap_dfr(possibly(~ .x %>% mutate(spec = .y))) %>%
                         mutate(group = 4),
                         AUC_block_5m_summ_6 %>%
                         imap_dfr(possibly(~ .x %>% mutate(spec = .y))) %>%
                         mutate(group = 6))),

  tar_target(TSS_summ, bind_rows(TSS_summ_1 %>%
                         imap_dfr(~ .x %>% mutate(spec = .y)) %>%
                         mutate(group = 1),
                         TSS_summ_2 %>%
                         imap_dfr(possibly(~ .x %>% mutate(spec = .y))) %>%
                         mutate(group = 2),
                         TSS_summ_3 %>%
                         imap_dfr(~ .x %>% mutate(spec = .y)) %>%
                         mutate(group = 3),
                         TSS_summ_4 %>%
                         imap_dfr(~ .x %>% mutate(spec = .y)) %>%
                         mutate(group = 4),
                         TSS_summ_6 %>%
                         imap_dfr(~ .x %>% mutate(spec = .y)) %>%
                         mutate(group = 6))),

  tar_target(AUC_mod, lmer(delta_auc ~ 0 + dataset*env + (1|spec) + (1|model) + (1 | group),
                            data = AUC_summ %>%
                              filter(env != "bc", grepl("test", dataset)))),

  tar_target(AUC_emmeans, emmeans(AUC_mod, specs = ~ dataset | env)),

  tar_target(AUC_plot_1, ggplot(AUC_summ %>%
                                  ungroup() %>%
                                  filter(env != "bc",
                                         dataset %in% c("test", "env_test")) %>%
                                  mutate(model = case_when(model == 'bc' ~ "BioClim",
                                                           model == 'glm' ~ "GLM",
                                                           model == 'rf' ~ "RandomForest"),
                                         dataset = case_when(dataset == 'env_test' ~ "Environmental Space",
                                                             dataset == 'test' ~ "Geographic Space"),
                                         env = case_when(env == 'bm' ~ "Manifold Variables",
                                                         env == 'pc' ~ "PCA Variables")),
                                aes(model,delta_auc)) +
               geom_violin(aes(fill = dataset),
                           draw_quantiles = c(0.025, 0.5, 0.975)) +
               facet_grid(vars(env)) +
               ylab(TeX(r'($\bar{ \Delta AUC }$)')) +
               scale_fill_discrete(name = "") +
               theme_minimal() +
               theme(legend.position = "bottom")
             ),

  tar_target(TSS_plot_1, ggplot(TSS_summ %>%
                                  ungroup() %>%
                                  filter(env != "bc",
                                         dataset %in% c("test", "env_test")) %>%
                                  mutate(model = case_when(model == 'bc' ~ "BioClim",
                                                           model == 'glm' ~ "GLM",
                                                           model == 'rf' ~ "RandomForest"),
                                         dataset = case_when(dataset == 'env_test' ~ "Environmental Space",
                                                             dataset == 'test' ~ "Geographic Space"),
                                         env = case_when(env == 'bm' ~ "Manifold Variables",
                                                         env == 'pc' ~ "PCA Variables")),
                                aes(model,delta_tss)) +
               geom_violin(aes(fill = dataset),
                           draw_quantiles = c(0.025, 0.5, 0.975)) +
               facet_grid(vars(env)) +
               ylab(TeX(r'($\bar{ \Delta TSS }$)')) +
               scale_fill_discrete(name = "") +
               theme_minimal() +
               theme(legend.position = "bottom")
             ),

  tar_target(TSS_plot_block_1, ggplot(TSS_block_summ %>%
                                  ungroup() %>%
                                  filter(env != "bc",
                                         dataset %in% c("test", "env_test")) %>%
                                  mutate(model = case_when(model == 'bc' ~ "BioClim",
                                                           model == 'glm' ~ "GLM",
                                                           model == 'rf' ~ "RandomForest"),
                                         dataset = case_when(dataset == 'env_test' ~ "Environmental Space",
                                                             dataset == 'test' ~ "Geographic Space"),
                                         env = case_when(env == 'bm' ~ "Manifold Variables",
                                                         env == 'pc' ~ "PCA Variables")),
                                aes(model,delta_tss)) +
               geom_violin(aes(fill = dataset),
                           draw_quantiles = c(0.025, 0.5, 0.975)) +
               facet_grid(vars(env)) +
               ylab(TeX(r'($\bar{ \Delta AUC }$)')) +
               scale_fill_discrete(name = "") +
               theme_minimal() +
               theme(legend.position = "bottom")
             ),

  tar_target(AUC_plot_block_1, ggplot(AUC_block_summ %>%
                                  ungroup() %>%
                                  filter(env != "bc",
                                         dataset %in% c("test", "env_test")) %>%
                                  mutate(model = case_when(model == 'bc' ~ "BioClim",
                                                           model == 'glm' ~ "GLM",
                                                           model == 'rf' ~ "RandomForest"),
                                         dataset = case_when(dataset == 'env_test' ~ "Environmental Space",
                                                             dataset == 'test' ~ "Geographic Space"),
                                         env = case_when(env == 'bm' ~ "Manifold Variables",
                                                         env == 'pc' ~ "PCA Variables")),
                                aes(model,delta_auc)) +
               geom_violin(aes(fill = dataset),
                           draw_quantiles = c(0.025, 0.5, 0.975)) +
               facet_grid(vars(env)) +
               ylab(TeX(r'($\bar{ \Delta AUC }$)')) +
               scale_fill_discrete(name = "") +
               theme_minimal() +
               theme(legend.position = "bottom")
             ),

  tar_target(AUC_block_mod, lmer(delta_auc ~ 0 + dataset*env + (1|spec) + (1|model) + (1|group),
                            data = AUC_block_summ %>%
                              filter(env != "bc", grepl("test", dataset)))),

  tar_target(AUC_block_emmeans, emmeans(AUC_block_mod, specs = ~ dataset | env,
                                        adjust = "sidak", cross.adjust = "sidak")),

  tar_target(TSS_mod, lmer(delta_tss ~ 0 + dataset*env + (1|spec),
                            data = TSS_summ %>%
                              filter(env != "bc", grepl("test", dataset)))),

  tar_target(TSS_emmeans, emmeans(TSS_mod, specs = ~ dataset | env)),

  tar_target(AUC_5m_mod, lmer(delta_auc ~ 0 + dataset*env + (1|spec) + (1|model) + (1 | group),
                            data = AUC_5m_summ %>%
                              filter(env != "bc", grepl("test", dataset)))),

  tar_target(AUC_5m_emmeans, emmeans(AUC_5m_mod, specs = ~ dataset | env)),

  tar_target(AUC_5m_plot_1, ggplot(AUC_5m_summ %>%
                                  ungroup() %>%
                                  filter(env != "bc",
                                         dataset %in% c("test", "env_test")) %>%
                                  mutate(model = case_when(model == 'bc' ~ "BioClim",
                                                           model == 'glm' ~ "GLM",
                                                           model == 'rf' ~ "RandomForest"),
                                         dataset = case_when(dataset == 'env_test' ~ "Environmental Space",
                                                             dataset == 'test' ~ "Geographic Space"),
                                         env = case_when(env == 'bm' ~ "Manifold Variables",
                                                         env == 'pc' ~ "PCA Variables")),
                                aes(model,delta_auc)) +
               geom_violin(aes(fill = dataset),
                           draw_quantiles = c(0.025, 0.5, 0.975)) +
               facet_grid(vars(env)) +
               ylab(TeX(r'($\bar{ \Delta AUC }$)')) +
               scale_fill_discrete(name = "") +
               theme_minimal() +
               theme(legend.position = "bottom")
             ),

  tar_target(AUC_summ_it, AUC_summ %>%
               filter(env != "bc", grepl("test", dataset)) %>%
               group_by(env, model, dataset) %>%
               summarise(delta_auc = mean(delta_auc, na.rm = TRUE)))


)
