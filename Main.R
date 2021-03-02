source("load_packages.R")
source("functions_and_constants.R")

for (file_num in 1:length(scenario_files)) {
  data_path <- paste(getwd(), scenario_files[file_num], sep='/')
  all_data <- fread(data_path)[order(c_price)]
  scenario <- basename(file_path_sans_ext(data_path))
  title = paste("Scenario = ",gsub(x = scenario,pattern = "\\_", replacement = " "))
  n_data <- nrow(all_data)
  beccs <- !grepl("no_beccs",scenario)          # test if BECCS available in this scenario
  do_Cprice_plot <- grepl("^op_space",scenario) # only generate carbon price plot for entire operating space

  Nbebcs <- all_data[BEBCS > 0, .N]
  Nbes   <- all_data[BES > 0, .N]
  Nbeccs <- ifelse(beccs, all_data[BECCS > 0, .N], 0)

  if (do_Cprice_plot) source("c_price_calcs.R")
  if (do_forest) source("random_forest.R")
}
source("parallel_boxplots.R")
source("parallel_coords.R")


