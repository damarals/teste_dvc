da_rec <- recipes::recipe(Species ~ Sepal.Length,
                          data = iris)

nnet_spec <- parsnip::mlp() |>
  parsnip::set_engine("nnet") |>
  parsnip::set_mode("classification")

nnet_wflow <- workflows::workflow() |>
  workflows::add_recipe(da_rec) |>
  workflows::add_model(nnet_spec)

nnet_fit <- nnet_wflow |>
  parsnip::fit(iris)

y_pred <- nnet_fit |>
  predict(iris) |>
  dplyr::pull()

p <- ggplot2::qplot(x = Species, y = Sepal.Length,
               data = iris, geom = 'boxplot')
ggplot2::ggsave(filename = 'plot.png', plot = p, device = 'png',
                  width = 3, height = 1.5, dpi = 300)

cm <- iris |>
  dplyr::transmute(truth = Species,
                   estimate = y_pred)

table(cm$truth, cm$estimate) |>
  kableExtra::kable() |>
  cat(file = 'cm.html')

metrics <- yardstick::metric_set(yardstick::accuracy,
                                 yardstick::kap)

cm |>
  yardstick::metrics(truth = truth, estimate = estimate) |>
  tidyr::pivot_wider(-.estimator, names_from = ".metric",
                     values_from = ".estimate") |>
  jsonlite::toJSON() |>
  jsonlite::write_json(path = 'metrics.json')
