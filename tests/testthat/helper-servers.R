# Shared test helper (testthat sources helper-*.R automatically).
#
# shiny::testServer() only accepts `args =` for moduleServer()-style modules
# (first formal argument `id`). FlexibleGLMM's modules are plain
# function(input, output, session, rv) server functions, so wrap them in a
# function(input, output, session) that closes over `rv`.
#
# The module's return value is kept in `srv` inside the wrapper's environment,
# which testServer() evaluates the test expression in - so tests can use
# `srv` (e.g. srv$current_data(), or srv() for model_fit_server's runModels).
as_server <- function(module_fn, rv) {
  force(module_fn)
  force(rv)
  function(input, output, session) {
    srv <- module_fn(input, output, session, rv)
    invisible(srv)
  }
}
