#**************************************************************
# emmeans.R
# Post-hoc estimated marginal means (EMMs) and pairwise / conditional
# pairwise comparisons for the factors selected in "Factors for
# Post-hoc (EMMs)".
#
# emmeans_server() is called from server.R with the shared
# `runModels` reactive.
#**************************************************************

emmeans_server <- function(input, output, session, rv, runModels) {

  # ------------------------------------------------------------------
  # POSTHOC - pairwise comparisons
  # ------------------------------------------------------------------

  output$emmeansOutput <- renderPrint({
    results <- runModels()
    ph_vars <- input$posthoc_vars

    for (nm in names(results)) {
      cat("\n--- EMMs for:", nm, "---\n")
      res <- results[[nm]]

      if (is.list(res) && length(ph_vars) > 0) {

        ## --- Main effects + pairwise contrasts ---
        for (fac in ph_vars) {
          cat("\nFactor:", fac, "\n")

          # EMMs
          emmeans_model <- emmeans(res$model, specs = fac)
          print(emmeans_model)

          # Pairwise contrasts
          cat("\nPairwise contrasts for:", fac, "\n")
          contrast_results <- contrast(emmeans_model, method = "pairwise")
          contrast_results_tukey <- contrast(emmeans_model, method = "pairwise",adjust="tukey")
          print(summary(contrast_results))
          print(summary(contrast_results_tukey))
        }

        ## --- Conditional pairwise comparisons for ALL combinations ---
        if (length(ph_vars) >= 2) {
          combs <- combn(ph_vars, 2, simplify = FALSE)

          for (pair in combs) {
            facA <- pair[1]
            facB <- pair[2]

            ## A | B
            cat(
              "\nConditional pairwise comparisons (",
              facA, " | ", facB, "):\n", sep = ""
            )

            form1 <- as.formula(paste("pairwise ~", facA, "|", facB))
            pw1 <- emmeans(res$model, form1)
            print(pw1)
            cat("\nTukey adjustment\n")
            cat("\n############## Note:Tukey will be changed to Sidak for one set of pairwise comparisons #############\n")
            print(pw1,adjust="tukey")

            ## B | A
            cat(
              "\nConditional pairwise comparisons (",
              facB, " | ", facA, "):\n", sep = ""
            )

            form2 <- as.formula(paste("pairwise ~", facB, "|", facA))
            pw2 <- emmeans(res$model, form2)
            print(pw2)
            cat("\nTukey adjustment\n")
            cat("\n############## Note:Tukey will be changed to Sidak for one set of pairwise comparisons #############\n")
            print(summary(pw2, adjust = "tukey"))
          }
        }
      } else {
        cat("No post-hoc factors selected or available.\n")
      }
    }
  })
}
