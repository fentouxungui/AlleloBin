#' Save AlleloBin results to Excel
#'
#' Utility function to export allele binning results into an Excel workbook.
#' Each marker gets its own worksheet containing bin summary, allele calls,
#' and marker summary with strict decimal formatting.
#'
#' @param results List of marker results from allele_binning()
#' @param file Output Excel file path
#' @return None (writes Excel file to disk)
#' @export
#'
#' @examples
#' \dontrun{
#' results <- allele_binning(data, repeat_lengths, ploidy = 2)
#' save_results(results, "AlleloBin_Output.xlsx")
#' }
save_results <- function(results, file) {
  wb <- openxlsx::createWorkbook()

  for (marker in names(results)) {
    openxlsx::addWorksheet(wb, marker)

    # Bin summary
    bs <- results[[marker]]$bin_summary

    # Calls
    calls <- results[[marker]]$calls

    # Marker summary
    min_val     <- results[[marker]]$min
    max_val     <- results[[marker]]$max
    true_repeat <- results[[marker]]$true_input_repeat
    max_poss_alleles <- ((max_val - min_val) / true_repeat) + 1

    summary_df <- data.frame(
      Marker              = marker,
      MinAllele           = min_val,
      MaxAllele           = max_val,
      Individuals         = nrow(calls),
      MaxPossibleAlleles  = sprintf("%.2f", max_poss_alleles),
      RepeatLength        = true_repeat,
      QualityIndex        = sprintf("%.4f", results[[marker]]$quality_index),
      OptimalRepeatLength = sprintf("%.2f", results[[marker]]$optimal_repeat),
      AllelicDrift        = sprintf("%.2f", results[[marker]]$allelic_drift)
    )

    # Write sections with spacing
    openxlsx::writeData(wb, marker, bs, startRow = 1, startCol = 1, withFilter = TRUE)
    openxlsx::writeData(wb, marker, calls, startRow = nrow(bs) + 3, startCol = 1, withFilter = TRUE)
    openxlsx::writeData(wb, marker, summary_df, startRow = nrow(bs) + nrow(calls) + 6, startCol = 1)
  }

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}
