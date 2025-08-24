########################
# Add gene annotations #
########################

#' A function to add annotations to a table of gene counts.
#'
#' @param object A table of gene counts (rows: genes, columns: samples, rownames are ENSEMBL gene IDs).
#' @param reference A reference table with the annotations including a column named "geneID".
#' @param variables Character vector of columns in `reference` to add. If NULL (default), all columns except geneID are used.
#' @param data_frame Logical; if TRUE, coerce `object` to a data.frame first. Default: FALSE.
#' @returns A data frame equal to `object` with its original columns plus the selected annotation columns from `reference`. Row order is preserved; unmatched IDs yield `NA` in the new columns.
#' @examples
#' \dontrun{
#' counts <- matrix(c(10, 20, 30, 40), nrow = 2,
#'                  dimnames = list(c("ENSG000001", "ENSG000002"), c("S1", "S2")))
#'
#' ref <- data.frame(geneID  = c("ENSG000001", "ENSG000002"),
#'                   symbol  = c("TP53", "EGFR"),
#'                   biotype = c("protein_coding", "protein_coding"),
#'                   stringsAsFactors = FALSE)
#'
#' add_annotations(counts, ref, variables = c("symbol", "biotype"))
#'
#' # Use all variables and coerce to data.frame
#' add_annotations(counts, ref, data_frame = TRUE)
#' }
#' @export

add_annotations <- function(object, reference, variables = NULL, data_frame = FALSE){

  df <- if (data_frame) as.data.frame(object, stringsAsFactors = FALSE) else object
  df$geneID <- rownames(df)

  if (is.null(variables)) {
    variables <- setdiff(colnames(reference), "geneID")
  }

  index <- match(df$geneID, reference$geneID)
  df[, variables] <- reference[index, variables]

  return(df)
}
