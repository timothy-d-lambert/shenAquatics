#' defineRoot
#'
#' Defines the shenAquatics directory path.
#'
#' @return path to the shenAquatics directory (which contains data, etc.)
#'
#' @export

defineRoot <- function() {
  # root <- system.file(package = "shenAquatics") # on T. Lambert's work PC, this points to "C:/Users/tlambert/AppData/Local/R/win-library/4.4/shenAquatics", which does not contain the
  root <- "C:/Users/tlambert/OneDrive - DOI/Documents/Repos/shenAquatics"
  return(root)
}
