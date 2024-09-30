#' Create common directory structure for analysis
#'
#' @export
fs_common_dir <- function(path = NULL, flavor = c("op", "gc"), create_project = TRUE) {
  flavor <- match.arg(flavor)
  if (is.null(path)) path <- getwd()

  if (flavor == "op") {
    dir_str <- c(
      "fig",
      "data/raw",
      "data/interim",
      "data/final",
      "data/modeling",
      "data/ref",
      "asset/email",
      "scripts", # where to put R scripts, usually with interactive use
      "src",     # where to put R functions for later sourcing
      "deliverable" # files to be sent out
    )
  } else if (flavor == "gc") {
    dir_str <- c(
      "Actuarial/Experience",
      "Actuarial/Exposure",
      "Actuarial/Ground-Up LR",
      "CatModeling/Exposure/DataLink",
      "CatModeling/Exposure/DataPrep",
      "CatModeling/Exposure/Information",
      "CatModeling/Models/AIR/Databases",
      "CatModeling/Models/AIR/Input",
      "CatModeling/Models/AIR/Output",
      "CatModeling/Models/APOTH/Databases",
      "CatModeling/Models/APOTH/Input",
      "CatModeling/Models/APOTH/Output",
      "CatModeling/Models/EQECAT/Databases",
      "CatModeling/Models/EQECAT/Input",
      "CatModeling/Models/EQECAT/Output",
      "CatModeling/Models/RMS/Databases",
      "CatModeling/Models/RMS/Input",
      "CatModeling/Models/RMS/Output",
      "CatModeling/SpecialRequests",
      "CatModeling/Working/Reports",
      "Deliverables/FTP",
      "Deliverables/ROLePlay",
      "Documentation/Correspondings",
      "Financial/RiskTransfer",
      "Metarisk",
      "OriginalData"
    )
  }

  Map(
    function(base_dir, sub_dir) {
      dir.create(file.path(base_dir, sub_dir), recursive = TRUE)
      cat("./", sub_dir, "\n", sep = "")
    },
    base_dir = path, sub_dir = dir_str
  )

  if (create_project) {
    rstudioapi::openProject(path, newSession = TRUE)
  }

  invisible(path)
}
