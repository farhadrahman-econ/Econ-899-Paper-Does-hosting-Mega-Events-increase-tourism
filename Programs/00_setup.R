# ECON 899 REPLICATION PACKAGE
# 00_setup.R


required_packages <- c(
  "tidyverse",
  "fixest",
  "WDI"
)



installed_packages <- rownames(installed.packages())

packages_to_install <- setdiff(
  required_packages,
  installed_packages
)



if (length(packages_to_install) > 0) {
  
  install.packages(
    packages_to_install,
    repos = "https://cloud.r-project.org"
  )
  
} else {
  
  message("All required R packages are already installed.")
  
}



installation_check <- vapply(
  required_packages,
  requireNamespace,
  quietly = TRUE,
  FUN.VALUE = logical(1)
)

if (!all(installation_check)) {
  
  stop(
    paste(
      "The following packages could not be loaded:",
      paste(
        required_packages[!installation_check],
        collapse = ", "
      )
    )
  )
}




message("ECON 899 replication-package setup completed successfully.")
