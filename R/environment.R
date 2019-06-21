## Code borrowed from inlabru
## https://github.com/fbachl/inlabru/blob/48607c3ae02fef46425e1988fb3ad5a15f52a0c3/R/environment.R
iinla.env = new.env()
iinla.env$log = sprintf("inlabru @ %s", date())

.onAttach <- function(libname, pkgname) {
  if (length(find.package("INLA", quiet = TRUE)) == 0) {
    iinla.env$inla.installed = FALSE
  } else {
    iinla.env$inla.installed = TRUE
  }

}

requireINLA = function(){
  tc = tryCatch(attachNamespace("INLA"), error = function(x){})
}
