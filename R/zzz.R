.onAttach <- function(libname, pkgname) {
  packageStartupMessage("")
  packageStartupMessage("   O      ")
  packageStartupMessage("  / \\     ")
  packageStartupMessage(" F   O  ")
  packageStartupMessage("    / \\   ")
  packageStartupMessage(paste0("   F   Trees ", utils::packageVersion("FFTrees")))
  packageStartupMessage("")
  packageStartupMessage("Nathaniel.D.Phillips.is@gmail.com")
  packageStartupMessage("FFTrees.guide() opens the guide.")
}
