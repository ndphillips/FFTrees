.onAttach <- function(libname, pkgname) {
  packageStartupMessage("   O      ")
  packageStartupMessage("  / \\     ")
  packageStartupMessage(" F   O  ")
  packageStartupMessage("    / \\   ")
  packageStartupMessage("   F   T  ")
  packageStartupMessage(paste0("FFTrees ", utils::packageVersion("FFTrees"), ". Email: Nathaniel.D.Phillips.is@gmail.com"))
  packageStartupMessage("FFTrees.guide() opens the guide. Citation info at citation('FFTrees')")
}
