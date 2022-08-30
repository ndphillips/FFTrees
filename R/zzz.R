# Final steps:

# Feedback message (on console):

.onAttach <- function(libname, pkgname) {

  version_nr <- utils::packageVersion("FFTrees")

  packageStartupMessage("")
  packageStartupMessage("   O      ")
  packageStartupMessage("  / \\     ")
  packageStartupMessage(" F   O  ")
  packageStartupMessage("    / \\   ")
  packageStartupMessage("   F   Trees ")
  packageStartupMessage("")
  packageStartupMessage(paste0("Welcome to FFTrees ", version_nr, "!"))
  packageStartupMessage("FFTrees.guide() opens the main guide.")

} # .onAttach().

# eof.
