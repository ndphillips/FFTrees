# Final steps:

# Feedback message (on console):

.onAttach <- function(libname, pkgname) {

  version_nr <- utils::packageVersion("FFTrees")

  packageStartupMessage("             ")
  packageStartupMessage("   O         ")
  packageStartupMessage("  / \\       ")
  packageStartupMessage(" F   O       ")
  packageStartupMessage("    / \\     ")
  packageStartupMessage("   F   Trees ")
  packageStartupMessage("             ")
  packageStartupMessage(crayon::silver("Welcome to ", crayon::blue(paste("FFTrees ", version_nr, sep = "")), "!", sep = ""))
  packageStartupMessage(crayon::silver("FFTrees.guide() opens the main guide."))

} # .onAttach().

# eof.
