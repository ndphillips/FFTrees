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
  packageStartupMessage(in_grey("Welcome to ", in_blue(paste("FFTrees ", version_nr, sep = "")), "!", sep = ""))
  packageStartupMessage(in_lightgrey(paste(in_darkgrey("FFTrees.guide()"), "opens the main guide."), sep = ""))

} # .onAttach().

# eof.
