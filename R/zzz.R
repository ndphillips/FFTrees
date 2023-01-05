# Final steps:

# .onAttach (from R base): Feedback message (on console) ------

.onAttach <- function(libname, pkgname) {

  version_nr <- utils::packageVersion("FFTrees")

  packageStartupMessage(in_grey("             "))
  packageStartupMessage(in_grey("   O         "))
  packageStartupMessage(in_grey("  / \\       "))
  packageStartupMessage(in_grey(" F   O       "))
  packageStartupMessage(in_grey("    / \\     "))
  packageStartupMessage(in_grey("   F   Trees "))
  packageStartupMessage(in_grey("             "))
  packageStartupMessage(in_grey("Welcome to ", in_blue(paste("FFTrees ", version_nr, sep = "")), "!", sep = ""))
  packageStartupMessage(in_grey(paste(in_darkgrey("FFTrees.guide()"), "opens the main guide."), sep = ""))

} # .onAttach().

# eof.
