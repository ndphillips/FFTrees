# util_color.R:
# Color-related auxiliary/utility functions.
# ------------------------------------------

# (1) User feedback: ------

# Define RGB colors: ----

# g_25 <- round( 63/255, 0)  # dark grey
# g_50 <- round(127/255, 0)  # mid grey
# g_75 <- round(191/255, 0)  # light grey
#
# grey_25 <- rgb(g_25, g_25, g_25)
# grey_50 <- rgb(g_50, g_50, g_50)
# grey_75 <- rgb(g_75, g_75, g_75)


# Define crayon styles: ----

# u_f_ini <- crayon::make_style("black", colors = 256)      # "darkgrey"
# u_f_fin <- crayon::make_style("darkgreen", colors = 256)  # "black"
#
# u_f_msg <- crayon::make_style("darkgrey", colors = 256)   # normal message
# u_f_hig <- crayon::make_style("darkblue", colors = 256)   # highlighted msg


# Define cli styles: ----


# ANSI color styles:

in_grey      <- cli::make_ansi_style("grey50", grey = TRUE, colors = 256)
in_darkgrey  <- cli::make_ansi_style("grey33", grey = TRUE, colors = 256)
in_lightgrey <- cli::make_ansi_style("grey67", grey = TRUE, colors = 256)

in_red   <- cli::make_ansi_style("firebrick3",  colors = 256)  # "red4" "darkred"
in_green <- cli::make_ansi_style("forestgreen", colors = 256)  # "green4" "darkgreen"
in_blue  <- cli::make_ansi_style("dodgerblue4", colors = 256)  # "steelblue4" "darkblue"

in_dpnk <- cli::make_ansi_style("deeppink",    colors = 256)
in_dsbl <- cli::make_ansi_style("deepskyblue", colors = 256)


# User feedback messages:

u_f_ini <- cli::make_ansi_style("grey33", grey = TRUE, colors = 256)  # "black"
u_f_fin <- cli::make_ansi_style("darkgreen",           colors = 256)  # "black"

u_f_msg <- cli::make_ansi_style("grey50", grey = TRUE, colors = 256)  # normal message
u_f_hig <- cli::make_ansi_style("dodgerblue4",         colors = 256)  # highlighted msg


# ToDo: ------

# - etc.

# eof.
