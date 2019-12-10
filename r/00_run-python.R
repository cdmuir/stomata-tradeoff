# The output from this script is included with the repo, so there is no need to rerun, but I have provided the code for transparency
library(glue)
library(reticulate)
use_python('/anaconda3/bin/python', required = TRUE)
source_python("python/tangents.py")

get_tangent_slopes <-
  glue("get_tangent_slopes <- function(R, x_c, y_c, x_p, y_p) {{
       m1 <- {m1}
       m2 <- {m2}
       c(m1, m2)
     }}", m1 = slopes[[1]], m2 = slopes[[2]]) 

get_tangent_points <-
  glue("get_tangent_points <- function(R, x_c, y_c, x_p, y_p) {{
       x1 <- {x1}
       x2 <- {x2}
       y1 <- {y1}
       y2 <- {y2}
       data.frame(
         x = c(x1, x2),
         y = c(y1, y2)
       )
     }}", x1 = x1, x2 = x2, y1 = y1, y2 = y2) 

cat(get_tangent_slopes, "\n", get_tangent_points, file = "r/functions-from-python.R")
