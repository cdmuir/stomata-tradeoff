# Calculations of gsmax follow Sack and Buckley 2016 ----
biophysical_constant <- function(D_wv, v) set_units(D_wv / v, mol / m / s)

morphological_constant <- function(c, h, j) {
  set_units((pi * c ^ 2) / (j ^ 0.5 * (4 * h * j + pi * c)), 1)
} 

gmax <- function(b, m, d, s) {
  set_units(b * m * d * sqrt(s), mol / m ^ 2 / s)
}

# Functions for converting from D to U, R to S ----
D2U <- function(D) {

  # D must have units of 1 / mm ^ 2
  # U = 2 * Rhex where Rhex is major radius of a regular hexagon
  # Ahex = 3 * sqrt(3) / 2 * Rhex ^ 2
  Ahex <- 1 / D
  Rhex <- sqrt(2 * Ahex / (3 * sqrt(3)))
  U <- 2 * Rhex # in mm
  1e3 * U # in um

}

U2D <- function(U) {

  # U must have units of um
  Rhex <- U / 2
  Ahex <- 3 * sqrt(3) / 2 * Rhex ^ 2
  D <- 1 / Ahex # 1 / um ^ 2
  D * 1e6 # 1 / mm ^ 2

}

S2R <- function(S) {
  
  # S must be in units of um ^ 2
  sqrt(S / pi) # units of um
  
}

R2S <- function(R) {
  
  # r must be in units of um
  pi * R ^ 2 # units of um ^ 2
  
}

# Functions for numerical integration ----
# Minimum and maximum possible positions for y_pathogen, given x_pathogen, r, ue
get_ymin <- function(x, R, U) {
  
  stopifnot(R < 2 * U)
  stopifnot(x > sqrt(3) / 2 * R)
  stopifnot(x <= U / 2)
  ifelse(x < R, sqrt(R ^ 2 - x ^ 2), 0)
  
}

get_ymax <- function(x, R, U) {
  
  stopifnot(R < 2 * U)
  stopifnot(x > sqrt(3) / 2 * R)
  stopifnot(x <= U / 2)
  sqrt(3) / 3 * x
  
}

# Modified from function "r/functions-from-python.R" for speed
get_tangent_slopes1 <- function(R, x_c, y_c, x_p, y_p) {
  
  e1 <- (R**2 - x_c**2 + 2*x_c*x_p - x_p**2)
  e2 <- sqrt(-e1 + (y_c - y_p) ^ 2)
  e3 <- - x_c*y_c + x_c*y_p + x_p*y_c - x_p*y_p
  m1 <- (-R * e2 + e3)
  m2 <- (R * e2 + e3)
  
  c(m1, m2) / e1
  
} 

# Function calculate p_colonize as a function stomatal density (D) and size (S)
p_colonize <- function(D, S, H) {
  
  f_s <- drop_units(D * S)
  
  checkmate::assert_double(D, lower = 0, finite = TRUE, len = 1L)
  checkmate::assert_class(D, "units")
  
  # Calculate interstomatal distance from density
  D %<>% 
    set_units(1 / mm ^ 2) %>%
    drop_units()
  U <- D2U(D)
  
  # Calculate stomatal radius from stomatal size assuming circle
  checkmate::assert_double(S, lower = 0, finite = TRUE, len = 1L)
  checkmate::assert_class(S, "units")
  S %<>% 
    set_units(um ^ 2) %>%
    drop_units()
  R <- S2R(S)

  p_l <- p_locate(R = R, U = U, H = H)
  
  f_s + (1 - f_s) * p_l
  
}

# Function calculate p_locate as a function interstomatal distance (U) and radius (R)
p_locate <- function(...) {
  
  params <- list(...)
  res <- integral2(
    plocate_xys, 
    xmin = sqrt(3) / 2 * params$R, xmax = params$U / 2,
    ymin = 0, ymax = sqrt(3) / 6 * params$U, 
    vectorized = TRUE, 
    params = params
  )
  
  res$Q / (a1(params$R, params$U) + a2(params$R, params$U))
  
}
  
# Vectorized version of plocate_xy()
plocate_xys <- function(x_p, y_p, params) {
  
  map2_dbl(.x = as.vector(x_p), .y = as.vector(y_p),
           .f = plocate_xy, R = params$R, U = params$U, H = params$H) %>%
    array(dim = dim(x_p))
  
}

# Get theta_i between x_p, y_p and stomate i located at x_i, y_i
get_theta <- function(x_p, y_p, x_i, y_i, R) {

  e1 <- (R**2 - x_i**2 + 2*x_i*x_p - x_p**2)
  e2 <- sqrt(-e1 + (y_i - y_p) ^ 2)
  e3 <- - x_i*y_i + x_i*y_p + x_p*y_i - x_p*y_p
  m1 <- (-R * e2 + e3) / e1
  m2 <- (R * e2 + e3) /e1
  theta_i <- atan(-(m2 - m1) / (1 + (m1 * m2)))
  
  theta_i 
  
}

# Probability of locating stomate at starting position x_p, y_p
plocate_xy <- function(x_p, y_p, R, U, H) {
  
  x_c <- c(0, U / 2, U)
  y_c <- c(0, sqrt(3) / 2 * U, 0)
  ymin <- get_ymin(x_p, R, U)
  ymax <- get_ymax(x_p, R, U)
  
  if (y_p < ymin | y_p > ymax) {
    
    return(0)
    
  } else {
    
    # Stomate A (reference stomate)
    x_i <- x_c[1]
    y_i <- y_c[1]
    theta_i <- get_theta(x_p, y_p, x_i, y_i, R)
    v_i <- sqrt((x_i - x_p) ^ 2 + (y_i - y_p) ^ 2) - R
    pA <- exp(-H * v_i) * abs(theta_i) / (2 * pi)
    
    # Stomate B
    x_i <- x_c[2]
    y_i <- y_c[2]
    theta_i <- get_theta(x_p, y_p, x_i, y_i, R)
    v_i <- sqrt((x_i - x_p) ^ 2 + (y_i - y_p) ^ 2) - R
    pB <- exp(-H * v_i) * abs(theta_i) / (2 * pi)
    
    # Stomate C
    x_i <- x_c[3]
    y_i <- y_c[3]
    theta_i <- get_theta(x_p, y_p, x_i, y_i, R)
    v_i <- sqrt((x_i - x_p) ^ 2 + (y_i - y_p) ^ 2) - R
    pC <- exp(-H * v_i) * abs(theta_i) / (2 * pi)
    
    return(pA + pB + pC)
    
  }
  
}

# Deprecated, but maintained for checking and plotting ----
# fy1 calculates angles and can be used for integration and is much faster
# Angle (in radians) between two intersecting lines with slopes m1 and m2
get_angle <- function(R, x_c, y_c, x_p, y_p) {
  
  # Slopes of tangent lines from pathogen to stomate
  m <- get_tangent_slopes1(R, x_c, y_c, x_p, y_p)
  m1 <- m[1:3]
  m2 <- m[4:6]
  ret <- atan(-(m2 - m1) / (1 + (m1 * m2))) / (2 * pi)
  # stopifnot(length(ret) == 3)
  ret
  
}

# Integrate across x
fx <- function(x, R, U, H) {
  
  X_stomata <- c(0, U / 2, U)
  Y_stomata <- c(0, sqrt(3) * U / 2, 0)
  
  purrr::map_dbl(x, ~ {
    
    ymin <- get_ymin(.x, R, U)
    ymax <- get_ymax(.x, R, U)
    iy <- integrate(fy1, lower = ymin, upper = ymax, x_p = .x, R = R, 
                    x_c = X_stomata, y_c = Y_stomata, H = H)
    iy$value / (ymax - ymin)
    
  })
  
}

# Given x, function to integrate across y
fy <- function(y_p, x_p, R, x_c, y_c) {
  
  purrr::map(y_p, get_angle, R = R, x_c = x_c, y_c = y_c, x_p = x_p) %>%
    purrr::map_dbl(sum)
  
}

# Functions for simulating ----
## The focal region is broken up in area with reference stomate (a1, sqrt(3) / 2 * r to r) and beyond (a2, r to u/2). Total area a = a1 + a2.
# Area of focal region minus stomate
a <- function(R, U) {U ^ 2 / (8 * sqrt(3)) - pi * R ^ 2 / 12}

# Area from x = sqrt(3) / 2 * r to x = r:
a1 <- function(R, U) R ^ 2 / (2 * sqrt(3)) - pi * R ^ 2 / 12

# Area from x = r to x = u/2:
a2 <- function(R, U) (U ^ 2 - 4 * R ^ 2) / (8 * sqrt(3))

# PDF, CDF, and Quantile functions so that I can generate uniform random variables throughout the focal area

# PDF of a1
f1 <- function(x, R, U) {(x / sqrt(3) - sqrt(R ^ 2 - x ^ 2)) / a1(R, U)}

# CDF of a1
F1 <- function(x, R, U) {
  cons <- a1(R, U)
  y <- sqrt(R ^ 2 - x ^ 2)
  A <- (1 / 6) * (-3 * x * y - 3 * R ^ 2 * atan(x / y) + sqrt(3) * x ^ 2) + 
    pi * R ^ 2 / 6
  A / cons
}

# Quantile function for a1
Q1 <- function(p, R, U) {
  purrr::map_dbl(p, ~ {
    uniroot(function(.y, p) {F1(.y, R, U) - p}, 
            interval = c(sqrt(3) / 2, 1) * R, p = .x)$root
  })
}

# PDF of a2
f2 <- function(x, R, U) {8 * x / (U ^ 2 - 4 * R ^ 2)}

# CDF of a2
F2 <- function(x, R, U) {4 * (R ^ 2 - x ^ 2) / (4 * R ^ 2 - U ^ 2)}

# Quantile function for a2
Q2 <- function(p, R, U) sqrt(R ^ 2 - p / 4 * (4 * R ^ 2 - U ^ 2))

# PDF of total area
ft <- function(x, R, U) {
  area1 <- a1(R, U)
  area2 <- a2(R, U)
  purrr::map_dbl(x, ~{
    if (.x < R) {
      ret <- area1 * f1(.x, R, U)
    } else {
      ret <- area2 * f2(.x, R, U)
    }
    ret / (area1 + area2)
  })
  
}

# CDF of total area
Ft <- function(x, R, U) {
  area1 <- a1(R, U)
  area2 <- a2(R, U)
  p1 <- area1 / (area1 + area2)
  p2 <- 1 - p1
  purrr::map_dbl(x, ~{
    if (.x < R) {
      ret <- p1 * F1(.x, R, U)
    } else {
      ret <- p1 + p2 * F2(.x, R, U)
    }
    ret
  })
}

# Quantile function for total area
Qt <- function(p, R, U) {
  area1 <- a1(R, U)
  area2 <- a2(R, U)
  p1 <- area1 / (area1 + area2)
  p2 <- 1 - p1
  
  purrr::map_dbl(p, ~ {
    
    if (.x < p1) {
      ret <- Q1(.x / p1, R, U)
    } else {
      ret <- Q2((.x - p1) / p2, R, U)
    }
    
    ret
    
  })
  
}

# Random position generator
rpos <- function(n, R, U) {
  
  # X position
  x <- Qt(runif(n), R, U)
  
  # Y position
  y <- runif(n, get_ymin(x, R, U), get_ymax(x, R, U))
  
  data.frame(x_p = x, y_p = y)
  
}

# Plot a single simulation
plot_sim <- function(x_p, y_p, R, U, H) {
  
  X_stomata <-  c(0, U / 2, U)
  Y_stomata <- c(0, sqrt(3) / 2 * U, 0)
  
  # Lines between pathogen and stomatal tangents
  # slopes solved using sympy (see 'python/tangents.py')
  # (x_p, y_p) is position of pathogen
  # (x_c, y_c) is center of stomate
  m <- get_tangent_slopes(R, X_stomata, Y_stomata, x_p, y_p)
  
  # Point of tangency between stomata and pathogen path
  df_tangent_points <- 
    get_tangent_points(R, X_stomata, Y_stomata, x_p, y_p) %>%
    mutate(stomate = rep(LETTERS[1:3], 2))
  
  df_tangent_lines <- tibble(
    x = rep(x_p, 3),
    y = rep(y_p, 3),
    stomate = LETTERS[1:3]
  ) %>% bind_rows(., df_tangent_points, .)
  
  df_triangle <- tibble(
    x = c(X_stomata, 0, U / 4, U / 2, U / 2, U / 2, 3 * U / 4),
    y = c(Y_stomata, 0, sqrt(3) * U / 4, U / 3, 0, U / 3, sqrt(3) * U / 4),
    stomate = NA
  )
  
  unit_circle <- crossing(x = seq(-1, 1, length.out = 1e2), b = c(-1, 1)) %>%
    mutate(y = b * sqrt(1 - x ^ 2)) %>%
    arrange(b, x)
  
  df_stomata <- crossing(
    unit_circle, 
    nesting(
      xmid = X_stomata, 
      ymid = Y_stomata, 
      stomate = LETTERS[1:3]
    )
  ) %>%
    mutate(
      x = (x * R) + xmid,
      y = (y * R) + ymid
    )
  
  p <- fy1(x_p, y_p, R, X_stomata, Y_stomata, H)
  
  ggplot(df_stomata, aes(x, y, color = stomate)) +
    geom_path(data = df_triangle) +
    geom_polygon(fill = "white", alpha = 0.1) +
    geom_path(data = df_tangent_lines) +
    geom_point(data = df_tangent_points, size = 3, pch = 21, fill = "white") +
    geom_point(data = data.frame(x = x_p, y = y_p, stomate = NA),
               color = "black") +
    labs(title = glue(" P_locate = {p}", p = round(p, 2))) +
    coord_equal() +
    theme_nothing() +
    theme(title = element_text())
  
}

# Functions for scaling analysis ----
find_S <- function(target_p, D, H) {
  
  max_S <- drop_units(set_units(1 / D, um ^ 2))
  res <- uniroot(.f, lower = 0, upper = max_S, target_p = target_p, D = D, H = H)
  set_units(res$root, um ^ 2)
  
}

.f <- function(.x, target_p, D, H) {
  
  S <- set_units(.x, um ^ 2)
  p <- p_colonize(D, S, H)
  p - target_p
  
}