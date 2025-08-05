library(ggplot2)
library(readr)
library(dplyr)

############ MENHADEN OG PROPOSAL ################
menhaden_base <- data.frame(
  x = c(17.34,	19.42,	25.94,	29.79),
  y = c(0, 1, 1, 0)
)

ggplot(menhaden_base, aes(x = x, y = y)) +
  geom_polygon(fill = "skyblue", color = "black") +
  xlim(15, 32) +
  geom_vline(xintercept = 19.42, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 25.94, linetype = "dashed", color = "red") +
  labs(title = "Menhaden Base Run")

#--------

menhaden_wide <- data.frame(
  x = c(16.3,	19.42,	25.94,	31.72),
  y = c(0, 1, 1, 0)
)

ggplot(menhaden_wide, aes(x = x, y = y)) +
  geom_polygon(fill = "lightpink", color = "black") +
  xlim(15, 32) +
  geom_vline(xintercept = 19.42, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 25.94, linetype = "dashed", color = "red") +
  labs(title = "Menhaden Wide Run")

#-----------

menhaden_narrow <- data.frame(
  x = c(18.38,	19.42,	25.94,	27.9),
  y = c(0, 1, 1, 0)
)

ggplot(menhaden_narrow, aes(x = x, y = y)) +
  geom_polygon(fill = "lightyellow", color = "black") +
  xlim(15, 32) +
  geom_vline(xintercept = 19.42, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 25.94, linetype = "dashed", color = "red") +
  labs(title = "Menhaden Narrow Run")

############ MENHADEN PROPOSAL 1 ################

menhaden_base <- data.frame(
  x = c(17.34,	19.42,	25.94,	29.79),
  y = c(0, 1, 1, 0)
)

ggplot(menhaden_base, aes(x = x, y = y)) +
  geom_polygon(fill = "lightgreen", color = "black") +
  xlim(11, 35) +
  geom_vline(xintercept = 19.42, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 25.94, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 17.34, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 29.79, linetype = "dashed", color = "black") +
  labs(title = "Menhaden Base Run")

#------

menhaden_wide_rec <- data.frame(
  x = c(17.34,	17.35,	29.79,	29.79),
  y = c(0, 1, 1, 0)
)

ggplot(menhaden_wide_rec, aes(x = x, y = y)) +
  geom_polygon(fill = "lightpink", color = "black") +
  xlim(11, 35) +
  geom_vline(xintercept = 19.42, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 25.94, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 17.34, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 29.79, linetype = "dashed", color = "black") +
  labs(title = "Menhaden Wide Rectangle Run")
#-----------

menhaden_narrow_rec <- data.frame(
  x = c(19.42,	19.42,	25.94,	25.94),
  y = c(0, 1, 1, 0)
)

ggplot(menhaden_narrow_rec, aes(x = x, y = y)) +
  geom_polygon(fill = "lightpink", color = "black") +
  xlim(11, 35) +
  geom_vline(xintercept = 19.42, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 25.94, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 17.34, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 29.79, linetype = "dashed", color = "black") +
  labs(title = "Menhaden Narrow Rectangle Run")

#-------------

menhaden_triangle <- data.frame(
  x = c(19.42, 22.68,	22.68,	25.94),
  y = c(0, 1, 1, 0)
)

ggplot(menhaden_triangle, aes(x = x, y = y)) +
  geom_polygon(fill = "lightyellow", color = "black") +
  xlim(11, 35) +
  geom_vline(xintercept = 19.42, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 25.94, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 17.34, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 29.79, linetype = "dashed", color = "black") +
  labs(title = "Menhaden Triangle Run")

#-----------------

menhaden_base_shift_right <- data.frame(
  x = c(22.34,	24.42, 30.94, 34.79),
  y = c(0, 1, 1, 0)
)

ggplot(menhaden_base_shift_right, aes(x = x, y = y)) +
  geom_polygon(fill = "orange", color = "black") +
  xlim(11, 35) +
  geom_vline(xintercept = 19.42, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 25.94, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 17.34, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 29.79, linetype = "dashed", color = "black") +
  labs(title = "Menhaden Shifted Right Run")

#---------------

menhaden_base_shift_left <- data.frame(
  x = c(12.34,	14.42,	21.94,	25.79),
  y = c(0, 1, 1, 0)
)

ggplot(menhaden_base_shift_left, aes(x = x, y = y)) +
  geom_polygon(fill = "lightblue", color = "black") +
  xlim(11, 35) +
  geom_vline(xintercept = 19.42, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 25.94, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 17.34, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 29.79, linetype = "dashed", color = "black") +
  labs(title = "Menhaden Shifted Left Run")

################### PROPOSAL 3 ##################################


menhaden_rec_right <- data.frame(
  x = c(24.42,	24.42,	29.94,	29.94),
  y = c(0, 1, 1, 0)
)

ggplot(menhaden_rec_right, aes(x = x, y = y)) +
  geom_polygon(fill = "pink3", color = "black") +
  xlim(11, 35) +
  geom_vline(xintercept = 19.42, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 25.94, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 17.34, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 29.79, linetype = "dashed", color = "black") +
  labs(title = "Menhaden Rectangle Shifted Right Run")

#----------

menhaden_rec_left <- data.frame(
  x = c(14.42,	14.42,	21.94,	21.94),
  y = c(0, 1, 1, 0)
)

ggplot(menhaden_rec_left, aes(x = x, y = y)) +
  geom_polygon(fill = "pink4", color = "black") +
  xlim(11, 35) +
  geom_vline(xintercept = 19.42, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 25.94, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 17.34, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 29.79, linetype = "dashed", color = "black") +
  labs(title = "Menhaden Rectangle Shifted Left Run")

#------------
menhaden_triangle_right <- data.frame(
  x = c(24.42, 27.68,	27.68, 30.94),
  y = c(0, 1, 1, 0)
)

ggplot(menhaden_triangle_right, aes(x = x, y = y)) +
  geom_polygon(fill = "yellow3", color = "black") +
  xlim(11, 35) +
  geom_vline(xintercept = 19.42, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 25.94, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 17.34, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 29.79, linetype = "dashed", color = "black") +
  labs(title = "Menhaden Triangle Shifted Right Run")

#------------
menhaden_triangle_left <- data.frame(
  x = c(14.42, 17.68,	17.68,	20.94),
  y = c(0, 1, 1, 0)
)

ggplot(menhaden_triangle_left, aes(x = x, y = y)) +
  geom_polygon(fill = "yellow4", color = "black") +
  xlim(11, 35) +
  geom_vline(xintercept = 19.42, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 25.94, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 17.34, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 29.79, linetype = "dashed", color = "black") +
  labs(title = "Menhaden Triangle Shifted Left Run")

#----------

menhaden_base <- data.frame(
  x = c(14.23, 17.35, 27.13, 32.90),
  y = c(0, 1, 1, 0)
)

ggplot(menhaden_base, aes(x = x, y = y)) +
  geom_polygon(fill = "lightgreen", color = "black") +
  xlim(11, 35) +
  geom_vline(xintercept = 19.42, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 25.94, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 17.34, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 29.79, linetype = "dashed", color = "black") +
  labs(title = "Menhaden Scaled 1.5x Base Run")
 
#-------------
menhaden_base <- data.frame(
  x = c(20.45, 21.49, 24.75, 26.68),
  y = c(0, 1, 1, 0)
)

ggplot(menhaden_base, aes(x = x, y = y)) +
  geom_polygon(fill = "green4", color = "black") +
  xlim(11, 35) +
  geom_vline(xintercept = 19.42, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 25.94, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 17.34, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 29.79, linetype = "dashed", color = "black") +
  labs(title = "Menhaden Scaled 0.5x Base Run")

