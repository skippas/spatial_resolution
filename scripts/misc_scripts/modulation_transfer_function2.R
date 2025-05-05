# Define spatial frequencies (v), from 0 to 30 cycles per degree
v <- seq(0, 0.18, length.out = 500)  # 500 points from 0 to 30 cycles/degree

# Define the receptive field width (delta_g), choose a reasonable value
delta_g <- 6.3  # Adjust this value based on the receptive field size

# Compute the MTF using the given formula: exp[-3.56 * (v * delta_g)^2]
mtf <- exp(-3.56 * (v * delta_g)^2)

# Plot the MTF
plot(v, mtf, type = "l", col = "blue", lwd = 2,
     xlab = "Spatial Frequency (cycles per degree)",
     ylab = "Contrast Modulation (MTF)",
     main = "MTF of Gaussian Receptive Field")
grid()
  

