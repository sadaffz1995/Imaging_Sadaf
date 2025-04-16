# Install and initialize renv
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}
library(renv)

# Initialize renv project (only run once per project)
renv::init()

# Install magick inside the renv environment
install.packages("magick")
library(magick)



# Save the environment state (creates/updates renv.lock)
renv::snapshot()

#Now I need to read the package documentation to see how I can convert this to bitmap image where I can see the raw velue of each pixel

help(package = "magick")


#print(img)
# Load necessary libraries
library(renv)
library(magick)

# Initialize renv project (only run once per project)
if (!file.exists("renv.lock")) {
  renv::init()
}

# Install magick inside the renv environment
if (!requireNamespace("magick", quietly = TRUE)) {
  install.packages("magick")
}
library(magick)

# Step 1: Read the image
img <- image_read("C:/Users/sadaf/Downloads/Test_image.jpg")

# Step 2: Extract RGB data from the image
img_data <- image_data(img)

# Step 3: Define helper functions for hex conversion
hex_to_int <- function(hex) {
  as.integer(strtoi(hex, base = 16))
}

int_to_hex <- function(int) {
  toupper(sprintf("%02x", int))
}

# Step 4: Define the grayscale conversion function using luminance formula
to_greyscale <- function(r, g, b) {
  r_val <- hex_to_int(r)
  g_val <- hex_to_int(g)
  b_val <- hex_to_int(b)

  # Apply luminance formula
  grey_val <- round(0.299 * r_val + 0.587 * g_val + 0.114 * b_val)
  grey_hex <- int_to_hex(grey_val)

  return(c(grey_hex, grey_hex, grey_hex))
}

# Step 5: Define the apply_filter function
apply_filter <- function(img_data, filter_function) {
  # Convert to character to assign hex strings
  filtered <- as.character(img_data)
  dim(filtered) <- dim(img_data)  # keep same 3D shape

  width <- dim(img_data)[2]
  height <- dim(img_data)[3]

  for (x in 1:width) {
    for (y in 1:height) {
      r <- img_data[1, x, y]
      g <- img_data[2, x, y]
      b <- img_data[3, x, y]

      new_rgb <- filter_function(r, g, b)

      filtered[1, x, y] <- new_rgb[1]  # R
      filtered[2, x, y] <- new_rgb[2]  # G
      filtered[3, x, y] <- new_rgb[3]  # B
    }
  }

  return(filtered)
}

# Step 6: Apply the grayscale filter
grey_data <- apply_filter(img_data, to_greyscale)

# Step 7: Convert the modified data back to a magick image
# Rearrange dimensions: from [3, width, height] to [height, width, 3]
rgb_array <- aperm(grey_data, c(3, 2, 1))

# Initialize an empty matrix to store color strings
height <- dim(rgb_array)[1]
width <- dim(rgb_array)[2]
color_matrix <- matrix(NA, nrow = height, ncol = width)

# Combine R, G, B hex values into color strings
for (i in 1:height) {
  for (j in 1:width) {
    r <- rgb_array[i, j, 1]
    g <- rgb_array[i, j, 2]
    b <- rgb_array[i, j, 3]
    color_matrix[i, j] <- paste0("#", r, g, b)
  }
}

# Convert the matrix to a raster object
raster_img <- as.raster(color_matrix)

# Read the raster object into a magick image
grey_img <- image_read(raster_img)

# Step 8: Display the image
print(grey_img)

# Step 9: Save the environment state (creates/updates renv.lock)
renv::snapshot()


# Convert hex to integer
hex_to_int <- function(hex) {
  as.integer(strtoi(hex, base = 16))
}

# Convert integer to hex
int_to_hex <- function(int) {
  toupper(sprintf("%02x", int))
}

# Grayscale filter using luminance method
to_greyscale <- function(r, g, b) {
  r_val <- hex_to_int(r)
  g_val <- hex_to_int(g)
  b_val <- hex_to_int(b)

  grey_val <- round(0.299 * r_val + 0.587 * g_val + 0.114 * b_val)
  grey_hex <- int_to_hex(grey_val)

  return(c(grey_hex, grey_hex, grey_hex))
}







# Function to apply thresholding to an image
apply_threshold <- function(img_data, cutoff = 127, low_color = c("00", "00", "00"), high_color = c("FF", "FF", "FF")) {
  # Convert to character to assign hex strings
  filtered <- as.character(img_data)
  dim(filtered) <- dim(img_data)  # keep same 3D shape

  width <- dim(img_data)[2]
  height <- dim(img_data)[3]

  for (x in 1:width) {
    for (y in 1:height) {
      r <- img_data[1, x, y]
      g <- img_data[2, x, y]
      b <- img_data[3, x, y]

      # Convert hex to integer
      r_val <- as.integer(strtoi(r, base = 16))
      g_val <- as.integer(strtoi(g, base = 16))
      b_val <- as.integer(strtoi(b, base = 16))

      # Calculate grayscale value using luminance formula
      grey_val <- round(0.299 * r_val + 0.587 * g_val + 0.114 * b_val)

      # Apply threshold
      if (grey_val < cutoff) {
        new_rgb <- low_color
      } else {
        new_rgb <- high_color
      }

      filtered[1, x, y] <- new_rgb[1]  # R
      filtered[2, x, y] <- new_rgb[2]  # G
      filtered[3, x, y] <- new_rgb[3]  # B
    }
  }

  return(filtered)
}






# Apply the thresholding function
thresholded_data <- apply_threshold(img_data, cutoff = 127)

# Rearrange dimensions: from [3, width, height] to [height, width, 3]
rgb_array <- aperm(thresholded_data, c(3, 2, 1))

# Initialize an empty matrix to store color strings
height <- dim(rgb_array)[1]
width <- dim(rgb_array)[2]
color_matrix <- matrix(NA, nrow = height, ncol = width)

# Combine R, G, B hex values into color strings
for (i in 1:height) {
  for (j in 1:width) {
    r <- rgb_array[i, j, 1]
    g <- rgb_array[i, j, 2]
    b <- rgb_array[i, j, 3]
    color_matrix[i, j] <- paste0("#", r, g, b)
  }
}

# Convert the matrix to a raster object
raster_img <- as.raster(color_matrix)

# Read the raster object into a magick image
thresholded_img <- image_read(raster_img)

# Display the image
print(thresholded_img)






apply_filter <- function(img_data, filter_function) {
  # Convert to character to assign hex strings
  filtered <- as.character(img_data)
  dim(filtered) <- dim(img_data)  # keep same 3D shape

  width <- dim(img_data)[2]
  height <- dim(img_data)[3]

  for (x in 1:width) {
    for (y in 1:height) {
      r <- img_data[1, x, y]
      g <- img_data[2, x, y]
      b <- img_data[3, x, y]

      new_rgb <- filter_function(r, g, b)

      filtered[1, x, y] <- new_rgb[1]  # R
      filtered[2, x, y] <- new_rgb[2]  # G
      filtered[3, x, y] <- new_rgb[3]  # B
    }
  }

  return(filtered)
}





# Read the image
img <- image_read("C:/Users/sadaf/Downloads/Test_image.jpg")

# Extract RGB data from the image
img_data <- image_data(img)

# Apply the grayscale filter
grey_data <- apply_filter(img_data, to_greyscale)

# Rearrange dimensions: from [3, width, height] to [height, width, 3]
rgb_array <- aperm(grey_data, c(3, 2, 1))

# Initialize an empty matrix to store color strings
height <- dim(rgb_array)[1]
width <- dim(rgb_array)[2]
color_matrix <- matrix(NA, nrow = height, ncol = width)

# Combine R, G, B hex values into color strings
for (i in 1:height) {
  for (j in 1:width) {
    r <- rgb_array[i, j, 1]
    g <- rgb_array[i, j, 2]
    b <- rgb_array[i, j, 3]
    color_matrix[i, j] <- paste0("#", r, g, b)
  }
}

# Convert the matrix to a raster object
raster_img <- as.raster(color_matrix)

# Read the raster object into a magick image
filtered_img <- image_read(raster_img)

# Display the image
print(filtered_img)






# Convert hex to integer
hex_to_int <- function(hex) {
  as.integer(strtoi(hex, base = 16))
}

# Function to apply thresholding to an image
apply_custom_threshold <- function(img_data, cutoff = 128, low_color = c("00", "00", "00"), high_color = c("FF", "00", "FF")) {
  # Convert to character to assign hex strings
  filtered <- as.character(img_data)
  dim(filtered) <- dim(img_data)  # keep same 3D shape

  width <- dim(img_data)[2]
  height <- dim(img_data)[3]

  for (x in 1:width) {
    for (y in 1:height) {
      r <- img_data[1, x, y]
      g <- img_data[2, x, y]
      b <- img_data[3, x, y]

      # Convert hex to integer
      r_val <- hex_to_int(r)
      g_val <- hex_to_int(g)
      b_val <- hex_to_int(b)

      # Calculate grayscale value using luminance formula
      grey_val <- round(0.299 * r_val + 0.587 * g_val + 0.114 * b_val)

      # Apply threshold
      if (grey_val < cutoff) {
        new_rgb <- low_color
      } else {
        new_rgb <- high_color
      }

      filtered[1, x, y] <- new_rgb[1]  # R
      filtered[2, x, y] <- new_rgb[2]  # G
      filtered[3, x, y] <- new_rgb[3]  # B
    }
  }

  return(filtered)
}








# Load necessary library
library(magick)

# Step 1: Read the image
img <- image_read("C:/Users/sadaf/Downloads/Test_image.jpg")

# Step 2: Extract RGB data from the image
img_data <- image_data(img)

# Step 3: Apply the custom thresholding function
thresholded_data <- apply_custom_threshold(img_data, cutoff = 128)

# Step 4: Rearrange dimensions: from [3, width, height] to [height, width, 3]
rgb_array <- aperm(thresholded_data, c(3, 2, 1))

# Step 5: Initialize an empty matrix to store color strings
height <- dim(rgb_array)[1]
width <- dim(rgb_array)[2]
color_matrix <- matrix(NA, nrow = height, ncol = width)

# Step 6: Combine R, G, B hex values into color strings
for (i in 1:height) {
  for (j in 1:width) {
    r <- rgb_array[i, j, 1]
    g <- rgb_array[i, j, 2]
    b <- rgb_array[i, j, 3]
    color_matrix[i, j] <- paste0("#", r, g, b)
  }
}

# Step 7: Convert the matrix to a raster object
raster_img <- as.raster(color_matrix)

# Step 8: Read the raster object into a magick image
thresholded_img <- image_read(raster_img)

# Step 9: Display the image
print(thresholded_img)


