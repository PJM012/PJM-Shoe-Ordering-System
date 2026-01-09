
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(DBI)
library(RSQLite)
library(ggplot2)
library(pool)
library(shinythemes)
library(digest)

icon <- function(name, ...) {
  shiny::icon(name, ...)
}

# --------------------- Database Setup ---------------------
db_pool <- dbPool(
  drv = RSQLite::SQLite(),
  dbname = "shoe_ordering.sqlite3",
  minSize = 1,
  maxSize = 10
)

# Password hashing function
simple_hash <- function(text) {
  digest::digest(text, algo = "sha256")
}

# Order status constants
ORDER_STATUSES <- list(
  PENDING = "Pending",
  PROCESSING = "Processing",
  TO_SHIP = "To Ship",
  SHIPPED = "Shipped",
  COMPLETED = "Completed",
  CANCELLED = "Cancelled"
)

# Function to get next status
get_next_status <- function(current_status) {
  switch(current_status,
         "Pending" = ORDER_STATUSES$PROCESSING,
         "Processing" = ORDER_STATUSES$TO_SHIP,
         "To Ship" = ORDER_STATUSES$SHIPPED,
         "Shipped" = ORDER_STATUSES$COMPLETED,
         current_status)
}
# Add these functions near other helper functions (around line 200)

# Function to format date properly - UPDATED FIXED VERSION
# Function to format date properly - FIXED CONSISTENT FORMAT
format_date <- function(date_str) {
  tryCatch({
    if(is.null(date_str) || is.na(date_str) || date_str == "") {
      return("")
    }
    
    # If it's already POSIXct
    if(inherits(date_str, "POSIXct") || inherits(date_str, "POSIXt")) {
      return(format(date_str, "%Y-%m-%d %I:%M:%S %p", tz = Sys.timezone()))
    }
    
    # Convert to POSIXct - handle various formats
    dt <- as.POSIXct(date_str, tz = "UTC")
    if(is.na(dt)) {
      dt <- as.POSIXct(date_str)
    }
    
    # Format consistently for ALL tables
    return(format(dt, "%Y-%m-%d %I:%M:%S %p", tz = Sys.timezone()))
  }, error = function(e) {
    return(date_str)
  })
}

# Function to format numbers with commas - UPDATED
format_number <- function(x, digits = 0) {
  if(is.numeric(x)) {
    return(formatC(x, format = "f", big.mark = ",", digits = digits))
  } else if(is.character(x)) {
    # Try to extract and format numbers from strings
    num_str <- gsub("[^0-9.-]", "", x)
    nums <- suppressWarnings(as.numeric(num_str))
    if(!is.na(nums) && !is.null(nums)) {
      return(formatC(nums, format = "f", big.mark = ",", digits = digits))
    }
  }
  return(x)
}

# Function to format currency with commas - UPDATED
format_currency <- function(x, symbol = "₱") {
  if(is.numeric(x)) {
    return(paste0(symbol, formatC(x, format = "f", big.mark = ",", digits = 2)))
  } else if(is.character(x)) {
    # Try to extract number from string
    num_str <- gsub("[^0-9.-]", "", x)
    num <- suppressWarnings(as.numeric(num_str))
    if(!is.na(num) && !is.null(num)) {
      return(paste0(symbol, formatC(num, format = "f", big.mark = ",", digits = 2)))
    }
  }
  return(x)
}

# Function to format date properly - UPDATED
format_date <- function(date_str) {
  tryCatch({
    # Parse the date and convert to local timezone
    if(grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$", date_str)) {
      # Already in correct format, just ensure timezone
      dt <- as.POSIXct(date_str, tz = "UTC")
      format(dt, "%Y-%m-%d %I:%M:%S %p", tz = Sys.timezone())
    } else {
      # Try to parse various formats
      dt <- as.POSIXct(date_str, tz = "UTC")
      if(is.na(dt)) {
        dt <- as.POSIXct(date_str)
      }
      format(dt, "%Y-%m-%d %I:%M:%S %p", tz = Sys.timezone())
    }
  }, error = function(e) {
    return(date_str)
  })
}

# Add this near other helper functions (around line 200)
myModalDialog <- function(..., options = list(backdrop = 'static', keyboard = FALSE)) {
  modalDialog(..., options = options)
}

# Database helper functions with safety wrapper
safe_db_operation <- function(operation) {
  conn <- poolCheckout(db_pool)
  on.exit({
    tryCatch({
      poolReturn(conn)
    }, error = function(e) {
      warning("Failed to return connection to pool: ", e$message)
    })
  })
  
  tryCatch({
    operation(conn)
  }, error = function(e) {
    stop(paste("Database operation failed:", e$message))
  })
}

dbExecutePool <- function(sql, params = NULL) {
  tryCatch({
    conn <- poolCheckout(db_pool)
    on.exit(poolReturn(conn))
    
    if (is.null(params)) {
      dbExecute(conn, sql)
    } else {
      dbExecute(conn, sql, params = params)
    }
  }, error = function(e) {
    stop(paste("Database execute error:", e$message))
  })
}

dbGetQueryPool <- function(sql, params = NULL) {
  tryCatch({
    conn <- poolCheckout(db_pool)
    on.exit(poolReturn(conn))
    
    if (is.null(params)) {
      result <- dbGetQuery(conn, sql)
    } else {
      result <- dbGetQuery(conn, sql, params = params)
    }
    return(result)
  }, error = function(e) {
    stop(paste("Database query error:", e$message))
  })
}

# Cached query function
cached_query <- function(sql, params = NULL, cache_time = 300) {
  cache_key <- paste0(sql, paste(params, collapse = ""))
  cache_file <- paste0("cache_", digest::digest(cache_key), ".rds")
  
  if(file.exists(cache_file)) {
    cache_info <- file.info(cache_file)
    if(difftime(Sys.time(), cache_info$mtime, units = "secs") < cache_time) {
      return(readRDS(cache_file))
    }
  }
  
  result <- dbGetQueryPool(sql, params = params)
  saveRDS(result, cache_file)
  return(result)
}

# Create tables
dbExecutePool("
CREATE TABLE IF NOT EXISTS users (
  user_id INTEGER PRIMARY KEY AUTOINCREMENT,
  username TEXT UNIQUE,
  password TEXT,
  role TEXT,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
)")

dbExecutePool("
CREATE TABLE IF NOT EXISTS shoes (
  shoe_id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT,
  price REAL,
  stock INTEGER,
  colors TEXT,
  sizes TEXT,
  image TEXT,
  available INTEGER,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
)")

dbExecutePool("
CREATE TABLE IF NOT EXISTS orders (
  order_id INTEGER PRIMARY KEY AUTOINCREMENT,
  customer_id INTEGER,
  status TEXT,
  total_price REAL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (customer_id) REFERENCES users(user_id)
)")

dbExecutePool("
CREATE TABLE IF NOT EXISTS order_items (
  order_item_id INTEGER PRIMARY KEY AUTOINCREMENT,
  order_id INTEGER,
  shoe_id INTEGER,
  quantity INTEGER,
  color TEXT,
  size TEXT,
  price REAL,
  FOREIGN KEY (order_id) REFERENCES orders(order_id),
  FOREIGN KEY (shoe_id) REFERENCES shoes(shoe_id)
)")

dbExecutePool("
CREATE TABLE IF NOT EXISTS sales (
  sale_id INTEGER PRIMARY KEY AUTOINCREMENT,
  order_id INTEGER,
  amount REAL,
  sale_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (order_id) REFERENCES orders(order_id)
)")

# NEW: Cart tables for persistent storage
dbExecutePool("
CREATE TABLE IF NOT EXISTS carts (
  cart_id INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id INTEGER UNIQUE,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (user_id) REFERENCES users(user_id)
)")

dbExecutePool("
CREATE TABLE IF NOT EXISTS cart_items (
  cart_item_id INTEGER PRIMARY KEY AUTOINCREMENT,
  cart_id INTEGER,
  shoe_id INTEGER,
  quantity INTEGER,
  color TEXT,
  size TEXT,
  added_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (cart_id) REFERENCES carts(cart_id),
  FOREIGN KEY (shoe_id) REFERENCES shoes(shoe_id)
)")

# Create indexes
dbExecutePool("CREATE INDEX IF NOT EXISTS idx_orders_customer ON orders(customer_id)")
dbExecutePool("CREATE INDEX IF NOT EXISTS idx_orders_status ON orders(status)")
dbExecutePool("CREATE INDEX IF NOT EXISTS idx_order_items_order ON order_items(order_id)")
dbExecutePool("CREATE INDEX IF NOT EXISTS idx_sales_date ON sales(sale_date)")
# NEW: Indexes for cart tables
dbExecutePool("CREATE INDEX IF NOT EXISTS idx_carts_user ON carts(user_id)")
dbExecutePool("CREATE INDEX IF NOT EXISTS idx_cart_items_cart ON cart_items(cart_id)")
dbExecutePool("CREATE INDEX IF NOT EXISTS idx_cart_items_shoe ON cart_items(shoe_id)")

# Create directories for image storage
if (!dir.exists("www")) dir.create("www")
if (!dir.exists("www/uploads")) {
  dir.create("www/uploads", recursive = TRUE)
}
if (!dir.exists("www/uploads/preview")) {
  dir.create("www/uploads/preview", recursive = TRUE)
}

# Create default image if it doesn't exist
default_image_path <- "www/default_shoe_image.jpg"
if (!file.exists(default_image_path)) {
  # Create a simple placeholder image (you can replace this with an actual image)
  download.file("https://images.unsplash.com/photo-1549298916-b41d501d3772?w=400&h=300&fit=crop",
                default_image_path, mode = "wb", quiet = TRUE)
}

# --------------------- Database Cart Functions ---------------------
get_or_create_user_cart <- function(user_id) {
  # Check if cart exists
  cart <- dbGetQueryPool(
    "SELECT cart_id FROM carts WHERE user_id = ?",
    params = list(user_id)
  )
  
  if (nrow(cart) == 0) {
    # Create new cart
    dbExecutePool(
      "INSERT INTO carts (user_id) VALUES (?)",
      params = list(user_id)
    )
    cart_id <- dbGetQueryPool("SELECT last_insert_rowid() as id")$id
  } else {
    cart_id <- cart$cart_id[1]
  }
  
  return(cart_id)
}

save_cart_to_db <- function(user_id, cart_items) {
  tryCatch({
    conn <- poolCheckout(db_pool)
    on.exit(poolReturn(conn))
    
    dbExecute(conn, "BEGIN TRANSACTION")
    
    # Get or create cart
    cart <- dbGetQuery(conn, 
                       "SELECT cart_id FROM carts WHERE user_id = ?",
                       params = list(user_id)
    )
    
    if (nrow(cart) == 0) {
      dbExecute(conn, 
                "INSERT INTO carts (user_id) VALUES (?)",
                params = list(user_id)
      )
      cart_id <- dbGetQuery(conn, "SELECT last_insert_rowid() as id")$id
    } else {
      cart_id <- cart$cart_id[1]
    }
    
    # Clear existing cart items
    dbExecute(conn,
              "DELETE FROM cart_items WHERE cart_id = ?",
              params = list(cart_id)
    )
    
    # Insert new cart items
    if (length(cart_items) > 0) {
      for (item in cart_items) {
        dbExecute(conn,
                  "INSERT INTO cart_items (cart_id, shoe_id, quantity, color, size) 
           VALUES (?, ?, ?, ?, ?)",
                  params = list(
                    cart_id,
                    item$shoe_id,
                    item$quantity,
                    item$color,
                    item$size
                  )
        )
      }
    }
    
    # Update cart timestamp
    dbExecute(conn,
              "UPDATE carts SET updated_at = datetime('now') WHERE cart_id = ?",
              params = list(cart_id)
    )
    
    dbExecute(conn, "COMMIT")
    return(TRUE)
    
  }, error = function(e) {
    if (exists("conn")) {
      dbExecute(conn, "ROLLBACK")
    }
    showNotification(paste("Error saving cart:", e$message), type = "error")
    return(FALSE)
  })
}

load_cart_from_db <- function(user_id) {
  tryCatch({
    # Get cart items from database
    cart_items <- dbGetQueryPool("
      SELECT 
        ci.shoe_id,
        ci.quantity,
        ci.color,
        ci.size,
        s.name,
        s.price,
        s.image,
        s.stock,
        s.available
      FROM cart_items ci
      JOIN carts c ON ci.cart_id = c.cart_id
      JOIN shoes s ON ci.shoe_id = s.shoe_id
      WHERE c.user_id = ? AND s.available = 1
      ORDER BY ci.added_at",
                                 params = list(user_id)
    )
    
    if (nrow(cart_items) == 0) {
      return(list())
    }
    
    # Convert to list format and validate stock
    cart_list <- list()
    for(i in 1:nrow(cart_items)) {
      item <- cart_items[i, ]
      
      # Check stock availability
      if (item$available == 1 && item$stock >= item$quantity) {
        cart_list[[length(cart_list) + 1]] <- list(
          shoe_id = item$shoe_id,
          name = item$name,
          price = item$price,
          color = item$color,
          size = item$size,
          quantity = item$quantity,
          image = item$image
        )
      }
    }
    
    return(cart_list)
    
  }, error = function(e) {
    showNotification(paste("Error loading cart:", e$message), type = "error")
    return(list())
  })
}

clear_cart_in_db <- function(user_id) {
  tryCatch({
    dbExecutePool("
      DELETE FROM cart_items 
      WHERE cart_id IN (SELECT cart_id FROM carts WHERE user_id = ?)",
                  params = list(user_id)
    )
    return(TRUE)
  }, error = function(e) {
    showNotification(paste("Error clearing cart:", e$message), type = "error")
    return(FALSE)
  })
}

# --------------------- Image Handling Functions ---------------------
save_uploaded_image <- function(file, shoe_id = NULL) {
  # Check if file is properly uploaded
  if(is.null(file)) {
    return("default_shoe_image.jpg")
  }
  
  # Validate file structure
  required_fields <- c("datapath", "name", "size", "type")
  if(!all(required_fields %in% names(file))) {
    showNotification("Invalid file upload", type = "warning")
    return("default_shoe_image.jpg")
  }
  
  # Check if file exists
  if(!file.exists(file$datapath)) {
    showNotification("Uploaded file not found", type = "warning")
    return("default_shoe_image.jpg")
  }
  
  # Check file size (limit to 5MB)
  if(file$size == 0) {
    return("default_shoe_image.jpg")
  }
  
  if(file$size > 5 * 1024 * 1024) { # 5MB limit
    showNotification("Image file is too large. Maximum size is 5MB.", 
                     type = "warning")
    return("default_shoe_image.jpg")
  }
  
  allowed_types <- c("image/jpeg", "image/jpg", "image/png", "image/gif", "image/webp")
  if(!file$type %in% allowed_types) {
    showNotification("Invalid file type. Please upload JPG, PNG, or GIF images.", 
                     type = "warning")
    return("default_shoe_image.jpg")
  }
  
  # Create uploads directory if it doesn't exist
  upload_dir <- "www/uploads"
  if (!dir.exists(upload_dir)) {
    dir.create(upload_dir, recursive = TRUE)
  }
  
  # Generate unique filename
  file_ext <- tools::file_ext(file$name)
  
  # Use shoe_id if provided for better organization, otherwise use timestamp
  if(!is.null(shoe_id)) {
    new_filename <- paste0("shoe_", shoe_id, "_", 
                           round(as.numeric(Sys.time()) * 1000), 
                           ".", tolower(file_ext))
  } else {
    # Get next shoe_id for naming
    last_id <- dbGetQueryPool("SELECT MAX(shoe_id) as max_id FROM shoes")$max_id
    new_shoe_id <- ifelse(is.na(last_id), 1, last_id + 1)
    new_filename <- paste0("shoe_", new_shoe_id, "_", 
                           round(as.numeric(Sys.time()) * 1000), 
                           ".", tolower(file_ext))
  }
  
  dest_path <- file.path(upload_dir, new_filename)
  
  # Try to copy the file
  success <- tryCatch({
    file.copy(file$datapath, dest_path, overwrite = TRUE)
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  if(success) {
    # Return just the filename (not full path)
    return(new_filename)
  } else {
    showNotification("Failed to save image. Using default image instead.", 
                     type = "warning")
    return("default_shoe_image.jpg")
  }
}

# Function to get image path for display
get_image_path <- function(image_name) {
  if(is.null(image_name) || is.na(image_name) || image_name == "") {
    return("default_shoe_image.jpg")
  }
  
  # Check if it's a URL
  if(grepl("^https?://", image_name)) {
    return(image_name)
  }
  
  # Check if file exists in uploads directory
  upload_path <- file.path("www/uploads", image_name)
  if(file.exists(upload_path)) {
    return(paste0("uploads/", image_name))
  }
  
  # Check if it's already a path
  if(grepl("^uploads/", image_name) && file.exists(file.path("www", image_name))) {
    return(image_name)
  }
  
  # Check if it's the default image
  if(image_name == "default_shoe_image.jpg" && file.exists("www/default_shoe_image.jpg")) {
    return("default_shoe_image.jpg")
  }
  
  # Fallback to default
  return("default_shoe_image.jpg")
}

# Clean up orphaned images
cleanup_orphaned_images <- function() {
  upload_dir <- "www/uploads"
  if(dir.exists(upload_dir)) {
    # Get all images in uploads directory
    image_files <- list.files(upload_dir, pattern = "\\.(jpg|jpeg|png|gif|webp)$", 
                              full.names = TRUE, recursive = FALSE)
    
    # Get images referenced in database
    used_images <- tryCatch({
      db_images <- dbGetQueryPool("SELECT image FROM shoes WHERE image IS NOT NULL")
      # Extract just filenames
      sapply(db_images$image, function(img) {
        if(grepl("^uploads/", img)) {
          basename(img)
        } else {
          img
        }
      })
    }, error = function(e) character(0))
    
    # Find orphaned images (not in database)
    orphaned <- image_files[!basename(image_files) %in% used_images]
    
    # Delete orphaned images older than 7 days (safety measure)
    if(length(orphaned) > 0) {
      file_info <- file.info(orphaned)
      old_orphaned <- orphaned[difftime(Sys.time(), file_info$mtime, units = "days") > 7]
      if(length(old_orphaned) > 0) {
        file.remove(old_orphaned)
      }
    }
  }
}

# --------------------- Cleanup Functions ---------------------
cleanup_preview_files <- function() {
  preview_dir <- "www/uploads/preview"
  if(dir.exists(preview_dir)) {
    # Delete files older than 1 hour
    files <- list.files(preview_dir, full.names = TRUE)
    if(length(files) > 0) {
      file_info <- file.info(files)
      old_files <- files[difftime(Sys.time(), file_info$mtime, units = "hours") > 1]
      if(length(old_files) > 0) {
        file.remove(old_files)
      }
    }
  }
}

# --------------------- Add Sample Data ---------------------
add_sample_data <- function() {
  # Check if users table is empty
  user_count <- dbGetQueryPool("SELECT COUNT(*) as count FROM users")$count
  if (user_count == 0) {
    sample_users <- data.frame(
      username = c('customer1', 'customer2', 'staff1', 'staff2'),
      password = c('pass123', 'pass123', 'admin123', 'admin123'),
      role = c('Customer', 'Customer', 'Staff', 'Staff')
    )
    
    for (i in 1:nrow(sample_users)) {
      dbExecutePool(
        "INSERT INTO users (username, password, role) VALUES (?, ?, ?)",
        params = list(sample_users$username[i], simple_hash(sample_users$password[i]), sample_users$role[i])
      )
    }
  }
  
  # Check if shoes table is empty
  shoe_count <- dbGetQueryPool("SELECT COUNT(*) as count FROM shoes")$count
  if (shoe_count == 0) {
    sample_shoes <- data.frame(
      name = c('Nike Air Max', 'Adidas Ultraboost', 'Puma RS-X', 
               'Converse Chuck Taylor', 'New Balance 574', 'Vans Old Skool'),
      price = c(129.99, 159.99, 89.99, 59.99, 84.99, 64.99),
      stock = c(50, 30, 40, 100, 25, 60),
      colors = c('Black,White,Blue', 'White,Black,Red', 'Black,Blue,Green',
                 'Black,White,Red,Navy', 'Gray,Navy,Green', 'Black,White,Checkerboard'),
      sizes = c('7,8,9,10,11,12', '8,9,10,11,12', '7,8,9,10',
                '6,7,8,9,10,11,12', '7,8,9,10,11', '7,8,9,10,11,12'),
      image = rep("default_shoe_image.jpg", 6),  # Use local default image
      available = rep(1, 6)
    )
    
    for (i in 1:nrow(sample_shoes)) {
      dbExecutePool(
        "INSERT INTO shoes (name, price, stock, colors, sizes, image, available) 
         VALUES (?, ?, ?, ?, ?, ?, ?)",
        params = list(
          sample_shoes$name[i],
          sample_shoes$price[i],
          sample_shoes$stock[i],
          sample_shoes$colors[i],
          sample_shoes$sizes[i],
          sample_shoes$image[i],
          sample_shoes$available[i]
        )
      )
    }
  }
}

add_sample_data()

# --------------------- Helper Functions ---------------------
# Product card module
product_card <- function(shoe_row) {
  shoe_id <- if(!is.null(shoe_row$shoe_id) && !is.na(shoe_row$shoe_id)) 
    as.character(shoe_row$shoe_id) else "0"
  
  name <- if(!is.null(shoe_row$name) && !is.na(shoe_row$name)) 
    shoe_row$name else "Unknown Product"
  
  price <- if(!is.null(shoe_row$price) && !is.na(shoe_row$price)) 
    as.numeric(shoe_row$price) else 0
  
  stock <- if(!is.null(shoe_row$stock) && !is.na(shoe_row$stock)) 
    as.numeric(shoe_row$stock) else 0
  
  # Get image path using the helper function
  image <- if(!is.null(shoe_row$image) && !is.na(shoe_row$image) && 
              nchar(shoe_row$image) > 0) 
    get_image_path(shoe_row$image) else "default_shoe_image.jpg"
  
  div(class = "card",
      tags$img(
        src = image, 
        style = "object-fit: cover; border-radius: 10px;",
        onerror = "this.onerror=null; this.src='default_shoe_image.jpg'"
      ),
      h4(name, style = "margin-top:15px; margin-bottom:10px;"),
      p(strong(paste0("₱", format_number(price))), 
        style = "font-size:20px; color:#1abc9c; margin-bottom:5px;"),
      p(paste0("In Stock: ", stock), 
        class = ifelse(stock > 10, "stock-ok", "stock-low"),
        style = "margin-bottom:15px;"),
      actionButton(
        paste0("add_cart_modal_", shoe_id),
        icon("cart-plus"), " Add to Cart",
        class = "btn-theme",
        style = "width:100%;",
        onclick = sprintf(
          "Shiny.setInputValue('show_cart_modal', %s, {priority:'event'})", 
          shoe_id
        )
      )
  )
}

# Helper function for cart merging
merge_carts <- function(existing_cart, new_cart) {
  # If existing cart is empty, return new cart
  if(length(existing_cart) == 0) return(new_cart)
  if(length(new_cart) == 0) return(existing_cart)
  
  merged_cart <- existing_cart
  
  for(new_item in new_cart) {
    found <- FALSE
    for(i in seq_along(merged_cart)) {
      existing_item <- merged_cart[[i]]
      if(as.numeric(existing_item$shoe_id) == as.numeric(new_item$shoe_id) &&
         existing_item$color == new_item$color &&
         existing_item$size == new_item$size) {
        # Merge quantities
        merged_cart[[i]]$quantity <- as.numeric(existing_item$quantity) + as.numeric(new_item$quantity)
        found <- TRUE
        break
      }
    }
    if(!found) {
      merged_cart <- c(merged_cart, list(new_item))
    }
  }
  
  return(merged_cart)
}

# In the dashboard_box function (around line 250), update to:
dashboard_box <- function(icon_name, value, label, gradient = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);") {
  div(
    class = "dashboard-card",
    style = paste0(
      gradient, 
      " display: flex; 
       flex-direction: column; 
       align-items: center; 
       justify-content: center;
       text-align: center;
       min-height: 180px;"
    ),
    
    div(
      style = "text-align: center; margin-bottom: 10px;",
      shiny::icon(icon_name, style = "font-size: 40px; opacity: 0.9;")
    ),
    
    div(
      style = "
        text-align: center; 
        font-size: 36px; 
        font-weight: bold; 
        margin: 15px 0;
        width: 100%;
        display: flex;
        justify-content: center;
        align-items: center;
        min-height: 50px;",
      span(class = "dashboard-value", value)
    ),
    
    div(
      style = "
        text-align: center; 
        font-size: 16px; 
        opacity: 0.9;
        width: 100%;
        min-height: 20px;",
      label
    )
  )
}

# --------------------- UI ---------------------
ui <- fluidPage(
  useShinyjs(),
  
  # Load modern fonts
  tags$head(
    # Google Fonts - Inter (modern sans-serif)
    tags$link(
      rel = "preconnect",
      href = "https://fonts.googleapis.com"
    ),
    tags$link(
      rel = "preconnect",
      href = "https://fonts.gstatic.com",
      crossorigin = "anonymous"
    ),
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap",
      rel = "stylesheet"
    ),
    
    # Font Awesome for icons
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"
    ),
    
    # Load external CSS
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    ),
    
    # Add viewport meta for responsive design
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1.0"
    ),
    
    # Add favicon (optional)
    tags$link(
      rel = "icon",
      type = "image/x-icon",
      href = "https://img.icons8.com/color/96/000000/sneakers.png"
    ),
    
    # Add JavaScript for CSS refresh after logout
    tags$script(HTML("
      Shiny.addCustomMessageHandler('refreshCSS', function(message) {
        // Force reapply CSS by toggling a class
        document.body.classList.remove('css-loaded');
        setTimeout(function() {
          document.body.classList.add('css-loaded');
        }, 10);
        
        // Reinitialize password toggles
        if (typeof initPasswordToggles === 'function') {
          initPasswordToggles();
        }
      });
    "))
  ),
  
  # Load external JavaScript
  tags$head(
    tags$script(src = "script.js")
  ),
  
  # Rest of your UI code...
  div(id = "login_container", uiOutput("login_ui")),
  hidden(div(id = "main_container", uiOutput("main_ui")))
)

server <- function(input, output, session){
  session_start_time <- reactiveVal(Sys.time())
  
  # Reactive values
  user_data <- reactiveValues(
    logged_in = FALSE,
    role = NULL,
    user_id = NULL,
    username = NULL,
    cart = list(),
    modal_shoe = NULL,
    modal_open = FALSE,
    cart_trigger = 0,
    is_logging_out = FALSE,
    is_logging_in = FALSE,  
    session_initialized = FALSE,
    show_logout_modal = FALSE
  )
  
  # ========== ENHANCED NOTIFICATION SYSTEM WITH QUEUING ==========
  # Queue for notifications to show after modal close
  notification_queue <- reactiveVal(list())
  
  # Function to show toast notifications with queuing
  show_notification <- function(message, type = "success", duration = 3000, wait_for_modal = FALSE) {
    # If modal is open and we should wait, add to queue
    if (wait_for_modal && user_data$modal_open) {
      current_queue <- notification_queue()
      current_queue[[length(current_queue) + 1]] <- list(
        message = message,
        type = type,
        duration = duration
      )
      notification_queue(current_queue)
      return()
    }
    
    # Otherwise show immediately
    session$sendCustomMessage(
      type = "showToast",
      message = list(
        text = message,
        type = type,
        duration = duration,
        waitForModal = wait_for_modal
      )
    )
  }
  
  # Function to send number formatting message
  format_dashboard_numbers <- function() {
    session$sendCustomMessage("formatNumbers", list())
  }
  
  apply_status_styles <- function() {
    session$sendCustomMessage("applyStatusStyles", list())
  }
  
  # Function to process notification queue when modal closes
  process_notification_queue <- function() {
    queue <- notification_queue()
    if (length(queue) > 0) {
      # Process with delay to ensure modal is fully closed
      delay(300, {
        for (item in queue) {
          show_notification(item$message, item$type, item$duration, wait_for_modal = FALSE)
        }
        # Clear queue
        notification_queue(list())
      })
    }
  }
  
  # Observe modal state changes
  observe({
    # When modal closes, process queued notifications
    if (!user_data$modal_open && length(notification_queue()) > 0) {
      process_notification_queue()
    }
  })
  
  # Add this observer for status updates
  observe({
    # Trigger when orders are updated
    input$mark_processing
    input$mark_to_ship
    input$mark_shipped
    input$mark_completed
    input$mark_cancelled
    
    # Apply status styles
    apply_status_styles()
  })
  
  # Also add to the order success modal
  observeEvent(input$make_order_btn, {
    # After showing order success modal
    delay(200, {
      apply_status_styles()
    })
  })
  
  # Fix add to cart modal layout
  observeEvent(input$show_cart_modal, {
    delay(200, {
      session$sendCustomMessage("fixModalLayout", list())
    })
  })
  
  # Add this observer to handle the cancel button
  observeEvent(input$cancel_add_cart, {
    removeModal()
    modal_state$is_open <- FALSE
    modal_state$shoe_id <- NULL
    modal_state$processing <- FALSE
    user_data$modal_open <- FALSE
    current_modal_shoe_id(NULL)
  })
  
  # Function to show JavaScript alert-style notification
  show_js_alert <- function(title, message, type = "info") {
    session$sendCustomMessage(
      type = "showAlert",
      message = list(
        title = title,
        text = message,
        icon = type
      )
    )
  }
  
  # Function to send number formatting message
  format_dashboard_numbers <- function() {
    session$sendCustomMessage("formatNumbers", list())
  }
  
  # Observe cart changes for notifications
  observe({
    # Trigger when cart changes
    user_data$cart_trigger
    
    if (user_data$cart_trigger > 0 && length(user_data$cart) > 0) {
      # Send cart update notification
      session$sendCustomMessage(
        type = "cartUpdated",
        message = list(count = length(user_data$cart))
      )
    }
  })
  
  # ========== END NOTIFICATION SYSTEM ==========
  
  modal_state <- reactiveValues(
    is_open = FALSE,
    shoe_id = NULL,
    processing = FALSE
  )
  
  current_modal_shoe_id <- reactiveVal(NULL)
  sales_report_trigger <- reactiveVal(0)  # For sales report filtering
  
  # Loading states
  loading_state <- reactiveValues(
    saving_cart = FALSE,
    placing_order = FALSE,
    updating_order = FALSE
  )
  
  # Refresh triggers
  refresh_trigger <- reactiveValues(
    shoes = 0,
    orders = 0
  )
  
  # Order refresh trigger
  order_refresh_trigger <- reactiveVal(0)
  
  # --------------------- Login UI ---------------------
  render_login_ui <- function() {
    output$login_ui <- renderUI({
      tags$div(
        class = "login-box",
        style = "text-align: center;",
        tags$div(
          class = "login-title-container",
          icon("shoe-prints", style = "font-size: 36px;"),
          h2("PJM SHOE ORDERING SYSTEM", style = "color:#2c3e50; margin:0;")
        ),
        p("Welcome! Please select your login option", style = "color:#666; margin:30px 0;"),
        
        tags$div(
          class = "login-buttons-container",
          actionButton("login_cust", 
                       tags$div(
                         icon("user", style = "margin-right:10px;"),
                         "Login as Customer"
                       ), 
                       width = '100%', 
                       class = "btn-theme", 
                       style = "margin-bottom:15px; height:50px; font-size:16px;"),
          
          actionButton("login_staff", 
                       tags$div(
                         icon("user-tie", style = "margin-right:10px;"),
                         "Login as Staff"
                       ), 
                       width = '100%', 
                       class = "btn-theme", 
                       style = "margin-bottom:15px; height:50px; font-size:16px;"),
          
          actionButton("register_btn", 
                       tags$div(
                         icon("user-plus", style = "margin-right:10px;"),
                         "Register an Account"
                       ), 
                       width = '100%', 
                       class = "btn-theme", 
                       style = "height:50px; font-size:16px;")
        )
      )
    })
    
    output$main_ui <- renderUI({ NULL })
    shinyjs::hide("main_container")
    shinyjs::show("login_container")
  }
  
  render_login_ui()
  
  observeEvent(input$register_btn, {
    output$login_ui <- renderUI({
      tags$div(
        class = "login-box registration-modal",  # Changed to registration-modal
        style = "text-align: center; max-height: 90vh; overflow: hidden;",
        
        # Scrollable container
        tags$div(
          class = "registration-scroll-container",
          style = "max-height: 70vh; overflow-y: auto; padding-right: 10px;",
          
          h3("Create Customer Account"),
          p("Register to start shopping for shoes", style = "color: #666; margin-bottom: 25px;"),
          
          div(
            class = "form-group",
            tags$label("Username", `for` = "reg_user"),
            textInput(
              "reg_user", 
              label = NULL, 
              placeholder = "Choose a username (min 3 characters)",
              width = "100%"
            )
          ),
          
          div(
            class = "form-group",
            tags$label("Password", `for` = "reg_pass"),
            tags$div(
              class = "password-input-wrapper",
              passwordInput(
                "reg_pass", 
                label = NULL, 
                placeholder = "Create a password (min 4 characters)",
                width = "100%"
              ),
              tags$button(
                class = "password-toggle-btn",
                type = "button",
                icon("eye"),
                onclick = "togglePasswordVisibility(this)"
              )
            )
          ),
          
          div(
            class = "form-group",
            tags$label("Confirm Password", `for` = "reg_pass_confirm"),
            tags$div(
              class = "password-input-wrapper",
              passwordInput(
                "reg_pass_confirm", 
                label = NULL, 
                placeholder = "Re-enter your password",
                width = "100%"
              ),
              tags$button(
                class = "password-toggle-btn",
                type = "button",
                icon("eye"),
                onclick = "togglePasswordVisibility(this)"
              )
            )
          ),
          
          # Hidden role field
          tags$div(
            style = "display: none;",
            selectInput(
              "reg_role", 
              "Role", 
              choices = c("Customer"), 
              selected = "Customer"
            )
          ),
          
          tags$div(
            style = "background: rgba(230, 57, 70, 0.1); padding: 12px; border-radius: 8px; margin: 15px 0;",
            icon("info-circle", style = "color: #e63946; margin-right: 8px;"),
            tags$span(
              "Note: All registrations are for Customer accounts only", 
              style = "color: #666; font-size: 14px;"
            )
          ),
          
          uiOutput("reg_validation"),
          
          actionButton(
            "register_account", 
            tags$div(
              icon("user-plus", style = "margin-right: 10px;"),
              "Create Account"
            ), 
            class = "btn-theme", 
            width = "100%", 
            style = "margin-top: 20px; height: 50px; font-size: 16px;"
          ),
          
          br(), br()
        ),
        
        # Back button outside scrollable area
        actionButton(
          "back_login", 
          tags$div(
            icon("arrow-left", style = "margin-right: 10px;"),
            "Back to Login"
          ), 
          class = "btn-theme", 
          width = "100%",
          style = "margin-top: 15px; background: linear-gradient(135deg, #666, #888);"
        )
      )
    })
  })
  
  output$reg_validation <- renderUI({
    errors <- c()
    
    if(!is.null(input$reg_user) && nchar(trimws(input$reg_user)) > 0) {
      if(nchar(trimws(input$reg_user)) < 3) {
        errors <- c(errors, "Username must be at least 3 characters")
      }
    } else {
      errors <- c(errors, "Username is required")
    }
    
    if (!is.null(input$reg_pass) && !is.null(input$reg_pass_confirm)) {
      if (input$reg_pass != input$reg_pass_confirm) {
        errors <- c(errors, "Passwords do not match")
      }
      if (nchar(input$reg_pass) < 4) {
        errors <- c(errors, "Password must be at least 4 characters")
      }
    }
    
    if (length(errors) > 0) {
      tags$div(
        class = "alert alert-danger",
        style = "padding: 10px; margin-bottom: 15px;",
        icon("exclamation-circle"),
        tags$strong(" Please fix the following issues:"),
        tags$ul(
          lapply(errors, function(x) tags$li(x))
        )
      )
    }
  })
  
  observeEvent(input$back_login, { render_login_ui() })
  
  
  
  
  observeEvent(input$register_account, {
    req(input$reg_user, input$reg_pass, input$reg_pass_confirm, input$reg_role)
    
    if (nchar(trimws(input$reg_user)) < 3) {
      showModal(myModalDialog(
        title = "Validation Error",
        "Username must be at least 3 characters.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      runjs("
  setTimeout(function() {
    // Fix modal centering
    $('.modal').css('display', 'flex');
    $('.modal').css('align-items', 'center');
    $('.modal').css('justify-content', 'center');
    
    // Fix logout tab scrolling
    $('#logout_tab, #staff_logout_tab').css({
      'max-height': 'calc(100vh - 200px)',
      'overflow-y': 'auto'
    });
  }, 100);
")
      return()
    }
    
    if (input$reg_pass != input$reg_pass_confirm) {
      showModal(myModalDialog(
        title = "Validation Error",
        "Passwords do not match.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      runjs("
  setTimeout(function() {
    // Fix modal centering
    $('.modal').css('display', 'flex');
    $('.modal').css('align-items', 'center');
    $('.modal').css('justify-content', 'center');
    
    // Fix logout tab scrolling
    $('#logout_tab, #staff_logout_tab').css({
      'max-height': 'calc(100vh - 200px)',
      'overflow-y': 'auto'
    });
  }, 100);
")
      return()
    }
    
    if (nchar(input$reg_pass) < 4) {
      showModal(myModalDialog(
        title = "Validation Error",
        "Password must be at least 4 characters.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      runjs("
  setTimeout(function() {
    // Fix modal centering
    $('.modal').css('display', 'flex');
    $('.modal').css('align-items', 'center');
    $('.modal').css('justify-content', 'center');
    
    // Fix logout tab scrolling
    $('#logout_tab, #staff_logout_tab').css({
      'max-height': 'calc(100vh - 200px)',
      'overflow-y': 'auto'
    });
  }, 100);
")
      return()
    }
    
    tryCatch({
      # Force Customer role for all registrations
      dbExecutePool(
        "INSERT INTO users (username, password, role) VALUES (?, ?, ?)",
        params = list(trimws(input$reg_user), simple_hash(input$reg_pass), "Customer")
      )
      showModal(myModalDialog(
        title = "Success",
        "Registration complete! Please login as a customer.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      render_login_ui()
    }, error = function(e) {
      showModal(myModalDialog(
        title = "Error",
        "Username already exists!",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      runjs("
  setTimeout(function() {
    // Fix modal centering
    $('.modal').css('display', 'flex');
    $('.modal').css('align-items', 'center');
    $('.modal').css('justify-content', 'center');
    
    // Fix logout tab scrolling
    $('#logout_tab, #staff_logout_tab').css({
      'max-height': 'calc(100vh - 200px)',
      'overflow-y': 'auto'
    });
  }, 100);
")
    })
  })
  
  # --------------------- Customer Login ---------------------
  observeEvent(input$login_cust, {
    output$login_ui <- renderUI({
      tags$div(
        class = "login-box",
        style = "text-align: center;",
        h3("Customer Login", class = "customer-login-title"),  # ADDED CLASS
        p("Enter your credentials to access your account", style = "color: #666; margin-bottom: 25px;"),
        
        div(
          class = "form-group",
          tags$label("Username", `for` = "cust_user"),
          textInput("cust_user", 
                    label = NULL, 
                    placeholder = "Enter your username",
                    width = "100%")
        ),
        
        # In the Login UI sections, update password inputs:
        div(
          class = "form-group",
          tags$label("Password", `for` = "cust_pass"),
          tags$div(
            class = "password-input-wrapper",
            passwordInput("cust_pass", 
                          label = NULL, 
                          placeholder = "Enter your password",
                          width = "100%"),
            tags$button(
              class = "password-toggle-btn",
              type = "button",
              icon("eye"),
              onclick = "togglePasswordVisibility(this)"
            )
          )
        ),
        
        actionButton("cust_login_btn", 
                     tags$div(
                       icon("sign-in-alt", style = "margin-right: 10px;"),
                       "Login"
                     ), 
                     class = "btn-theme", 
                     width = "100%", 
                     style = "margin-top: 20px; height: 50px; font-size: 16px;"),
        
        br(), br(),
        
        actionButton("back_login2", 
                     tags$div(
                       icon("arrow-left", style = "margin-right: 10px;"),
                       "Back to Main Menu"
                     ), 
                     class = "btn-theme", 
                     width = "100%",
                     style = "background: linear-gradient(135deg, #666, #888);")
      )
    })
  })
  
  observeEvent(input$back_login2, { render_login_ui() })
  
  # --------------------- Staff Login ---------------------
  observeEvent(input$login_staff, {
    output$login_ui <- renderUI({
      tags$div(
        class = "login-box",
        style = "text-align: center;",
        h3("Staff Login", class = "staff-login-title"),  # ADDED CLASS
        p("Enter your staff credentials", style = "color: #666; margin-bottom: 25px;"),
        
        div(
          class = "form-group",
          tags$label("Username", `for` = "staff_user"),
          textInput("staff_user", 
                    label = NULL, 
                    placeholder = "Enter staff username",
                    width = "100%")
        ),
        
        div(
          class = "form-group",
          tags$label("Password", `for` = "staff_pass"),
          tags$div(
            class = "password-input-wrapper",
            passwordInput("staff_pass", 
                          label = NULL, 
                          placeholder = "Enter staff password",
                          width = "100%"),
            tags$button(
              class = "password-toggle-btn",
              type = "button",
              icon("eye"),
              onclick = "togglePasswordVisibility(this)"
            )
          )
        ),
        
        actionButton("staff_login_btn", 
                     tags$div(
                       icon("user-tie", style = "margin-right: 10px;"),
                       "Staff Login"
                     ), 
                     class = "btn-theme", 
                     width = "100%", 
                     style = "margin-top: 20px; height: 50px; font-size: 16px;"),
        
        br(), br(),
        
        actionButton("back_login3", 
                     tags$div(
                       icon("arrow-left", style = "margin-right: 10px;"),
                       "Back to Main Menu"
                     ), 
                     class = "btn-theme", 
                     width = "100%",
                     style = "background: linear-gradient(135deg, #666, #888);")
      )
    })
  })
  
  observeEvent(input$back_login3, { render_login_ui() })
  
  # --------------------- Customer Login Handler ---------------------
  observeEvent(input$cust_login_btn, {
    req(input$cust_user, input$cust_pass)
    
    tryCatch({
      res <- dbGetQueryPool(
        "SELECT * FROM users WHERE username = ? AND password = ? AND role = 'Customer'",
        params = list(trimws(input$cust_user), simple_hash(input$cust_pass))
      )
      
      if (nrow(res) == 1) {
        # SET LOGIN FLAG FIRST
        user_data$is_logging_in <- TRUE
        
        user_data$logged_in <- TRUE
        user_data$role <- "Customer"
        user_data$user_id <- res$user_id
        user_data$username <- res$username
        
        # Load cart from database
        db_cart <- load_cart_from_db(user_data$user_id)
        
        # If there's any existing cart in memory (shouldn't happen, but just in case)
        if(length(user_data$cart) > 0) {
          # Merge database cart with any existing session cart
          user_data$cart <- merge_carts(user_data$cart, db_cart)
        } else {
          user_data$cart <- db_cart
        }
        
        user_data$cart_trigger <- user_data$cart_trigger + 1
        
        # Clear any queued notifications
        notification_queue(list())
        
        output$login_ui <- renderUI({ NULL })
        shinyjs::hide("login_container")
        
        output$main_ui <- renderUI({
          tagList(
            navbarPage(
              title = tags$div(
                icon("shoe-prints", style = "margin-right:10px;"),
                paste("PJM Shoes -", user_data$username)
              ),
              id = "customer_nav",
              windowTitle = "PJM Shoe Ordering System",
              collapsible = TRUE,
              theme = shinythemes::shinytheme("flatly"),
              
              tabPanel("Home", 
                       br(),
                       # Cart container always visible at the top
                       div(
                         style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                         div(
                           actionButton("refresh_products", "Refresh Products", 
                                        class = "btn-theme", icon = shiny::icon("sync"))
                         ),
                         uiOutput("cart_container")
                       ),
                       
                       fluidRow(
                         column(6,
                                div(
                                  tags$label("Search Products", `for` = "product_search-search"),
                                  searchInput("product_search", 
                                              label = NULL,
                                              placeholder = "Search by name, color, etc.",
                                              btnSearch = icon("search"),
                                              btnReset = icon("remove"),
                                              width = "100%")
                                )
                         ),
                         column(6,
                                div(
                                  tags$label("Filter by Price", `for` = "price_filter"),
                                  selectInput("price_filter", label = NULL,
                                              choices = c("All", "Under ₱2,000", "₱2,000 - ₱4,000", "₱5,000 - ₱10,000", "Over ₱10,000"),
                                              selected = "All")
                                )
                         )
                       ),
                       uiOutput("product_cards")),
              
              tabPanel("My Orders", 
                       br(),
                       # Cart container also visible here
                       div(
                         style = "display: flex; justify-content: flex-end; margin-bottom: 15px;",
                         uiOutput("cart_container_orders")
                       ),
                       DTOutput("customer_orders")),
              
              tabPanel("Order Status",
                       br(),
                       # Cart container also visible here
                       div(
                         style = "display: flex; justify-content: flex-end; margin-bottom: 15px;",
                         uiOutput("cart_container_status")
                       ),
                       fluidRow(
                         column(6,
                                div(
                                  tags$label("Search Track ID", `for` = "search_track_id"),
                                  textInput("search_track_id", label = NULL, 
                                            placeholder = "Enter Track ID (e.g., SOS001)")
                                ),
                                actionButton("search_order_btn", "Search", class = "btn-theme")
                         )
                       ),
                       br(),
                       DTOutput("customer_status")),
              
              tabPanel("Order History", 
                       br(),
                       # Cart container also visible here
                       div(
                         style = "display: flex; justify-content: flex-end; margin-bottom: 15px;",
                         uiOutput("cart_container_history")
                       ),
                       DTOutput("customer_completed")),
              
              tabPanel(
                title = tags$span(icon("sign-out-alt"), "Logout"),
                value = "logout_tab",
                div(
                  class = "logout-tab-container",
                  style = "text-align: center; max-width: 800px; margin: 0 auto; padding: 20px;",
                  br(),
                  h3("Click the logout button to sign out", style = "color: #2c3e50; margin-bottom: 20px;"),
                  p("You will be redirected to the login page.", 
                    style = "color: #7f8c8d; font-size: 16px; margin-bottom: 40px;"),
                  
                  div(
                    style = "margin-top: 30px;",
                    actionButton("show_logout_modal_customer", "Logout", 
                                 class = "btn-theme",
                                 style = "background-color: #e74c3c; width: 200px; font-size: 16px; padding: 12px;",
                                 icon = icon("sign-out-alt"))
                  ),
                  
                  tags$div(
                    style = "margin-top: 40px; padding: 20px; background-color: #f8f9fa; border-radius: 10px; max-width: 500px; margin: 40px auto 0;",
                    h5(icon("info-circle"), " Logout Information", style = "color: #3498db;"),
                    p("• Your cart will be saved for next time", style = "color: #7f8c8d; margin: 5px 0;"),
                    p("• Active orders will continue processing", style = "color: #7f8c8d; margin: 5px 0;"),
                    p("• You can login again anytime", style = "color: #7f8c8d; margin: 5px 0;")
                  )
                )
              )
            )
          )
        })
        
        shinyjs::show("main_container")
        
        # Send message to format dashboard numbers
        delay(500, format_dashboard_numbers())
        
        # Reset flag after delay
        delay(1000, {
          user_data$is_logging_in <- FALSE
        })
        
        show_notification(paste("Welcome back, ", user_data$username, "!", sep = ""), "success")
        
      } else {
        showModal(myModalDialog(
          title = tags$div(
            icon("exclamation-triangle", style = "color: #e63946; margin-right: 10px;"),
            "Login Failed"
          ),
          tags$div(
            style = "text-align: center; padding: 20px;",
            icon("times-circle", style = "font-size: 48px; color: #e63946; margin-bottom: 15px;"),
            p("Invalid username or password.", style = "font-size: 16px; color: #333;"),
            p("Please try again.", style = "font-size: 14px; color: #666;")
          ),
          easyClose = TRUE,
          footer = modalButton("OK"),
          size = "s",
          options = list(backdrop = 'static', keyboard = FALSE)  # ADD THIS LINE
        ))
        runjs("
          setTimeout(function() {
            Shiny.setInputValue('fix_modal_centering', Math.random());
          }, 100);
        ")
      }
    }, error = function(e) {
      show_notification(paste("Login failed:", e$message), "error")
    })
  })
  
  # --------------------- Staff Login Handler ---------------------
  observeEvent(input$staff_login_btn, {
    req(input$staff_user, input$staff_pass)
    
    tryCatch({
      res <- dbGetQueryPool(
        "SELECT * FROM users WHERE username = ? AND password = ? AND role = 'Staff'",
        params = list(trimws(input$staff_user), simple_hash(input$staff_pass))
      )
      
      if (nrow(res) == 1) {
        user_data$logged_in <- TRUE
        user_data$role <- "Staff"
        user_data$user_id <- res$user_id
        user_data$username <- res$username
        
        # Clear any queued notifications
        notification_queue(list())
        
        output$login_ui <- renderUI({ NULL })
        shinyjs::hide("login_container")
        
        output$main_ui <- renderUI({
          tagList(
            navbarPage(
              title = tags$div(
                icon("shoe-prints", style = "margin-right:10px;"),
                paste("PJM Shoes - Staff")
              ),
              id = "staff_nav",
              windowTitle = "PJM Shoe Ordering System - Staff",
              collapsible = TRUE,
              theme = shinythemes::shinytheme("flatly"),
              
              tabPanel("Dashboard",
                       br(),
                       h3("Dashboard Overview", style = "margin-bottom:20px; font-weight:bold;"),
                       uiOutput("low_stock_alerts"),
                       fluidRow(
                         column(3, uiOutput("total_orders_box")),
                         column(3, uiOutput("pending_orders_box")),
                         column(3, uiOutput("total_sales_box")),
                         column(3, uiOutput("total_stock_box"))
                       ),
                       br(),
                       plotOutput("sales_plot", height = "350px")),
              
              tabPanel("Manage Orders", 
                       br(),
                       div(
                         style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                         h4("Manage Orders", style = "margin: 0;"),
                         actionButton("refresh_staff_orders", "Refresh Orders", 
                                      class = "btn-theme", icon = icon("sync"))
                       ),
                       DTOutput("staff_orders_table"),
                       uiOutput("staff_order_actions")),
              
              tabPanel("Shoe Inventory", 
                       br(),
                       fluidRow(
                         column(6,
                                div(
                                  tags$label("Search Shoes", `for` = "inventory_search"),
                                  searchInput("inventory_search", 
                                              label = NULL,
                                              placeholder = "Search by name, color, etc.",
                                              btnSearch = icon("search"),
                                              btnReset = icon("remove"),
                                              width = "100%")
                                )
                         ),
                         column(3,
                                div(
                                  tags$label("Filter by Availability", `for` = "availability_filter"),
                                  selectInput("availability_filter", label = NULL,
                                              choices = c("All", "Available Only", "Unavailable Only"),
                                              selected = "All")
                                )
                         ),
                         column(3,
                                div(style = "margin-top: 25px; display: flex; gap: 10px;",
                                    actionButton("refresh_shoes_btn", "Refresh", 
                                                 class = "btn-theme", 
                                                 icon = icon("sync")),
                                    actionButton("add_shoe_btn", "Add New Shoe", 
                                                 class = "btn-theme",
                                                 style = "background: linear-gradient(135deg, #27ae60, #2ecc71);")
                                )
                         )
                       ),
                       br(),
                       DTOutput("staff_shoes_table"),
                       br()
              ),
              
              tabPanel("Completed Orders", 
                       br(),
                       DTOutput("staff_completed_orders")),
              
              tabPanel("Sales Report", 
                       br(),
                       fluidRow(
                         column(3,
                                div(
                                  tags$label("Start Date", `for` = "sales_start_date"),
                                  dateInput("sales_start_date", label = NULL,
                                            value = Sys.Date() - 30,
                                            max = Sys.Date(),
                                            format = "yyyy-mm-dd")
                                )
                         ),
                         column(3,
                                div(
                                  tags$label("End Date", `for` = "sales_end_date"),
                                  dateInput("sales_end_date", label = NULL,
                                            value = Sys.Date(),
                                            min = as.Date("2024-01-01"),
                                            max = Sys.Date(),
                                            format = "yyyy-mm-dd")
                                )
                         ),
                         column(6,
                                div(
                                  style = "display: flex; align-items: flex-end; height: 100%; padding-top: 24px;",
                                  actionButton("apply_date_filter", "Apply Filter",
                                               class = "btn-theme", 
                                               style = "margin-right: 10px;"),
                                  actionButton("reset_date_filter", "Reset",
                                               class = "btn-theme",
                                               style = "background-color: #95a5a6;")
                                )
                         )
                       ),
                       br(),
                       DTOutput("staff_sales"),
                       br(),
                       plotOutput("sales_trend_plot", height = "400px")
              ),
              
              tabPanel(
                title = tags$span(icon("sign-out-alt"), "Logout"),
                value = "staff_logout_tab",
                div(
                  class = "logout-tab-container",
                  style = "text-align: center; padding: 60px 20px;",
                  br(),
                  h3("Click the logout button to sign out", style = "color: #2c3e50; margin-bottom: 20px;"),
                  p("You will be redirected to the login page.", 
                    style = "color: #7f8c8d; font-size: 16px; margin-bottom: 40px;"),
                  
                  div(
                    style = "margin-top: 30px;",
                    actionButton("show_logout_modal_staff", "Staff Logout", 
                                 class = "btn-theme",
                                 style = "background-color: #e74c3c; width: 200px; font-size: 16px; padding: 12px;",
                                 icon = icon("sign-out-alt"))
                  ),
                  
                  tags$div(
                    style = "margin-top: 40px; padding: 20px; background-color: #f8f9fa; border-radius: 10px; max-width: 500px; margin: 40px auto 0;",
                    h5(icon("info-circle"), " Staff Logout Information", style = "color: #3498db;"),
                    p("• You will be redirected to the login page", style = "color: #7f8c8d; margin: 5px 0;"),
                    p("• System data will remain secure", style = "color: #7f8c8d; margin: 5px 0;"),
                    p("• Customer orders will continue processing", style = "color: #7f8c8d; margin: 5px 0;")
                  )
                )
              )
            )
          )
        })
        
        
        shinyjs::show("main_container")
        
        # Send message to format dashboard numbers
        delay(500, format_dashboard_numbers())
        
        show_notification(paste("Welcome,", user_data$username, "!", sep = ""), "success")
      } else {
        showModal(myModalDialog(
          title = "Login Failed",
          "Invalid username or password. Please try again.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        runjs("
          setTimeout(function() {
            Shiny.setInputValue('fix_modal_centering', Math.random());
          }, 100);
        ")
      }
    }, error = function(e) {
      show_notification(paste("Login failed:", e$message), "error")
    })
  })
  
  # --------------------- Cart Container UI ---------------------
  output$cart_container <- renderUI({
    if(loading_state$saving_cart) {
      tags$div(
        class = "cart-container",
        tags$div(class = "loading-indicator"),
        tags$span("Saving...", style = "margin-left: 5px; color: white; font-weight: bold;")
      )
    } else {
      tags$div(
        class = "cart-container",
        onclick = "Shiny.setInputValue('show_cart', Math.random(), {priority: 'event'})",
        shiny::icon("shopping-cart", class = "cart-icon"),
        tags$span(
          class = "cart-badge",
          textOutput("cart_count", inline = TRUE, container = span)
        ),
        tags$span("Shopping Cart", style = "margin-left: 5px; color: white; font-weight: bold;")
      )
    }
  })
  
  output$cart_container_orders <- renderUI({
    tags$div(
      class = "cart-container",
      onclick = "Shiny.setInputValue('show_cart', Math.random(), {priority: 'event'})",
      icon("shopping-cart", class = "cart-icon"),
      tags$span(
        class = "cart-badge",
        textOutput("cart_count_orders", inline = TRUE, container = span)
      ),
      tags$span("Shopping Cart", style = "margin-left: 5px; color: white; font-weight: bold;")
    )
  })
  
  output$cart_container_status <- renderUI({
    tags$div(
      class = "cart-container",
      onclick = "Shiny.setInputValue('show_cart', Math.random(), {priority: 'event'})",
      icon("shopping-cart", class = "cart-icon"),
      tags$span(
        class = "cart-badge",
        textOutput("cart_count_status", inline = TRUE, container = span)
      ),
      tags$span("Shopping Cart", style = "margin-left: 5px; color: white; font-weight: bold;")
    )
  })
  
  output$cart_container_history <- renderUI({
    tags$div(
      class = "cart-container",
      onclick = "Shiny.setInputValue('show_cart', Math.random(), {priority: 'event'})",
      icon("shopping-cart", class = "cart-icon"),
      tags$span(
        class = "cart-badge",
        textOutput("cart_count_history", inline = TRUE, container = span)
      ),
      tags$span("Shopping Cart", style = "margin-left: 5px; color: white; font-weight: bold;")
    )
  })
  
  # --------------------- Logout Handlers ---------------------
  cleanup_user_session <- function(show_message = TRUE) {
    # Save cart if it exists
    if (user_data$logged_in && length(user_data$cart) > 0) {
      save_cart_to_db(user_data$user_id, user_data$cart)
    }
    
    # Reset all reactive values
    user_data$logged_in <- FALSE
    user_data$role <- NULL
    user_data$user_id <- NULL
    user_data$username <- NULL
    user_data$cart <- list()
    user_data$modal_shoe <- NULL
    user_data$modal_open <- FALSE
    user_data$cart_trigger <- 0
    user_data$is_logging_out <- FALSE
    
    # Reset modal state
    modal_state$is_open <- FALSE
    modal_state$shoe_id <- NULL
    modal_state$processing <- FALSE
    current_modal_shoe_id(NULL)
    
    # Clear session data
    session$userData$editing_shoe_id <- NULL
    session$userData$deleting_shoe_id <- NULL
    session$userData$current_image <- NULL
    session$sendCustomMessage("forceCSSReload", list())
    
    delay(100, {
      session$sendCustomMessage("refreshCSS", list())
    })
    
    # Reset refresh triggers
    refresh_trigger$shoes <- 0
    refresh_trigger$orders <- 0
    
    # Clear notification queue
    notification_queue(list())
    
    # Clear inputs
    shinyjs::reset("product_search")
    shinyjs::reset("inventory_search")
    
    # Only show message if explicitly requested
    if (show_message) {
      show_notification("Logged out successfully", "message", duration = 3)
    }
  }
  
  
  # Customer logout modal
  observeEvent(input$show_logout_modal_customer, {
    showModal(myModalDialog(
      title = tags$div(icon("sign-out-alt"), " Confirm Logout"),
      tags$div(
        style = "text-align: center; padding: 20px;",
        icon("sign-out-alt", style = "font-size: 48px; color: #e74c3c; margin-bottom: 15px;"),
        h4("Are you sure you want to logout?", style = "color: #2c3e50; margin-bottom: 15px;"),
        p("You will be redirected to the login page.", style = "color: #7f8c8d;")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_logout_customer", "Yes, Logout", 
                     class = "btn-theme",
                     style = "background-color: #e74c3c;")
      ),
      size = "s",
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$confirm_logout_customer, {
    removeModal()
    # Set logout flag
    user_data$is_logging_out <- TRUE
    
    # Don't show message here, will be shown after redirect
    cleanup_user_session(show_message = FALSE)
    
    # Clear main UI
    output$main_ui <- renderUI({ NULL })
    shinyjs::hide("main_container")
    
    # Show login UI
    render_login_ui()
    
    # Show ONE notification
    showNotification("Logged out successfully. See you next time!", 
                     type = "message", duration = 3)
  })
  
  # Staff logout modal
  observeEvent(input$show_logout_modal_staff, {
    showModal(myModalDialog(
      title = tags$div(icon("sign-out-alt"), " Confirm Staff Logout"),
      tags$div(
        style = "text-align: center; padding: 20px;",
        icon("user-tie", style = "font-size: 48px; color: #e74c3c; margin-bottom: 15px;"),
        h4("Are you sure you want to logout?", style = "color: #2c3e50; margin-bottom: 15px;"),
        p("You will be redirected to the login page.", style = "color: #7f8c8d;")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_logout_staff", "Yes, Logout", 
                     class = "btn-theme",
                     style = "background-color: #e74c3c;")
      ),
      size = "s",
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$confirm_logout_staff, {
    removeModal()
    # Don't show message here, will be shown after redirect
    cleanup_user_session(show_message = FALSE)
    
    # Clear main UI
    output$main_ui <- renderUI({ NULL })
    shinyjs::hide("main_container")
    
    # Show login UI
    render_login_ui()
    
    # Show ONE notification
    showNotification("Logged out successfully", type = "message", duration = 3)
  })
  
  # Handle cancel from customer logout tab
  observeEvent(input$cancel_logout_from_tab, {
    updateNavbarPage(session, "customer_nav", selected = "Home")
  })
  
  # Handle cancel from staff logout tab
  observeEvent(input$staff_cancel_logout_from_tab, {
    updateNavbarPage(session, "staff_nav", selected = "Dashboard")
  })
  
  observeEvent(input$logout_from_tab, {
    # Set logout flag
    user_data$is_logging_out <- TRUE
    
    # Don't show message here, will be shown after redirect
    cleanup_user_session(show_message = FALSE)
    
    # Clear main UI
    output$main_ui <- renderUI({ NULL })
    shinyjs::hide("main_container")
    
    # Show login UI
    render_login_ui()
    
    # Show ONE notification
    showNotification("Logged out successfully. See you next time!", 
                     type = "message", duration = 3)
  })
  
  observeEvent(input$staff_logout_from_tab, {
    # Don't show message here, will be shown after redirect
    cleanup_user_session(show_message = FALSE)
    
    # Clear main UI
    output$main_ui <- renderUI({ NULL })
    shinyjs::hide("main_container")
    
    # Show login UI
    render_login_ui()
    
    # Show ONE notification
    showNotification("Logged out successfully", type = "message", duration = 3)
  })
  
  # --------------------- Cart Functions ---------------------
  cart_count <- reactive({
    if(length(user_data$cart) > 0){
      sum(sapply(user_data$cart, function(x) as.numeric(x$quantity)))
    } else {
      0
    }
  })
  
  output$cart_count <- renderText({
    as.character(cart_count())
  })
  
  # Additional cart count outputs for different tabs
  output$cart_count_orders <- renderText({
    as.character(cart_count())
  })
  
  output$cart_count_status <- renderText({
    as.character(cart_count())
  })
  
  output$cart_count_history <- renderText({
    as.character(cart_count())
  })
  
  # Save cart to database whenever it changes
  observe({
    # Trigger on cart changes
    user_data$cart_trigger
    
    # Skip if not logged in
    if (!user_data$logged_in || is.null(user_data$user_id)) {
      return()
    }
    
    # Skip if we're logging out (cart will be saved in logout handler)
    if (user_data$is_logging_out) {
      return()
    }
    
    # Show loading
    loading_state$saving_cart <- TRUE
    
    # Save to database
    if (length(user_data$cart) > 0) {
      save_cart_to_db(user_data$user_id, user_data$cart)
    } else {
      # Cart is empty - clear from database
      clear_cart_in_db(user_data$user_id)
    }
    
    # Hide loading
    loading_state$saving_cart <- FALSE
  })
  
  # --------------------- Product Display ---------------------
  customer_shoes_data <- reactive({
    input$refresh_products
    invalidateLater(30000)
    
    tryCatch({
      # Base query
      query <- "SELECT * FROM shoes WHERE available = 1"
      params <- list()
      
      # Add search filter
      if(!is.null(input$product_search) && nchar(input$product_search) > 0) {
        query <- paste(query, "AND (name LIKE ? OR colors LIKE ?)")
        search_term <- paste0("%", input$product_search, "%")
        params <- c(params, list(search_term, search_term))
      }
      
      # Add price filter
      if(!is.null(input$price_filter) && input$price_filter != "All") {
        price_conditions <- switch(input$price_filter,
                                   "Under ₱2,000" = "price < 2000",
                                   "₱2,000 - ₱4,000" = "price >= 2000 AND price <= 4000",
                                   "₱5,000 - ₱10,000" = "price >= 5000 AND price <= 10000",
                                   "Over ₱10,000" = "price > 10000")
        query <- paste(query, "AND", price_conditions)
      }
      
      # Add ORDER BY clause
      query <- paste(query, "ORDER BY shoe_id")
      
      if(length(params) > 0) {
        shoes <- dbGetQueryPool(query, params = params)
      } else {
        shoes <- dbGetQueryPool(query)
      }
      
      required_cols <- c("shoe_id", "name", "price", "stock", "image")
      if(!all(required_cols %in% colnames(shoes))) {
        stop("Missing required columns in shoes data")
      }
      return(shoes)
    }, error = function(e) {
      showNotification(paste("Failed to load products:", e$message), type = "error")
      return(data.frame(shoe_id=integer(), name=character(), price=numeric(), 
                        stock=integer(), image=character()))
    })
  })
  
  output$product_cards <- renderUI({
    shoes <- customer_shoes_data()
    
    if(is.null(shoes) || nrow(shoes) == 0) {
      return(fluidRow(
        column(12, 
               tags$div(
                 style = "text-align:center; padding:50px;",
                 icon("shoe-prints", style = "font-size: 48px; color: #ddd; margin-bottom: 20px;"),
                 h3("No products available at the moment"),
                 p("Please check back later!")
               )
        )
      ))
    }
    
    card_list <- lapply(1:nrow(shoes), function(i) {
      shoe <- shoes[i, ]
      shoe_list <- as.list(shoe)
      product_card(shoe_list)
    })
    
    tags$div(
      class = "shoe-card-grid",
      style = "
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(250px, 1fr));
        gap: 20px;
        margin-top: 20px;
      ",
      card_list
    )
  })
  
  # --------------------- Fix Modal Centering ---------------------
  observeEvent(input$fix_modal_centering, {
    runjs("
      if (typeof fixAllModalCentering === 'function') {
        fixAllModalCentering();
      }
      if (typeof fixAddToCartModalLayout === 'function') {
        fixAddToCartModalLayout();
      }
    ")
  })
  
  # --------------------- Cart Modal ---------------------
  observeEvent(input$show_cart_modal, {
    if(modal_state$is_open && modal_state$processing) {
      return()
    }
    
    shoe_id <- as.numeric(input$show_cart_modal)
    
    # Validate shoe_id
    if(is.na(shoe_id) || shoe_id <= 0) {
      showNotification("Invalid shoe selection", type = "error")
      return()
    }
    
    current_modal_shoe_id(shoe_id)
    modal_state$shoe_id <- shoe_id
    modal_state$is_open <- TRUE
    modal_state$processing <- FALSE
    user_data$modal_open <- TRUE
    
    tryCatch({
      shoe <- dbGetQueryPool("SELECT * FROM shoes WHERE shoe_id = ? AND available = 1", 
                             params = list(shoe_id))
      
      if(nrow(shoe) == 0) {
        user_data$modal_open <- FALSE
        modal_state$is_open <- FALSE
        modal_state$shoe_id <- NULL
        current_modal_shoe_id(NULL)
        showNotification("Shoe not found or unavailable", type = "error")
        return()
      }
      
      if(shoe$stock == 0){
        user_data$modal_open <- FALSE
        modal_state$is_open <- FALSE
        modal_state$shoe_id <- NULL
        current_modal_shoe_id(NULL)
        showNotification("This item is out of stock", type = "error")
        return()
      }
      
      colors <- tryCatch({
        if(!is.null(shoe$colors) && !is.na(shoe$colors) && shoe$colors != "") {
          trimws(unlist(strsplit(as.character(shoe$colors), ",")))
        } else {
          c("Black", "White", "Blue")
        }
      }, error = function(e) {
        c("Black", "White", "Blue")
      })
      
      sizes <- tryCatch({
        if(!is.null(shoe$sizes) && !is.na(shoe$sizes) && shoe$sizes != "") {
          trimws(unlist(strsplit(as.character(shoe$sizes), ",")))
        } else {
          c("7", "8", "9", "10")
        }
      }, error = function(e) {
        c("7", "8", "9", "10")
      })
      
      user_data$modal_shoe <- as.list(shoe[1, ])
      
      showModal(myModalDialog(
        title = tags$div(
          icon("plus-circle"),
          paste("Add", shoe$name, "to Cart")
        ),
        tags$img(src = get_image_path(shoe$image), width = "100%", style = "border-radius:10px; margin-bottom:15px;",
                 onerror = "this.onerror=null; this.src='default_shoe_image.jpg'"),
        p(strong("Price: "), format_currency(shoe$price)),
        p(strong("Available Stock: "), format_number(shoe$stock, digits = 0)),
        selectInput("sel_color", "Select Color", choices = colors),
        selectInput("sel_size", "Select Size", choices = sizes),
        numericInput("sel_qty", "Quantity", value = 1, min = 1, max = min(shoe$stock, 10)),
        uiOutput("modal_validation"),
        footer = tagList(
          div(class = "modal-button-group",
              actionButton("confirm_add_cart", "Add to Cart", class = "btn-theme",
                           style = "width: 100%; margin-bottom: 10px;"),
              div(style = "text-align: center; width: 100%;",
                  actionButton("cancel_add_cart", "Cancel", 
                               class = "btn-theme btn-cancel",
                               style = "width: 100%;")
              )
          )
        ),
        easyClose = TRUE,
        size = "m",
        class = "centered-modal",
        options = list(backdrop = 'static', keyboard = FALSE)  # ADD THIS LINE
      ))
      runjs("
  setTimeout(function() {
    // Fix modal centering
    $('.modal').css('display', 'flex');
    $('.modal').css('align-items', 'center');
    $('.modal').css('justify-content', 'center');
    
    // Fix logout tab scrolling
    $('#logout_tab, #staff_logout_tab').css({
      'max-height': 'calc(100vh - 200px)',
      'overflow-y': 'auto'
    });
  }, 100);
")
    }, error = function(e) {
      user_data$modal_open <- FALSE
      modal_state$is_open <- FALSE
      modal_state$shoe_id <- NULL
      current_modal_shoe_id(NULL)
      showNotification(paste("Failed to load shoe details:", e$message), type = "error")
    })
  })
  
  output$modal_validation <- renderUI({
    req(user_data$modal_shoe)
    
    errors <- c()
    
    if(!is.null(input$sel_qty)) {
      if(is.na(input$sel_qty) || input$sel_qty < 1) {
        errors <- c(errors, "Quantity must be at least 1")
      }
      if(input$sel_qty > as.numeric(user_data$modal_shoe$stock)) {
        errors <- c(errors, paste("Maximum available:", user_data$modal_shoe$stock))
      }
    }
    
    if(length(errors) > 0) {
      tags$div(
        class = "validation-error",
        HTML(paste(errors, collapse = "<br>"))
      )
    }
  })
  
  observeEvent(input$confirm_add_cart, {
    req(input$sel_color, input$sel_size, input$sel_qty)
    
    shoe_id <- current_modal_shoe_id()
    
    if(is.null(shoe_id) || is.na(shoe_id)) {
      show_notification("Shoe information is no longer available. Please try again.", 
                        "error")
      removeModal()
      modal_state$is_open <- FALSE
      modal_state$shoe_id <- NULL
      modal_state$processing <- FALSE
      user_data$modal_open <- FALSE
      return()
    }
    
    tryCatch({
      shoe <- dbGetQueryPool("SELECT * FROM shoes WHERE shoe_id = ? AND available = 1", 
                             params = list(shoe_id))
      
      if(nrow(shoe) == 0) {
        show_notification("Shoe not found or unavailable", "error")
        removeModal()
        modal_state$is_open <- FALSE
        modal_state$shoe_id <- NULL
        modal_state$processing <- FALSE
        user_data$modal_open <- FALSE
        current_modal_shoe_id(NULL)
        return()
      }
      
      shoe <- as.list(shoe[1, ])
      
      if(is.na(input$sel_qty) || input$sel_qty < 1) {
        show_notification("Quantity must be at least 1", "error")
        return()
      }
      
      if(input$sel_qty > as.numeric(shoe$stock)){
        show_notification(paste("Quantity exceeds available stock. Only", 
                                shoe$stock, "units available."), 
                          "error")
        return()
      }
      
      existing_item_index <- NULL
      if(length(user_data$cart) > 0) {
        for (i in 1:length(user_data$cart)) {
          item <- user_data$cart[[i]]
          if (as.numeric(item$shoe_id) == as.numeric(shoe_id) && 
              item$color == input$sel_color && 
              item$size == input$sel_size) {
            existing_item_index <- i
            break
          }
        }
      }
      
      if (!is.null(existing_item_index)) {
        new_quantity <- as.numeric(user_data$cart[[existing_item_index]]$quantity) + 
          as.numeric(input$sel_qty)
        
        if(new_quantity > as.numeric(shoe$stock)) {
          show_notification(paste("Cannot add more. Total would exceed available stock of", 
                                  shoe$stock), 
                            "error")
          return()
        }
        
        user_data$cart[[existing_item_index]]$quantity <- new_quantity
      } else {
        new_item <- list(
          shoe_id = shoe_id,
          name = shoe$name,
          price = shoe$price,
          color = input$sel_color,
          size = input$sel_size,
          quantity = input$sel_qty,
          image = shoe$image
        )
        user_data$cart <- c(user_data$cart, list(new_item))
      }
      
      user_data$cart_trigger <- user_data$cart_trigger + 1
      
      removeModal()
      modal_state$is_open <- FALSE
      modal_state$shoe_id <- NULL
      modal_state$processing <- FALSE
      user_data$modal_open <- FALSE
      current_modal_shoe_id(NULL)
      
      # Show notification AFTER modal is closed with wait_for_modal parameter
      show_notification(
        paste(shoe$name, "added to cart!"),
        "success",
        wait_for_modal = FALSE  # Changed to FALSE since modal is already closed
      )
      
    }, error = function(e) {
      show_notification(paste("Error adding to cart:", e$message), "error")
      removeModal()
      modal_state$is_open <- FALSE
      modal_state$shoe_id <- NULL
      modal_state$processing <- FALSE
      user_data$modal_open <- FALSE
      current_modal_shoe_id(NULL)
    })
  })
  
  # --------------------- Cart Display (FIXED) ---------------------
  observeEvent(input$show_cart, {
    # Add debounce to prevent rapid clicks
    shinyjs::disable("show_cart")
    on.exit(shinyjs::enable("show_cart"))
    
    # Check cart content
    if(length(user_data$cart) == 0) {
      showModal(myModalDialog(
        title = tags$h3(icon("shopping-cart"), " Your Shopping Cart"),
        tags$div(
          class = "empty-cart-message",
          icon("shopping-cart", style = "font-size: 80px; color: #ddd; margin-bottom: 20px;"),
          h4("Your cart is empty"),
          p("Add some shoes to get started!")
        ),
        footer = tagList(
          div(style = "display: flex; justify-content: center; width: 100%; gap: 10px;",
              modalButton("Continue Shopping"),
              actionButton("clear_cart_btn", "Clear Cart", class = "btn-theme",
                           style = "background-color: #f39c12;"),
              actionButton("make_order_btn", "Place Order", class = "btn-theme")
          )
        ),
        size = "m",
        easyClose = TRUE,
        class = "centered-modal",
        options = list(backdrop = 'static', keyboard = FALSE)  
      ))
      return()
    }
    
    # Calculate cart total
    cart_total <- sum(sapply(user_data$cart, function(x) {
      as.numeric(x$price) * as.numeric(x$quantity)
    }))
    
    # Format with commas:
    formatted_total <- format_currency(cart_total)
    
    # Create cart data frame
    cart_df <- do.call(rbind, lapply(1:length(user_data$cart), function(i) {
      item <- user_data$cart[[i]]
      data.frame(
        Index = i,
        Name = item$name,
        Color = item$color,
        Size = item$size,
        Quantity = format_number(item$quantity, digits = 0),
        Price = format_currency(item$price),
        Total = format_currency(as.numeric(item$price) * as.numeric(item$quantity)),
        stringsAsFactors = FALSE
      )
    }))
    
    # Render the cart table - IMPORTANT: This will now re-render when cart changes
    output$cart_table_display <- renderDT({
      datatable(
        cart_df[, c("Name", "Color", "Size", "Quantity", "Price", "Total")],
        colnames = c("Product", "Color", "Size", "Qty", "Price", "Total"),
        options = list(
          pageLength = 10,
          dom = 't',
          ordering = FALSE,
          scrollX = TRUE,
          scrollY = "300px",
          scrollCollapse = TRUE,
          paging = FALSE,
          info = FALSE,
          columnDefs = list(
            list(width = '150px', targets = 0),
            list(width = '80px', targets = 1),
            list(width = '80px', targets = 2),
            list(width = '60px', targets = 3),
            list(width = '100px', targets = 4),
            list(width = '100px', targets = 5)
          )
        ),
        rownames = FALSE,
        class = 'compact stripe hover'
      )
    })
    
    # Show the modal with cart contents
    showModal(myModalDialog(
      title = tags$h3(icon("shopping-cart"), " Your Shopping Cart"),
      div(style = "max-height: 400px; overflow-y: auto;",
          DTOutput("cart_table_display")
      ),
      br(),
      tags$div(
        style = "background:#f8f9fa; padding:15px; border-radius:10px;",
        h4(paste("Total Items:", cart_count())),
        h4(paste("Cart Total:", formatted_total))
      ),
      footer = tagList(
        modalButton("Continue Shopping"),
        actionButton("clear_cart_btn", "Clear Cart", 
                     class = "btn-theme",
                     style = "background-color: #f39c12;"),
        actionButton("make_order_btn", "Place Order", class = "btn-theme")
      ),
      size = "l",
      easyClose = TRUE
    ))
    runjs("
  setTimeout(function() {
    // Fix modal centering
    $('.modal').css('display', 'flex');
    $('.modal').css('align-items', 'center');
    $('.modal').css('justify-content', 'center');
    
    // Fix logout tab scrolling
    $('#logout_tab, #staff_logout_tab').css({
      'max-height': 'calc(100vh - 200px)',
      'overflow-y': 'auto'
    });
  }, 100);
")
  })
  
  observeEvent(input$clear_cart_btn, {
    # Show confirmation dialog
    showModal(myModalDialog(
      title = "Confirm Clear Cart",
      "Are you sure you want to clear your cart? This action cannot be undone.",
      footer = tagList(
        actionButton("cancel_clear_cart", "Cancel", 
                     class = "btn-theme",
                     style = "background: linear-gradient(135deg, #6c757d, #495057);"),
        actionButton("confirm_clear_cart", "Yes, Clear Cart", 
                     class = "btn-theme",
                     style = "background-color: #e74c3c;")
      )
    ))
    runjs("
  setTimeout(function() {
    // Fix modal centering
    $('.modal').css('display', 'flex');
    $('.modal').css('align-items', 'center');
    $('.modal').css('justify-content', 'center');
    
    // Fix logout tab scrolling
    $('#logout_tab, #staff_logout_tab').css({
      'max-height': 'calc(100vh - 200px)',
      'overflow-y': 'auto'
    });
  }, 100);
")
  })
  
  # Add this observer for cancel clear cart
  observeEvent(input$cancel_clear_cart, {
    removeModal()
  })
  
  observeEvent(input$confirm_clear_cart, {
    user_data$cart <- list()
    user_data$cart_trigger <- user_data$cart_trigger + 1
    
    # Clear cart from database
    if(user_data$logged_in && !is.null(user_data$user_id)) {
      clear_cart_in_db(user_data$user_id)
    }
    
    removeModal()
    show_notification("Shopping cart cleared", "info")
    removeModal()  # Close cart modal
  })
  
  # --------------------- Place Order - FIXED ---------------------
  observeEvent(input$make_order_btn, {
    # Add debounce to prevent double clicks
    shinyjs::disable("make_order_btn")
    shinyjs::addClass("make_order_btn", "loading-state")
    on.exit({
      shinyjs::enable("make_order_btn")
      shinyjs::removeClass("make_order_btn", "loading-state")
    })
    
    if(length(user_data$cart) == 0){
      showModal(myModalDialog(
        title = tags$div(
          icon("shopping-cart", style = "color: #e63946; margin-right: 10px;"),
          "Empty Cart"
        ),
        tags$div(
          style = "text-align: center; padding: 20px;",
          icon("cart-arrow-down", style = "font-size: 48px; color: #ddd; margin-bottom: 15px;"),
          h4("Your cart is empty", style = "color: #333;"),
          p("Please add items before placing an order.", style = "color: #666;")
        ),
        easyClose = TRUE,
        footer = modalButton("OK"),
        size = "s"
      ))
      return()
    }
    
    # Validate stock before proceeding
    stock_issues <- c()
    for(item in user_data$cart){
      current_stock <- dbGetQueryPool(
        "SELECT stock FROM shoes WHERE shoe_id = ?", 
        params = list(item$shoe_id)
      )$stock
      
      if(as.numeric(current_stock) < as.numeric(item$quantity)){
        stock_issues <- c(stock_issues, 
                          paste0(item$name, " only has ", current_stock, " units available."))
      }
    }
    
    if(length(stock_issues) > 0){
      showModal(myModalDialog(
        title = "Insufficient Stock",
        HTML(paste(stock_issues, collapse = "<br>")),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    total <- sum(sapply(user_data$cart, function(x) as.numeric(x$price) * as.numeric(x$quantity)))
    
    conn <- poolCheckout(db_pool)
    dbExecute(conn, "BEGIN TRANSACTION")
    
    tryCatch({
      dbExecute(
        conn, 
        "INSERT INTO orders (customer_id, status, total_price, created_at, updated_at) 
       VALUES (?, ?, ?, datetime('now'), datetime('now'))",
        params = list(user_data$user_id, "Pending", total)
      )
      
      order_id <- dbGetQuery(conn, "SELECT last_insert_rowid() as id")$id
      track_id <- paste0("SOS", sprintf("%03d", order_id))
      
      for(item in user_data$cart){
        dbExecute(
          conn, 
          "INSERT INTO order_items (order_id, shoe_id, quantity, color, size, price) 
         VALUES (?, ?, ?, ?, ?, ?)",
          params = list(order_id, item$shoe_id, item$quantity, item$color, item$size, item$price)
        )
        
        dbExecute(
          conn, 
          "UPDATE shoes SET stock = stock - ?, updated_at = datetime('now') WHERE shoe_id = ?",
          params = list(item$quantity, item$shoe_id)
        )
      }
      
      dbExecute(conn, "COMMIT")
      
      # Clear cart and update UI immediately
      user_data$cart <- list()
      user_data$cart_trigger <- user_data$cart_trigger + 1
      clear_cart_in_db(user_data$user_id)
      
      # Remove the cart modal first
      removeModal()
      
      # Show success message - wait for modal to close
      delay(100, {
        showModal(myModalDialog(
          title = tags$div(
            icon("check-circle", style = "color: #28a745; margin-right: 10px;"),
            "Order Placed Successfully!"
          ),
          tags$div(
            style = "text-align: center; padding: 20px;",
            icon("party-horn", style = "font-size: 60px; color: #28a745; margin-bottom: 20px;"),
            h4("🎉 Order Confirmed!", style = "color: #28a745; margin-bottom: 15px;"),
            tags$div(
              style = "background: linear-gradient(135deg, rgba(230,57,70,0.1), rgba(26,26,26,0.05)); 
                     padding: 15px; border-radius: 8px; margin: 15px 0;",
              p(style = "font-size: 16px; margin-bottom: 8px;", 
                strong("Track ID: "), 
                tags$span(track_id, style = "font-family: monospace; background: #f8f9fa; 
                        padding: 4px 8px; border-radius: 4px; font-weight: bold;")
              ),
              p(style = "font-size: 16px; margin-bottom: 8px;", 
                strong("Total Amount: "), 
                tags$span(format_currency(total),
                          style = "color: #e63946; font-weight: bold;")
              ),
              tags$p(tags$span("Status: Pending", 
                               class = "status-pending-badge",
                               style = "font-size: 14px; display: inline-block; margin-top: 10px;"))
            ),
            tags$div(
              style = "margin: 20px 0; padding: 15px; background: #f8f9fa; border-radius: 8px;",
              h5("📧 Order Confirmation Sent!", style = "color: #333; margin-bottom: 10px;"),
              p("Thanks for your order! You can track it in the 'Order Status' tab.", 
                style = "color: #666;")
            )
          ),
          footer = tagList(
            actionButton("continue_shopping_btn", "Continue Shopping", 
                         class = "btn-theme",
                         style = "width: 100%;")
          ),
          size = "m",
          easyClose = TRUE
        ))
        
        # Show toast notification after order success modal appears
        delay(200, {
          show_notification(
            paste("Order", track_id, "placed successfully!"),
            "success",
            wait_for_modal = TRUE  # Wait for order success modal to close
          )
        })
      })
      
    }, error = function(e){
      dbExecute(conn, "ROLLBACK")
      showModal(myModalDialog(
        title = tags$div(
          icon("times-circle", style = "color: #dc3545; margin-right: 10px;"),
          "Order Failed"
        ),
        tags$div(
          style = "text-align: center; padding: 20px;",
          icon("exclamation-triangle", style = "font-size: 48px; color: #dc3545; margin-bottom: 15px;"),
          h4("Something went wrong", style = "color: #333;"),
          p(paste("Error:", e$message), style = "color: #666;")
        ),
        easyClose = TRUE,
        footer = modalButton("OK"),
        size = "s"
      ))
    }, finally = {
      poolReturn(conn)
    })
  })
  
  # Add this observer for continue shopping button
  observeEvent(input$continue_shopping_btn, {
    removeModal()
  })
  
  # --------------------- Customer Orders (ACTIVE ONLY) ---------------------
  order_refresh_trigger <- reactiveVal(0)
  # Update the customer_orders_data reactive to include the trigger
  # --------------------- Customer Orders (ACTIVE ONLY) ---------------------
  customer_orders_data <- reactive({
    # Add dependencies
    input$cancel_order
    order_refresh_trigger()
    invalidateLater(10000)
    
    if(user_data$logged_in && user_data$role == "Customer"){
      tryCatch({
        # Only show active orders (Pending/Processing)
        orders <- dbGetQueryPool(
          "SELECT order_id, total_price, created_at, status 
         FROM orders 
         WHERE customer_id = ? 
           AND status NOT IN ('Completed', 'Cancelled')
         ORDER BY datetime(created_at, 'localtime') DESC",
          params = list(user_data$user_id)
        )
        
        if(nrow(orders) > 0) {
          orders$track_id <- paste0("SOS", sprintf("%03d", orders$order_id))
          orders$formatted_date <- sapply(orders$created_at, format_date)
          orders$formatted_total <- format_currency(orders$total_price)
        }
        
        return(orders)
      }, error = function(e) {
        showNotification(paste("Failed to load orders:", e$message), type = "error")
        return(data.frame())
      })
    } else {
      return(data.frame())
    }
  })
  
  output$customer_orders <- renderDT({
    orders <- customer_orders_data()
    
    if(is.null(orders) || nrow(orders) == 0) {
      return(datatable(
        data.frame(Message = "You haven't placed any ongoing orders yet"), 
        options = list(
          dom = 't', 
          ordering = FALSE,
          language = list(zeroRecords = "You haven't placed any ongoing orders yet")
        ),
        rownames = FALSE
      ))
    }
    
    orders$action <- sapply(1:nrow(orders), function(i){
      if(orders$status[i] == "Pending"){
        as.character(actionButton(
          paste0("cancel_", orders$order_id[i]), 
          "Cancel", 
          class = "btn-theme btn-sm",
          style = "background-color: #e74c3c; font-size: 12px; padding: 4px 10px;",
          onclick = sprintf(
            "Shiny.setInputValue('cancel_order', %d, {priority:'event'})", 
            orders$order_id[i]
          )
        ))
      } else {
        "-"
      }
    })
    
    # Use formatted columns with proper dates
    display <- data.frame(
      "Track ID" = orders$track_id,
      "Total" = orders$formatted_total,
      "Date" = orders$formatted_date,
      "Action" = orders$action,
      stringsAsFactors = FALSE
    )
    
    datatable(
      display, 
      escape = FALSE,
      options = list(
        pageLength = 10, 
        dom = 'tip',
        scrollX = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all'),
          list(width = '120px', targets = 0), # Track ID
          list(width = '120px', targets = 1), # Total
          list(width = '200px', targets = 2), # Date
          list(width = '100px', targets = 3)  # Action
        ),
        language = list(
          emptyTable = "You haven't placed any ongoing orders yet",
          zeroRecords = "You haven't placed any ongoing orders yet"
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'  # Standard table class
    )
  })
  
  # --------------------- Cancel Order ---------------------
  observeEvent(input$cancel_order, {
    order_id <- input$cancel_order
    
    showModal(myModalDialog(
      title = "Confirm Cancellation",
      tags$div(
        style = "text-align:center;",
        icon("exclamation-triangle", style = "font-size:48px; color:#e74c3c;"),
        br(), br(),
        p(paste("Are you sure you want to cancel order ID", order_id, "?")),
        p("This action cannot be undone.", style = "color:#7f8c8d;")
      ),
      footer = tagList(
        actionButton("cancel_cancel_order", "No, Keep Order", 
                     class = "btn-theme",
                     style = "background: linear-gradient(135deg, #6c757d, #495057);"),
        actionButton("confirm_cancel", "Yes, Cancel Order", 
                     class = "btn-theme",
                     style = "background-color: #e74c3c;")
      )
    ))
    runjs("
  setTimeout(function() {
    // Fix modal centering
    $('.modal').css('display', 'flex');
    $('.modal').css('align-items', 'center');
    $('.modal').css('justify-content', 'center');
    
    // Fix logout tab scrolling
    $('#logout_tab, #staff_logout_tab').css({
      'max-height': 'calc(100vh - 200px)',
      'overflow-y': 'auto'
    });
  }, 100);
")
  })
  
  # Add this observer for cancel cancel order
  observeEvent(input$cancel_cancel_order, {
    removeModal()
  })
  
  # Update the cancellation handler to trigger immediate refresh
  observeEvent(input$confirm_cancel, {
    order_id <- input$cancel_order
    
    conn <- poolCheckout(db_pool)
    dbExecute(conn, "BEGIN TRANSACTION")
    
    tryCatch({
      order_items <- dbGetQuery(
        conn,
        "SELECT shoe_id, quantity FROM order_items WHERE order_id = ?",
        params = list(order_id)
      )
      
      for(i in 1:nrow(order_items)){
        dbExecute(
          conn, 
          "UPDATE shoes SET stock = stock + ?, updated_at = datetime('now') WHERE shoe_id = ?",
          params = list(order_items$quantity[i], order_items$shoe_id[i])
        )
      }
      
      dbExecute(
        conn, 
        "UPDATE orders SET status = 'Cancelled', updated_at = datetime('now') WHERE order_id = ?",
        params = list(order_id)
      )
      
      dbExecute(conn, "COMMIT")
      removeModal()
      
      # TRIGGER IMMEDIATE REFRESH
      order_refresh_trigger(order_refresh_trigger() + 1)
      
      showNotification("Order cancelled successfully. Stock has been restored.", 
                       type = "message", duration = 3)
      
    }, error = function(e){
      dbExecute(conn, "ROLLBACK")
      showNotification(paste("Error cancelling order:", e$message), type = "error")
    }, finally = {
      poolReturn(conn)
    })
  })
  
  # --------------------- Order Status Search (ENHANCED) ---------------------
  observeEvent(input$search_order_btn, {
    output$customer_status <- renderDT({
      req(input$search_order_btn)
      
      track_id <- trimws(input$search_track_id)
      
      if(track_id == "") {
        return(datatable(
          data.frame(Message = "Enter a Track ID above and click Search"), 
          options = list(dom = 't', ordering = FALSE),
          rownames = FALSE
        ))
      }
      
      if(!grepl("^SOS[0-9]+$", track_id)) {
        return(datatable(
          data.frame(Message = "Invalid Track ID format. Should be SOS followed by numbers (e.g., SOS001)"), 
          options = list(dom = 't', ordering = FALSE),
          rownames = FALSE
        ))
      }
      
      order_id <- as.numeric(sub("SOS", "", track_id))
      
      tryCatch({
        order <- dbGetQueryPool(
          "SELECT * FROM orders WHERE order_id = ? AND customer_id = ?",
          params = list(order_id, user_data$user_id)
        )
        
        if(nrow(order) == 0) {
          return(datatable(
            data.frame(Message = "Order not found or does not belong to you"), 
            options = list(dom = 't', ordering = FALSE),
            rownames = FALSE
          ))
        }
        
        # Format date and total price
        order$formatted_date <- format_date(order$created_at)
        order$formatted_total <- format_currency(order$total_price)
        
        # Create summary table with status prominently displayed
        summary_df <- data.frame(
          Detail = c("Track ID", "Status", "Date", "Total"),
          Value = c(
            track_id,
            paste0("<span class='status-", tolower(gsub(" ", "-", order$status)), "-badge'>", order$status, "</span>"),
            order$formatted_date,
            order$formatted_total
          ),
          stringsAsFactors = FALSE
        )
        
        # Get order items
        order_items <- dbGetQueryPool(
          "SELECT s.name, oi.quantity, oi.color, oi.size, oi.price 
         FROM order_items oi 
         JOIN shoes s ON oi.shoe_id = s.shoe_id 
         WHERE oi.order_id = ?",
          params = list(order$order_id)
        )
        
        if(nrow(order_items) > 0) {
          # Format order items
          order_items$formatted_price <- format_currency(order_items$price)
          order_items$formatted_total <- format_currency(order_items$price * order_items$quantity)
          order_items$formatted_quantity <- format_number(order_items$quantity, digits = 0)
          
          # Combine summary and items
          combined_df <- rbind(
            data.frame(
              Item = "ORDER SUMMARY",
              Quantity = "",
              Color = "",
              Size = "",
              Price = "",
              Total = "",
              stringsAsFactors = FALSE
            ),
            data.frame(
              Item = summary_df$Detail,
              Quantity = summary_df$Value,
              Color = "",
              Size = "",
              Price = "",
              Total = "",
              stringsAsFactors = FALSE
            ),
            data.frame(
              Item = "",
              Quantity = "",
              Color = "",
              Size = "",
              Price = "",
              Total = "",
              stringsAsFactors = FALSE
            ),
            data.frame(
              Item = "ORDER ITEMS",
              Quantity = "",
              Color = "",
              Size = "",
              Price = "",
              Total = "",
              stringsAsFactors = FALSE
            ),
            data.frame(
              Item = order_items$name,
              Quantity = order_items$formatted_quantity,
              Color = order_items$color,
              Size = order_items$size,
              Price = order_items$formatted_price,
              Total = order_items$formatted_total,
              stringsAsFactors = FALSE
            )
          )
          
          datatable(
            combined_df, 
            escape = FALSE,
            options = list(
              pageLength = 20,
              dom = 't',
              ordering = FALSE,
              scrollX = FALSE,
              paging = FALSE,
              info = FALSE
            ),
            rownames = FALSE
          )
        } else {
          datatable(
            summary_df,
            colnames = c("Detail", "Value"),
            escape = FALSE,
            options = list(dom = 't', ordering = FALSE),
            rownames = FALSE
          )
        }
      }, error = function(e) {
        showNotification(paste("Failed to search order:", e$message), type = "error")
        return(datatable(
          data.frame(Message = "Error searching order"), 
          options = list(dom = 't', ordering = FALSE),
          rownames = FALSE
        ))
      })
    })
    apply_status_styles()
  })
  
  # Apply status badge styles
  session$sendCustomMessage("applyStatusStyles", list())
  
  output$customer_completed <- renderDT({
    tryCatch({
      orders <- dbGetQueryPool(
        "SELECT order_id, total_price, created_at, status 
       FROM orders 
       WHERE customer_id = ? AND status IN ('Completed', 'Cancelled')
       ORDER BY datetime(created_at, 'localtime') DESC",
        params = list(user_data$user_id)
      )
      
      if(nrow(orders) == 0) {
        return(datatable(
          data.frame(Message = "No completed or cancelled orders"), 
          options = list(dom = 't', ordering = FALSE),
          rownames = FALSE
        ))
      }
      
      orders$track_id <- paste0("SOS", sprintf("%03d", orders$order_id))
      orders$status_formatted <- sapply(orders$status, function(s){
        paste0("<span class='status-", tolower(gsub(" ", "-", s)), "-badge'>", s, "</span>")
      })
      
      # FORMAT DATE CONSISTENTLY - USING THE SAME FORMAT AS MANAGE ORDERS
      orders$formatted_date <- sapply(orders$created_at, format_date)
      orders$formatted_total <- format_currency(orders$total_price)
      
      display <- data.frame(
        "Track ID" = orders$track_id,
        "Total" = orders$formatted_total,
        "Date" = orders$formatted_date,
        "Status" = orders$status_formatted,
        stringsAsFactors = FALSE
      )
      
      datatable(
        display, 
        escape = FALSE,
        selection = 'single',
        options = list(
          pageLength = 10, 
          dom = 'tip',
          columnDefs = list(
            list(className = 'dt-center', targets = '_all'),
            list(width = '120px', targets = 0), # Track ID
            list(width = '120px', targets = 1), # Total
            list(width = '180px', targets = 2), # Date
            list(width = '120px', targets = 3)  # Status
          ),
          language = list(
            emptyTable = "No completed or cancelled orders",
            zeroRecords = "No completed or cancelled orders"
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover'
      )
    }, error = function(e) {
      showNotification(paste("Failed to load order history:", e$message), type = "error")
      return(datatable(
        data.frame(Message = "Error loading order history"), 
        options = list(dom = 't', ordering = FALSE),
        rownames = FALSE
      ))
    })
  })
  
  # --------------------- Order History Modal (Customer) ---------------------
  observeEvent(input$customer_completed_rows_selected, {
    selected <- input$customer_completed_rows_selected
    if(length(selected) == 0) return()
    
    tryCatch({
      orders <- dbGetQueryPool(
        "SELECT order_id, total_price, created_at, status 
       FROM orders 
       WHERE customer_id = ? AND status IN ('Completed', 'Cancelled')
       ORDER BY datetime(created_at, 'localtime') DESC",
        params = list(user_data$user_id)
      )
      
      if(nrow(orders) == 0) return()
      
      order <- orders[selected, ]
      track_id <- paste0("SOS", sprintf("%03d", order$order_id))
      
      # Format date and total
      order$formatted_date <- format_date(order$created_at)
      order$formatted_total <- format_currency(order$total_price)
      
      # Get order items
      order_items <- dbGetQueryPool(
        "SELECT s.name, oi.quantity, oi.color, oi.size, oi.price 
       FROM order_items oi 
       JOIN shoes s ON oi.shoe_id = s.shoe_id 
       WHERE oi.order_id = ?",
        params = list(order$order_id)
      )
      
      # Create order summary HTML
      order_summary <- tags$div(
        class = "order-summary",
        h4(paste("Order Details -", track_id), style = "color: #2c3e50; margin-bottom: 15px;"),
        hr(),
        p(strong("Status: "), 
          tags$span(order$status, 
                    class = paste0("status-", tolower(order$status), "-badge"))),
        p(strong("Date: "), order$formatted_date),
        p(strong("Total: "), order$formatted_total),
        br()
      )
      
      # Create items table
      if(nrow(order_items) > 0) {
        # Format order items
        order_items$formatted_price <- format_currency(order_items$price)
        order_items$formatted_total <- format_currency(order_items$price * order_items$quantity)
        order_items$formatted_quantity <- format_number(order_items$quantity, digits = 0)
        
        items_html <- datatable(
          order_items[, c("name", "formatted_quantity", "color", "size", "formatted_price", "formatted_total")],
          colnames = c("Product", "Qty", "Color", "Size", "Price", "Total"),
          options = list(
            pageLength = 5,
            dom = 't',
            ordering = FALSE,
            scrollX = FALSE,
            columnDefs = list(
              list(className = 'dt-center', targets = '_all'),
              list(width = '200px', targets = 0),  # Product
              list(width = '80px', targets = 1),   # Qty
              list(width = '100px', targets = 2),  # Color
              list(width = '80px', targets = 3),   # Size
              list(width = '100px', targets = 4),  # Price
              list(width = '100px', targets = 5)   # Total
            )
          ),
          rownames = FALSE,
          class = 'cell-border stripe hover'
        )
        
        # Show modal with details
        showModal(myModalDialog(
          title = tags$div(
            icon("receipt"),
            paste("Order History -", track_id)
          ),
          order_summary,
          tags$h5("Items:", style = "color: #2c3e50; margin-top: 15px;"),
          DTOutput("history_order_items"),
          footer = modalButton("Close"),
          size = "l",
          easyClose = TRUE,
          class = "order-history-modal"
        ))
        runjs("
        setTimeout(function() {
          $('.modal').css('display', 'flex');
          $('.modal').css('align-items', 'center');
          $('.modal').css('justify-content', 'center');
        }, 100);
      ")
        
        # Render the items table
        output$history_order_items <- renderDT({
          items_html
        })
      }
    }, error = function(e) {
      showNotification(paste("Failed to load order details:", e$message), type = "error")
    })
  })
  
  # --------------------- Dashboard Trigger ---------------------
  dashboard_data <- reactive({
    input$mark_completed
    input$mark_cancelled
    invalidateLater(10000)
    apply_status_styles()
    
    return(list(timestamp = Sys.time()))
  })
  
  # --------------------- Staff Dashboard ---------------------
  output$total_orders_box <- renderUI({
    dashboard_data()
    
    tryCatch({
      total <- dbGetQueryPool("SELECT COUNT(*) as count FROM orders")$count
      total <- ifelse(is.na(total), 0, total)
      formatted_total <- format_number(total, digits = 0)
    }, error = function(e) {
      formatted_total <- "0"
    })
    
    dashboard_box(
      icon_name = "shopping-bag",
      value = formatted_total,  # USE FORMATTED NUMBER
      label = "Total Orders",
      gradient = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);"
    )
  })
  
  output$pending_orders_box <- renderUI({
    dashboard_data()
    
    tryCatch({
      pending <- dbGetQueryPool("SELECT COUNT(*) as count FROM orders WHERE status = 'Pending'")$count
      pending <- ifelse(is.na(pending), 0, pending)
      formatted_pending <- formatC(pending, format = "d", big.mark = ",")
    }, error = function(e) {
      formatted_pending <- "0"
    })
    
    dashboard_box(
      icon_name = "clock",
      value = formatted_pending,  # USE FORMATTED NUMBER
      label = "Pending Orders",
      gradient = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);"
    )
  })
  
  output$total_sales_box <- renderUI({
    dashboard_data()
    
    tryCatch({
      sales <- dbGetQueryPool("SELECT SUM(total_price) as total FROM orders WHERE status = 'Completed'")$total
      sales <- ifelse(is.na(sales), 0, sales)
      formatted_sales <- paste0("₱", formatC(sales, format = "f", big.mark = ",", digits = 2))
    }, error = function(e) {
      formatted_sales <- "₱0.00"
    })
    
    dashboard_box(
      icon_name = "money-bill-wave",
      value = formatted_sales,  # USE FORMATTED NUMBER
      label = "Total Sales",
      gradient = "background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%);"
    )
  })
  
  output$total_stock_box <- renderUI({
    dashboard_data()
    
    tryCatch({
      stock <- dbGetQueryPool("SELECT SUM(stock) as total FROM shoes")$total
      stock <- ifelse(is.na(stock), 0, stock)
      formatted_stock <- formatC(stock, format = "d", big.mark = ",")
    }, error = function(e) {
      formatted_stock <- "0"
    })
    
    dashboard_box(
      icon_name = "boxes",
      value = formatted_stock,  # USE FORMATTED NUMBER
      label = "Total Stock",
      gradient = "background: linear-gradient(135deg, #43e97b 0%, #38f9d7 100%);"
    )
  })
  
  # --------------------- Low Stock Alerts ---------------------
  output$low_stock_alerts <- renderUI({
    req(user_data$logged_in && user_data$role == "Staff")
    
    tryCatch({
      low_stock <- dbGetQueryPool("
        SELECT name, stock 
        FROM shoes 
        WHERE stock <= 10 AND available = 1 
        ORDER BY stock
        LIMIT 5")
      
      if(nrow(low_stock) > 0) {
        alert_items <- lapply(1:nrow(low_stock), function(i) {
          tags$li(
            style = paste0("color:", ifelse(low_stock$stock[i] <= 3, "#e74c3c", "#f39c12"), ";"),
            paste(low_stock$name[i], "-", low_stock$stock[i], "units left")
          )
        })
        
        tags$div(
          class = "alert alert-warning",
          style = "padding: 15px; border-radius: 5px; background-color: #fff3cd; border: 1px solid #ffeaa7;",
          h5(icon("exclamation-triangle"), " Low Stock Alert"),
          tags$ul(style = "margin-bottom: 0;", alert_items)
        )
      }
    }, error = function(e) {
      return(NULL)
    })
  })
  
  output$sales_plot <- renderPlot({
    dashboard_data()
    
    tryCatch({
      sales_data <- dbGetQueryPool("
        SELECT DATE(created_at) as date, SUM(total_price) as daily_sales 
        FROM orders 
        WHERE status = 'Completed' 
        GROUP BY DATE(created_at) 
        ORDER BY date DESC 
        LIMIT 14")
      
      if(nrow(sales_data) > 0){
        sales_data <- sales_data[order(sales_data$date), ]
        sales_data$date <- as.Date(sales_data$date)
        
        ggplot(sales_data, aes(x = date, y = daily_sales)) +
          geom_bar(stat = "identity", fill = "#1abc9c", width = 0.7) +
          # In the sales plot renderPlot, update geom_text label:
          geom_text(aes(label = format_currency(daily_sales)), 
                    vjust = -0.5, size = 3.5, fontface = "bold") +
          labs(title = "Last 14 Days Sales Performance", 
               x = "Date", 
               y = "Sales (₱)") +
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#2c3e50"),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            axis.text.y = element_text(size = 10),
            axis.title = element_text(size = 12, face = "bold"),
            panel.grid.major.y = element_line(color = "gray90"),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA)
          ) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
      } else {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = "No sales data available yet", 
                   size = 7, color = "#95a5a6") +
          theme_void()
      }
    }, error = function(e) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Error loading sales data", 
                 size = 7, color = "#e74c3c") +
        theme_void()
    })
  })
  
  staff_orders_data <- reactive({
    input$refresh_staff_orders
    input$mark_processing
    input$mark_to_ship
    input$mark_shipped
    input$mark_completed
    input$mark_cancelled
    
    tryCatch({
      # Only show active orders (Pending/Processing/To Ship/Shipped)
      orders <- dbGetQueryPool("
      SELECT o.*, u.username 
      FROM orders o 
      JOIN users u ON o.customer_id = u.user_id 
      WHERE o.status NOT IN ('Completed', 'Cancelled')
      ORDER BY 
        CASE o.status 
          WHEN 'Pending' THEN 1 
          WHEN 'Processing' THEN 2 
          WHEN 'To Ship' THEN 3
          WHEN 'Shipped' THEN 4
          ELSE 5 
        END,
        o.created_at DESC")
      
      if(nrow(orders) > 0){
        orders$status_formatted <- sapply(orders$status, function(s){
          paste0("<span class='status-", tolower(gsub(" ", "-", s)), "-badge'>", s, "</span>")
        })
        
        orders$track_id <- paste0("SOS", sprintf("%03d", orders$order_id))
        # Format date and total
        orders$formatted_date <- sapply(orders$created_at, format_date)
        orders$formatted_total <- format_currency(orders$total_price)
      }
      
      return(orders)
    }, error = function(e) {
      showNotification(paste("Failed to load orders:", e$message), type = "error")
      return(data.frame())
    })
  })
  
  # --------------------- Staff Orders Management (ACTIVE ONLY) ---------------------
  output$staff_orders_table <- renderDT({
    orders <- staff_orders_data()
    
    if(is.null(orders) || nrow(orders) == 0){
      return(datatable(
        data.frame(Message = "No active orders to manage"), 
        options = list(
          dom = 't', 
          ordering = FALSE
        ),
        selection = 'none',
        rownames = FALSE
      ))
    }
    
    # Use formatted columns with proper dates
    display <- data.frame(
      "Track ID" = orders$track_id,
      "Customer" = orders$username,
      "Total" = orders$formatted_total,
      "Date" = orders$formatted_date,
      "Status" = orders$status_formatted,
      stringsAsFactors = FALSE
    )
    
    datatable(
      display, 
      escape = FALSE,
      selection = 'single',
      options = list(
        pageLength = 10, 
        dom = 'tip',
        scrollX = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all'),
          list(width = '120px', targets = 0), # Track ID
          list(width = '150px', targets = 1), # Customer
          list(width = '120px', targets = 2), # Total
          list(width = '180px', targets = 3), # Date
          list(width = '120px', targets = 4)  # Status
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'  # Standard table class
    )
  })
  
  output$staff_order_actions <- renderUI({
    selected <- input$staff_orders_table_rows_selected
    if(length(selected) == 0) return(
      tags$div(
        style = "text-align:center; padding:20px; color:#7f8c8d;",
        icon("hand-pointer", style = "font-size:24px;"),
        p("Select an order from the table above to manage it")
      )
    )
    
    order <- staff_orders_data()[selected, ]
    current_status <- order$status
    
    # Determine which buttons to show based on current status
    show_processing <- current_status == "Pending"
    show_to_ship <- current_status == "Processing"
    show_shipped <- current_status == "To Ship"
    show_completed <- current_status == "Shipped"
    show_cancelled <- current_status %in% c("Pending", "Processing")  # Only allow cancellation before shipping
    
    
    # Get order items
    order_items <- tryCatch({
      dbGetQueryPool(
        "SELECT s.name, oi.quantity, oi.color, oi.size, oi.price 
     FROM order_items oi 
     JOIN shoes s ON oi.shoe_id = s.shoe_id 
     WHERE oi.order_id = ?",
        params = list(order$order_id)
      )
    }, error = function(e) {
      data.frame()
    })
    
    fluidRow(
      column(12,
             div(
               class = "order-actions-container",
               h4(paste("Managing Order ID:", order$order_id), style = "color:#2c3e50;"),
               hr(),
               p(icon("hashtag"), strong(" Track ID: "), 
                 tags$span(order$track_id, class = "track-id-badge")),
               p(icon("user"), strong(" Customer: "), order$username),
               p(icon("money-bill-wave"), strong(" Total: "), format_currency(order$total_price)),
               p(icon("calendar"), strong(" Date: "), format_date(order$created_at)),
               p(icon("info-circle"), strong(" Current Status: "), 
                 tags$span(order$status, class = paste0("status-", tolower(gsub(" ", "-", order$status)), "-badge"))),
               
               # Order Items Section
               if(nrow(order_items) > 0) {
                 tagList(
                   hr(),
                   h5("Order Items:", style = "color:#2c3e50; margin-top:15px;"),
                   tags$div(
                     class = "order-items-table",
                     DTOutput("order_items_display")
                   )
                 )
               },
               
               br(),
               div(class = "status-buttons-container",
                   if(show_processing) actionButton("mark_processing", icon("cogs"), "Mark as Processing", 
                                                    class = "btn-theme", style = "margin-right:10px; background-color: #3498db;"),
                   if(show_to_ship) actionButton("mark_to_ship", icon("shipping-fast"), "Mark as To Ship", 
                                                 class = "btn-theme", style = "margin-right:10px; background-color: #9b59b6;"),
                   if(show_shipped) actionButton("mark_shipped", icon("truck"), "Mark as Shipped", 
                                                 class = "btn-theme", style = "margin-right:10px; background-color: #1abc9c;"),
                   if(show_completed) actionButton("mark_completed", icon("check-circle"), "Mark as Completed", 
                                                   class = "btn-theme", style = "margin-right:10px; background-color: #27ae60;"),
                   if(show_cancelled) actionButton("mark_cancelled", icon("times-circle"), "Cancel Order", 
                                                   class = "btn-theme", style = "background-color: #e74c3c;")
               )
             )
      )
    )
  })
  
  # Render order items table
  output$order_items_display <- renderDT({
    selected <- input$staff_orders_table_rows_selected
    if(length(selected) == 0) return()
    
    order <- staff_orders_data()[selected, ]
    
    tryCatch({
      order_items <- dbGetQueryPool(
        "SELECT s.name, oi.quantity, oi.color, oi.size, oi.price 
         FROM order_items oi 
         JOIN shoes s ON oi.shoe_id = s.shoe_id 
         WHERE oi.order_id = ?",
        params = list(order$order_id)
      )
      
      if(nrow(order_items) > 0) {
        order_items$Total <- order_items$price * order_items$quantity
        
        datatable(
          order_items[, c("name", "quantity", "color", "size", "price", "Total")],
          colnames = c("Product", "Qty", "Color", "Size", "Price", "Total"),
          options = list(
            pageLength = 5,
            dom = 't',
            ordering = FALSE
          ),
          rownames = FALSE
        ) %>%
          formatCurrency(c("price", "Total"), "₱")
      }
    }, error = function(e) {
      return(datatable(data.frame(Message = "Error loading order items")))
    })
  })
  
  observeEvent(input$mark_processing, {
    selected <- input$staff_orders_table_rows_selected
    if(length(selected) == 0) return()
    
    order <- staff_orders_data()[selected, ]
    tryCatch({
      dbExecutePool(
        "UPDATE orders SET status = 'Processing', updated_at = datetime('now') WHERE order_id = ?",
        params = list(order$order_id)
      )
      showNotification(
        HTML(paste(
          "<div style='text-align:center;'>",
          "<h4 style='color:#3498db;'>🔄 Order Processing!</h4>",
          "<p>Order", order$track_id, "has been marked as Processing.</p>",
          "</div>"
        )), 
        type = "message",
        duration = 3
      )
    }, error = function(e) {
      showNotification(paste("Failed to update order status:", e$message), type = "error")
    })
  })
  
  observeEvent(input$mark_to_ship, {
    selected <- input$staff_orders_table_rows_selected
    if(length(selected) == 0) return()
    
    order <- staff_orders_data()[selected, ]
    tryCatch({
      dbExecutePool(
        "UPDATE orders SET status = 'To Ship', updated_at = datetime('now') WHERE order_id = ?",
        params = list(order$order_id)
      )
      show_notification(
        paste("Order", order$track_id, "has been marked as 'To Ship'."),
        "success",
        duration = 3000
      )
    }, error = function(e) {
      showNotification(paste("Failed to update order status:", e$message), type = "error")
    })
  })
  
  observeEvent(input$mark_shipped, {
    selected <- input$staff_orders_table_rows_selected
    if(length(selected) == 0) return()
    
    order <- staff_orders_data()[selected, ]
    tryCatch({
      dbExecutePool(
        "UPDATE orders SET status = 'Shipped', updated_at = datetime('now') WHERE order_id = ?",
        params = list(order$order_id)
      )
      show_notification(
        paste("Order", order$track_id, "has been marked as 'Shipped'."),
        "success",
        duration = 3000
      )
    }, error = function(e) {
      showNotification(paste("Failed to update order status:", e$message), type = "error")
    })
  })
  
  observeEvent(input$mark_completed, {
    selected <- input$staff_orders_table_rows_selected
    if(length(selected) == 0) return()
    
    order <- staff_orders_data()[selected, ]
    
    conn <- poolCheckout(db_pool)
    dbExecute(conn, "BEGIN TRANSACTION")
    
    tryCatch({
      # Update order status
      dbExecute(
        conn,
        "UPDATE orders SET status = 'Completed', updated_at = datetime('now') WHERE order_id = ?",
        params = list(order$order_id)
      )
      
      # Add to sales table
      dbExecute(
        conn,
        "INSERT INTO sales (order_id, amount, sale_date) VALUES (?, ?, datetime('now'))",
        params = list(order$order_id, order$total_price)
      )
      
      dbExecute(conn, "COMMIT")
      
      showNotification(
        HTML(paste(
          "<div style='text-align:center;'>",
          "<h4 style='color:#27ae60;'>✅ Order Completed!</h4>",
          "<p>Order", order$track_id, "has been marked as completed.</p>",
          "<p>Sale of $", sprintf("%.2f", order$total_price), " recorded.</p>",
          "</div>"
        )), 
        type = "message",
        duration = 3
      )
      
    }, error = function(e){
      dbExecute(conn, "ROLLBACK")
      showNotification(paste("Error:", e$message), type = "error")
    }, finally = {
      poolReturn(conn)
    })
  })
  
  observeEvent(input$mark_cancelled, {
    selected <- input$staff_orders_table_rows_selected
    if(length(selected) == 0) return()
    
    order <- staff_orders_data()[selected, ]
    
    conn <- poolCheckout(db_pool)
    dbExecute(conn, "BEGIN TRANSACTION")
    
    tryCatch({
      order_items <- dbGetQuery(
        conn,
        "SELECT shoe_id, quantity FROM order_items WHERE order_id = ?",
        params = list(order$order_id)
      )
      
      for(i in 1:nrow(order_items)){
        dbExecute(
          conn, 
          "UPDATE shoes SET stock = stock + ?, updated_at = datetime('now') WHERE shoe_id = ?",
          params = list(order_items$quantity[i], order_items$shoe_id[i])
        )
      }
      
      dbExecute(
        conn, 
        "UPDATE orders SET status = 'Cancelled', updated_at = datetime('now') WHERE order_id = ?",
        params = list(order$order_id)
      )
      
      dbExecute(conn, "COMMIT")
      showNotification(
        paste("Order", order$track_id, "cancelled and stock restored"), 
        type = "warning"
      )
      
    }, error = function(e){
      dbExecute(conn, "ROLLBACK")
      showNotification(paste("Error:", e$message), type = "error")
    }, finally = {
      poolReturn(conn)
    })
  })
  
  # --------------------- Staff Shoes Management ---------------------
  staff_shoes_data <- reactive({
    refresh_trigger$shoes  # Add dependency
    
    # Use debounce for search inputs
    inventory_search <- input$inventory_search
    availability_filter <- input$availability_filter
    
    tryCatch({
      # Base query
      query <- "SELECT * FROM shoes WHERE 1=1"
      params <- list()
      
      # Add search filter
      if(!is.null(inventory_search) && nchar(inventory_search) > 0) {
        query <- paste(query, "AND (name LIKE ? OR colors LIKE ? OR sizes LIKE ?)")
        search_term <- paste0("%", inventory_search, "%")
        params <- c(params, list(search_term, search_term, search_term))
      }
      
      # Add availability filter
      if(!is.null(availability_filter) && availability_filter != "All") {
        if(availability_filter == "Available Only") {
          query <- paste(query, "AND available = 1")
        } else if(availability_filter == "Unavailable Only") {
          query <- paste(query, "AND available = 0")
        }
      }
      
      query <- paste(query, "ORDER BY shoe_id")
      
      if(length(params) > 0) {
        shoes <- dbGetQueryPool(query, params = params)
      } else {
        shoes <- dbGetQueryPool(query)
      }
      
      return(shoes)
    }, error = function(e) {
      showNotification(paste("Failed to load shoes:", e$message), type = "error")
      return(data.frame())
    })
  }) %>% debounce(500)
  
  # Add manual refresh trigger
  observeEvent(input$refresh_shoes_btn, {
    refresh_trigger$shoes <- refresh_trigger$shoes + 1
    showNotification("Inventory refreshed", type = "message", duration = 2)
  })
  
  output$staff_shoes_table <- renderDT({
    shoes <- staff_shoes_data()
    
    if(is.null(shoes) || nrow(shoes) == 0){
      search_status <- ""
      if(!is.null(input$inventory_search) && nchar(input$inventory_search) > 0) {
        search_status <- paste("No shoes found matching:", input$inventory_search)
      } else if(!is.null(input$availability_filter) && input$availability_filter != "All") {
        search_status <- paste("No shoes found with filter:", input$availability_filter)
      } else {
        search_status <- "No shoes in inventory. Click 'Add New Shoe' to get started."
      }
      
      return(datatable(
        data.frame(Message = search_status), 
        options = list(dom = 't', ordering = FALSE),
        rownames = FALSE
      ))
    }
    
    shoes$actions <- sapply(shoes$shoe_id, function(id){
      paste(
        as.character(actionButton(
          paste0("edit_", id), 
          icon("edit"), " Edit",
          class = "btn-theme btn-sm",
          style = "margin-right: 5px; background-color: #f39c12; font-size: 12px; padding: 4px 10px;",
          onclick = sprintf("Shiny.setInputValue('edit_shoe', %d, {priority:'event'})", id)
        )),
        as.character(actionButton(
          paste0("delete_", id), 
          icon("trash"), " Delete",
          class = "btn-theme btn-sm",
          style = "background-color: #e74c3c; font-size: 12px; padding: 4px 10px;",
          onclick = sprintf("Shiny.setInputValue('delete_shoe', %d, {priority:'event'})", id)
        ))
      )
    })
    
    shoes$available <- ifelse(shoes$available == 1, 
                              "<span style='color:#27ae60; font-weight:bold;'>Yes</span>", 
                              "<span style='color:#e74c3c; font-weight:bold;'>No</span>")
    
    # Format stock with commas
    shoes$formatted_stock <- format_number(shoes$stock, digits = 0)
    shoes$formatted_price <- format_currency(shoes$price)
    
    # Add row count information
    if(!is.null(input$inventory_search) && nchar(input$inventory_search) > 0) {
      message <- paste("Showing", nrow(shoes), "shoes matching:", input$inventory_search)
    } else {
      message <- paste("Total shoes:", nrow(shoes))
    }
    
    display <- data.frame(
      "ID" = shoes$shoe_id,
      "Name" = shoes$name,
      "Price" = shoes$formatted_price,
      "Stock" = shoes$formatted_stock,
      "Colors" = shoes$colors,
      "Sizes" = shoes$sizes,
      "Available" = shoes$available,
      "Actions" = shoes$actions,
      stringsAsFactors = FALSE
    )
    
    datatable(
      display,
      caption = tags$caption(style = "caption-side: top; font-size: 16px; font-weight: bold; color: #2c3e50;",
                             message),
      escape = FALSE,
      options = list(
        pageLength = 10, 
        dom = 'tip',
        scrollX = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = c(0, 2, 3, 6, 7)),
          list(width = '80px', targets = 0),   # ID
          list(width = '200px', targets = 1),  # Name
          list(width = '100px', targets = 2),  # Price
          list(width = '80px', targets = 3),   # Stock
          list(width = '150px', targets = 4),  # Colors
          list(width = '150px', targets = 5),  # Sizes
          list(width = '100px', targets = 6),  # Available
          list(width = '150px', targets = 7)   # Actions
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'  # Standard table class
    )
  })
  
  # --------------------- Staff Shoes CRUD ---------------------
  
  observeEvent(input$add_shoe_btn, {
    showModal(myModalDialog(
      title = tags$h4(icon("plus-circle"), " Add New Shoe"),
      textInput("new_shoe_name", tags$label("Shoe Name", `for` = "new_shoe_name"), 
                placeholder = "e.g., Nike Air Max"),
      numericInput("new_shoe_price", tags$label("Price (₱)", `for` = "new_shoe_price"), 
                   value = 99.99, min = 0.01, step = 0.01),
      numericInput("new_shoe_stock", tags$label("Initial Stock", `for` = "new_shoe_stock"), 
                   value = 50, min = 0),
      textInput("new_shoe_colors", tags$label("Available Colors (comma separated)", `for` = "new_shoe_colors"), 
                placeholder = "Black,White,Blue,Red"),
      textInput("new_shoe_sizes", tags$label("Available Sizes (comma separated)", `for` = "new_shoe_sizes"), 
                placeholder = "7,8,9,10,11,12"),
      # REPLACED FILE UPLOAD WITH URL INPUT
      textInput("new_shoe_image_url", tags$label("Image URL", `for` = "new_shoe_image_url"), 
                value = "",  # Start empty
                placeholder = "https://example.com/shoe-image.jpg"),
      tags$div(
        style = "margin: 10px 0; padding: 10px; background: #f8f9fa; border-radius: 8px;",
        icon("info-circle"),
        tags$span(" Enter full image URL starting with http:// or https://", style = "margin-left: 10px;"),
        br(),
        tags$small("Leave empty to use default shoe image", style = "color: #666;")
      ),
      selectInput("new_shoe_available", "Available for Purchase", 
                  choices = c("Yes" = "1", "No" = "0"), 
                  selected = "1"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_new_shoe", "Save Shoe", class = "btn-theme")
      ),
      size = "l"
    ))
    runjs("
      setTimeout(function() {
        Shiny.setInputValue('fix_modal_centering', Math.random());
      }, 100);
    ")
  })
  
  observeEvent(input$save_new_shoe, {
    req(input$new_shoe_name, input$new_shoe_price, input$new_shoe_stock)
    
    if(nchar(trimws(input$new_shoe_name)) == 0){
      show_notification("Shoe name cannot be empty", "error")
      return()
    }
    
    if(is.null(input$new_shoe_price) || input$new_shoe_price <= 0) {
      show_notification("Price must be greater than 0", "error")
      return()
    }
    
    if(is.null(input$new_shoe_stock) || input$new_shoe_stock < 0) {
      show_notification("Stock cannot be negative", "error")
      return()
    }
    
    tryCatch({
      # Handle image URL - REPLACED FILE UPLOAD LOGIC
      image_path <- if(!is.null(input$new_shoe_image_url) && 
                       nchar(trimws(input$new_shoe_image_url)) > 0) {
        url <- trimws(input$new_shoe_image_url)
        # Validate URL format
        if(grepl("^https?://", url)) {
          url
        } else {
          show_notification("Please enter a valid URL starting with http:// or https://", 
                            "warning")
          "default_shoe_image.jpg"
        }
      } else {
        "default_shoe_image.jpg"
      }
      
      available_value <- ifelse(input$new_shoe_available == "1", 1, 0)
      
      dbExecutePool("
      INSERT INTO shoes (name, price, stock, colors, sizes, image, available) 
      VALUES (?, ?, ?, ?, ?, ?, ?)",
                    params = list(
                      trimws(input$new_shoe_name),
                      as.numeric(input$new_shoe_price),
                      as.numeric(input$new_shoe_stock),
                      trimws(ifelse(is.null(input$new_shoe_colors) || input$new_shoe_colors == "", 
                                    "Black,White,Blue", input$new_shoe_colors)),
                      trimws(ifelse(is.null(input$new_shoe_sizes) || input$new_shoe_sizes == "", 
                                    "7,8,9,10", input$new_shoe_sizes)),
                      image_path,
                      available_value
                    ))
      
      removeModal()
      show_notification("Shoe added successfully!", "success", wait_for_modal = TRUE)
      
      # Refresh the shoes table
      refresh_trigger$shoes <- refresh_trigger$shoes + 1
      
    }, error = function(e){
      show_notification(paste("Error adding shoe:", e$message), "error")
    })
  })
  
  observeEvent(input$edit_shoe, {
    shoe_id <- input$edit_shoe
    
    tryCatch({
      shoe <- dbGetQueryPool("SELECT * FROM shoes WHERE shoe_id = ?", params = list(shoe_id))
      
      if(nrow(shoe) == 1) {
        session$userData$current_image <- shoe$image
        session$userData$editing_shoe_id <- shoe_id
        
        # Determine if current image is a URL or local file
        current_image_value <- if(grepl("^https?://", shoe$image)) {
          shoe$image
        } else {
          ""
        }
        
        showModal(myModalDialog(
          title = tags$h4(icon("edit"), " Edit Shoe"),
          textInput("edit_shoe_name", tags$label("Shoe Name", `for` = "edit_shoe_name"), value = shoe$name),
          numericInput("edit_shoe_price", tags$label("Price (₱)", `for` = "edit_shoe_price"), 
                       value = shoe$price, min = 0.01, step = 0.01),
          numericInput("edit_shoe_stock", tags$label("Stock", `for` = "edit_shoe_stock"), 
                       value = shoe$stock, min = 0),
          textInput("edit_shoe_colors", tags$label("Available Colors", `for` = "edit_shoe_colors"), 
                    value = shoe$colors),
          textInput("edit_shoe_sizes", tags$label("Available Sizes", `for` = "edit_shoe_sizes"), 
                    value = shoe$sizes),
          tags$p(strong("Current Image:")),
          tags$img(src = get_image_path(shoe$image), 
                   class = "preview-image",
                   style = "max-width: 200px; max-height: 150px; margin-bottom: 15px;",
                   onerror = "this.onerror=null; this.src='default_shoe_image.jpg'"),
          br(),
          # REPLACED FILE UPLOAD WITH URL INPUT
          textInput("edit_shoe_image_url", "New Image URL (Optional)", 
                    value = current_image_value,
                    placeholder = "https://example.com/new-image.jpg"),
          tags$div(
            style = "margin: 5px 0 15px 0; padding: 8px; background: #f8f9fa; border-radius: 5px;",
            icon("info-circle", style = "color: #666;"),
            tags$span(" Leave empty to keep current image", style = "margin-left: 8px; color: #666; font-size: 12px;")
          ),
          selectInput("edit_shoe_available", "Available for Purchase", 
                      choices = c("Yes" = "1", "No" = "0"), 
                      selected = as.character(shoe$available)),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("update_shoe", "Update Shoe", class = "btn-theme")
          ),
          size = "l"
        ))
        runjs("
          setTimeout(function() {
            Shiny.setInputValue('fix_modal_centering', Math.random());
          }, 100);
        ")
      }
    }, error = function(e) {
      show_notification(paste("Failed to load shoe details:", e$message), "error")
    })
  })
  
  observeEvent(input$update_shoe, {
    req(session$userData$editing_shoe_id)
    
    if(nchar(trimws(input$edit_shoe_name)) == 0){
      show_notification("Shoe name cannot be empty", "error")
      return()
    }
    
    if(input$edit_shoe_price <= 0) {
      show_notification("Price must be greater than 0", "error")
      return()
    }
    
    if(input$edit_shoe_stock < 0) {
      show_notification("Stock cannot be negative", "error")
      return()
    }
    
    tryCatch({
      # Handle image URL - REPLACED FILE UPLOAD LOGIC
      image_path <- if(!is.null(input$edit_shoe_image_url) && 
                       nchar(trimws(input$edit_shoe_image_url)) > 0) {
        url <- trimws(input$edit_shoe_image_url)
        if(grepl("^https?://", url)) {
          url
        } else {
          show_notification("Please enter a valid URL starting with http:// or https://", 
                            "warning")
          session$userData$current_image
        }
      } else {
        session$userData$current_image
      }
      
      available_value <- ifelse(input$edit_shoe_available == "1", 1, 0)
      
      dbExecutePool("
        UPDATE shoes 
        SET name = ?, price = ?, stock = ?, colors = ?, sizes = ?, image = ?, available = ?, updated_at = datetime('now')
        WHERE shoe_id = ?",
                    params = list(
                      trimws(input$edit_shoe_name),
                      as.numeric(input$edit_shoe_price),
                      as.numeric(input$edit_shoe_stock),
                      trimws(input$edit_shoe_colors),
                      trimws(input$edit_shoe_sizes),
                      image_path,
                      available_value,
                      session$userData$editing_shoe_id
                    ))
      
      removeModal()
      show_notification("Shoe updated successfully!", "success", wait_for_modal = TRUE)
      session$userData$editing_shoe_id <- NULL
      
      # Refresh the shoes table
      refresh_trigger$shoes <- refresh_trigger$shoes + 1
      
    }, error = function(e){
      show_notification(paste("Error updating shoe:", e$message), "error")
    })
  })
  
  observeEvent(input$delete_shoe, {
    shoe_id <- input$delete_shoe
    
    tryCatch({
      shoe <- dbGetQueryPool("SELECT name FROM shoes WHERE shoe_id = ?", params = list(shoe_id))
      
      if(nrow(shoe) == 0) return()
      
      session$userData$deleting_shoe_id <- shoe_id
      session$userData$deleting_shoe_name <- shoe$name
      
      showModal(myModalDialog(
        title = tags$h4(icon("exclamation-triangle"), " Confirm Deletion"),
        tags$div(
          style = "text-align:center;",
          icon("exclamation-circle", style = "font-size:48px; color:#e74c3c;"),
          h4(paste("Delete:", shoe$name)),
          p("This action is irreversible! All data for this shoe will be permanently deleted."),
          tags$div(
            style = "background-color: #f8d7da; padding: 10px; border-radius: 5px; margin: 15px 0;",
            icon("exclamation-triangle"),
            strong(" Warning: "),
            "If this shoe exists in order history, consider marking it as unavailable instead."
          ),
          br(),
          p("Type the shoe name below to confirm deletion:"),
          textInput("confirm_shoe_name", NULL, placeholder = "Enter shoe name")
        ),
        footer = tagList(
          actionButton("cancel_delete_shoe", "Cancel", 
                       class = "btn-theme",
                       style = "background: linear-gradient(135deg, #6c757d, #495057);"),
          actionButton("final_delete_shoe", "Delete Permanently", 
                       class = "btn-theme",
                       style = "background-color: #e74c3c;",
                       disabled = TRUE)
        )
      ))
      runjs("
  setTimeout(function() {
    // Fix modal centering
    $('.modal').css('display', 'flex');
    $('.modal').css('align-items', 'center');
    $('.modal').css('justify-content', 'center');
    
    // Fix logout tab scrolling
    $('#logout_tab, #staff_logout_tab').css({
      'max-height': 'calc(100vh - 200px)',
      'overflow-y': 'auto'
    });
  }, 100);
")
    }, error = function(e) {
      showNotification(paste("Failed to load shoe details:", e$message), type = "error")
    })
  })
  
  # Add this observer for cancel delete shoe
  observeEvent(input$cancel_delete_shoe, {
    removeModal()
    session$userData$deleting_shoe_id <- NULL
    session$userData$deleting_shoe_name <- NULL
  })
  
  # Enable delete button only when name matches
  observeEvent(input$confirm_shoe_name, {
    if(!is.null(session$userData$deleting_shoe_name)) {
      if(tolower(trimws(input$confirm_shoe_name)) == tolower(trimws(session$userData$deleting_shoe_name))) {
        shinyjs::enable("final_delete_shoe")
      } else {
        shinyjs::disable("final_delete_shoe")
      }
    }
  }, ignoreInit = TRUE)
  
  # Final delete handler
  observeEvent(input$final_delete_shoe, {
    req(session$userData$deleting_shoe_id)
    
    tryCatch({
      order_count <- dbGetQueryPool(
        "SELECT COUNT(*) as count FROM order_items WHERE shoe_id = ?",
        params = list(session$userData$deleting_shoe_id)
      )$count
      
      if(order_count > 0) {
        dbExecutePool("UPDATE shoes SET available = 0, updated_at = datetime('now') WHERE shoe_id = ?", 
                      params = list(session$userData$deleting_shoe_id))
        showNotification("Shoe exists in order history. Marked as unavailable instead of deleting.", 
                         type = "warning", duration = 5)
      } else {
        # Also delete the image file if it's not the default
        shoe <- dbGetQueryPool("SELECT image FROM shoes WHERE shoe_id = ?", 
                               params = list(session$userData$deleting_shoe_id))
        
        if(nrow(shoe) == 1 && shoe$image != "default_shoe_image.jpg") {
          # Check if image file exists and delete it
          image_path <- file.path("www/uploads", shoe$image)
          if(file.exists(image_path)) {
            file.remove(image_path)
          }
        }
        
        dbExecutePool("DELETE FROM shoes WHERE shoe_id = ?", 
                      params = list(session$userData$deleting_shoe_id))
        showNotification("Shoe deleted successfully", type = "message")
      }
      
      removeModal()
      session$userData$deleting_shoe_id <- NULL
      session$userData$deleting_shoe_name <- NULL
      
      # Refresh the shoes table
      refresh_trigger$shoes <- refresh_trigger$shoes + 1
      
    }, error = function(e){
      showNotification(paste("Error deleting shoe:", e$message), type = "error")
    })
  })
  
  # --------------------- Staff Completed Orders ---------------------
  output$staff_completed_orders <- renderDT({
    invalidateLater(10000)
    input$mark_completed  # Refresh when orders are completed
    
    tryCatch({
      orders <- dbGetQueryPool("
      SELECT o.*, u.username 
      FROM orders o 
      JOIN users u ON o.customer_id = u.user_id 
      WHERE o.status IN ('Completed', 'Cancelled')
      ORDER BY o.created_at DESC")
      
      if(nrow(orders) == 0){
        return(datatable(
          data.frame(Message = "No completed or cancelled orders yet"), 
          options = list(dom = 't', ordering = FALSE),
          rownames = FALSE
        ))
      }
      
      orders$status_formatted <- sapply(orders$status, function(s){
        paste0("<span class='status-", tolower(gsub(" ", "-", s)), "-badge'>", s, "</span>")
      })
      
      orders$track_id <- paste0("SOS", sprintf("%03d", orders$order_id))
      # Format date and total
      orders$formatted_date <- sapply(orders$created_at, format_date)
      orders$formatted_total <- format_currency(orders$total_price)
      
      display <- data.frame(
        "Track ID" = orders$track_id,
        "Customer" = orders$username,
        "Total" = orders$formatted_total,
        "Date" = orders$formatted_date,
        "Status" = orders$status_formatted,
        stringsAsFactors = FALSE
      )
      
      datatable(
        display, 
        escape = FALSE,
        selection = 'single',
        options = list(
          pageLength = 10, 
          dom = 'tip',
          scrollX = FALSE,
          columnDefs = list(
            list(className = 'dt-center', targets = '_all'),
            list(width = '120px', targets = 0), # Track ID
            list(width = '150px', targets = 1), # Customer
            list(width = '120px', targets = 2), # Total
            list(width = '180px', targets = 3), # Date
            list(width = '120px', targets = 4)  # Status
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover'
      )
    }, error = function(e) {
      showNotification(paste("Failed to load completed orders:", e$message), type = "error")
      return(datatable(
        data.frame(Message = "Error loading completed orders"), 
        options = list(dom = 't', ordering = FALSE),
        rownames = FALSE
      ))
    })
  })
  
  # --------------------- Order History Modal (Staff) ---------------------
  observeEvent(input$staff_completed_orders_rows_selected, {
    selected <- input$staff_completed_orders_rows_selected
    if(length(selected) == 0) return()
    
    tryCatch({
      orders <- dbGetQueryPool("
      SELECT o.*, u.username 
      FROM orders o 
      JOIN users u ON o.customer_id = u.user_id 
      WHERE o.status IN ('Completed', 'Cancelled')
      ORDER BY o.created_at DESC")
      
      if(nrow(orders) == 0) return()
      
      order <- orders[selected, ]
      track_id <- paste0("SOS", sprintf("%03d", order$order_id))
      
      # Format date and total
      order$formatted_date <- format_date(order$created_at)
      order$formatted_total <- format_currency(order$total_price)
      
      # Get order items
      order_items <- dbGetQueryPool(
        "SELECT s.name, oi.quantity, oi.color, oi.size, oi.price 
       FROM order_items oi 
       JOIN shoes s ON oi.shoe_id = s.shoe_id 
       WHERE oi.order_id = ?",
        params = list(order$order_id)
      )
      
      # Create order summary HTML
      order_summary <- tags$div(
        class = "order-summary",
        h4(paste("Order Details -", track_id), style = "color: #2c3e50; margin-bottom: 15px;"),
        hr(),
        p(strong("Customer: "), order$username),
        p(strong("Status: "), 
          tags$span(order$status, 
                    class = paste0("status-", tolower(order$status), "-badge"))),
        p(strong("Date: "), order$formatted_date),
        p(strong("Total: "), order$formatted_total),
        br()
      )
      
      # Create items table
      if(nrow(order_items) > 0) {
        # Format order items
        order_items$formatted_price <- format_currency(order_items$price)
        order_items$formatted_total <- format_currency(order_items$price * order_items$quantity)
        order_items$formatted_quantity <- format_number(order_items$quantity, digits = 0)
        
        items_html <- datatable(
          order_items[, c("name", "formatted_quantity", "color", "size", "formatted_price", "formatted_total")],
          colnames = c("Product", "Qty", "Color", "Size", "Price", "Total"),
          options = list(
            pageLength = 5,
            dom = 't',
            ordering = FALSE,
            scrollX = FALSE,
            columnDefs = list(
              list(className = 'dt-center', targets = '_all'),
              list(width = '200px', targets = 0),  # Product
              list(width = '80px', targets = 1),   # Qty
              list(width = '100px', targets = 2),  # Color
              list(width = '80px', targets = 3),   # Size
              list(width = '100px', targets = 4),  # Price
              list(width = '100px', targets = 5)   # Total
            )
          ),
          rownames = FALSE,
          class = 'cell-border stripe hover'
        )
        
        # Show modal with details
        showModal(myModalDialog(
          title = tags$div(
            icon("receipt"),
            paste("Order History -", track_id)
          ),
          order_summary,
          tags$h5("Items:", style = "color: #2c3e50; margin-top: 15px;"),
          DTOutput("staff_history_order_items"),
          footer = modalButton("Close"),
          size = "l",
          easyClose = TRUE,
          class = "order-history-modal"
        ))
        runjs("
        setTimeout(function() {
          $('.modal').css('display', 'flex');
          $('.modal').css('align-items', 'center');
          $('.modal').css('justify-content', 'center');
        }, 100);
      ")
        
        # Render the items table
        output$staff_history_order_items <- renderDT({
          items_html
        })
      }
    }, error = function(e) {
      showNotification(paste("Failed to load order details:", e$message), type = "error")
    })
  })
  
  # --------------------- Staff Sales Report - FIXED ---------------------
  observeEvent(input$apply_date_filter, {
    sales_report_trigger(sales_report_trigger() + 1)
  })
  
  observeEvent(input$reset_date_filter, {
    updateDateInput(session, "sales_start_date", value = Sys.Date() - 30)
    updateDateInput(session, "sales_end_date", value = Sys.Date())
    sales_report_trigger(sales_report_trigger() + 1)
  })
  
  
  
  output$staff_sales <- renderDT({
    req(user_data$logged_in && user_data$role == "Staff")
    
    # Add dependency on the trigger
    sales_report_trigger()
    
    # Show loading message
    if(sales_report_trigger() == 0) {
      return(datatable(
        data.frame(Message = "Please select date range and click 'Apply Filter'"), 
        options = list(dom = 't', ordering = FALSE),
        rownames = FALSE
      ))
    }
    
    tryCatch({
      # Get date range from inputs
      start_date <- if(!is.null(input$sales_start_date)) {
        as.Date(input$sales_start_date)
      } else {
        Sys.Date() - 30
      }
      
      end_date <- if(!is.null(input$sales_end_date)) {
        as.Date(input$sales_end_date)
      } else {
        Sys.Date()
      }
      
      # Validate dates
      if(end_date < start_date) {
        showNotification("End date cannot be before start date", type = "warning")
        end_date <- start_date
      }
      
      # Use separate connection for better performance
      conn <- poolCheckout(db_pool)
      on.exit(poolReturn(conn))
      
      # Optimized query with explicit date handling
      query <- "
        SELECT 
          DATE(o.created_at) as Date,
          COUNT(*) as Orders,
          SUM(o.total_price) as Revenue,
          AVG(o.total_price) as Avg_Order
        FROM orders o
        WHERE o.status = 'Completed'
          AND DATE(o.created_at) BETWEEN ? AND ?
        GROUP BY DATE(o.created_at)
        ORDER BY Date DESC"
      
      sales <- dbGetQuery(conn, query, 
                          params = list(as.character(start_date), 
                                        as.character(end_date)))
      
      if(is.null(sales) || nrow(sales) == 0){
        return(datatable(
          data.frame(Message = paste("No sales data available for", 
                                     format(start_date, "%Y-%m-%d"), 
                                     "to", 
                                     format(end_date, "%Y-%m-%d"))), 
          options = list(dom = 't', ordering = FALSE),
          rownames = FALSE
        ))
      }
      
      # Reorder columns
      sales_display <- sales[, c("Date", "Orders", "Revenue", "Avg_Order")]
      colnames(sales_display) <- c("Date", "Orders", "Revenue", "Avg Order Value")
      
      # Calculate summary row
      summary_row <- data.frame(
        Date = "TOTAL",
        Orders = sum(sales$Orders),
        Revenue = sum(sales$Revenue),
        `Avg Order Value` = mean(sales$Avg_Order),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      
      # Combine data with summary
      final_data <- rbind(sales_display, summary_row)
      
      datatable(
        final_data,
        options = list(
          pageLength = 15, 
          dom = 'tip',
          columnDefs = list(
            list(className = 'dt-center', targets = '_all'),
            list(
              targets = nrow(final_data) - 1,  # Last row (summary)
              className = 'bold-row'
            )
          ),
          rowCallback = JS(
            "function(row, data, index) {
              if (data[0] === 'TOTAL') {
                $(row).css('background-color', '#f8f9fa');
                $(row).css('font-weight', 'bold');
                $(row).css('border-top', '2px solid #2c3e50');
              }
            }"
          )
        ),
        rownames = FALSE
      ) %>%
        formatCurrency(c("Revenue", "Avg Order Value"), "₱") %>%
        formatRound("Avg Order Value", 2)
      
    }, error = function(e) {
      showNotification(paste("Failed to load sales report:", e$message), type = "error")
      return(datatable(
        data.frame(Message = "Error loading sales report"), 
        options = list(dom = 't', ordering = FALSE),
        rownames = FALSE
      ))
    })
  })
  
  # Sales trend plot - FIXED
  output$sales_trend_plot <- renderPlot({
    req(user_data$logged_in && user_data$role == "Staff")
    
    # Add dependency on the trigger
    sales_report_trigger()
    
    # Add validation for date inputs
    if(is.null(input$sales_start_date) || is.null(input$sales_end_date)) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = "Please select date range and click Apply Filter", 
                   size = 6, color = "#95a5a6") +
          theme_void()
      )
    }
    
    tryCatch({
      start_date <- as.Date(input$sales_start_date)
      end_date <- as.Date(input$sales_end_date)
      
      # Validate dates
      if(end_date < start_date) {
        end_date <- start_date
      }
      
      # Use cached connection
      conn <- poolCheckout(db_pool)
      on.exit(poolReturn(conn))
      
      query <- "
        SELECT 
          DATE(created_at) as Date,
          SUM(total_price) as Daily_Revenue,
          COUNT(*) as Daily_Orders
        FROM orders
        WHERE status = 'Completed'
          AND DATE(created_at) BETWEEN ? AND ?
        GROUP BY DATE(created_at)
        ORDER BY Date"
      
      sales <- dbGetQuery(conn, query,
                          params = list(as.character(start_date), 
                                        as.character(end_date)))
      
      if(nrow(sales) == 0) {
        return(
          ggplot() +
            annotate("text", x = 0.5, y = 0.5, 
                     label = "No sales data available for the selected period", 
                     size = 6, color = "#95a5a6") +
            theme_void()
        )
      }
      
      # Create the plot
      p <- ggplot(sales, aes(x = as.Date(Date))) +
        geom_line(aes(y = Daily_Revenue, color = "Revenue"), size = 1.5) +
        geom_point(aes(y = Daily_Revenue, color = "Revenue"), size = 3) +
        geom_bar(aes(y = Daily_Orders * mean(Daily_Revenue/Daily_Orders, na.rm = TRUE), 
                     fill = "Orders"), 
                 stat = "identity", alpha = 0.3) +
        scale_y_continuous(
          name = "Revenue (₱)",
          sec.axis = sec_axis(~./mean(sales$Daily_Revenue/sales$Daily_Orders, na.rm = TRUE), 
                              name = "Number of Orders")
        ) +
        scale_color_manual(values = c("Revenue" = "#1abc9c")) +
        scale_fill_manual(values = c("Orders" = "#3498db")) +
        labs(
          title = paste("Sales Trend from", format(start_date, "%Y-%m-%d"), 
                        "to", format(end_date, "%Y-%m-%d")),
          x = "Date",
          color = "Metric",
          fill = "Metric"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#2c3e50"),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12, face = "bold"),
          legend.position = "bottom",
          panel.grid.major.y = element_line(color = "gray90"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)
        )
      
      return(p)
      
    }, error = function(e) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = "Error loading sales trend data", 
                   size = 6, color = "#e74c3c") +
          theme_void()
      )
    })
  })
  
  
  
  # --------------------- Cleanup Preview Files ---------------------
  observe({
    invalidateLater(3600000) # Every hour
    cleanup_preview_files()
    cleanup_orphaned_images()
  })
  
  
  # --------------------- Session Timeout Warning ---------------------
  observe({
    # Warn user 5 minutes before session timeout (Shiny default is 15 minutes)
    invalidateLater(10 * 60 * 1000) # 10 minutes
    
    if(user_data$logged_in) {
      showNotification(
        "Your session will expire in 5 minutes. Please save your work.",
        type = "warning",
        duration = 10
      )
    }
  })
  
  # --------------------- Cleanup ---------------------
  session$onSessionEnded(function() {
    # Clean up preview files on session end
    cleanup_preview_files()
    cleanup_orphaned_images()
    poolClose(db_pool)
  })
}

# --------------------- Run App ---------------------
shinyApp(ui, server)

