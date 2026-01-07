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
      p(strong(paste0("₱", sprintf("%.2f", price))), 
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

# Helper function for dashboard boxes - UPDATED FOR CONSISTENT SIZE
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
       min-height: 180px;"  # Added fixed height
    ),
    
    div(
      style = "text-align: center; margin-bottom: 10px;",
      icon(icon_name, style = "font-size: 40px; opacity: 0.9;")
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
        min-height: 50px;"  # Ensure consistent height for value
      ,
      value
    ),
    
    div(
      style = "
        text-align: center; 
        font-size: 16px; 
        opacity: 0.9;
        width: 100%;
        min-height: 20px;"  # Ensure consistent height for label
      ,
      label
    )
  )
}

# --------------------- UI ---------------------
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML("
    /* Base Styles */
    .shoe-card-grid {
      display: grid;
      grid-template-columns: repeat(auto-fill, minmax(250px, 1fr));
      gap: 20px;
      margin-top: 20px;
    }
    
    .card {
      border:1px solid #ddd; 
      border-radius:10px; 
      padding:15px; 
      text-align:center;
      transition: transform 0.2s;
      background: white;
      display: flex;
      flex-direction: column;
      justify-content: space-between;
      height: 100%;
      min-height: 400px;
    }
    
    .card:hover {
      transform: translateY(-5px);
      box-shadow: 0 8px 16px rgba(0,0,0,0.1);
    }
    
    .card img {
      border-radius:10px;
      object-fit: cover;
      width: 100%;
      height: 200px;
      min-height: 200px;
    }
    
    .btn-theme {
      background-color:#1abc9c; 
      color:white; 
      border:none;
      border-radius: 5px;
      padding: 8px 15px;
      cursor: pointer;
    }
    
    /* Fix label accessibility issues */
    .shiny-input-container label[for] {
      display: block;
      margin-bottom: 5px;
      font-weight: bold;
    }
    
    /* Ensure all inputs have proper IDs */
    .shiny-bound-input {
      position: relative;
    }
    
    /* Hide empty labels */
    label:empty {
      display: none !important;
    }
    
    /* Ensure password inputs have proper labels */
    .form-group label[for*='pass'] {
      cursor: pointer;
    }
    
    /* Registration form specific fixes */
    #reg_user-label, 
    #reg_pass-label,
    #reg_pass_confirm-label {
      display: block !important;
    }
    
    .btn-theme:hover {
      background-color:#16a085;
      color:white;
      transform: scale(1.05);
    }
    
    .btn-theme:disabled {
      background-color:#95a5a6;
      cursor: not-allowed;
    }
    
    /* Password input with eye icon */
    .password-input-container {
      position: relative;
    }
    
    .password-toggle {
      position: absolute;
      right: 10px;
      top: 50%;
      transform: translateY(-50%);
      background: none;
      border: none;
      cursor: pointer;
      color: #7f8c8d;
      z-index: 1000;
    }
    
    .password-toggle:hover {
      color: #34495e;
    }
    
    /* Cart Styles */
    .cart-badge {
      position: absolute;
      top: -8px;
      right: -8px;
      background-color: #e74c3c;
      color: white;
      border-radius: 50%;
      padding: 2px 6px;
      font-size: 12px;
      font-weight: bold;
      min-width: 20px;
      text-align: center;
    }
    
    .cart-container {
      position: relative;
      display: inline-flex;
      align-items: center;
      cursor: pointer;
      padding: 8px 15px;
      background-color: #34495e;
      border-radius: 5px;
      margin-right: 10px;
      transition: background-color 0.3s;
      min-width: 150px;
    }
    
    .cart-container:hover {
      background-color: #2c3e50;
      transform: scale(1.02);
    }
    
    .cart-icon {
      font-size: 20px;
      color: white;
      margin-right: 8px;
    }
    
    /* Dashboard Styles - UPDATED FOR CONSISTENCY */
    .dashboard-card {
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      border-radius: 15px; 
      padding: 25px; 
      margin-bottom: 20px; 
      text-align: center;
      box-shadow: 0 4px 15px rgba(0,0,0,0.1);
      color: white;
      transition: transform 0.3s;
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      min-height: 180px;
    }
    
    .dashboard-card:hover {
      transform: translateY(-5px);
    }
    
    /* Dashboard value and label for consistency */
    .dashboard-value {
      font-size: 36px;
      font-weight: bold;
      margin: 15px 0;
      min-height: 50px;
      display: flex;
      align-items: center;
      justify-content: center;
      width: 100%;
    }
    
    .dashboard-label {
      font-size: 16px;
      opacity: 0.9;
      min-height: 20px;
      width: 100%;
    }
    
    /* Status Styles - UPDATED WITH MORE PADDING FOR BETTER SPACING */
    .status-pending { 
      background-color: #f39c12 !important;
      color: white !important;
      font-weight: bold;
      padding: 6px 16px !important;  /* Increased from 4px 12px */
      border-radius: 25px !important; /* Increased from 20px */
      display: inline-block;
      text-shadow: none;
      border: none;
      font-size: 12px;
      text-transform: uppercase;
      letter-spacing: 0.5px;
      line-height: 1.2;
    }
    
    .status-processing { 
      background-color: #3498db !important;
      color: white !important;
      font-weight: bold;
      padding: 6px 16px !important;  /* Increased from 4px 12px */
      border-radius: 25px !important; /* Increased from 20px */
      display: inline-block;
      text-shadow: none;
      border: none;
      font-size: 12px;
      text-transform: uppercase;
      letter-spacing: 0.5px;
      line-height: 1.2;
    }
    
    .status-completed {
      background-color: #27ae60 !important;
      color: white !important;
      font-weight: bold;
      padding: 6px 16px !important;  /* Increased from 4px 12px */
      border-radius: 25px !important; /* Increased from 20px */
      display: inline-block;
      text-shadow: none;
      border: none;
      font-size: 12px;
      text-transform: uppercase;
      letter-spacing: 0.5px;
      line-height: 1.2;
    }
    
    .status-cancelled { 
      background-color: #e74c3c !important;
      color: white !important;
      font-weight: bold;
      padding: 6px 16px !important;  /* Increased from 4px 12px */
      border-radius: 25px !important; /* Increased from 20px */
      display: inline-block;
      text-shadow: none;
      border: none;
      font-size: 12px;
      text-transform: uppercase;
      letter-spacing: 0.5px;
      line-height: 1.2;
    }
    
    /* Status Badge Classes (for DataTables) - WITH MORE PADDING */
    .status-badge {
      display: inline-block;
      padding: 6px 16px !important;  /* Increased from 4px 12px */
      border-radius: 25px !important; /* Increased from 20px */
      font-weight: bold;
      font-size: 12px;
      text-transform: uppercase;
      letter-spacing: 0.5px;
      white-space: nowrap;
      text-shadow: none;
      border: none;
      line-height: 1.2;
    }
    
    .status-pending-badge {
      background-color: #f39c12 !important;
      color: white !important;
      border-radius: 25px !important; /* Increased from 20px */
      padding: 6px 16px !important;  /* Increased from 4px 12px */
    }
    
    .status-processing-badge {
      background-color: #3498db !important;
      color: white !important;
      border-radius: 25px !important; /* Increased from 20px */
      padding: 6px 16px !important;  /* Increased from 4px 12px */
    }
    
    .status-completed-badge {
      background-color: #27ae60 !important;
      color: white !important;
      border-radius: 25px !important; /* Increased from 20px */
      padding: 6px 16px !important;  /* Increased from 4px 12px */
    }
    
    .status-cancelled-badge {
      background-color: #e74c3c !important;
      color: white !important;
      border-radius: 25px !important; /* Increased from 20px */
      padding: 6px 16px !important;  /* Increased from 4px 12px */
    }
    
    /* Track ID Badge */
    .track-id-badge {
      display: inline-block;
      background-color: #34495e;
      color: white;
      padding: 6px 16px;
      border-radius: 25px;
      font-weight: bold;
      font-family: monospace;
      letter-spacing: 1px;
    }
    
    /* Order History Modal */
    .order-history-modal {
      max-height: 80vh;
      overflow-y: auto;
    }
    
    .order-summary {
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      color: white;
      padding: 15px;
      border-radius: 10px;
      margin-bottom: 20px;
    }
    
    .clickable-row {
      cursor: pointer;
      transition: background-color 0.3s;
    }
    
    .clickable-row:hover {
      background-color: #f0f7ff !important;
    }
    
    /* DataTables Styles */
    .dt-center {
      text-align: center !important;
    }
    
    /* Logout Button */
    .logout-btn {
      margin-left: 10px;
      background-color: #e74c3c;
      border: none;
      color: white;
      padding: 8px 15px;
      border-radius: 5px;
      cursor: pointer;
      transition: background-color 0.3s;
    }
    
    .logout-btn:hover {
      background-color: #c0392b;
    }
    
    /* Login Container */
    #login_container {
      min-height: 100vh;
      display: flex;
      align-items: center;
      justify-content: center;
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    }
    
    .login-box {
      background: white;
      padding: 40px;
      border-radius: 15px;
      box-shadow: 0 20px 60px rgba(0,0,0,0.3);
      width: 100%;
      max-width: 400px;
    }
    
    /* Validation Error */
    .validation-error {
      color: #e74c3c;
      font-size: 12px;
      margin-top: 5px;
    }
    
    /* Alert styles */
    .alert-danger {
      padding: 10px;
      margin-bottom: 15px;
      background-color: #f8d7da;
      border: 1px solid #f5c6cb;
      border-radius: 5px;
      color: #721c24;
    }
    
    /* Stock Status */
    .stock-low {
      color: #e74c3c !important;
      font-weight: bold;
    }
    
    .stock-ok {
      color: #27ae60 !important;
    }
    
    /* Empty Cart */
    .empty-cart-message {
      text-align: center;
      padding: 40px;
      color: #7f8c8d;
    }
    
    .empty-cart-message i {
      font-size: 60px;
      margin-bottom: 20px;
    }
    
    /* AJAX Loading */
    .AJAX-loading {
      display: inline-block;
      width: 20px;
      height: 20px;
      border: 3px solid rgba(26, 188, 156, 0.3);
      border-radius: 50%;
      border-top-color: #1abc9c;
      animation: spin 1s ease-in-out infinite;
    }
    
    /* Loading indicator */
    .loading-indicator {
      display: inline-block;
      width: 20px;
      height: 20px;
      border: 3px solid rgba(26, 188, 156, 0.3);
      border-radius: 50%;
      border-top-color: #1abc9c;
      animation: spin 1s ease-in-out infinite;
      margin-right: 10px;
    }
    
    @keyframes spin {
      to { transform: rotate(360deg); }
    }
    
    @keyframes pulse {
      0% { transform: scale(1); }
      50% { transform: scale(1.1); }
      100% { transform: scale(1); }
    }
    
    /* Modal Header */
    .modal-header {
      background-color: #34495e;
      color: white;
      border-radius: 10px 10px 0 0;
    }
    
    /* Order Actions Container */
    .order-actions-container {
      background: #f8f9fa;
      padding: 20px;
      border-radius: 10px;
      margin-top: 20px;
      border-left: 5px solid #1abc9c;
      position: relative;
    }
    
    /* Button loading state */
    .btn-loading {
      position: relative;
      color: transparent !important;
    }
    
    .btn-loading::after {
      content: '';
      position: absolute;
      left: 50%;
      top: 50%;
      width: 20px;
      height: 20px;
      margin: -10px 0 0 -10px;
      border: 2px solid rgba(255,255,255,0.3);
      border-radius: 50%;
      border-top-color: white;
      animation: spin 1s ease-in-out infinite;
    }
    
    /* DataTable improvements */
    .dataTables_wrapper .dataTables_length,
    .dataTables_wrapper .dataTables_filter,
    .dataTables_wrapper .dataTables_info,
    .dataTables_wrapper .dataTables_paginate {
      padding: 10px 0;
    }
    
    .dataTables_wrapper .dataTables_filter input {
      border: 1px solid #ddd;
      border-radius: 4px;
      padding: 4px 8px;
    }
    
    .bold-row {
      font-weight: bold !important;
      background-color: #f8f9fa !important;
    }
    
    /* Alert styles */
    .alert-warning {
      padding: 15px;
      border-radius: 5px;
      background-color: #fff3cd;
      border: 1px solid #ffeaa7;
      margin-bottom: 20px;
    }
    
    /* Image preview styles */
    .preview-image {
      border: 2px dashed #ddd;
      padding: 5px;
      background-color: #f8f9fa;
      max-width: 100%;
      height: auto;
    }
    
    /* Form improvements */
    .shiny-input-container {
      margin-bottom: 15px;
    }
    
    .form-group label {
      font-weight: 600;
      margin-bottom: 5px;
      display: block;
    }
    
    /* Responsive tables */
    @media (max-width: 768px) {
      .dataTables_wrapper .dataTables_length,
      .dataTables_wrapper .dataTables_filter {
        float: none !important;
        text-align: center !important;
        margin-bottom: 10px;
      }
      .shoe-card-grid {
        grid-template-columns: repeat(auto-fill, minmax(200px, 1fr));
      }
      .dashboard-card {
        min-height: 160px;
        padding: 20px;
      }
      .dashboard-value {
        font-size: 28px;
      }
    }
    
    /* Filter button alignment */
    .filter-buttons {
      display: flex;
      gap: 10px;
      margin-top: 25px;
    }
    
    /* Order items table */
    .order-items-table {
      margin-top: 20px;
      border-top: 2px solid #eee;
      padding-top: 15px;
    }
    
    /* Make logout tab button consistent */
    .navbar-nav > li > a.btn-theme {
      margin: 8px 15px;
      color: white;
      background-color: #e74c3c;
      border: none;
    }
    
    .navbar-nav > li > a.btn-theme:hover {
      background-color: #c0392b;
      color: white;
    }
    
    /* Style for Logout tab */
    .navbar-nav > li > a[data-value=\"logout_tab\"] {
      background-color: #e74c3c !important;
      color: white !important;
      border-radius: 5px;
      margin: 8px 5px;
      padding: 8px 15px !important;
      transition: background-color 0.3s;
    }
    
    .navbar-nav > li > a[data-value=\"logout_tab\"]:hover {
      background-color: #c0392b !important;
      color: white !important;
    }
    
    .navbar-nav > li > a[data-value=\"logout_tab\"] i {
      margin-right: 8px;
    }
    
    /* Staff Logout tab */
    .navbar-nav > li > a[data-value=\"staff_logout_tab\"] {
      background-color: #e74c3c !important;
      color: white !important;
      border-radius: 5px;
      margin: 8px 5px;
      padding: 8px 15px !important;
      transition: background-color 0.3s;
    }
    
    .navbar-nav > li > a[data-value=\"staff_logout_tab\"]:hover {
      background-color: #c0392b !important;
      color: white !important;
    }
  "))),
  
  tags$script(HTML("
// Password visibility toggle
function togglePasswordVisibility(inputId) {
  var input = document.getElementById(inputId);
  var toggleBtn = document.querySelector('[data-for=\"' + inputId + '\"]');
  
  if (input.type === 'password') {
    input.type = 'text';
    toggleBtn.innerHTML = '<i class=\"fa fa-eye-slash\"></i>';
  } else {
    input.type = 'password';
    toggleBtn.innerHTML = '<i class=\"fa fa-eye\"></i>';
  }
}

// Initialize password toggles
$(document).ready(function() {
  // Add eye icons to password inputs
  function addPasswordToggles() {
    $('input[type=\"password\"]').each(function() {
      var inputId = $(this).attr('id');
      if (inputId && !$(this).parent().find('.password-toggle').length) {
        $(this).wrap('<div class=\"password-input-container\"></div>');
        $(this).after(
          '<button type=\"button\" class=\"password-toggle\" ' +
          'data-for=\"' + inputId + '\" ' +
          'onclick=\"togglePasswordVisibility(\\'' + inputId + '\\')\">' +
          '<i class=\"fa fa-eye\"></i>' +
          '</button>'
        );
      }
    });
  }
  
  // Initial call
  addPasswordToggles();
  
  // Re-add toggles when inputs are dynamically added
  $(document).on('shiny:inputchanged', function() {
    setTimeout(addPasswordToggles, 100);
  });
});
")),
  
  # Main UI structure
  div(id = "login_container", uiOutput("login_ui")),
  hidden(div(id = "main_container", uiOutput("main_ui")))
)

# --------------------- SERVER ---------------------
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
  
  # --------------------- Login UI ---------------------
  render_login_ui <- function() {
    output$login_ui <- renderUI({
      tags$div(
        class = "login-box",
        h2("PJM Shoe Ordering System", style="color:#2c3e50;text-align:center;margin-bottom:30px; font-weight:bolder;"),
        actionButton("login_cust", "Login as Customer", width = '100%', 
                     class="btn-theme", style="margin-bottom:15px; height:50px; font-size:16px;"),
        actionButton("login_staff", "Login as Staff", width = '100%', 
                     class="btn-theme", style="margin-bottom:15px; height:50px; font-size:16px;"),
        actionButton("register_btn", "Register an Account", width = '100%', 
                     class="btn-theme", style="height:50px; font-size:16px;")
      )
    })
    
    output$main_ui <- renderUI({ NULL })
    shinyjs::hide("main_container")
    shinyjs::show("login_container")
  }
  
  render_login_ui()
  
  # --------------------- Registration (CUSTOMER ONLY) ---------------------
  observeEvent(input$register_btn, {
    output$login_ui <- renderUI({
      tags$div(
        class = "login-box",
        h3("Register Customer Account", style="color:#2c3e50;text-align:center;margin-bottom:20px;"),
        p("Create a customer account to start shopping", style="text-align:center;color:#7f8c8d;margin-bottom:20px;"),
        div(
          tags$label("Username", `for` = "reg_user"),
          textInput("reg_user", label = NULL, placeholder = "Enter username (min 3 characters)")
        ),
        div(
          tags$label("Password", `for` = "reg_pass"),
          passwordInput("reg_pass", label = NULL, placeholder = "Enter password (min 4 characters)")
        ),
        div(
          tags$label("Confirm Password", `for` = "reg_pass_confirm"),
          passwordInput("reg_pass_confirm", label = NULL, placeholder = "Re-enter password")
        ),
        # Hidden role field - always Customer
        tags$div(style="display:none;",
                 selectInput("reg_role", tags$label("Role", `for` = "reg_role"), 
                             choices=c("Customer"), selected="Customer")
        ),
        p("Role: Customer (Staff accounts can only be created by administrators)", 
          style="color:#3498db;font-style:italic;margin-bottom:15px;"),
        uiOutput("reg_validation"),
        actionButton("register_account", "Register as Customer", class="btn-theme", width="100%", style="margin-top:10px;"),
        br(), br(),
        actionButton("back_login", "Back to Login", class="btn-theme", width="100%")
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
      showModal(modalDialog(
        title = "Validation Error",
        "Username must be at least 3 characters.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    if (input$reg_pass != input$reg_pass_confirm) {
      showModal(modalDialog(
        title = "Validation Error",
        "Passwords do not match.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    if (nchar(input$reg_pass) < 4) {
      showModal(modalDialog(
        title = "Validation Error",
        "Password must be at least 4 characters.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    tryCatch({
      # Force Customer role for all registrations
      dbExecutePool(
        "INSERT INTO users (username, password, role) VALUES (?, ?, ?)",
        params = list(trimws(input$reg_user), simple_hash(input$reg_pass), "Customer")
      )
      showModal(modalDialog(
        title = "Success",
        "Registration complete! Please login as a customer.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      render_login_ui()
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        "Username already exists!",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    })
  })
  
  # --------------------- Customer Login ---------------------
  observeEvent(input$login_cust, {
    output$login_ui <- renderUI({
      tags$div(
        class = "login-box",
        h3("Customer Login", style="color:#2c3e50;text-align:center;margin-bottom:20px;"),
        div(
          tags$label("Username", `for` = "cust_user"),
          textInput("cust_user", label = NULL, placeholder = "Enter username")
        ),
        div(
          tags$label("Password", `for` = "cust_pass"),
          passwordInput("cust_pass", label = NULL, placeholder = "Enter password")
        ),
        actionButton("cust_login_btn", "Login", class="btn-theme", width="100%", style="margin-top:10px;"),
        br(), br(),
        actionButton("back_login2", "Back", class = "btn-theme", width="100%")
      )
    })
  })
  
  observeEvent(input$back_login2, { render_login_ui() })
  
  # --------------------- Staff Login ---------------------
  observeEvent(input$login_staff, {
    output$login_ui <- renderUI({
      tags$div(
        class = "login-box",
        h3("Staff Login", style="color:#2c3e50;text-align:center;margin-bottom:20px;"),
        div(
          tags$label("Username", `for` = "staff_user"),
          textInput("staff_user", label = NULL, placeholder = "Enter username")
        ),
        div(
          tags$label("Password", `for` = "staff_pass"),
          passwordInput("staff_pass", label = NULL, placeholder = "Enter password")
        ),
        actionButton("staff_login_btn", "Login", class="btn-theme", width="100%", style="margin-top:10px;"),
        br(), br(),
        actionButton("back_login3", "Back", class = "btn-theme", width="100%")
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
                                        class = "btn-theme", icon = icon("sync"))
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
                                              choices = c("All", "Under ₱2000", "₱2000 - ₱4000", "₱5000 - ₱10000", "Over ₱10000"),
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
              
              # NEW: Separate Logout tab
              tabPanel(
                title = tags$span(icon("sign-out-alt"), "Logout"),
                value = "logout_tab",
                div(
                  style = "text-align: center; padding: 60px 20px;",
                  icon("sign-out-alt", style = "font-size: 72px; color: #e74c3c; margin-bottom: 30px;"),
                  h3("Ready to Logout?", style = "color: #2c3e50; margin-bottom: 20px;"),
                  p("Click the button below to securely logout from your account.", 
                    style = "color: #7f8c8d; font-size: 16px; margin-bottom: 40px;"),
                  
                  div(
                    style = "background-color: #f8f9fa; padding: 30px; border-radius: 10px; max-width: 400px; margin: 0 auto;",
                    h4("Account Information", style = "color: #34495e; margin-bottom: 20px;"),
                    p(icon("user"), strong(" Username: "), user_data$username, 
                      style = "font-size: 16px; margin-bottom: 15px;"),
                    p(icon("user-tag"), strong(" Role: "), user_data$role, 
                      style = "font-size: 16px; margin-bottom: 15px;"),
                    hr(style = "border-color: #ddd; margin: 25px 0;"),
                    
                    div(
                      style = "margin-top: 30px;",
                      actionButton("logout_from_tab", "Confirm Logout", 
                                   class = "btn-theme",
                                   style = "background-color: #e74c3c; width: 200px; font-size: 16px; padding: 12px;",
                                   icon = icon("sign-out-alt")),
                      br(), br(),
                      actionButton("cancel_logout_from_tab", "Cancel", 
                                   class = "btn-theme",
                                   style = "background-color: #95a5a6; width: 200px; font-size: 16px; padding: 12px;")
                    )
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
        
        # Reset flag after delay
        delay(1000, {
          user_data$is_logging_in <- FALSE
        })
        
        showNotification(paste("Welcome back,", user_data$username), type = "default", duration = 3)
        
      } else {
        showModal(modalDialog(
          title = "Login Failed",
          "Invalid username or password. Please try again.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }
    }, error = function(e) {
      showNotification(paste("Login failed:", e$message), type = "error")
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
                                div(style = "margin-top: 25px;",
                                    actionButton("refresh_shoes_btn", "Refresh", 
                                                 class = "btn-theme", 
                                                 icon = icon("sync"))
                                )
                         )
                       ),
                       br(),
                       DTOutput("staff_shoes_table"),
                       br(),
                       actionButton("add_shoe_btn", "Add New Shoe", class = "btn-theme")),
              
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
                                  class = "filter-buttons",
                                  actionButton("apply_date_filter", "Apply Filter",
                                               class = "btn-theme"),
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
              
              # Staff Logout tab
              tabPanel(
                title = tags$span(icon("sign-out-alt"), "Logout"),
                value = "staff_logout_tab",
                div(
                  style = "text-align: center; padding: 60px 20px;",
                  icon("sign-out-alt", style = "font-size: 72px; color: #e74c3c; margin-bottom: 30px;"),
                  h3("Staff Logout", style = "color: #2c3e50; margin-bottom: 20px;"),
                  p("Click the button below to securely logout from the staff dashboard.", 
                    style = "color: #7f8c8d; font-size: 16px; margin-bottom: 40px;"),
                  
                  div(
                    style = "background-color: #f8f9fa; padding: 30px; border-radius: 10px; max-width: 400px; margin: 0 auto;",
                    h4("Staff Account Information", style = "color: #34495e; margin-bottom: 20px;"),
                    p(icon("user"), strong(" Username: "), user_data$username, 
                      style = "font-size: 16px; margin-bottom: 15px;"),
                    p(icon("user-tag"), strong(" Role: "), user_data$role, 
                      style = "font-size: 16px; margin-bottom: 15px;"),
                    hr(style = "border-color: #ddd; margin: 25px 0;"),
                    
                    div(
                      style = "margin-top: 30px;",
                      actionButton("staff_logout_from_tab", "Confirm Logout", 
                                   class = "btn-theme",
                                   style = "background-color: #e74c3c; width: 200px; font-size: 16px; padding: 12px;",
                                   icon = icon("sign-out-alt")),
                      br(), br(),
                      actionButton("staff_cancel_logout_from_tab", "Cancel", 
                                   class = "btn-theme",
                                   style = "background-color: #95a5a6; width: 200px; font-size: 16px; padding: 12px;")
                    )
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
        showNotification(paste("Welcome,", user_data$username), type = "default", duration = 3)
      } else {
        showModal(modalDialog(
          title = "Login Failed",
          "Invalid username or password. Please try again.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }
    }, error = function(e) {
      showNotification(paste("Login failed:", e$message), type = "error")
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
        icon("shopping-cart", class = "cart-icon"),
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
    # Save cart if exists
    if(user_data$logged_in && length(user_data$cart) > 0) {
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
    modal_state$processing = FALSE
    current_modal_shoe_id(NULL)
    
    # Clear session data
    session$userData$editing_shoe_id <- NULL
    session$userData$deleting_shoe_id <- NULL
    session$userData$current_image <- NULL
    
    # Reset refresh triggers
    refresh_trigger$shoes <- 0
    refresh_trigger$orders <- 0
    
    # Clear inputs
    shinyjs::reset("product_search")
    shinyjs::reset("inventory_search")
    
    # Only show message if explicitly requested
    if(show_message) {
      showNotification("Logged out successfully", type = "message", duration = 3)
    }
  }
  
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
                                   "Under ₱2000" = "price < 2000",
                                   "₱2000 - ₱4000" = "price >= 2000 AND price <= 4000",
                                   "₱5000 - ₱10000" = "price >= 5000 AND price <= 10000",
                                   "Over ₱10000" = "price > 10000")
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
      
      showModal(modalDialog(
        title = tags$div(
          icon("plus-circle"),
          paste("Add", shoe$name, "to Cart")
        ),
        tags$img(src = get_image_path(shoe$image), width = "100%", style = "border-radius:10px; margin-bottom:15px;",
                 onerror = "this.onerror=null; this.src='default_shoe_image.jpg'"),
        p(strong("Price: "), paste0("₱", sprintf("%.2f", shoe$price))),
        p(strong("Available Stock: "), shoe$stock),
        selectInput("sel_color", "Select Color", choices = colors),
        selectInput("sel_size", "Select Size", choices = sizes),
        numericInput("sel_qty", "Quantity", value = 1, min = 1, max = min(shoe$stock, 10)),
        uiOutput("modal_validation"),
        footer = tagList(
          actionButton("confirm_add_cart", "Add to Cart", class = "btn-theme",
                       style = "width: 100%; margin-bottom: 10px;"),
          modalButton("Cancel")
        ),
        easyClose = TRUE,
        size = "m"
      ))
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
      showNotification("Shoe information is no longer available. Please try again.", 
                       type = "error")
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
        showNotification("Shoe not found or unavailable", type = "error")
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
        showNotification("Quantity must be at least 1", type = "error")
        return()
      }
      
      if(input$sel_qty > as.numeric(shoe$stock)){
        showNotification(paste("Quantity exceeds available stock. Only", 
                               shoe$stock, "units available."), 
                         type = "error")
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
          showNotification(paste("Cannot add more. Total would exceed available stock of", 
                                 shoe$stock), 
                           type = "error")
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
      
      showNotification(
        HTML(paste(
          "<div style='text-align:center;'>",
          "<h4 style='color:#27ae60;'>✅ Added to Cart!</h4>",
          "<p>", shoe$name, "(", input$sel_color, "-", input$sel_size, ")</p>",
          "<p>Quantity:", input$sel_qty, "</p>",
          "<p>Cart now has", length(user_data$cart), "items</p>",  # Debug info
          "</div>"
        )), 
        type = "message",
        duration = 5
      )
      
    }, error = function(e) {
      showNotification(paste("Error adding to cart:", e$message), type = "error")
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
    # Add dependency on cart changes
    user_data$cart_trigger
    
    # Check cart content - this will now re-run when cart changes
    if(length(user_data$cart) == 0) {
      showModal(modalDialog(
        title = tags$h3(icon("shopping-cart"), " Your Shopping Cart"),
        tags$div(
          class = "empty-cart-message",
          icon("shopping-cart", style = "font-size:60px; color:#ddd;"),
          h4("Your cart is empty"),
          p("Add some shoes to get started!")
        ),
        footer = modalButton("Continue Shopping"),
        easyClose = TRUE
      ))
      return()
    }
    
    # Calculate cart total
    cart_total <- sum(sapply(user_data$cart, function(x) {
      as.numeric(x$price) * as.numeric(x$quantity)
    }))
    
    # Create cart data frame
    cart_df <- do.call(rbind, lapply(1:length(user_data$cart), function(i) {
      item <- user_data$cart[[i]]
      data.frame(
        Index = i,
        Name = item$name,
        Color = item$color,
        Size = item$size,
        Quantity = item$quantity,
        Price = item$price,
        Total = as.numeric(item$price) * as.numeric(item$quantity),
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
          ordering = FALSE
        ),
        rownames = FALSE
      ) %>%
        formatCurrency(c("Price", "Total"), "₱")
    })
    
    # Show the modal with cart contents
    showModal(modalDialog(
      title = tags$h3(icon("shopping-cart"), " Your Shopping Cart"),
      DTOutput("cart_table_display"),
      br(),
      tags$div(
        style = "background:#f8f9fa; padding:15px; border-radius:10px;",
        h4(paste("Total Items:", cart_count())),
        h4(paste("Cart Total: ₱", sprintf("%.2f", cart_total)))
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
  })
  
  observeEvent(input$clear_cart_btn, {
    # Show confirmation dialog
    showModal(modalDialog(
      title = "Confirm Clear Cart",
      "Are you sure you want to clear your cart? This action cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_clear_cart", "Yes, Clear Cart", 
                     class = "btn-theme",
                     style = "background-color: #e74c3c;")
      )
    ))
  })
  
  observeEvent(input$confirm_clear_cart, {
    user_data$cart <- list()
    user_data$cart_trigger <- user_data$cart_trigger + 1
    
    # Clear cart from database
    if(user_data$logged_in && !is.null(user_data$user_id)) {
      clear_cart_in_db(user_data$user_id)
    }
    
    removeModal()
    showNotification("Cart cleared", type = "warning", duration = 3)
    removeModal()  # Close cart modal
  })
  
  # --------------------- Place Order ---------------------
  observeEvent(input$make_order_btn, {
    if(length(user_data$cart) == 0){
      showModal(modalDialog(
        title = "Empty Cart",
        "Your cart is empty! Please add items before placing an order.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
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
      showModal(modalDialog(
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
      
      # Clear cart after successful order
      user_data$cart <- list()
      user_data$cart_trigger <- user_data$cart_trigger + 1
      clear_cart_in_db(user_data$user_id)
      
      # Show order confirmation modal
      showModal(modalDialog(
        title = tags$h3(icon("check-circle"), " Order Placed Successfully!"),
        tags$div(
          style = "text-align:center; padding:20px;",
          icon("check-circle", style = "font-size: 48px; color: #27ae60; margin-bottom: 20px;"),
          h4(paste("Track ID:", track_id), 
             class = "track-id-badge"),
          h4(paste("Total Amount: ₱", sprintf("%.2f", total))),
          p(tags$span("Status: Pending", class = "status-pending-badge", 
                      style = "font-size: 12px;")),
          br(),
          tags$div(
            style = "background-color: #f8f9fa; padding: 15px; border-radius: 10px; margin: 20px 0;",
            h5("Order Confirmation Sent!"),
            p("Thanks for Ordering!"),
            p("===============================================")
          ),
          p("You can track your order in the 'Order Status' tab."),
          br(),
          tags$div(
            style = "margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 10px;",
            icon("info-circle", style = "color: #3498db; margin-right: 10px;"),
            "This modal will close automatically in 5 seconds..."
          )
        ),
        footer = NULL,
        size = "m",
        easyClose = FALSE
      ))
      
      delay(5000, {
        removeModal()
      })
      
    }, error = function(e){
      dbExecute(conn, "ROLLBACK")
      showModal(modalDialog(
        title = "Order Failed", 
        paste("Failed to place order:", e$message),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }, finally = {
      poolReturn(conn)
    })
  })
  
  # --------------------- Customer Orders (ACTIVE ONLY) ---------------------
  order_refresh_trigger <- reactiveVal(0)
  # Update the customer_orders_data reactive to include the trigger
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
         ORDER BY created_at DESC",
          params = list(user_data$user_id)
        )
        
        if(nrow(orders) > 0) {
          orders$track_id <- paste0("SOS", sprintf("%03d", orders$order_id))
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
        data.frame(Message = "You haven't any placed ongoing orders yet"), 
        options = list(dom = 't', ordering = FALSE),
        rownames = FALSE
      ))
    }
    
    orders$action <- sapply(1:nrow(orders), function(i){
      if(orders$status[i] == "Pending"){
        as.character(actionButton(
          paste0("cancel_", orders$order_id[i]), 
          "Cancel", 
          class = "btn-theme btn-sm",
          style = "background-color: #e74c3c;",
          onclick = sprintf(
            "Shiny.setInputValue('cancel_order', %d, {priority:'event'})", 
            orders$order_id[i]
          )
        ))
      } else {
        "-"
      }
    })
    
    display <- orders[, c("track_id", "total_price", "created_at", "action")]
    colnames(display) <- c("Track ID", "Total", "Date", "Action")
    
    datatable(
      display, 
      escape = FALSE,
      options = list(
        pageLength = 10, 
        dom = 'tip',
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      ),
      rownames = FALSE
    ) %>%
      formatCurrency("Total", "₱")
  })
  
  # --------------------- Cancel Order ---------------------
  observeEvent(input$cancel_order, {
    order_id <- input$cancel_order
    
    showModal(modalDialog(
      title = "Confirm Cancellation",
      tags$div(
        style = "text-align:center;",
        icon("exclamation-triangle", style = "font-size:48px; color:#e74c3c;"),
        br(), br(),
        p(paste("Are you sure you want to cancel order ID", order_id, "?")),
        p("This action cannot be undone.", style = "color:#7f8c8d;")
      ),
      footer = tagList(
        modalButton("No, Keep Order"),
        actionButton("confirm_cancel", "Yes, Cancel Order", 
                     class = "btn-theme",
                     style = "background-color: #e74c3c;")
      )
    ))
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
        
        # Create summary table with status prominently displayed
        summary_df <- data.frame(
          Detail = c("Track ID", "Status", "Date", "Total"),
          Value = c(
            track_id,
            paste0("<span class='status-", tolower(order$status), "-badge'>", order$status, "</span>"),
            order$created_at,
            paste0("₱", sprintf("%.2f", order$total_price))
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
              Quantity = order_items$quantity,
              Color = order_items$color,
              Size = order_items$size,
              Price = paste0("₱", sprintf("%.2f", order_items$price)),
              Total = paste0("₱", sprintf("%.2f", order_items$price * order_items$quantity)),
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
              columnDefs = list(
                list(
                  targets = 0,
                  render = JS(
                    "function(data, type, row, meta) {
                      if (meta.row === 0 || meta.row === 7) {
                        return '<strong style=\"color: #2c3e50; font-size: 16px;\">' + data + '</strong>';
                      }
                      if (meta.row === 1 || meta.row === 2 || meta.row === 3 || meta.row === 4) {
                        return '<strong>' + data + '</strong>';
                      }
                      return data;
                    }"
                  )
                )
              )
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
  })
  
  # --------------------- Customer Completed Orders (HISTORY) ---------------------
  output$customer_completed <- renderDT({
    tryCatch({
      orders <- dbGetQueryPool(
        "SELECT order_id, total_price, created_at, status 
         FROM orders 
         WHERE customer_id = ? AND status IN ('Completed', 'Cancelled')
         ORDER BY created_at DESC",
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
        paste0("<span class='status-", tolower(s), "-badge'>", s, "</span>")
      })
      
      display <- orders[, c("track_id", "total_price", "created_at", "status_formatted")]
      colnames(display) <- c("Track ID", "Total", "Date", "Status")
      
      datatable(
        display, 
        escape = FALSE,
        selection = 'single',
        options = list(
          pageLength = 10, 
          dom = 'tip',
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        rownames = FALSE
      ) %>%
        formatCurrency("Total", "₱")
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
         ORDER BY created_at DESC",
        params = list(user_data$user_id)
      )
      
      if(nrow(orders) == 0) return()
      
      order <- orders[selected, ]
      track_id <- paste0("SOS", sprintf("%03d", order$order_id))
      
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
        h4(paste("Order Details -", track_id)),
        hr(),
        p(strong("Status: "), 
          tags$span(order$status, 
                    class = paste0("status-", tolower(order$status), "-badge"))),
        p(strong("Date: "), order$created_at),
        p(strong("Total: "), paste0("₱", sprintf("%.2f", order$total_price))),
        br()
      )
      
      # Create items table
      if(nrow(order_items) > 0) {
        order_items$Total <- order_items$price * order_items$quantity
        
        items_html <- datatable(
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
        
        # Show modal with details
        showModal(modalDialog(
          title = tags$div(
            icon("receipt"),
            paste("Order History -", track_id)
          ),
          order_summary,
          tags$h5("Items:"),
          DTOutput("history_order_items"),
          footer = modalButton("Close"),
          size = "l",
          easyClose = TRUE,
          class = "order-history-modal"
        ))
        
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
    
    return(list(timestamp = Sys.time()))
  })
  
  # --------------------- Staff Dashboard ---------------------
  output$total_orders_box <- renderUI({
    dashboard_data()
    
    tryCatch({
      total <- dbGetQueryPool("SELECT COUNT(*) as count FROM orders")$count
      total <- ifelse(is.na(total), 0, total)
    }, error = function(e) {
      total <- 0
    })
    
    dashboard_box(
      icon_name = "shopping-bag",
      value = format(total, big.mark = ","),
      label = "Total Orders",
      gradient = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);"
    )
  })
  
  output$pending_orders_box <- renderUI({
    dashboard_data()
    
    tryCatch({
      pending <- dbGetQueryPool("SELECT COUNT(*) as count FROM orders WHERE status = 'Pending'")$count
      pending <- ifelse(is.na(pending), 0, pending)
    }, error = function(e) {
      pending <- 0
    })
    
    dashboard_box(
      icon_name = "clock",
      value = format(pending, big.mark = ","),
      label = "Pending Orders",
      gradient = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);"
    )
  })
  
  output$total_sales_box <- renderUI({
    dashboard_data()
    
    tryCatch({
      sales <- dbGetQueryPool("SELECT SUM(total_price) as total FROM orders WHERE status = 'Completed'")$total
      sales <- ifelse(is.na(sales), 0, sales)
    }, error = function(e) {
      sales <- 0
    })
    
    dashboard_box(
      icon_name = "money-bill-wave",
      value = paste0("₱", format(round(sales, 2), nsmall = 2)),
      label = "Total Sales",
      gradient = "background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%);"
    )
  })
  
  output$total_stock_box <- renderUI({
    dashboard_data()
    
    tryCatch({
      stock <- dbGetQueryPool("SELECT SUM(stock) as total FROM shoes")$total
      stock <- ifelse(is.na(stock), 0, stock)
    }, error = function(e) {
      stock <- 0
    })
    
    dashboard_box(
      icon_name = "boxes",
      value = format(stock, big.mark = ","),
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
          geom_text(aes(label = paste0("₱", round(daily_sales, 2))), 
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
  
  # --------------------- Staff Orders Management (ACTIVE ONLY) ---------------------
  staff_orders_data <- reactive({
    input$refresh_staff_orders
    input$mark_processing
    input$mark_completed
    input$mark_cancelled
    
    tryCatch({
      # Only show active orders (Pending/Processing)
      orders <- dbGetQueryPool("
        SELECT o.*, u.username 
        FROM orders o 
        JOIN users u ON o.customer_id = u.user_id 
        WHERE o.status NOT IN ('Completed', 'Cancelled')
        ORDER BY 
          CASE o.status 
            WHEN 'Pending' THEN 1 
            WHEN 'Processing' THEN 2 
            ELSE 3 
          END,
          o.created_at DESC")
      
      if(nrow(orders) > 0){
        orders$status_formatted <- sapply(orders$status, function(s){
          paste0("<span class='status-", tolower(s), "-badge'>", s, "</span>")
        })
        
        orders$track_id <- paste0("SOS", sprintf("%03d", orders$order_id))
      }
      
      return(orders)
    }, error = function(e) {
      showNotification(paste("Failed to load orders:", e$message), type = "error")
      return(data.frame())
    })
  })
  
  observeEvent(input$refresh_staff_orders, {
    showNotification("Orders refreshed", type = "message", duration = 2)
  })
  
  output$staff_orders_table <- renderDT({
    orders <- staff_orders_data()
    
    if(is.null(orders) || nrow(orders) == 0){
      return(datatable(
        data.frame(Message = "No active orders to manage"), 
        options = list(dom = 't', ordering = FALSE),
        rownames = FALSE
      ))
    }
    
    display <- orders[, c("track_id", "username", "total_price", "created_at", "status_formatted")]
    colnames(display) <- c("Track ID", "Customer", "Total", "Date", "Status")
    
    datatable(
      display, 
      selection = 'single',
      escape = FALSE,
      options = list(
        pageLength = 10, 
        dom = 'tip',
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      ),
      rownames = FALSE
    ) %>%
      formatCurrency("Total", "₱")
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
               p(icon("money-bill-wave"), strong(" Total: "), paste0("₱", sprintf("%.2f", order$total_price))),
               p(icon("calendar"), strong(" Date: "), order$created_at),
               p(icon("info-circle"), strong(" Current Status: "), 
                 tags$span(order$status, class = paste0("status-", tolower(order$status), "-badge"))),
               
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
               actionButton("mark_processing", icon("cogs"), "Mark as Processing", 
                            class = "btn-theme", style = "margin-right:10px; background-color: #3498db;"),
               actionButton("mark_completed", icon("check-circle"), "Mark as Completed", 
                            class = "btn-theme", style = "margin-right:10px; background-color: #27ae60;"),
               actionButton("mark_cancelled", icon("times-circle"), "Cancel Order", 
                            class = "btn-theme", style = "background-color: #e74c3c;")
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
          style = "margin-right: 5px; background-color: #f39c12;",
          onclick = sprintf("Shiny.setInputValue('edit_shoe', %d, {priority:'event'})", id)
        )),
        as.character(actionButton(
          paste0("delete_", id), 
          icon("trash"), " Delete",
          class = "btn-theme btn-sm",
          style = "background-color: #e74c3c;",
          onclick = sprintf("Shiny.setInputValue('delete_shoe', %d, {priority:'event'})", id)
        ))
      )
    })
    
    shoes$available <- ifelse(shoes$available == 1, 
                              "<span style='color:#27ae60; font-weight:bold;'>Yes</span>", 
                              "<span style='color:#e74c3c; font-weight:bold;'>No</span>")
    
    # Add row count information
    if(!is.null(input$inventory_search) && nchar(input$inventory_search) > 0) {
      message <- paste("Showing", nrow(shoes), "shoes matching:", input$inventory_search)
    } else {
      message <- paste("Total shoes:", nrow(shoes))
    }
    
    display <- shoes[, c("shoe_id", "name", "price", "stock", "colors", "sizes", "available", "actions")]
    colnames(display) <- c("ID", "Name", "Price", "Stock", "Colors", "Sizes", "Available", "Actions")
    
    datatable(
      display,
      caption = tags$caption(style = "caption-side: top; font-size: 16px; font-weight: bold; color: #2c3e50;",
                             message),
      escape = FALSE,
      options = list(
        pageLength = 10, 
        dom = 'tip',
        stateSave = TRUE,  # FIX: Save table state (pagination, sorting)
        stateDuration = 60 * 60 * 24,  # Save state for 24 hours
        columnDefs = list(
          list(className = 'dt-center', targets = c(0, 2, 3, 6, 7))
        )
      ),
      rownames = FALSE
    ) %>%
      formatCurrency("Price", "₱")
  })
  
  # --------------------- Staff Shoes CRUD ---------------------
  observeEvent(input$add_shoe_btn, {
    showModal(modalDialog(
      title = tags$h4(icon("plus-circle"), " Add New Shoe"),
      textInput("new_shoe_name", tags$label("Shoe Name", `for` = "new_shoe_name"), 
                placeholder = "e.g., Nike Air Max"),
      numericInput("new_shoe_price", tags$label("Price (₱)", `for` = "new_shoe_price"), 
                   value = 99.99, min = 0.01, step = 0.01),
      numericInput("new_shoe_stock", tags$label("Initial Stock", `for` = "new_shoe_stock"), 
                   value = 50, min = 0),
      textInput("new_shoe_colors", tags$label("Available Colors (comma separated)", `for` = "new_shoe_colors"), 
                value = "Black,White,Blue,Red"),
      textInput("new_shoe_sizes", tags$label("Available Sizes (comma separated)", `for` = "new_shoe_sizes"), 
                value = "7,8,9,10,11,12"),
      fileInput("new_shoe_image", "Upload Shoe Image", 
                accept = c("image/png", "image/jpeg", "image/jpg", "image/gif", "image/webp")),
      selectInput("new_shoe_available", "Available for Purchase", 
                  choices = c("Yes" = "1", "No" = "0"), 
                  selected = "1"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_new_shoe", "Save Shoe", class = "btn-theme")
      ),
      size = "l"
    ))
  })
  
  observeEvent(input$save_new_shoe, {
    req(input$new_shoe_name, input$new_shoe_price, input$new_shoe_stock)
    
    if(nchar(trimws(input$new_shoe_name)) == 0){
      showNotification("Shoe name cannot be empty", type = "error")
      return()
    }
    
    if(input$new_shoe_price <= 0) {
      showNotification("Price must be greater than 0", type = "error")
      return()
    }
    
    if(input$new_shoe_stock < 0) {
      showNotification("Stock cannot be negative", type = "error")
      return()
    }
    
    tryCatch({
      # Get next shoe_id for image naming
      last_id <- dbGetQueryPool("SELECT MAX(shoe_id) as max_id FROM shoes")$max_id
      new_shoe_id <- ifelse(is.na(last_id), 1, last_id + 1)
      
      image_path <- if(!is.null(input$new_shoe_image) && file.exists(input$new_shoe_image$datapath)) {
        save_uploaded_image(input$new_shoe_image, shoe_id = new_shoe_id)
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
                      trimws(input$new_shoe_colors),
                      trimws(input$new_shoe_sizes),
                      image_path,
                      available_value
                    ))
      
      removeModal()
      showNotification("Shoe added successfully!", type = "message")
      
      # Refresh the shoes table
      refresh_trigger$shoes <- refresh_trigger$shoes + 1
      
    }, error = function(e){
      showNotification(paste("Error adding shoe:", e$message), type = "error")
    })
  })
  
  observeEvent(input$edit_shoe, {
    shoe_id <- input$edit_shoe
    
    tryCatch({
      shoe <- dbGetQueryPool("SELECT * FROM shoes WHERE shoe_id = ?", params = list(shoe_id))
      
      if(nrow(shoe) == 1) {
        session$userData$current_image <- shoe$image
        session$userData$editing_shoe_id <- shoe_id
        
        showModal(modalDialog(
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
                   style = "max-width: 200px; max-height: 150px;",
                   onerror = "this.onerror=null; this.src='default_shoe_image.jpg'"),
          br(),
          fileInput("edit_shoe_image", "Upload New Image (Optional)", 
                    accept = c("image/png", "image/jpeg", "image/jpg", "image/gif", "image/webp")),
          selectInput("edit_shoe_available", "Available for Purchase", 
                      choices = c("Yes" = "1", "No" = "0"), 
                      selected = as.character(shoe$available)),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("update_shoe", "Update Shoe", class = "btn-theme")
          ),
          size = "l"
        ))
      }
    }, error = function(e) {
      showNotification(paste("Failed to load shoe details:", e$message), type = "error")
    })
  })
  
  observeEvent(input$update_shoe, {
    req(session$userData$editing_shoe_id)
    
    if(nchar(trimws(input$edit_shoe_name)) == 0){
      showNotification("Shoe name cannot be empty", type = "error")
      return()
    }
    
    if(input$edit_shoe_price <= 0) {
      showNotification("Price must be greater than 0", type = "error")
      return()
    }
    
    if(input$edit_shoe_stock < 0) {
      showNotification("Stock cannot be negative", type = "error")
      return()
    }
    
    tryCatch({
      image_path <- if(!is.null(input$edit_shoe_image) && 
                       file.exists(input$edit_shoe_image$datapath)) {
        save_uploaded_image(input$edit_shoe_image, shoe_id = session$userData$editing_shoe_id)
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
      showNotification("Shoe updated successfully!", type = "message")
      session$userData$editing_shoe_id <- NULL
      
      # Refresh the shoes table
      refresh_trigger$shoes <- refresh_trigger$shoes + 1
      
    }, error = function(e){
      showNotification(paste("Error updating shoe:", e$message), type = "error")
    })
  })
  
  observeEvent(input$delete_shoe, {
    shoe_id <- input$delete_shoe
    
    tryCatch({
      shoe <- dbGetQueryPool("SELECT name FROM shoes WHERE shoe_id = ?", params = list(shoe_id))
      
      if(nrow(shoe) == 0) return()
      
      session$userData$deleting_shoe_id <- shoe_id
      session$userData$deleting_shoe_name <- shoe$name
      
      showModal(modalDialog(
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
          modalButton("Cancel"),
          actionButton("final_delete_shoe", "Delete Permanently", 
                       class = "btn-theme",
                       style = "background-color: #e74c3c;",
                       disabled = TRUE)
        )
      ))
    }, error = function(e) {
      showNotification(paste("Failed to load shoe details:", e$message), type = "error")
    })
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
        paste0("<span class='status-", tolower(s), "-badge'>", s, "</span>")
      })
      
      orders$track_id <- paste0("SOS", sprintf("%03d", orders$order_id))
      
      display <- orders[, c("track_id", "username", "total_price", "created_at", "status_formatted")]
      colnames(display) <- c("Track ID", "Customer", "Total", "Date", "Status")
      
      datatable(
        display, 
        escape = FALSE,
        selection = 'single',
        options = list(
          pageLength = 10, 
          dom = 'tip',
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        rownames = FALSE
      ) %>%
        formatCurrency("Total", "₱")
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
        h4(paste("Order Details -", track_id)),
        hr(),
        p(strong("Customer: "), order$username),
        p(strong("Status: "), 
          tags$span(order$status, 
                    class = paste0("status-", tolower(order$status), "-badge"))),
        p(strong("Date: "), order$created_at),
        p(strong("Total: "), paste0("₱", sprintf("%.2f", order$total_price))),
        br()
      )
      
      # Create items table
      if(nrow(order_items) > 0) {
        order_items$Total <- order_items$price * order_items$quantity
        
        items_html <- datatable(
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
        
        # Show modal with details
        showModal(modalDialog(
          title = tags$div(
            icon("receipt"),
            paste("Order History -", track_id)
          ),
          order_summary,
          tags$h5("Items:"),
          DTOutput("staff_history_order_items"),
          footer = modalButton("Close"),
          size = "l",
          easyClose = TRUE,
          class = "order-history-modal"
        ))
        
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
          name = "Revenue ($)",
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