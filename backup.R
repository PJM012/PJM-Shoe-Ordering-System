library(shiny)
library(shinyjs)
library(shinyFeedback)
library(DT)

# ===========================
# UI
# ===========================
ui <- fluidPage(
  useShinyjs(),
  useShinyFeedback(),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "script.js")
  ),
  
  # ===========================
  # Header
  # ===========================
  fluidRow(
    column(12, class = "app-header",
           h2("PJM Shoe Ordering System"),
           actionButton("cart_btn", "Cart", icon = icon("shopping-cart")),
           actionButton("login_btn", "Login / Register", icon = icon("user"))
    )
  ),
  
  # ===========================
  # Product Listing
  # ===========================
  fluidRow(
    id = "product_grid",
    # Example products, these could be dynamically generated
    lapply(1:6, function(i){
      column(4,
             div(class = "product-card",
                 h4(paste("Shoe", i)),
                 img(src = paste0("images/shoe", i, ".jpg"), width = "100%"),
                 p(class = "price", paste0("₱", 2000 + i*100)),
                 actionButton(paste0("add_cart_", i), "Add to Cart", class = "btn-add-cart")
             )
      )
    })
  ),
  
  # ===========================
  # Modals
  # ===========================
  # Cart Modal
  bsModal("cart_modal", "Cart", "cart_btn", size = "large",
          div(id = "cart_table_container",
              DTOutput("cart_table"),
              div(class = "cart-summary",
                  h5("Total Items: ", span(id="total_items", "0")),
                  h5("Cart Total: ", span(id="cart_total", "₱0"))
              )
          )
  ),
  
  # Login / Register Modal
  bsModal("login_modal", "Login / Register", "login_btn", size = "medium",
          tabsetPanel(
            tabPanel("Login",
                     textInput("login_email", "Email"),
                     passwordInput("login_password", "Password"),
                     actionButton("login_submit", "Login")
            ),
            tabPanel("Register",
                     textInput("reg_name", "Full Name"),
                     textInput("reg_email", "Email"),
                     passwordInput("reg_password", "Password"),
                     passwordInput("reg_password_confirm", "Confirm Password"),
                     actionButton("reg_submit", "Register")
            )
          )
  )
)


server <- function(input, output, session) {
  
  # ===========================
  # Reactive Values
  # ===========================
  cart <- reactiveVal(data.frame(
    item = character(),
    quantity = numeric(),
    price = numeric(),
    stringsAsFactors = FALSE
  ))
  
  users <- reactiveVal(data.frame(
    email = character(),
    password = character(),
    name = character(),
    stringsAsFactors = FALSE
  ))
  
  # ===========================
  # Add to Cart
  # ===========================
  observe({
    lapply(1:6, function(i) {
      observeEvent(input[[paste0("add_cart_", i)]], {
        current_cart <- cart()
        item_name <- paste("Shoe", i)
        item_price <- 2000 + i*100
        
        if (item_name %in% current_cart$item) {
          current_cart$quantity[current_cart$item == item_name] <- current_cart$quantity[current_cart$item == item_name] + 1
        } else {
          current_cart <- rbind(current_cart, data.frame(item = item_name, quantity = 1, price = item_price))
        }
        
        cart(current_cart)
        shinyjs::runjs('fixCartTable(); updateCartSummary();')
      })
    })
  })
  
  # ===========================
  # Render Cart Table
  # ===========================
  output$cart_table <- renderDT({
    df <- cart()
    if(nrow(df) == 0) return(NULL)
    datatable(df, rownames = FALSE, options = list(
      paging = FALSE,
      scrollX = TRUE,
      dom = 't'
    ))
  })
  
  # ===========================
  # Update Cart Summary
  # ===========================
  observe({
    df <- cart()
    total_items <- sum(df$quantity)
    cart_total <- sum(df$quantity * df$price)
    shinyjs::html("total_items", total_items)
    shinyjs::html("cart_total", paste0("₱", formatC(cart_total, format="d", big.mark=",")))
  })
  
  
  # ===========================
  # Order Tracking
  # ===========================
  orders <- reactiveVal(data.frame(
    track_id = character(),
    status = character(),
    date = as.Date(character()),
    stringsAsFactors = FALSE
  ))
  
  # Example orders
  if(nrow(orders()) == 0) {
    orders(data.frame(
      track_id = c("ORD001", "ORD002", "ORD003"),
      status = c("Processing", "Shipped", "Delivered"),
      date = as.Date(c("2026-01-05", "2026-01-06", "2026-01-07"))
    ))
  }
  
  # Search for order
  observeEvent(input$order_search_btn, {
    track_id <- input$order_track_id
    df <- orders()
    order <- df[df$track_id == track_id, ]
    
    if(nrow(order) == 0){
      shinyFeedback::showToast("Order not found", type = "error")
      shinyjs::hide("order_summary_container")
    } else {
      shinyjs::show("order_summary_container")
      output$order_summary <- renderUI({
        div(class = "order-summary",
            h4(paste("Order Details -", track_id)),
            hr(),
            p(strong("Status: "), tags$span(order$status, class = paste0("status-", tolower(order$status)))),
            p(strong("Date: "), order$date)
        )
      })
    }
  })
  
  
  # ===========================
  # User Registration
  # ===========================
  observeEvent(input$reg_submit, {
    req(input$reg_name, input$reg_email, input$reg_password, input$reg_password_confirm)
    
    if(input$reg_password != input$reg_password_confirm){
      shinyFeedback::showToast("Passwords do not match", type="error")
      return()
    }
    
    current_users <- users()
    if(input$reg_email %in% current_users$email){
      shinyFeedback::showToast("Email already registered", type="error")
      return()
    }
    
    new_user <- data.frame(
      email = input$reg_email,
      password = input$reg_password,
      name = input$reg_name,
      stringsAsFactors = FALSE
    )
    
    users(rbind(current_users, new_user))
    shinyFeedback::showToast("Registration successful", type="success")
    shinyjs::hide("login_modal")
  })
  
  # ===========================
  # User Login
  # ===========================
  observeEvent(input$login_submit, {
    req(input$login_email, input$login_password)
    current_users <- users()
    
    user <- current_users[current_users$email == input$login_email & 
                            current_users$password == input$login_password, ]
    if(nrow(user) == 0){
      shinyFeedback::showToast("Invalid email or password", type="error")
    } else {
      shinyFeedback::showToast(paste("Welcome,", user$name), type="success")
      shinyjs::hide("login_modal")
    }
  })
  
} # End server

# ===========================
# Run App
# ===========================
shinyApp(ui, server)
