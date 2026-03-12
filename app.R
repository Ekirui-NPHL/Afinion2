################################################################################
# AFINION 2 STUDY - DEBUG VERSION WITH LOGIN FIXES
################################################################################

# ==============================================================================
# 1. LOAD REQUIRED LIBRARIES
# ==============================================================================

library(shiny)
library(shinydashboard)
library(shinyauthr)
library(tidyverse)
library(DT)
library(plotly)
library(openxlsx)
library(lubridate)
library(DBI)
library(pool)
library(RMySQL)
library(sodium)
library(jsonlite)

# ==============================================================================
# 2. DATABASE CONFIGURATION
# ==============================================================================

db_config <- list(
  driver = "MySQL",
  dbname = "afinion2_study",
  host = "1cccc",
  port = 3306,
  user = "dbadmin",
  password = "cccc@2026"
)

# ==============================================================================
# 3. DATABASE CONNECTION FUNCTIONS
# ==============================================================================

create_pool <- function(config) {
  dbPool(
    drv = RMySQL::MySQL(),
    dbname = config$dbname,
    host = config$host,
    port = config$port,
    user = config$user,
    password = config$password,
    minSize = 2,
    maxSize = 10,
    idleTimeout = 3600000
  )
}

# Initialize database with all tables
init_database <- function(pool) {
  
  # Users table with roles
  dbExecute(pool, "
    CREATE TABLE IF NOT EXISTS users (
      id INT AUTO_INCREMENT PRIMARY KEY,
      username VARCHAR(50) UNIQUE NOT NULL,
      password_hash VARCHAR(255) NOT NULL,
      full_name VARCHAR(100),
      email VARCHAR(100),
      role ENUM('admin', 'data_entry', 'viewer') NOT NULL,
      facility VARCHAR(100),
      is_active BOOLEAN DEFAULT TRUE,
      last_login TIMESTAMP NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      INDEX idx_username (username),
      INDEX idx_role (role),
      INDEX idx_facility (facility)
    )
  ")
  
  # Participants table
  dbExecute(pool, "
    CREATE TABLE IF NOT EXISTS participants (
      id INT AUTO_INCREMENT PRIMARY KEY,
      participant_id VARCHAR(50) NOT NULL,
      facility VARCHAR(100) NOT NULL,
      enrollment_date DATE,
      age INT,
      sex VARCHAR(10),
      county VARCHAR(100),
      hiv_status BOOLEAN,
      hiv_diagnosis_date DATE,
      on_art BOOLEAN,
      art_regimen VARCHAR(50),
      art_duration_months INT,
      cd4_count INT,
      viral_load INT,
      tb_status VARCHAR(50),
      known_diabetes BOOLEAN,
      known_hypertension BOOLEAN,
      known_cvd BOOLEAN,
      hba1c_result DECIMAL(4,1),
      crp_result DECIMAL(5,1),
      total_cholesterol DECIMAL(4,1),
      hdl DECIMAL(4,1),
      ldl DECIMAL(4,1),
      triglycerides DECIMAL(4,1),
      acr_result INT,
      turnaround_minutes INT,
      same_day_counseling BOOLEAN,
      referred BOOLEAN,
      linkage_confirmed VARCHAR(20),
      patient_satisfied BOOLEAN,
      created_by VARCHAR(50),
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_by VARCHAR(50),
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      INDEX idx_facility (facility),
      INDEX idx_enrollment_date (enrollment_date),
      INDEX idx_participant_id (participant_id),
      UNIQUE KEY unique_participant_facility (participant_id, facility)
    )
  ")
  
  # QC Log table
  dbExecute(pool, "
    CREATE TABLE IF NOT EXISTS qc_log (
      id INT AUTO_INCREMENT PRIMARY KEY,
      date DATE,
      facility VARCHAR(100),
      device_serial VARCHAR(50),
      operator VARCHAR(100),
      test VARCHAR(50),
      qc_level VARCHAR(20),
      pass_fail VARCHAR(10),
      created_by VARCHAR(50),
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      INDEX idx_facility (facility),
      INDEX idx_date (date)
    )
  ")
  
  # Audit log for tracking all changes
  dbExecute(pool, "
    CREATE TABLE IF NOT EXISTS audit_log (
      id INT AUTO_INCREMENT PRIMARY KEY,
      username VARCHAR(50),
      action VARCHAR(50),
      table_name VARCHAR(50),
      record_id VARCHAR(100),
      old_values JSON,
      new_values JSON,
      ip_address VARCHAR(50),
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      INDEX idx_username (username),
      INDEX idx_action (action),
      INDEX idx_created_at (created_at)
    )
  ")
  
  message("✅ Database tables initialized")
}

# Create connection pool
pool <- create_pool(db_config)

# Initialize database
tryCatch({
  init_database(pool)
}, error = function(e) {
  message("Database initialization warning: ", e$message)
})

# ==============================================================================
# 4. LOAD USERS WITH DEBUGGING
# ==============================================================================

# Function to verify password
verify_password <- function(stored_hash, provided_password) {
  result <- tryCatch({
    sodium::password_verify(stored_hash, provided_password)
  }, error = function(e) {
    message("Password verification error: ", e$message)
    FALSE
  })
  return(result)
}

# Load users with debugging
load_users_static <- function() {
  message("Loading users from database...")
  
  tryCatch({
    users_df <- dbGetQuery(pool, "
      SELECT 
        username,
        password_hash as password,
        role,
        facility,
        full_name
      FROM users 
      WHERE is_active = TRUE
    ")
    
    message("Found ", nrow(users_df), " active users")
    
    if(nrow(users_df) > 0) {
      # Print usernames for debugging
      message("Usernames in database: ", paste(users_df$username, collapse = ", "))
      
      users_df$permissions <- users_df$role
      users_df$name <- users_df$full_name
    } else {
      message("WARNING: No active users found in database!")
      
      # Create emergency admin user if no users exist
      message("Creating emergency admin user...")
      emergency_hash <- password_store("admin123")
      
      dbExecute(pool, "
        INSERT INTO users (username, password_hash, full_name, role, is_active)
        VALUES ('emergency_admin', ?, 'Emergency Admin', 'admin', TRUE)
      ", params = list(emergency_hash))
      
      # Reload users
      users_df <- dbGetQuery(pool, "
        SELECT 
          username,
          password_hash as password,
          role,
          facility,
          full_name
        FROM users 
        WHERE is_active = TRUE
      ")
      
      users_df$permissions <- users_df$role
      users_df$name <- users_df$full_name
      
      message("Emergency admin created - Username: emergency_admin, Password: admin123")
    }
    
    return(users_df)
    
  }, error = function(e) {
    message("Error loading users: ", e$message)
    
    # Return a minimal data frame for testing
    return(data.frame(
      username = c("test_user"),
      password = c(password_store("test123")),
      role = c("admin"),
      facility = c("Mbagathi"),
      full_name = c("Test User"),
      permissions = c("admin"),
      name = c("Test User"),
      stringsAsFactors = FALSE
    ))
  })
}

# Load users
user_base_static <- load_users_static()

# Print summary
message("\n=== LOGIN DEBUG INFO ===")
message("User base loaded with ", nrow(user_base_static), " users")
if(nrow(user_base_static) > 0) {
  message("Available users: ", paste(user_base_static$username, collapse = ", "))
  message("User roles: ", paste(user_base_static$role, collapse = ", "))
}
message("=======================\n")

# Log user login
log_login <- function(pool, username, ip = "unknown") {
  tryCatch({
    dbExecute(pool, "
      UPDATE users 
      SET last_login = NOW() 
      WHERE username = ?
    ", params = list(username))
    
    dbExecute(pool, "
      INSERT INTO audit_log (username, action, table_name, ip_address)
      VALUES (?, 'login', 'users', ?)
    ", params = list(username, ip))
    
    message("Login logged for user: ", username)
  }, error = function(e) {
    message("Login logging error: ", e$message)
  })
}

# ==============================================================================
# 5. USER INTERFACE
# ==============================================================================

ui <- dashboardPage(
  
  dashboardHeader(
    title = "Afinion 2 Study - Integrated TB/HIV/Diabetes Care",
    titleWidth = 450,
    tags$li(
      class = "dropdown",
      style = "padding: 8px;",
      uiOutput("user_info")
    )
  ),
  
  dashboardSidebar(
    width = 250,
    uiOutput("sidebar")
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .skin-blue .main-header .logo { background-color: #3c8dbc; }
        .skin-blue .main-header .navbar { background-color: #3c8dbc; }
        .content-wrapper, .right-side { background-color: #f4f4f4; }
        .small-box { border-radius: 10px; }
        .btn-primary { background-color: #3c8dbc; }
        .role-badge {
          padding: 3px 8px;
          border-radius: 12px;
          font-size: 12px;
          font-weight: bold;
          color: white;
        }
        .role-admin { background-color: #dc3545; }
        .role-data_entry { background-color: #28a745; }
        .role-viewer { background-color: #ffc107; color: black; }
        .facility-badge {
          background-color: #17a2b8;
          padding: 3px 8px;
          border-radius: 12px;
          font-size: 12px;
          color: white;
        }
        .debug-box {
          background-color: #f8f9fa;
          border: 1px solid #dee2e6;
          padding: 10px;
          margin: 10px 0;
          font-family: monospace;
        }
      "))
    ),
    
    # Debug info box (only shown when not logged in)
    conditionalPanel(
      condition = "output.show_debug == true",
      div(class = "debug-box",
          h4("Login Debug Information"),
          p(paste("Available users:", paste(user_base_static$username, collapse = ", "))),
          p("If no users appear, run the setup script first.")
      )
    ),
    
    shinyauthr::loginUI("login"),
    uiOutput("main_content")
  )
)

# ==============================================================================
# 6. SERVER LOGIC
# ==============================================================================

server <- function(input, output, session) {
  
  # Show debug info when not authenticated
  output$show_debug <- reactive({
    is.null(credentials()$user_auth) || !credentials()$user_auth
  })
  outputOptions(output, "show_debug", suspendWhenHidden = FALSE)
  
  # ----------------------------------------------------------------------------
  # 6.1 Authentication
  # ----------------------------------------------------------------------------
  
  # Authentication call
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base_static,
    user_col = "username",
    pwd_col = "password",
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # Logout button state
  logout_init <- reactiveVal(FALSE)
  
  # User authentication state
  user_auth <- reactive({
    if(!is.null(credentials()$user_auth)) {
      credentials()$user_auth
    } else {
      FALSE
    }
  })
  
  # User info
  user_info <- reactive({
    if(!is.null(credentials()$info)) {
      credentials()$info
    } else {
      NULL
    }
  })
  
  # Observe login
  observe({
    if(user_auth()) {
      logout_init(FALSE)
      log_login(pool, user_info()$username)
      showNotification(
        paste("Welcome", user_info()$full_name, "(", user_info()$role, ")"),
        type = "message",
        duration = 5
      )
    }
  })
  
  # Handle logout
  observeEvent(input$logout_btn, {
    logout_init(TRUE)
  })
  
  # User info header
  output$user_info <- renderUI({
    req(user_auth())
    
    role_color <- switch(user_info()$role,
                         "admin" = "role-admin",
                         "data_entry" = "role-data_entry",
                         "viewer" = "role-viewer")
    
    div(
      span(user_info()$full_name, style = "margin-right: 10px;"),
      span(class = paste("role-badge", role_color), user_info()$role),
      if(!is.null(user_info()$facility) && user_info()$facility != "") {
        span(class = "facility-badge", style = "margin-left: 5px;", user_info()$facility)
      },
      actionButton("logout_btn", "Logout", class = "btn-danger btn-sm", style = "margin-left: 10px;")
    )
  })
  
  # ----------------------------------------------------------------------------
  # 6.2 Sidebar
  # ----------------------------------------------------------------------------
  
  output$sidebar <- renderUI({
    req(user_auth())
    
    role <- user_info()$role
    
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      
      if(role %in% c("admin", "data_entry")) {
        menuItem("Data Entry", tabName = "data_entry", icon = icon("edit"))
      },
      
      menuItem("View Data", tabName = "view_data", icon = icon("table")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-bar")),
      
      if(role %in% c("admin", "data_entry")) {
        menuItem("QC Log", tabName = "qc_log", icon = icon("check-circle"))
      },
      
      menuItem("Export", tabName = "export", icon = icon("download")),
      
      if(role == "admin") {
        menuItem("User Management", tabName = "user_mgmt", icon = icon("users-cog"))
      },
      
      if(role == "admin") {
        menuItem("Audit Log", tabName = "audit", icon = icon("history"))
      },
      
      if(role == "admin") {
        menuItem("Database Status", tabName = "db_status", icon = icon("database"))
      },
      
      menuItem("Study Info", tabName = "info", icon = icon("info-circle"))
    )
  })
  
  # ----------------------------------------------------------------------------
  # 6.3 Main Content (Simplified for debugging)
  # ----------------------------------------------------------------------------
  
  output$main_content <- renderUI({
    req(user_auth())
    
    tab <- input$tabs
    if(is.null(tab)) tab <- "dashboard"
    
    # Simple dashboard for testing
    if(tab == "dashboard") {
      tagList(
        fluidRow(
          valueBox(
            value = "Welcome!",
            subtitle = paste("Logged in as", user_info()$full_name),
            icon = icon("check"),
            color = "green"
          )
        ),
        fluidRow(
          box(title = "Login Successful", status = "success", width = 12,
              h3("You have successfully logged in!"),
              p(paste("Username:", user_info()$username)),
              p(paste("Role:", user_info()$role)),
              p(paste("Facility:", ifelse(is.null(user_info()$facility) || user_info()$facility == "", "None", user_info()$facility)))
          )
        )
      )
    } else {
      # Placeholder for other tabs
      fluidRow(
        box(title = "Under Construction", width = 12,
            h3(paste("Tab:", tab)),
            p("This tab is under construction.")
        )
      )
    }
  })
  
  # Keep the pool alive
  session$onSessionEnded(function() {
    poolClose(pool)
  })
}

# ==============================================================================
# 7. RUN THE APP
# ==============================================================================

shinyApp(ui = ui, server = server)