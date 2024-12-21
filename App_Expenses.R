# Load required libraries
library(magrittr)

calculate_balances <- function(trans) {
  balances <- data.frame(
    From = c("Adrian", "Adrian", "Fernando", "Fernando", "Family", "Family"),
    To = c("Fernando", "Family", "Adrian", "Family", "Adrian", "Fernando"),
    Amount = rep(0, 6)
  )
  
  # Calculate balances
  for (i in 1:nrow(trans)) {
    payer <- trans$Payer[i]
    amounts <- c(Adrian = trans$Contribution_Adrian[i],
                 Fernando = trans$Contribution_Fernando[i],
                 Family = trans$Contribution_Family[i])
    total_paid <- trans$Amount[i]
    
    for (person in names(amounts)) {
      if (payer != person) {
        balances$Amount[balances$From == payer & balances$To == person] <- 
          balances$Amount[balances$From == payer & balances$To == person] + 
          (total_paid * (amounts[person] / sum(amounts)))
      }
    }
  }
  
  # Add self-balance rows
  self_balances <- tibble::tibble(
    From = c("Adrian", "Fernando", "Family"),
    To = c("Adrian", "Fernando", "Family"),
    Amount = c(0, 0, 0)
  )
  
  balances <- dplyr::bind_rows(balances, self_balances) %>%
    dplyr::arrange(From, To) %>%
    tidyr::pivot_wider(names_from = To, values_from = Amount, values_fill = list(Amount = 0)) %>%
    tibble::column_to_rownames("From")
  
  net_balances <- rowSums(balances) - colSums(balances)
  
  # Return net balances as a tibble
  tibble::tibble(
    Adrian = net_balances["Adrian"],
    Fernando = net_balances["Fernando"],
    Family = net_balances["Family"]
  )
}


# Authenticate Google Sheets (run this once interactively)
googlesheets4::gs4_auth(path = "family-budget-397209-3c287def2b7d.json")

# Google Sheets URL (replace with your actual Google Sheets URL)
sheet_url <- "https://docs.google.com/spreadsheets/d/15rsACC6AVjkGE3J-2WbCmu1Qo01hXdhu41zDu7r8QIc"

ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Family Expense Tracker"),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Add Transaction", tabName = "add_transaction", icon = shiny::icon("plus")),
      shinydashboard::menuItem("Dashboard CHF", tabName = "dashboard_chf", icon = shiny::icon("dashboard")),
      shinydashboard::menuItem("Dashboard EUR", tabName = "dashboard_eur", icon = shiny::icon("euro"))
    )
  ),
  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),  # Initialize shinyjs
    # Add a loading indicator that will be shown during data loading
    tags$div(id = "loading", "Loading data...", style = "display:none;"),
    
    shinydashboard::tabItems(
      # Add Transaction Tab
      shinydashboard::tabItem(
        tabName = "add_transaction",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Enter Transaction Details", width = 12,
            shiny::dateInput(
              inputId = "transaction_date",
              label = "Transaction Date",
              value = Sys.Date() # Default to the current date
            ),
            shiny::radioButtons(
              inputId = "payer",
              label = "Payer",
              choices = c("Adrian", "Fernando", "Family"),
              selected = "Adrian",
              inline = TRUE
            ),
            shiny::radioButtons(
              inputId = "currency",
              label = "Currency",
              choices = c("CHF", "EUR"),
              selected = "CHF",
              inline = TRUE
            ),
            shiny::numericInput("amount", "Amount", value = 0, min = 0),
            shiny::numericInput("contribution_adrian", "Adrian's Contribution (%)", value = 41.97, min = 0, max = 100),
            shiny::numericInput("contribution_fernando", "Fernando's Contribution (%)", value = 58.03, min = 0, max = 100),
            shiny::numericInput("contribution_family", "Family's Contribution (%)", value = 0, min = 0, max = 100),
            shiny::radioButtons(
              inputId = "category",
              label = "Category",
              choices = c(
                "Books", "Cleaning", "Clothing", "Eating out", "Education",
                "Electronics", "Elevenses","Energie", "Flights", "Gas", "Gifts",
                "Groceries (inkl. Pharmacy)", "Health", "Holidays", "Hotels",
                "iCloud", "Insurances", "Internet TV", "Others", "Personal Care","Real Estate",
                "Taxes", "Transportation", "Wellness"
              ),
              selected = "Others"
            ),
            shiny::textAreaInput("notes", "Notes", value = ""),
            shiny::actionButton("save_transaction", "Save Transaction")
          )
        )
      ),
      # Dashboard CHF
      shinydashboard::tabItem(
        tabName = "dashboard_chf",
        shiny::fluidRow(
          shinydashboard::box(title = "Summary (CHF)", width = 12,
                              shiny::verbatimTextOutput("summary_chf")),
          shinydashboard::box(title = "Balances (CHF)", width = 12,
                              reactable::reactableOutput("balances_table_chf")),
          shinydashboard::box(title = "Spending by Category (CHF)", width = 12, 
                              shiny::uiOutput("plot_with_selector_chf")),  # Dynamic UI for the plot and selector
          shinydashboard::box(title = "Monthly Spending Trends (CHF)", width = 12, 
                              shiny::uiOutput("plot_trend_with_selector_chf"))
          
        )
      ),
      # Dashboard EUR
      shinydashboard::tabItem(
        tabName = "dashboard_eur",
        shiny::fluidRow(
          shinydashboard::box(title = "Summary (EUR)", width = 12,
                              shiny::verbatimTextOutput("summary_eur")),
          shinydashboard::box(title = "Balances (EUR)", width = 12,
                              reactable::reactableOutput("balances_table_eur")),
          shinydashboard::box(title = "Spending by Category (EUR)", width = 12,
                              shiny::uiOutput("plot_with_selector_eur")),  # Dynamic UI for the plot and selector
          shinydashboard::box(title = "Monthly Spending Trends (EUR)", width = 12, 
                              shiny::uiOutput("plot_trend_with_selector_eur"))  # Dynamic UI for the plot and selector
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Cache transactions using reactiveVal to store data
  transactions_data <- shiny::reactiveVal(NULL)  # Initialize reactive variable to store transactions
  
  # Function to load transactions from Google Sheets
  load_transactions <- function() {
    if (is.null(transactions_data())) {  # If data is not cached, load it
      tryCatch({
        sheet_data <- googlesheets4::read_sheet(sheet_url)
        
        # Clean and format data
        sheet_data <- sheet_data %>%
          dplyr::mutate(
            Timestamp = as.POSIXct(Timestamp, tz = "UTC"),
            TransactionDate = as.Date(TransactionDate),
            Month = factor(format(as.Date(TransactionDate), "%B"), levels = month.name),
            Payer = as.character(Payer),
            Currency = as.character(Currency),
            Amount = as.numeric(Amount),
            Contribution_Adrian = as.numeric(Contribution_Adrian),
            Contribution_Fernando = as.numeric(Contribution_Fernando),
            Contribution_Family = as.numeric(Contribution_Family),
            Category = as.character(Category),
            Notes = as.character(Notes)
          )
        
        transactions_data(sheet_data)  # Cache the loaded data
      }, error = function(e) {
        shiny::showNotification("Error loading Google Sheets data: Check permissions or data structure.", type = "error")
      })
    }
  }
  
  # Load transactions on app start or when necessary
  shiny::observe({
    load_transactions()  # Trigger data loading
  })
  
  # Save Transaction to Google Sheets
  shiny::observeEvent(input$save_transaction, {
    total_contribution <- input$contribution_adrian + input$contribution_fernando + input$contribution_family
    
    if (total_contribution != 100) {
      shiny::showNotification("Error: Contributions must sum to 100%", type = "error")
      return()
    }
    
    if (input$amount <= 0) {
      shiny::showNotification("Error: Amount must be positive", type = "error")
      return()
    }
    
    new_transaction <- data.frame(
      Timestamp = Sys.time(),
      TransactionDate = as.Date(input$transaction_date),
      Month = factor(format(as.Date(input$transaction_date), "%B"), levels = month.name),
      Payer = input$payer,
      Currency = input$currency,
      Amount = round(input$amount, 2),
      Contribution_Adrian = round(input$amount * (input$contribution_adrian / 100), 2),
      Contribution_Fernando = round(input$amount * (input$contribution_fernando / 100), 2),
      Contribution_Family = round(input$amount * (input$contribution_family / 100), 2),
      Category = input$category,
      Notes = input$notes,
      stringsAsFactors = FALSE
    )
    
    # Append the transaction to the data
    transactions(dplyr::bind_rows(transactions(), new_transaction))
    googlesheets4::sheet_append(sheet_url, new_transaction)
    shiny::showNotification("Transaction Saved!")
  })
        
  # Filtered Transactions
  transactions_chf <- shiny::reactive({
    transactions_data() %>% dplyr::filter(Currency == "CHF")
  })
  
  transactions_eur <- shiny::reactive({
    transactions_data() %>% dplyr::filter(Currency == "EUR")
  })
  
  # Summary Outputs
  output$summary_chf <- shiny::renderText({
    total_spent <- sum(transactions_chf()$Amount, na.rm = TRUE)
    paste("Total Spent (CHF):", total_spent)
  })
  
  output$summary_eur <- shiny::renderText({
    total_spent <- sum(transactions_eur()$Amount, na.rm = TRUE)
    paste("Total Spent (EUR):", total_spent)
  })
  
  # Balances Tables
  balances_chf <- shiny::reactive({
    trans <- transactions_chf()
    calculate_balances(trans)
  })

  # Balances Tables    
  balances_eur <- shiny::reactive({
    trans <- transactions_eur()
    calculate_balances(trans)
  })
  
  # Render Balances Table for CHF
  output$balances_table_chf <- reactable::renderReactable({
    # Retrieve the net balances for CHF
    net_balances <- balances_chf()
    
    # Convert the single-row tibble to a format suitable for reactable
    reactable_data <- tibble::tibble(
      Member = names(net_balances),
      NetBalance = as.numeric(net_balances)
    )
    
    # Render the reactable table
    reactable::reactable(
      reactable_data,
      columns = list(
        Member = reactable::colDef(name = "Member"),
        NetBalance = reactable::colDef(
          name = "Net Balance (CHF)",
          format = reactable::colFormat(separators = TRUE, digits = 2, prefix = "Fr. ")
        )
      ),
      bordered = TRUE,
      highlight = TRUE,
      defaultPageSize = 3,  # Show all members in a single page
      striped = TRUE
    )
  })
  
  # Render Balances Table for EUR
  output$balances_table_eur <- reactable::renderReactable({
    # Retrieve the net balances for EUR
    net_balances <- balances_eur()
    
    # Convert the single-row tibble to a format suitable for reactable
    reactable_data <- tibble::tibble(
      Member = names(net_balances),
      NetBalance = as.numeric(net_balances)
    )
    
    # Render the reactable table
    reactable::reactable(
      reactable_data,
      columns = list(
        Member = reactable::colDef(name = "Member"),
        NetBalance = reactable::colDef(
          name = "Net Balance (EUR)",
          format = reactable::colFormat(separators = TRUE, digits = 2, prefix = "€ ")
        )
      ),
      bordered = TRUE,
      highlight = TRUE,
      defaultPageSize = 3,  # Show all members in a single page
      striped = TRUE
    )
  })
  
  output$plot_with_selector_chf <- renderUI({
    # Inline UI elements
    tagList(
      shiny::selectInput("monthInputCHF", "Month", choices = month.name, selected = format(Sys.Date(), "%B")),
      shiny::plotOutput("category_plot_chf")
    )
  })
  
  # Spending by Category Plots
  output$category_plot_chf <- renderPlot({
    # Fetch and prepare data
    data <- transactions_chf() %>%
                    dplyr::group_by(Month, Category) %>%
                    dplyr::summarize(Total = sum(Amount, na.rm = TRUE))
    
    # Initial plot
    initial_data <- dplyr::filter(data, Month == input$monthInputCHF)
    ggplot2::ggplot(initial_data, ggplot2::aes(y = stats::reorder(Category, Total), x = Total)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = "Total Amount", y = "Category")
  })

  output$plot_with_selector_eur <- renderUI({
    # Inline UI elements
    tagList(
      shiny::selectInput("monthInputEUR", "Month", choices = month.name, selected = format(Sys.Date(), "%B")),
      shiny::plotOutput("category_plot_eur")
    )
  })  
    
  # Spending by Category Plots
  output$category_plot_eur <- renderPlot({
    # Fetch and prepare data
    data <- transactions_eur() %>%
      dplyr::group_by(Month, Category) %>%
      dplyr::summarize(Total = sum(Amount, na.rm = TRUE))
    
    # Initial plot
    initial_data <- dplyr::filter(data, Month == input$monthInputEUR)
    ggplot2::ggplot(initial_data, ggplot2::aes(y = stats::reorder(Category, Total), x = Total)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = "Total Amount", y = "Category")
  })
  
  output$plot_trend_with_selector_chf <- renderUI({
    # Inline UI elements
    tagList(
      shiny::selectInput("yearInputCHF", "Month", choices = c(2024,2025), selected = 2024),
      shiny::plotOutput("category_plot_CHF")
    )
  })  
  
  # Spending by Category Plots
  output$category_plot_CHF <- renderPlot({
    # Fetch and prepare data
    data <- transactions_chf() %>%
      dplyr::filter(lubridate::year(TransactionDate) == input$yearInputCHF) %>%
      dplyr::group_by(Month, Category) %>%
      dplyr::summarize(Total = sum(Amount),
                       Total_A = sum(Contribution_Adrian),
                       Total_F = sum(Contribution_Fernando), .groups = 'drop') %>%
      dplyr::arrange(desc(Total))
      
    # Total Monthly Spending
    total_monthly <- data %>%
      dplyr::group_by(Month) %>%
      dplyr::summarize(Total = sum(Total), .groups = 'drop') %>%
      dplyr::mutate(Group = "Payer",
                    Category = "Total") %>%
      dplyr::relocate(Group, .before = Month) %>%
      dplyr::relocate(Category, .before = Total)
    
    # Total per Payer
    adrian_monthly <- data %>%
      dplyr::group_by(Month) %>%
      dplyr::summarize(Total = sum(Total_A), .groups = 'drop') %>%
      dplyr::mutate(Group = "Payer",
                    Category = "Adrian") %>%
      dplyr::relocate(Group, .before = Month) %>%
      dplyr::relocate(Category, .before = Total)
    
    fernando_monthly <- data %>%
      dplyr::group_by(Month) %>%
      dplyr::summarize(Total = sum(Total_F), .groups = 'drop') %>%
      dplyr::mutate(Group = "Payer",
                    Category = "Fernando") %>%
      dplyr::relocate(Group, .before = Month) %>%
      dplyr::relocate(Category, .before = Total)
    
    # Top 5 Categories
    top_categories <- data %>%
      dplyr::group_by(Category) %>%
      dplyr::summarize(YearTotal = sum(Total), .groups = 'drop') %>%
      dplyr::top_n(6, YearTotal) %>%
      dplyr::pull(Category)
    
    category_monthly <- data %>%
      dplyr::filter(Category %in% top_categories) %>%
      dplyr::group_by(Month, Category) %>%
      dplyr::summarize(Total = sum(Total), .groups = 'drop') %>%
      dplyr::mutate(Group = "Category") %>%
      dplyr::relocate(Group, .before = Month)
    
    df <- dplyr::bind_rows(total_monthly, adrian_monthly, fernando_monthly, category_monthly)
    
    cat <- levels(factor(unique(df$Category)[-c(1:3)]))
    
    df <- df %>% dplyr::mutate(Group = base::factor(Group, levels = c("Payer","Category")),
                               Category = base::factor(Category, levels = c("Total","Adrian","Fernando",cat)))
  
    ggplot2::ggplot(df, ggplot2::aes(x = Month, y = Total, colour = Category, group = Category)) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(ggplot2::vars(Group)) +
      ggplot2::labs(y = "Total Spending", x = "Month") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom",legend.title = ggplot2::element_blank()) +
      ggplot2::guides(colour = ggplot2::guide_legend(ncol = 3)) 
  })
  
  output$plot_trend_with_selector_eur <- renderUI({
    # Inline UI elements
    tagList(
      shiny::selectInput("yearInputEUR", "Month", choices = c(2024,2025), selected = 2024),
      shiny::plotOutput("category_plot_EUR")
    )
  })  
  
  # Spending by Category Plots
  output$category_plot_EUR <- renderPlot({
    # Fetch and prepare data
    data <- transactions_eur() %>%
      dplyr::filter(lubridate::year(TransactionDate) == input$yearInputEUR) %>%
      dplyr::group_by(Month, Category) %>%
      dplyr::summarize(Total = sum(Amount),
                       Total_A = sum(Contribution_Adrian),
                       Total_F = sum(Contribution_Fernando), .groups = 'drop') %>%
      dplyr::arrange(desc(Total))
    
    # Total Monthly Spending
    total_monthly <- data %>%
      dplyr::group_by(Month) %>%
      dplyr::summarize(Total = sum(Total), .groups = 'drop') %>%
      dplyr::mutate(Group = "Payer",
                    Category = "Total") %>%
      dplyr::relocate(Group, .before = Month) %>%
      dplyr::relocate(Category, .before = Total)
    
    # Total per Payer
    adrian_monthly <- data %>%
      dplyr::group_by(Month) %>%
      dplyr::summarize(Total = sum(Total_A), .groups = 'drop') %>%
      dplyr::mutate(Group = "Payer",
                    Category = "Adrian") %>%
      dplyr::relocate(Group, .before = Month) %>%
      dplyr::relocate(Category, .before = Total)
    
    fernando_monthly <- data %>%
      dplyr::group_by(Month) %>%
      dplyr::summarize(Total = sum(Total_F), .groups = 'drop') %>%
      dplyr::mutate(Group = "Payer",
                    Category = "Fernando") %>%
      dplyr::relocate(Group, .before = Month) %>%
      dplyr::relocate(Category, .before = Total)
    
    # Top 5 Categories
    top_categories <- data %>%
      dplyr::group_by(Category) %>%
      dplyr::summarize(YearTotal = sum(Total), .groups = 'drop') %>%
      dplyr::top_n(6, YearTotal) %>%
      dplyr::pull(Category)
    
    category_monthly <- data %>%
      dplyr::filter(Category %in% top_categories) %>%
      dplyr::group_by(Month, Category) %>%
      dplyr::summarize(Total = sum(Total), .groups = 'drop') %>%
      dplyr::mutate(Group = "Category") %>%
      dplyr::relocate(Group, .before = Month)
    
    df <- dplyr::bind_rows(total_monthly, adrian_monthly, fernando_monthly, category_monthly)
    
    cat <- levels(factor(unique(df$Category)[-c(1:3)]))
    
    df <- df %>% dplyr::mutate(Group = base::factor(Group, levels = c("Payer","Category")),
                               Category = base::factor(Category, levels = c("Total","Adrian","Fernando",cat)))
    
    ggplot2::ggplot(df, ggplot2::aes(x = Month, y = Total, colour = Category, group = Category)) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(ggplot2::vars(Group)) +
      ggplot2::labs(y = "Total Spending", x = "Month") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom",legend.title = ggplot2::element_blank()) +
      ggplot2::guides(colour = ggplot2::guide_legend(ncol = 3)) 
  })
  
}

shiny::shinyApp(ui, server)