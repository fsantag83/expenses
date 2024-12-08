# Load required libraries
library(shiny)
library(shinydashboard)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(DT)


# Authenticate Google Sheets (run this once interactively)
googlesheets4::gs4_auth()

# Google Sheets URL (replace with your actual Google Sheets URL)
sheet_url <- "https://docs.google.com/spreadsheets/d/15rsACC6AVjkGE3J-2WbCmu1Qo01hXdhu41zDu7r8QIc"

ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Family Expense Tracker"),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Add Transaction", tabName = "add_transaction", icon = shiny::icon("plus")),
      shinydashboard::menuItem("Dashboard", tabName = "dashboard", icon = shiny::icon("dashboard"))
    )
  ),
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      # Add Transaction Tab
      shinydashboard::tabItem(
        tabName = "add_transaction",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Enter Transaction Details", width = 12,
            shiny::textInput("payer", "Payer", value = "Adrian"),
            shiny::numericInput("amount", "Amount", value = 0, min = 0),
            shiny::numericInput("contribution_adrian", "Adrian's Contribution (%)", value = 41.97, min = 0, max = 100),
            shiny::numericInput("contribution_fernando", "Fernando's Contribution (%)", value = 58.03, min = 0, max = 100),
            shiny::numericInput("contribution_family", "Family's Contribution (%)", value = 0, min = 0, max = 100),
            shiny::textInput("category", "Category", value = "General"),
            shiny::textAreaInput("notes", "Notes", value = ""),
            shiny::actionButton("save_transaction", "Save Transaction")
          )
        )
      ),
      # Dashboard Tab
      shinydashboard::tabItem(
        tabName = "dashboard",
        shiny::fluidRow(
          shinydashboard::box(title = "Summary", width = 12,
                              shiny::verbatimTextOutput("summary_text")),
          shinydashboard::box(title = "Spending by Category", width = 12,
                              shiny::plotOutput("category_plot")),
          shinydashboard::box(title = "Transaction Table", width = 12,
                              DT::DTOutput("transaction_table"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive Values to Store Transactions
  transactions <- shiny::reactiveVal(data.frame(
    Date = as.Date(character()),
    Payer = character(),
    Amount = numeric(),
    Contribution_Adrian = numeric(),
    Contribution_Fernando = numeric(),
    Contribution_Family = numeric(),
    Category = character(),
    Notes = character(),
    stringsAsFactors = FALSE
  ))
  
  # Save Transaction to Google Sheets
  shiny::observeEvent(input$save_transaction, {
    # Validate that contributions sum to 100%
    total_contribution <- input$contribution_adrian + input$contribution_fernando + input$contribution_family
    if (total_contribution != 100) {
      shiny::showNotification("Error: Contributions must sum to 100%", type = "error")
      return()
    }
    
    new_transaction <- data.frame(
      Date = Sys.Date(),
      Payer = input$payer,
      Amount = input$amount,
      Contribution_Adrian = input$amount * (input$contribution_adrian / 100),
      Contribution_Fernando = input$amount * (input$contribution_fernando / 100),
      Contribution_Family = input$amount * (input$contribution_family / 100),
      Category = input$category,
      Notes = input$notes
    )
    
    # Append to Transactions Reactive Value
    transactions(dplyr::bind_rows(transactions(), new_transaction))
    
    # Save to Google Sheets
    googlesheets4::sheet_append(sheet_url, new_transaction)
    
    shiny::showNotification("Transaction Saved!")
  })
  
  # Summary Output
  output$summary_text <- shiny::renderText({
    trans <- transactions()
    total_spent <- sum(trans$Amount, na.rm = TRUE)
    paste("Total Spent:", total_spent)
  })
  
  # Spending by Category Plot
  output$category_plot <- shiny::renderPlot({
    trans <- transactions()
    category_summary <- dplyr::group_by(trans, Category) %>%
      dplyr::summarize(Total = sum(Amount, na.rm = TRUE))
    
    ggplot2::ggplot(category_summary, ggplot2::aes(x = Category, y = Total)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Spending by Category", x = "Category", y = "Total Amount")
  })
  
  # Transaction Table
  output$transaction_table <- DT::renderDT({
    DT::datatable(transactions())
  })
}

shiny::shinyApp(ui, server)