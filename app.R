
# Before running, add ledger.csv to data/ , using:
# `ledger csv -f ~/.ledger.journal > ledger.csv`

# Packages ----
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

# Import data and fix ----
journal_names <- c("date", "NA", "description",
									 "source", "currency", "amount", 
									 "pending_cleared", "NA")

ledger <- read_csv("data/ledger.csv", 
									 col_names = journal_names)

ledger$good_date <- as.Date(ledger$date, format = "%Y/%m/%d")

# Sort by good_date - helps geom_text work better
ledger_sort <- ledger[order(ledger$good_date),]

# Add cumulative column for each source
ledger_cumsum <- ledger_sort %>%
	group_by(source) %>%
	mutate(cumulative = cumsum(amount))

# Create dataframe only with assets, excluding expenses
assets <- ledger_cumsum %>%
	filter(grepl("assets", source)) %>%
	ungroup()

# Create some variables ----
today <- Sys.Date()

# Total equity
equity <- sum(assets$amount)


# Yearly profit
year_end <- equity

year_start_df <- assets %>%
	filter(good_date < (today - 365))

year_start <- sum(year_start_df$amount)

annual_profit <- year_end - year_start

# Monthly profit
month_start_df <- assets %>%
	filter(good_date > (today - 30)) %>%
	mutate(expense_income = if_else(amount > 0, "Income", "Expense")) %>%
	group_by(expense_income) %>%
	summarise(total = sum(amount)) %>%
	mutate(total = abs(total))

month_profit <- max(month_start_df$total) - min(month_start_df$total)

# Food costs 30 days
food_30_df <- ledger_cumsum %>%
	filter(grepl("expenses:food", source),
				 good_date > (today - 30))

food_30 <- sum(food_30_df$amount)

# Transport costs 30 days
trans_30_df <- ledger_cumsum %>%
	filter(grepl("expenses:transport", source),
				 good_date > (today - 30))

trans_30 <- sum(trans_30_df$amount)


# ui.R ----
ui <- dashboardPage(
	dashboardHeader(title = "Analysing Ledger data"),
	dashboardSidebar(disable = TRUE),
	dashboardBody(
		fluidRow(
			column(width = 6,
						 fluidRow(width = 6,
						 				 infoBox("Today's date",
						 								 format.Date(today, format = "%d-%b-%Y"),
						 								 icon = icon("calendar"),
						 								 color = "teal",
						 								 width = 6),
						 				 infoBox("Total equity",
						 				 				paste("£", equity), 
						 				 				icon = icon("credit-card"), 
						 				 				color = "teal",
						 				 				width = 6)
						 ),
						 fluidRow(width = 6,
						 				 infoBox("Annual profit", 
						 				 				paste("£", round(annual_profit, digits = 2)),
						 				 				icon = 
						 				 					if(annual_profit > 0){
						 				 						icon("arrow-up")
						 				 					} else { 
						 				 						icon("arrow-down")
						 				 						},
						 				 				color = ifelse(annual_profit > 0, "teal", "red"),
						 				 				width = 6),
						 				 infoBox("30 day profit", 
						 				 				paste("£", round(month_profit, digits = 2)),
						 				 				icon = 
						 				 					if(month_profit > 0){
						 				 						icon("arrow-up")
						 				 					} else { 
						 				 						icon("arrow-down")
						 				 					},
						 				 				color = ifelse(month_profit > 0, "teal", "red"),
						 				 				width = 6)
						 				 ),
						 fluidRow(width = 6,
						 				 infoBox("Transport costs (30 days)",
						 				 				paste("£", trans_30),
						 				 				icon = icon("train"),
						 				 				color = "teal",
						 				 				width = 6),
						 				 infoBox("Food costs (30 days)", 
						 				 				paste("£", food_30),
						 				 				icon = icon("shopping-cart"),
						 				 				color = "teal",
						 				 				width = 6)
						 )
			),
			box(
				title = "Assets over time",
				plotOutput("plot1", height = 250),
				width = 6)
		),
		fluidRow(
			box(
				title = "Expenses vs. income over last 30 days",
				plotOutput("plot4", height = 250),
				width = 6),
			box(
				title = "Expenses by group",
				plotOutput("plot3", height = 250),
				width = 6)
		),
		fluidRow(
			box(
				title = "Student account over time",
				plotOutput("plot2", height = 250),
				width = 6),
			box(
				title = "Total equity over time",
				plotOutput("plot5", height = 250),
				width = 6)
			)
			)
		)


# server.R ----
server <- function(input, output) {
	
	## Create df only with Assets
	
	output$plot1 <- renderPlot({
		assets <- ledger_cumsum %>%
			filter(grepl("assets", source)) %>%
			ungroup() %>%
			mutate(source = gsub("assets:bank:", "", source)) %>%
			mutate(source = gsub("assets:", "", source)) %>%
			filter(source != "cash")
		
		ggplot(assets, aes(x = good_date, y = cumulative, group = source)) + 
			geom_hline(aes(yintercept = 0), colour = "red") +
			geom_line(aes(colour = source), size = 1.2) + 
			geom_point(aes(colour = source), size = 2) +
			scale_x_date(date_breaks = "1 month", date_labels = "%b/%Y") + 
			xlab("Date") + 
			ylab("Balance (£)") + 
			theme_classic() + 
			theme(axis.text.x=element_text(angle=45, 
																		 vjust=1, 
																		 hjust=1)) + 
			theme(legend.title = element_blank()) 
	})
	
	output$plot2 <- renderPlot({
		## Create df only with student account 
		assets_bank_student <- ledger_cumsum %>%
			filter(source == "assets:bank:student")
		
		## Line plot of student account over time with description of expenditure
		ggplot(assets_bank_student, aes(x = good_date, y = cumulative, group = source)) + 
			geom_line() + 
			scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") + 
			xlab("Date") + 
			ylab("Balance (£)") + 
			theme_classic() + 
			theme(axis.text.x=element_text(angle=45, 
																		 vjust=1, 
																		 hjust=1))
	})
	
	output$plot3 <- renderPlot({
		## Create df of Expenses summary
		expenses_sum <- ledger_cumsum %>%
			filter(grepl("expenses", source)) %>%
			group_by(source) %>%
			summarise(amount = sum(amount)) %>%
			mutate(percentage = amount / sum(amount) * 100) %>%
			mutate(source = gsub("expenses:", "", source)) %>%
			mutate(source = factor(source, levels = source[order(amount, decreasing = TRUE)]))  # Create ordered factor for x axis)
		
		## Bar plot of Expenses
		ggplot(expenses_sum, aes(x = source, y = amount)) + 
			geom_bar(stat = "identity", aes(fill = source)) + 
			xlab("Expense") + 
			ylab("Amount (£)") + 
			theme_classic() + 
			theme(legend.position = "none") +
			theme(axis.text.x=element_text(angle=45, 
																		 vjust=1, 
																		 hjust=1))
	})
	
	output$plot4 <- renderPlot({
		
		## Last (x) days income/expense summary
		ledger_30d_summ <- assets %>%
			filter(good_date > (today - 30)) %>%
			mutate(expense_income = if_else(amount > 0, "Income", "Expense")) %>%
			group_by(expense_income) %>%
			summarise(total = sum(amount)) %>%
			mutate(total = abs(total))
		
		## Colour palette
		expense_income_palette <- c("#D43131", "#1CB5DB")
		
		## Bar plot income vs Expenses for last (x) days.
		ggplot(ledger_30d_summ, aes(x = expense_income, y = total), 
																	 environment = environment()) + 
			geom_bar(stat = "identity", fill = expense_income_palette) + 
			geom_errorbar(aes(x = ifelse(ledger_30d_summ$total[1] > ledger_30d_summ$total[2], "Income", "Expense"), 
												ymax = max(ledger_30d_summ$total),
												ymin = min(ledger_30d_summ$total))) +
			geom_text(aes(x = ifelse(ledger_30d_summ$total[1] > ledger_30d_summ$total[2], "Income", "Expense"),
										y = min(ledger_30d_summ$total) + 0.5*(max(ledger_30d_summ$total) - min(ledger_30d_summ$total)),
										label = ifelse(ledger_30d_summ$total[1] > ledger_30d_summ$total[2],
																	 paste("£ -", round(max(ledger_30d_summ$total) - min(ledger_30d_summ$total), digits = 2), sep = ""),
																	 paste("£ ", round(max(ledger_30d_summ$total) - min(ledger_30d_summ$total), digits = 2), sep = "")),
										hjust = -0.5)) + 
			xlab("") + 
			ylab("Amount (£)") + 
			theme_classic()
		})
	
	output$plot5 <- renderPlot({
		assets <- assets %>%
			mutate(source = gsub("assets:bank:", "", source)) %>%
			mutate(source = gsub("assets:", "", source)) 
		
		ts <- seq.Date(from = min(assets$good_date), to = max(assets$good_date), by="day")
		
		df <- data.frame(good_date=ts)
		
		assets_interp <- full_join(df,assets, by = "good_date")
		
		assets_total <- assets_interp %>%
			group_by(good_date) %>%
			summarise(sum_cum = sum(amount)) %>%
			mutate(cumulative = cumsum(replace_na(sum_cum, 0)))
		
		ggplot(assets_total, aes(x = good_date, y = cumulative)) + 
			geom_line() + 
			scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") + 
			xlab("Date") + 
			ylab("Balance (£)") + 
			theme_classic() + 
			theme(axis.text.x=element_text(angle=45, 
																		 vjust=1, 
																		 hjust=1))
	})
}

# Run the application ----
shinyApp(ui = ui, server = server)

