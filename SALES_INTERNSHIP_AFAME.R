library(readxl)
library(dplyr)
library(ggplot2)

ECOMM_DATA <- read_excel("Downloads/ECOMM DATA.xlsx")

ECOMM_DATA$Order_Date <- as.Date(ECOMM_DATA$`Order Date`)

ECOMM_DATA$Order_Year <- format(ECOMM_DATA$`Order Date`, "%Y")

year_groups <- split(ECOMM_DATA, ECOMM_DATA$Order_Year)

par(mfrow=c(ceiling(length(year_groups) / 2), 2))  # Adjust the layout

for (year in names(year_groups)) {
  year_data <- year_groups[[year]]
  year_data <- year_data[order(year_data$Order_Date), ]
  
  plot(year_data$Order_Date, year_data$Sales, 
       type = "h", main = paste("Sales for Year:", year), 
       xlab = "Date", ylab = "Sales", col = "blue", pch = 16)
  
  total_sales <- sum(year_data$Sales)
  mean_sales <- mean(year_data$Sales)
  cat(year, "sales\n")
  cat("Total sales: ", format(total_sales, big.mark = ","), "\n")
  cat("Mean sales: ", format(mean_sales, big.mark = ","), "\n\n")
  
  monthly_sales <- aggregate(year_data$Sales, 
                             by = list(Order_Month = format(year_data$Order_Date, "%B")), 
                             FUN = sum)
  cat(year, "monthly sales\n")
  print(monthly_sales)
  cat("\n")
  
  quarterly_sales <- aggregate(year_data$Sales, 
                               by = list(Order_Quarter = quarters(year_data$Order_Date)), 
                               FUN = sum)
  cat(year, "quarterly sales\n")
  print(quarterly_sales)
  cat("\n")
}

product_sales <- ECOMM_DATA %>%
  group_by(`Product Name`) %>%
  summarise(total_sales = sum(Sales)) %>%
  arrange(desc(total_sales))  
top_products <- head(product_sales, 10)  
print(ggplot(top_products, aes(x = reorder(`Product Name`, total_sales), y = total_sales)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(x = "Product Name", y = "Total Sales", title = "Top 10 Best-Selling Products") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)))

