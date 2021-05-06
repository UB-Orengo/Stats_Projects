
setwd("G:\\My Drive\\1_SPRING 2021\\2_STATISTICS AND ANALYSIS\\")

##Libraries Used
library(tidyverse)
library(ggplot2)
library(stats)
library(dplyr)
library(tidyr)
library(data.table)
library(moments)
library(ggpubr)
library(readxl)
library(plotrix)

##INITIAL EXPLORATION


#LOAD DATA, CONVERT TO DF
amazon_dataset <- read_excel("G:\\My Drive\\1_SPRING 2021\\2_STATISTICS AND ANALYSIS\\amazon_dataset.xlsx")
str(amazon_dataset)
as.data.frame(amazon_dataset)

# CLEAN TITLES // DROP NAs PRODUCT CATEGORY 1 (I DELIMITED THE PRODUCT CATEGORY COLUMN IN EXCEL, EACH PRODUCT HAS A MAXIMUM OF 7 PRODUCT CATEGORIES)
amazon_dataset_clean_title <- amazon_dataset
amazon_dataset_old_names <- colnames(amazon_dataset)
amazon_dataset_names <- c("Uniq_Id","Crawl_Timestamp","Product_Id","Product_Barcode","Product_Company_Type_Source","Retailer","Product_Category_1","Product_Category_2","Product_Category_3","Product_Category_4","Product_Category_5",	"Product_Category_6","Product_Category_7", "Product_Brand","Product_Name","Product_Price","Sku","Upc","Product_Url","Market","Product_Description","Product_Currency","Product_Available_Inventory","Product_Image_Url","Product_Model_Number","Product_Tags","Product_Contents","Product_Rating","Product_Reviews_Count","Bsr","Joining_Key")

setnames(amazon_dataset_clean_title, old = amazon_dataset_old_names, new = amazon_dataset_names)
amazon_dataset_clean_title <- drop_na(amazon_dataset_clean_title, "Product_Category_1")
amazon_dataset <- amazon_dataset_clean_title

#MEAN PRICE OF PRODUCTS ACROSS THE ENTIRE DATASET
ad_avg_price <- mean(amazon_dataset$Product_Price, na.rm = TRUE)

#PRODUCT CATEGORIES IN THE DATASET
prod_cats <- unique(amazon_dataset[c("Product_Category_1")])

str(prod_cats)

##MEANS AND COUNTS FOR PRODUCT CATEGORIES
mean_prod_cat_1 <- amazon_dataset %>%
  group_by(product_category = amazon_dataset$Product_Category_1) %>%
  summarize(average_price = mean(Product_Price, na.rm = TRUE)) %>%
  arrange(product_category)

count_prod_cat_1 <- amazon_dataset %>%
  group_by(product_category = amazon_dataset$Product_Category_1) %>%
  tally() %>%
  arrange(product_category)

view(count_prod_cat_1)

names(count_prod_cat_1)[names(count_prod_cat_1) == "n"] <- "product_count"

#TABLE OF PRODUCT CATEGORIES
product_mean_count <- cbind(mean_prod_cat_1, count_prod_cat_1$product_count)
names(product_mean_count)[names(product_mean_count) == "count_prod_cat_1$product_count"] <- "product_count"
product_mean_count <- product_mean_count %>%
  arrange(desc(product_count))

view(product_mean_count)

total_obs <- sum(product_mean_count[1:27,3])

#CHOOSING PRODUCT CATEGORIES I WISH TO EXPLORE FURTHER BASED ON PRODUCT COUNT/MEAN

desired_cats <- product_mean_count[1:9,]
desired_cats_list <- subset(desired_cats, select = -c(average_price,product_count))

str(desired_cats)
total_obs_9 <- sum(product_mean_count[1:9,3])
print(total_obs)
print(total_obs_9)

percentage_top_9 <- round(total_obs_9/total_obs * 100, digits = 2)

print(percentage_top_9)

percentages_prod_cat <- round(c((product_mean_count[1,3]/total_obs_9)*100 , (product_mean_count[2,3]/total_obs_9)*100, (product_mean_count[3,3]/total_obs_9)*100, (product_mean_count[4,3]/total_obs_9)*100, (product_mean_count[5,3]/total_obs_9)*100, (product_mean_count[6,3]/total_obs_9)*100, (product_mean_count[7,3]/total_obs_9)*100, (product_mean_count[8,3]/total_obs_9)*100, (product_mean_count[9,3]/total_obs_9)*100), digits = 2)

totals_cat_top_9 <- cbind.data.frame(desired_cats[,1], percentages_prod_cat, product_mean_count[1:9,2] ,product_mean_count[1:9,3])
colnames(totals_cat_top_9) <- c("Product Category", "Percentage", "Mean, Product Price", "Product Count")

view(totals_cat_top_9)

#CREATING DF FOR EACH PRODUCT CATEGORY

amazon_dataset_1 <- filter(amazon_dataset, Product_Category_1 == "Home & Kitchen")
amazon_dataset_2 <- filter(amazon_dataset, Product_Category_1 == "Health & Household")
amazon_dataset_3 <- filter(amazon_dataset, Product_Category_1 == "Beauty & Personal Care")
amazon_dataset_4 <- filter(amazon_dataset, Product_Category_1 == "Tools & Home Improvement")
amazon_dataset_5 <- filter(amazon_dataset, Product_Category_1 == "Toys & Games")
amazon_dataset_6 <- filter(amazon_dataset, Product_Category_1 == "Baby Products")
amazon_dataset_7 <- filter(amazon_dataset, Product_Category_1 == "Office Products")
amazon_dataset_8 <- filter(amazon_dataset, Product_Category_1 == "Industrial & Scientific")
amazon_dataset_9 <- filter(amazon_dataset, Product_Category_1 == "Clothing, Shoes & Jewelry")

##INTITIAL FINDINGS -- MEAN, MEDIAN, MODE, HISTOGRAMS 

# CREATED VECTOR AND DATAFRAMES FOR PRODUCT PRICE
amazon_dataset_1_1 <- as.data.frame(na.omit(amazon_dataset_1$Product_Price))
amazon_dataset_2_1 <- as.data.frame(na.omit(amazon_dataset_2$Product_Price))
amazon_dataset_3_1 <- as.data.frame(na.omit(amazon_dataset_3$Product_Price))
amazon_dataset_4_1 <- as.data.frame(na.omit(amazon_dataset_4$Product_Price))
amazon_dataset_5_1 <- as.data.frame(na.omit(amazon_dataset_5$Product_Price))
amazon_dataset_6_1 <- as.data.frame(na.omit(amazon_dataset_6$Product_Price))
amazon_dataset_7_1 <- as.data.frame(na.omit(amazon_dataset_7$Product_Price))
amazon_dataset_8_1 <- as.data.frame(na.omit(amazon_dataset_8$Product_Price))
amazon_dataset_9_1 <- as.data.frame(na.omit(amazon_dataset_9$Product_Price))

amazon_dataset_1_vector <- as.vector(na.omit(amazon_dataset_1$Product_Price))
amazon_dataset_2_vector <- as.vector(na.omit(amazon_dataset_2$Product_Price))
amazon_dataset_3_vector <- as.vector(na.omit(amazon_dataset_3$Product_Price))
amazon_dataset_4_vector <- as.vector(na.omit(amazon_dataset_4$Product_Price))
amazon_dataset_5_vector <- as.vector(na.omit(amazon_dataset_5$Product_Price))
amazon_dataset_6_vector <- as.vector(na.omit(amazon_dataset_6$Product_Price))
amazon_dataset_7_vector <- as.vector(na.omit(amazon_dataset_7$Product_Price))
amazon_dataset_8_vector <- as.vector(na.omit(amazon_dataset_8$Product_Price))
amazon_dataset_9_vector <- as.vector(na.omit(amazon_dataset_9$Product_Price))

colnames(amazon_dataset_1_1) <- "product_price"
colnames(amazon_dataset_2_1) <- "product_price"
colnames(amazon_dataset_3_1) <- "product_price"
colnames(amazon_dataset_4_1) <- "product_price"
colnames(amazon_dataset_5_1) <- "product_price"
colnames(amazon_dataset_6_1) <- "product_price"
colnames(amazon_dataset_7_1) <- "product_price"
colnames(amazon_dataset_8_1) <- "product_price"
colnames(amazon_dataset_9_1) <- "product_price"

#MEAN OF PRODUCT PRICE
mean1 <- mean(amazon_dataset_1$Product_Price, na.rm = TRUE)
mean2 <- mean(amazon_dataset_2$Product_Price, na.rm = TRUE)
mean3 <- mean(amazon_dataset_3$Product_Price, na.rm = TRUE)
mean4 <- mean(amazon_dataset_4$Product_Price, na.rm = TRUE)
mean5 <- mean(amazon_dataset_5$Product_Price, na.rm = TRUE)
mean6 <- mean(amazon_dataset_6$Product_Price, na.rm = TRUE)
mean7 <- mean(amazon_dataset_7$Product_Price, na.rm = TRUE)
mean8 <- mean(amazon_dataset_8$Product_Price, na.rm = TRUE)
mean9 <- mean(amazon_dataset_9$Product_Price, na.rm = TRUE)

#MEDIAN OF PRODUCT PRICE
median1 <- median(amazon_dataset_1$Product_Price, na.rm = TRUE)
median2 <- median(amazon_dataset_2$Product_Price, na.rm = TRUE)
median3 <- median(amazon_dataset_3$Product_Price, na.rm = TRUE)
median4 <- median(amazon_dataset_4$Product_Price, na.rm = TRUE)
median5 <- median(amazon_dataset_5$Product_Price, na.rm = TRUE)
median6 <- median(amazon_dataset_6$Product_Price, na.rm = TRUE)
median7 <- median(amazon_dataset_7$Product_Price, na.rm = TRUE)
median8 <- median(amazon_dataset_8$Product_Price, na.rm = TRUE)
median9 <- median(amazon_dataset_9$Product_Price, na.rm = TRUE)

#MODE OF PRODUCT PRICE
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mode1 <- getmode(amazon_dataset_1_1)
mode2 <- getmode(amazon_dataset_2_1)
mode3 <- getmode(amazon_dataset_3_1)
mode4 <- getmode(amazon_dataset_4_1)
mode5 <- getmode(amazon_dataset_5_1)
mode6 <- getmode(amazon_dataset_6_1)
mode7 <- getmode(amazon_dataset_7_1)
mode8 <- getmode(amazon_dataset_8_1)
mode9 <- getmode(amazon_dataset_9_1)

mode1 <- as.numeric(unlist(mode1)) 
mode2 <- as.numeric(unlist(mode2))
mode3 <- as.numeric(unlist(mode3))
mode4 <- as.numeric(unlist(mode4))
mode5 <- as.numeric(unlist(mode5))
mode6 <- as.numeric(unlist(mode6))
mode7 <- as.numeric(unlist(mode7))
mode8 <- as.numeric(unlist(mode8))
mode9 <- as.numeric(unlist(mode9))


##MIN AND MAX VALUES FOR PRICE
min_max_1 <- c(min(amazon_dataset_1_vector), max(amazon_dataset_1_vector))
min_max_2 <- c(min(amazon_dataset_2_vector), max(amazon_dataset_2_vector))
min_max_3 <- c(min(amazon_dataset_3_vector), max(amazon_dataset_3_vector))
min_max_4 <- c(min(amazon_dataset_4_vector), max(amazon_dataset_4_vector))
min_max_5 <- c(min(amazon_dataset_5_vector), max(amazon_dataset_5_vector))
min_max_6 <- c(min(amazon_dataset_6_vector), max(amazon_dataset_6_vector))
min_max_7 <- c(min(amazon_dataset_7_vector), max(amazon_dataset_7_vector))
min_max_8 <- c(min(amazon_dataset_8_vector), max(amazon_dataset_8_vector))
min_max_9 <- c(min(amazon_dataset_9_vector), max(amazon_dataset_9_vector))

#HISTOGRAMS FOR PRODUCT PRICE
price_hist_1 <- ggplot(amazon_dataset_1, aes(x=Product_Price)) + geom_histogram(binwidth = 50)
price_hist_2 <- ggplot(amazon_dataset_2, aes(x=Product_Price)) + geom_histogram(binwidth = 50)
price_hist_3 <- ggplot(amazon_dataset_3, aes(x=Product_Price)) + geom_histogram(binwidth = 50)
price_hist_4 <- ggplot(amazon_dataset_4, aes(x=Product_Price)) + geom_histogram(binwidth = 50)
price_hist_5 <- ggplot(amazon_dataset_5, aes(x=Product_Price)) + geom_histogram(binwidth = 10)
price_hist_6 <- ggplot(amazon_dataset_6, aes(x=Product_Price)) + geom_histogram(binwidth = 50)
price_hist_7 <- ggplot(amazon_dataset_7, aes(x=Product_Price)) + geom_histogram(binwidth = 50)
price_hist_8 <- ggplot(amazon_dataset_8, aes(x=Product_Price)) + geom_histogram(binwidth = 50)
price_hist_9 <- ggplot(amazon_dataset_9, aes(x=Product_Price)) + geom_histogram(binwidth = 50)

print(price_hist_1)
print(price_hist_2)
print(price_hist_3)
print(price_hist_4)
print(price_hist_5)
print(price_hist_6)
print(price_hist_7)
print(price_hist_8)
print(price_hist_8)

#AFTER LOOKING AT THE HISTOGRAMS, I DESIRE TO REMOVE ALL VALUES THAT ARE LESS THAN A CERTAIN VALUE AND MORE THAN A CERTAIN VALUE
#BUT I DISCOVERED ANOTHER ISSUE WITH MY DATASET

penny_lines <- subset(amazon_dataset_1, amazon_dataset_1$Product_Price <= 0.01)
view(penny_lines)

#AFTER LOOKING AT THESE VALUES, I SAW THAT THE PRODUCT DESCRIPTION DID NOT MATCH UP WITH THE PRODUCT CATEGORY.
#I WAS CONCERNED THAT THIS MAY HAVE BEEN AN ISSUE THROUGHOUT THE DATASET, SO I DECIDED TO DO MORE EXTENSIVE CLEANING

## FUZZY TEXT MATCHING USING AGREP FUNCTION AND MANUAL AUDIT

#EXPERIMENT PHASE -- I USED THE FIRST DATAFRAME TO TEST WHAT EDIT DISTANCE I WOULD NEED FROM AGREP IN ORDER TO HAVE THE HIGHEST THRESHOLD OF MATCHES WITHOUT TAKING ON THE ENTIRE DATAFRAME
#I DETERMINED THAT 0.75 WAS THE BEST EDIT DISTANCE AND THE WORKFLOW NEEDED

#DETERMINING BEST EDIT DISTANCE 
#CREATING COLUMN FOR LAST CATEGORY VALUES
amazon_dataset_1_last_cat_value <- read_excel("G:\\My Drive\\1_SPRING 2021\\2_STATISTICS AND ANALYSIS\\dirty datasets\\amazon_dataset_1_last_cat_value.xlsx")
matching_list <- agrep(amazon_dataset_1_last_cat_value$LAST_CAT_VALUE, amazon_dataset_1_last_cat_value$Product_Contents, ignore.case = TRUE, value = FALSE, max.distance = 0.75)
str(matching_list)
str(amazon_dataset_1_last_cat_value)

#ADDING A MATCH COLUMN (I CREATE A COLUMN WITH ALL NO VALUES, THEN EDITED THAT COLUMN WITH YES VALUES USING THE INDEX LIST OF MATCHES CREATED BY AGREP)
amazon_dataset_1_last_cat_value$match <- "NO"
view(amazon_dataset_1_last_cat_value)
amazon_dataset_1_last_cat_value$match[matching_list] <- "YES"

#CREATED NEW OBJECT WITH EDITED VALUES
amazon_dataset_1_match <- amazon_dataset_1_last_cat_value

#WROTE CSV WITH MATCH COLUMN
write.csv(amazon_dataset_1_match, "G:\\My Drive\\1_SPRING 2021\\2_STATISTICS AND ANALYSIS\\amazon_dataset_1_match_prodcon.csv", row.names = FALSE)

#MOVING FORWARD WITH THE REST OF THE DATAFRAMES

#WRITE CSVS FOR EACH DATAFRAME, THEN I ADD A COLUMN FOR THE LAST CATEGORY VALUE IN EXCEL
write.csv(amazon_dataset_2, "G:\\My Drive\\1_SPRING 2021\\2_STATISTICS AND ANALYSIS\\dirty datasets\\amazon_dataset_2.csv", row.names = FALSE)
write.csv(amazon_dataset_3, "G:\\My Drive\\1_SPRING 2021\\2_STATISTICS AND ANALYSIS\\dirty datasets\\amazon_dataset_3.csv", row.names = FALSE)
write.csv(amazon_dataset_4, "G:\\My Drive\\1_SPRING 2021\\2_STATISTICS AND ANALYSIS\\dirty datasets\\amazon_dataset_4.csv", row.names = FALSE)
write.csv(amazon_dataset_5, "G:\\My Drive\\1_SPRING 2021\\2_STATISTICS AND ANALYSIS\\dirty datasets\\amazon_dataset_5.csv", row.names = FALSE)
write.csv(amazon_dataset_6, "G:\\My Drive\\1_SPRING 2021\\2_STATISTICS AND ANALYSIS\\dirty datasets\\amazon_dataset_6.csv", row.names = FALSE)
write.csv(amazon_dataset_7, "G:\\My Drive\\1_SPRING 2021\\2_STATISTICS AND ANALYSIS\\dirty datasets\\amazon_dataset_7.csv", row.names = FALSE)
write.csv(amazon_dataset_8, "G:\\My Drive\\1_SPRING 2021\\2_STATISTICS AND ANALYSIS\\dirty datasets\\amazon_dataset_8.csv", row.names = FALSE)
write.csv(amazon_dataset_9, "G:\\My Drive\\1_SPRING 2021\\2_STATISTICS AND ANALYSIS\\dirty datasets\\amazon_dataset_9.csv", row.names = FALSE)

#IMPORTED DATAFRAMES WITH LAST CATEGORY VALUE COLUMN

setwd("G:\\My Drive\\1_SPRING 2021\\2_STATISTICS AND ANALYSIS\\dirty datasets")
getwd()
all_files <- list.files(path = "G:\\My Drive\\1_SPRING 2021\\2_STATISTICS AND ANALYSIS\\dirty datasets", pattern = "*.xlsx")
as.vector(all_files)

amazon_dataset_2_lcv <- as.data.frame(lapply(all_files[1], read_xlsx))
amazon_dataset_3_lcv <- as.data.frame(lapply(all_files[2], read_xlsx))
amazon_dataset_4_lcv <- as.data.frame(lapply(all_files[3], read_xlsx))
amazon_dataset_5_lcv <- as.data.frame(lapply(all_files[4], read_xlsx))
amazon_dataset_6_lcv <- as.data.frame(lapply(all_files[5], read_xlsx))
amazon_dataset_7_lcv <- as.data.frame(lapply(all_files[6], read_xlsx))
amazon_dataset_8_lcv <- as.data.frame(lapply(all_files[7], read_xlsx))
amazon_dataset_9_lcv <- as.data.frame(lapply(all_files[8], read_xlsx))


#MATCHING FUNCTION FOR EACH DATAFRAME
#I PERFORMED THE FOLLOWING MATCHES ON THE FOLLOWING CONTENTS
#Last Category Value to Product Description
#Last Category Value to Product Contents
#Product Category 1 to Bsr (product category ranking)

match_list2_1 <- agrep(amazon_dataset_2_lcv$LAST_CAT_VALUE, amazon_dataset_2_lcv$Product_Description, ignore.case = TRUE, value = FALSE, max.distance = 0.75)
match_list2_2 <- agrep(amazon_dataset_2_lcv$LAST_CAT_VALUE, amazon_dataset_2_lcv$Product_Contents, ignore.case = TRUE, value = FALSE, max.distance = 0.75)
match_list2_3 <- agrep(amazon_dataset_2_lcv$Product_Category_1, amazon_dataset_2_lcv$Bsr, ignore.case = TRUE, value = FALSE, max.distance = 0.75)

match_list3_1 <- agrep(amazon_dataset_3_lcv$LAST_CAT_VALUE, amazon_dataset_3_lcv$Product_Description, ignore.case = TRUE, value = FALSE, max.distance = 0.75)
match_list3_2 <- agrep(amazon_dataset_3_lcv$LAST_CAT_VALUE, amazon_dataset_3_lcv$Product_Contents, ignore.case = TRUE, value = FALSE, max.distance = 0.75)
match_list3_3 <- agrep(amazon_dataset_3_lcv$Product_Category_1, amazon_dataset_3_lcv$Bsr, ignore.case = TRUE, value = FALSE, max.distance = 0.75)

match_list4_1 <- agrep(amazon_dataset_4_lcv$LAST_CAT_VALUE, amazon_dataset_4_lcv$Product_Description, ignore.case = TRUE, value = FALSE, max.distance = 0.75)
match_list4_2 <- agrep(amazon_dataset_4_lcv$LAST_CAT_VALUE, amazon_dataset_4_lcv$Product_Contents, ignore.case = TRUE, value = FALSE, max.distance = 0.75)
match_list4_3 <- agrep(amazon_dataset_4_lcv$Product_Category_1, amazon_dataset_4_lcv$Bsr, ignore.case = TRUE, value = FALSE, max.distance = 0.75)

match_list5_1 <- agrep(amazon_dataset_5_lcv$LAST_CAT_VALUE, amazon_dataset_5_lcv$Product_Description, ignore.case = TRUE, value = FALSE, max.distance = 0.75)
match_list5_2 <- agrep(amazon_dataset_5_lcv$LAST_CAT_VALUE, amazon_dataset_5_lcv$Product_Contents, ignore.case = TRUE, value = FALSE, max.distance = 0.75)
match_list5_3 <- agrep(amazon_dataset_5_lcv$Product_Category_1, amazon_dataset_5_lcv$Bsr, ignore.case = TRUE, value = FALSE, max.distance = 0.75)

match_list6_1 <- agrep(amazon_dataset_6_lcv$LAST_CAT_VALUE, amazon_dataset_6_lcv$Product_Description, ignore.case = TRUE, value = FALSE, max.distance = 0.75)
match_list6_2 <- agrep(amazon_dataset_6_lcv$LAST_CAT_VALUE, amazon_dataset_6_lcv$Product_Contents, ignore.case = TRUE, value = FALSE, max.distance = 0.75)
match_list6_3 <- agrep(amazon_dataset_6_lcv$Product_Category_1, amazon_dataset_6_lcv$Bsr, ignore.case = TRUE, value = FALSE, max.distance = 0.75)

match_list7_1 <- agrep(amazon_dataset_7_lcv$LAST_CAT_VALUE, amazon_dataset_7_lcv$Product_Description, ignore.case = TRUE, value = FALSE, max.distance = 0.75)
match_list7_2 <- agrep(amazon_dataset_7_lcv$LAST_CAT_VALUE, amazon_dataset_7_lcv$Product_Contents, ignore.case = TRUE, value = FALSE, max.distance = 0.75)
match_list7_3 <- agrep(amazon_dataset_7_lcv$Product_Category_1, amazon_dataset_7_lcv$Bsr, ignore.case = TRUE, value = FALSE, max.distance = 0.75)

match_list8_1 <- agrep(amazon_dataset_8_lcv$LAST_CAT_VALUE, amazon_dataset_8_lcv$Product_Description, ignore.case = TRUE, value = FALSE, max.distance = 0.75)
match_list8_2 <- agrep(amazon_dataset_8_lcv$LAST_CAT_VALUE, amazon_dataset_8_lcv$Product_Contents, ignore.case = TRUE, value = FALSE, max.distance = 0.75)
match_list8_3 <- agrep(amazon_dataset_8_lcv$Product_Category_1, amazon_dataset_8_lcv$Bsr, ignore.case = TRUE, value = FALSE, max.distance = 0.75)

match_list9_1 <- agrep(amazon_dataset_9_lcv$LAST_CAT_VALUE, amazon_dataset_9_lcv$Product_Description, ignore.case = TRUE, value = FALSE, max.distance = 0.75)
match_list9_2 <- agrep(amazon_dataset_9_lcv$LAST_CAT_VALUE, amazon_dataset_9_lcv$Product_Contents, ignore.case = TRUE, value = FALSE, max.distance = 0.75)
match_list9_3 <- agrep(amazon_dataset_9_lcv$Product_Category_1, amazon_dataset_9_lcv$Bsr, ignore.case = TRUE, value = FALSE, max.distance = 0.75)

#APPENDING MATCH COLUMNS TO DATAFRAMES
amazon_dataset_2_lcv$match1 <- "NO"
amazon_dataset_2_lcv$match2 <- "NO"
amazon_dataset_2_lcv$match3 <- "NO"

amazon_dataset_3_lcv$match1 <- "NO"
amazon_dataset_3_lcv$match2 <- "NO"
amazon_dataset_3_lcv$match3 <- "NO"

amazon_dataset_4_lcv$match1 <- "NO"
amazon_dataset_4_lcv$match2 <- "NO"
amazon_dataset_4_lcv$match3 <- "NO"

amazon_dataset_5_lcv$match1 <- "NO"
amazon_dataset_5_lcv$match2 <- "NO"
amazon_dataset_5_lcv$match3 <- "NO"

amazon_dataset_6_lcv$match1 <- "NO"
amazon_dataset_6_lcv$match2 <- "NO"
amazon_dataset_6_lcv$match3 <- "NO"

amazon_dataset_7_lcv$match1 <- "NO"
amazon_dataset_7_lcv$match2 <- "NO"
amazon_dataset_7_lcv$match3 <- "NO"

amazon_dataset_8_lcv$match1 <- "NO"
amazon_dataset_8_lcv$match2 <- "NO"
amazon_dataset_8_lcv$match3 <- "NO"

amazon_dataset_9_lcv$match1 <- "NO"
amazon_dataset_9_lcv$match2 <- "NO"
amazon_dataset_9_lcv$match3 <- "NO"

#INPUT YES VALUES ON INDEX WHERE MATCHES WERE FOUND BY AGREP
amazon_dataset_2_lcv$match1[match_list2_1] <- "YES"
amazon_dataset_2_lcv$match2[match_list2_2] <- "YES"
amazon_dataset_2_lcv$match3[match_list2_3] <- "YES"

amazon_dataset_3_lcv$match1[match_list3_1] <- "YES"
amazon_dataset_3_lcv$match2[match_list3_2] <- "YES"
amazon_dataset_3_lcv$match3[match_list3_3] <- "YES"

amazon_dataset_4_lcv$match1[match_list4_1] <- "YES"
amazon_dataset_4_lcv$match2[match_list4_2] <- "YES"
amazon_dataset_4_lcv$match3[match_list4_3] <- "YES"

amazon_dataset_5_lcv$match1[match_list5_1] <- "YES"
amazon_dataset_5_lcv$match2[match_list5_2] <- "YES"
amazon_dataset_5_lcv$match3[match_list5_3] <- "YES"

amazon_dataset_6_lcv$match1[match_list6_1] <- "YES"
amazon_dataset_6_lcv$match2[match_list6_2] <- "YES"
amazon_dataset_6_lcv$match3[match_list6_3] <- "YES"

amazon_dataset_7_lcv$match1[match_list7_1] <- "YES"
amazon_dataset_7_lcv$match2[match_list7_2] <- "YES"
amazon_dataset_7_lcv$match3[match_list7_3] <- "YES"

amazon_dataset_8_lcv$match1[match_list8_1] <- "YES"
amazon_dataset_8_lcv$match2[match_list8_2] <- "YES"
amazon_dataset_8_lcv$match3[match_list8_3] <- "YES"

amazon_dataset_9_lcv$match1[match_list9_1] <- "YES"
amazon_dataset_9_lcv$match2[match_list9_2] <- "YES"
amazon_dataset_9_lcv$match3[match_list9_3] <- "YES"

#WRITING CSV FILES FOR MANUAL AUDIT

write.csv(amazon_dataset_2_lcv,"G:\\My Drive\\1_SPRING 2021\\2_STATISTICS AND ANALYSIS\\dirty datasets\\amazon_dataset_2_matchcolumns.csv", row.names = FALSE)
write.csv(amazon_dataset_3_lcv,"G:\\My Drive\\1_SPRING 2021\\2_STATISTICS AND ANALYSIS\\dirty datasets\\amazon_dataset_3_matchcolumns.csv", row.names = FALSE)
write.csv(amazon_dataset_4_lcv,"G:\\My Drive\\1_SPRING 2021\\2_STATISTICS AND ANALYSIS\\dirty datasets\\amazon_dataset_4_matchcolumns.csv", row.names = FALSE)
write.csv(amazon_dataset_5_lcv,"G:\\My Drive\\1_SPRING 2021\\2_STATISTICS AND ANALYSIS\\dirty datasets\\amazon_dataset_5_matchcolumns.csv", row.names = FALSE)
write.csv(amazon_dataset_6_lcv,"G:\\My Drive\\1_SPRING 2021\\2_STATISTICS AND ANALYSIS\\dirty datasets\\amazon_dataset_6_matchcolumns.csv", row.names = FALSE)
write.csv(amazon_dataset_7_lcv,"G:\\My Drive\\1_SPRING 2021\\2_STATISTICS AND ANALYSIS\\dirty datasets\\amazon_dataset_7_matchcolumns.csv", row.names = FALSE)
write.csv(amazon_dataset_8_lcv,"G:\\My Drive\\1_SPRING 2021\\2_STATISTICS AND ANALYSIS\\dirty datasets\\amazon_dataset_8_matchcolumns.csv", row.names = FALSE)
write.csv(amazon_dataset_9_lcv,"G:\\My Drive\\1_SPRING 2021\\2_STATISTICS AND ANALYSIS\\dirty datasets\\amazon_dataset_9_matchcolumns.csv", row.names = FALSE)

#AFTER MANUAL AUDITING, I IMPORT THE AUDITED DATASETS BACK IN

#LOADING BACK IN AUDITED FILES
setwd("C:\\Users\\ubore\\Desktop\\clean ds")
all_matched_cleaned_files <- as.vector(list.files(path = "C:\\Users\\ubore\\Desktop\\clean ds", pattern = "*.xlsx"))
view(all_matched_cleaned_files)

clean_files_full_path <- all_matched_cleaned_files
clean_files_full_path <- paste0("C:\\Users\\ubore\\Desktop\\clean ds\\", all_matched_cleaned_files)

#CONVERTING IMPORTED EXCEL FILES TO DATA FRAMES
amazon_dataset_1_match_clean <- as.data.frame(lapply(all_matched_cleaned_files[1], read_xlsx))
amazon_dataset_2_match_clean <- as.data.frame(lapply(all_matched_cleaned_files[2], read_xlsx))
amazon_dataset_3_match_clean <- as.data.frame(lapply(all_matched_cleaned_files[3], read_xlsx))
amazon_dataset_4_match_clean <- as.data.frame(lapply(all_matched_cleaned_files[4], read_xlsx))
amazon_dataset_5_match_clean <- as.data.frame(lapply(all_matched_cleaned_files[5], read_xlsx))
amazon_dataset_6_match_clean <- as.data.frame(lapply(all_matched_cleaned_files[6], read_xlsx))
amazon_dataset_7_match_clean <- as.data.frame(lapply(all_matched_cleaned_files[7], read_xlsx))
amazon_dataset_8_match_clean <- as.data.frame(lapply(all_matched_cleaned_files[8], read_xlsx))
amazon_dataset_9_match_clean <- as.data.frame(lapply(all_matched_cleaned_files[9], read_xlsx))

##DROP NAS FOR PRICE POINT
amazon_dataset_1_match_price <- drop_na(amazon_dataset_1_match_clean, "Product_Price")
amazon_dataset_2_match_price <- drop_na(amazon_dataset_2_match_clean, "Product_Price")
amazon_dataset_3_match_price <- drop_na(amazon_dataset_3_match_clean, "Product_Price")
amazon_dataset_4_match_price <- drop_na(amazon_dataset_4_match_clean, "Product_Price")
amazon_dataset_5_match_price <- drop_na(amazon_dataset_5_match_clean, "Product_Price")
amazon_dataset_6_match_price <- drop_na(amazon_dataset_6_match_clean, "Product_Price")
amazon_dataset_7_match_price <- drop_na(amazon_dataset_7_match_clean, "Product_Price")
amazon_dataset_8_match_price <- drop_na(amazon_dataset_8_match_clean, "Product_Price")
amazon_dataset_9_match_price <- drop_na(amazon_dataset_9_match_clean, "Product_Price")

#MEAN OF PRODUCT PRICE FOR EACH DF

mean1 <- mean(amazon_dataset_1_match_price$Product_Price)
mean2 <- mean(amazon_dataset_2_match_price$Product_Price)
mean3 <- mean(amazon_dataset_3_match_price$Product_Price)
mean4 <- mean(amazon_dataset_4_match_price$Product_Price)
mean5 <- mean(amazon_dataset_5_match_price$Product_Price)
mean6 <- mean(amazon_dataset_6_match_price$Product_Price)
mean7 <- mean(amazon_dataset_7_match_price$Product_Price)
mean8 <- mean(amazon_dataset_8_match_price$Product_Price)
mean9 <- mean(amazon_dataset_9_match_price$Product_Price)

#MEDIAN OF PRODUCT PRICE FOR EACH DF

median1 <- median(amazon_dataset_1_match_price$Product_Price)
median2 <- median(amazon_dataset_2_match_price$Product_Price)
median3 <- median(amazon_dataset_3_match_price$Product_Price)
median4 <- median(amazon_dataset_4_match_price$Product_Price)
median5 <- median(amazon_dataset_5_match_price$Product_Price)
median6 <- median(amazon_dataset_6_match_price$Product_Price)
median7 <- median(amazon_dataset_7_match_price$Product_Price)
median8 <- median(amazon_dataset_8_match_price$Product_Price)
median9 <- median(amazon_dataset_9_match_price$Product_Price)

#MODE OF PRODUCT PRICE FOR EACH DF
## MODE FUNCTION
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mode1 <- getmode(amazon_dataset_1_match_price$Product_Price)
mode2 <- getmode(amazon_dataset_2_match_price$Product_Price)
mode3 <- getmode(amazon_dataset_3_match_price$Product_Price)
mode4 <- getmode(amazon_dataset_4_match_price$Product_Price)
mode5 <- getmode(amazon_dataset_5_match_price$Product_Price)
mode6 <- getmode(amazon_dataset_6_match_price$Product_Price)
mode7 <- getmode(amazon_dataset_7_match_price$Product_Price)
mode8 <- getmode(amazon_dataset_8_match_price$Product_Price)
mode9 <- getmode(amazon_dataset_9_match_price$Product_Price)

#MIN AND MAX VALUES OF PRODUCT PRICE FOR EACH DF

min_max_1 <- c(min(amazon_dataset_1_match_price$Product_Price), max(amazon_dataset_1_match_price$Product_Price))
min_max_2 <- c(min(amazon_dataset_2_match_price$Product_Price), max(amazon_dataset_2_match_price$Product_Price))
min_max_3 <- c(min(amazon_dataset_3_match_price$Product_Price), max(amazon_dataset_3_match_price$Product_Price))
min_max_4 <- c(min(amazon_dataset_4_match_price$Product_Price), max(amazon_dataset_4_match_price$Product_Price))
min_max_5 <- c(min(amazon_dataset_5_match_price$Product_Price), max(amazon_dataset_5_match_price$Product_Price))
min_max_6 <- c(min(amazon_dataset_6_match_price$Product_Price), max(amazon_dataset_6_match_price$Product_Price))
min_max_7 <- c(min(amazon_dataset_7_match_price$Product_Price), max(amazon_dataset_7_match_price$Product_Price))
min_max_8 <- c(min(amazon_dataset_8_match_price$Product_Price), max(amazon_dataset_8_match_price$Product_Price))
min_max_9 <- c(min(amazon_dataset_9_match_price$Product_Price), max(amazon_dataset_9_match_price$Product_Price))

print(min_max_1)
print(min_max_2)
print(min_max_3)
print(min_max_4)
print(min_max_5)
print(min_max_6)
print(min_max_7)
print(min_max_8)
print(min_max_9)

#HISTOGRAMS FOR PRODUCT PRICE FOR EACH DF

price_hist_1 <- ggplot(amazon_dataset_1_match_price, aes(x=Product_Price)) + geom_histogram(binwidth = 10)
price_hist_2 <- ggplot(amazon_dataset_2_match_price, aes(x=Product_Price)) + geom_histogram(binwidth = 10)
price_hist_3 <- ggplot(amazon_dataset_3_match_price, aes(x=Product_Price)) + geom_histogram(binwidth = 10)
price_hist_4 <- ggplot(amazon_dataset_4_match_price, aes(x=Product_Price)) + geom_histogram(binwidth = 10)
price_hist_5 <- ggplot(amazon_dataset_5_match_price, aes(x=Product_Price)) + geom_histogram(binwidth = 10)
price_hist_6 <- ggplot(amazon_dataset_6_match_price, aes(x=Product_Price)) + geom_histogram(binwidth = 10)
price_hist_7 <- ggplot(amazon_dataset_7_match_price, aes(x=Product_Price)) + geom_histogram(binwidth = 10)
price_hist_8 <- ggplot(amazon_dataset_8_match_price, aes(x=Product_Price)) + geom_histogram(binwidth = 10)
price_hist_9 <- ggplot(amazon_dataset_9_match_price, aes(x=Product_Price)) + geom_histogram(binwidth = 10)

##ASSEMBLING VECTOR WITH BASE VALUES, INCLUDING NAMES
base_values_df_1 <- as.vector(c(desired_cats[1,1], round(mean1, digits = 2), round(median1,digits = 2), round(mode1,digits = 2), round(min_max_1,digits = 2), count1))
base_values_df_2 <- as.vector(c(desired_cats[2,1], round(mean2, digits = 2), round(median2,digits = 2), round(mode2,digits = 2), round(min_max_2,digits = 2), count2))
base_values_df_3 <- as.vector(c(desired_cats[3,1], round(mean3, digits = 2), round(median3,digits = 2), round(mode3,digits = 2), round(min_max_3,digits = 2), count3))
base_values_df_4 <- as.vector(c(desired_cats[4,1], round(mean4, digits = 2), round(median4,digits = 2), round(mode4,digits = 2), round(min_max_4,digits = 2), count4))
base_values_df_5 <- as.vector(c(desired_cats[5,1], round(mean5, digits = 2), round(median5,digits = 2), round(mode5,digits = 2), round(min_max_5,digits = 2), count5))
base_values_df_6 <- as.vector(c(desired_cats[6,1], round(mean6, digits = 2), round(median6,digits = 2), round(mode6,digits = 2), round(min_max_6,digits = 2), count6))
base_values_df_7 <- as.vector(c(desired_cats[7,1], round(mean7, digits = 2), round(median7,digits = 2), round(mode7,digits = 2), round(min_max_7,digits = 2), count7))
base_values_df_8 <- as.vector(c(desired_cats[8,1], round(mean8, digits = 2), round(median8,digits = 2), round(mode8,digits = 2), round(min_max_8,digits = 2), count8))
base_values_df_9 <- as.vector(c(desired_cats[9,1], round(mean9, digits = 2), round(median9,digits = 2), round(mode9,digits = 2), round(min_max_9,digits = 2), count9))

names(base_values_df_1) <- c("Product Category","Mean","Median","Mode","Min","Max","Product Count")
names(base_values_df_2) <- c("Product Category","Mean","Median","Mode","Min","Max","Product Count")
names(base_values_df_3) <- c("Product Category","Mean","Median","Mode","Min","Max","Product Count")
names(base_values_df_4) <- c("Product Category","Mean","Median","Mode","Min","Max","Product Count")
names(base_values_df_5) <- c("Product Category","Mean","Median","Mode","Min","Max","Product Count")
names(base_values_df_6) <- c("Product Category","Mean","Median","Mode","Min","Max","Product Count")
names(base_values_df_7) <- c("Product Category","Mean","Median","Mode","Min","Max","Product Count")
names(base_values_df_8) <- c("Product Category","Mean","Median","Mode","Min","Max","Product Count")
names(base_values_df_9) <- c("Product Category","Mean","Median","Mode","Min","Max","Product Count")

print(price_hist_1)
print(base_values_df_1)
print(price_hist_2)
print(base_values_df_2)
print(price_hist_3)
print(base_values_df_3)
print(price_hist_4)
print(base_values_df_4) 
print(price_hist_5)
print(base_values_df_5)
print(price_hist_6)
print(base_values_df_6)
print(price_hist_7)
print(base_values_df_7)
print(price_hist_8)
print(base_values_df_8)
print(base_values_df_9)
print(price_hist_9)

#AFTER OBSERVING THE HISTOGRAMS AND BASE VALUES, I DECIDED TO REMOVE ALL PRODUCTS BELOW 5 DOLLARS

##REMOVING UNNECESSARY OUTLIERS THAT DONT REPRESENT THE CATEGORY

#SUBSET -- REMOVAL OF MIN VALUES BASED ON BASE VALUES/HISTOGRAMS
removal_threshold_min<- 4.99

amazon_dataset_1_removed_mins <- subset(amazon_dataset_1_match_price, amazon_dataset_1_match_price$Product_Price > removal_threshold_min)
amazon_dataset_2_removed_mins <- subset(amazon_dataset_2_match_price, amazon_dataset_2_match_price$Product_Price > removal_threshold_min)
amazon_dataset_3_removed_mins <- subset(amazon_dataset_3_match_price, amazon_dataset_3_match_price$Product_Price > removal_threshold_min)
amazon_dataset_4_removed_mins <- subset(amazon_dataset_4_match_price, amazon_dataset_4_match_price$Product_Price > removal_threshold_min)
amazon_dataset_5_removed_mins <- subset(amazon_dataset_5_match_price, amazon_dataset_5_match_price$Product_Price > removal_threshold_min)
amazon_dataset_6_removed_mins <- subset(amazon_dataset_6_match_price, amazon_dataset_6_match_price$Product_Price > removal_threshold_min)
amazon_dataset_7_removed_mins <- subset(amazon_dataset_7_match_price, amazon_dataset_7_match_price$Product_Price > removal_threshold_min)
amazon_dataset_8_removed_mins <- subset(amazon_dataset_8_match_price, amazon_dataset_8_match_price$Product_Price > removal_threshold_min)
amazon_dataset_9_removed_mins <- subset(amazon_dataset_9_match_price, amazon_dataset_9_match_price$Product_Price > removal_threshold_min)

#SUBSET -- REMOVAL OF LARGE OUTLIER VALUES BASED ON BASE VALUES/HISTOGRAMS
#REMOVED CATEGORY 8 -- INDUSTRIAL AND SCIENTIFIC -- IT IS THE MOST NON-NORMAL DF AND A VERTICAL I DON'T HAVE INTEREST IN EXPLORING FURTHER

removal_threshold_max_1 <- 800
removal_threshold_max_2 <- 1000
removal_threshold_max_3 <- 280
removal_threshold_max_4 <- 625
removal_threshold_max_5 <- 50
removal_threshold_max_6 <- 275
removal_threshold_max_7 <- 125
removal_threshold_max_9 <- 100

amazon_dataset_1_removed_outliers <- subset(amazon_dataset_1_removed_mins, amazon_dataset_1_removed_mins$Product_Price <= removal_threshold_max_1)
amazon_dataset_2_removed_outliers <- subset(amazon_dataset_2_removed_mins, amazon_dataset_2_removed_mins$Product_Price <= removal_threshold_max_2)
amazon_dataset_3_removed_outliers <- subset(amazon_dataset_3_removed_mins, amazon_dataset_3_removed_mins$Product_Price <= removal_threshold_max_3)
amazon_dataset_4_removed_outliers <- subset(amazon_dataset_4_removed_mins, amazon_dataset_4_removed_mins$Product_Price <= removal_threshold_max_4)
amazon_dataset_5_removed_outliers <- subset(amazon_dataset_5_removed_mins, amazon_dataset_5_removed_mins$Product_Price <= removal_threshold_max_5)
amazon_dataset_6_removed_outliers <- subset(amazon_dataset_6_removed_mins, amazon_dataset_6_removed_mins$Product_Price <= removal_threshold_max_6)
amazon_dataset_7_removed_outliers <- subset(amazon_dataset_7_removed_mins, amazon_dataset_7_removed_mins$Product_Price <= removal_threshold_max_7)
amazon_dataset_9_removed_outliers <- subset(amazon_dataset_9_removed_mins, amazon_dataset_9_removed_mins$Product_Price <= removal_threshold_max_9)

#RECALCULATING BASE VALUES AND CREATING NEW HISTOGRAMS AND VECTORS FOR BASE VALUES

#CALCULATING MEAN, MEDIAN, MODE, PRODUCT COUNT
mean1 <- mean(amazon_dataset_1_removed_outliers$Product_Price)
mean2 <- mean(amazon_dataset_2_removed_outliers$Product_Price)
mean3 <- mean(amazon_dataset_3_removed_outliers$Product_Price)
mean4 <- mean(amazon_dataset_4_removed_outliers$Product_Price)
mean5 <- mean(amazon_dataset_5_removed_outliers$Product_Price)
mean6 <- mean(amazon_dataset_6_removed_outliers$Product_Price)
mean7 <- mean(amazon_dataset_7_removed_outliers$Product_Price)
mean9 <- mean(amazon_dataset_9_removed_outliers$Product_Price)

median1 <- median(amazon_dataset_1_removed_outliers$Product_Price)
median2 <- median(amazon_dataset_2_removed_outliers$Product_Price)
median3 <- median(amazon_dataset_3_removed_outliers$Product_Price)
median4 <- median(amazon_dataset_4_removed_outliers$Product_Price)
median5 <- median(amazon_dataset_5_removed_outliers$Product_Price)
median6 <- median(amazon_dataset_6_removed_outliers$Product_Price)
median7 <- median(amazon_dataset_7_removed_outliers$Product_Price)
median9 <- median(amazon_dataset_9_removed_outliers$Product_Price)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]

mode1 <- getmode(amazon_dataset_1_removed_outliers$Product_Price)
mode2 <- getmode(amazon_dataset_2_removed_outliers$Product_Price)
mode3 <- getmode(amazon_dataset_3_removed_outliers$Product_Price)
mode4 <- getmode(amazon_dataset_4_removed_outliers$Product_Price)
mode5 <- getmode(amazon_dataset_5_removed_outliers$Product_Price)
mode6 <- getmode(amazon_dataset_6_removed_outliers$Product_Price)
mode7 <- getmode(amazon_dataset_7_removed_outliers$Product_Price)
mode9 <- getmode(amazon_dataset_9_removed_outliers$Product_Price)

count1 <- nrow(amazon_dataset_1_removed_outliers)
count2 <- nrow(amazon_dataset_2_removed_outliers)
count3 <- nrow(amazon_dataset_3_removed_outliers)
count4 <- nrow(amazon_dataset_4_removed_outliers)
count5 <- nrow(amazon_dataset_5_removed_outliers)
count6 <- nrow(amazon_dataset_6_removed_outliers)
count7 <- nrow(amazon_dataset_7_removed_outliers)
count9 <- nrow(amazon_dataset_9_removed_outliers)

#MIN AND MAX VALUES OF PRODUCT PRICE FOR EACH DF
min_max_1 <- c(min(amazon_dataset_1_removed_outliers$Product_Price), max(amazon_dataset_1_removed_outliers$Product_Price))
min_max_2 <- c(min(amazon_dataset_2_removed_outliers$Product_Price), max(amazon_dataset_2_removed_outliers$Product_Price))
min_max_3 <- c(min(amazon_dataset_3_removed_outliers$Product_Price), max(amazon_dataset_3_removed_outliers$Product_Price))
min_max_4 <- c(min(amazon_dataset_4_removed_outliers$Product_Price), max(amazon_dataset_4_removed_outliers$Product_Price))
min_max_5 <- c(min(amazon_dataset_5_removed_outliers$Product_Price), max(amazon_dataset_5_removed_outliers$Product_Price))
min_max_6 <- c(min(amazon_dataset_6_removed_outliers$Product_Price), max(amazon_dataset_6_removed_outliers$Product_Price))
min_max_7 <- c(min(amazon_dataset_7_removed_outliers$Product_Price), max(amazon_dataset_7_removed_outliers$Product_Price))
min_max_9 <- c(min(amazon_dataset_9_removed_outliers$Product_Price), max(amazon_dataset_9_removed_outliers$Product_Price))

#BASE VALUES VECTOR
base_values_df_1 <- as.vector(c(amazon_dataset_1_removed_outliers[1,7], round(mean1, digits = 2), round(median1,digits = 2), round(mode1,digits = 2), round(min_max_1,digits = 2), count1))
base_values_df_2 <- as.vector(c(amazon_dataset_2_removed_outliers[1,7], round(mean2, digits = 2), round(median2,digits = 2), round(mode2,digits = 2), round(min_max_2,digits = 2), count2))
base_values_df_3 <- as.vector(c(amazon_dataset_3_removed_outliers[1,7], round(mean3, digits = 2), round(median3,digits = 2), round(mode3,digits = 2), round(min_max_3,digits = 2), count3))
base_values_df_4 <- as.vector(c(amazon_dataset_4_removed_outliers[1,7], round(mean4, digits = 2), round(median4,digits = 2), round(mode4,digits = 2), round(min_max_4,digits = 2), count4))
base_values_df_5 <- as.vector(c(amazon_dataset_5_removed_outliers[1,7], round(mean5, digits = 2), round(median5,digits = 2), round(mode5,digits = 2), round(min_max_5,digits = 2), count5))
base_values_df_6 <- as.vector(c(amazon_dataset_6_removed_outliers[1,7], round(mean6, digits = 2), round(median6,digits = 2), round(mode6,digits = 2), round(min_max_6,digits = 2), count6))
base_values_df_7 <- as.vector(c(amazon_dataset_7_removed_outliers[1,7], round(mean7, digits = 2), round(median7,digits = 2), round(mode7,digits = 2), round(min_max_7,digits = 2), count7))
base_values_df_9 <- as.vector(c(amazon_dataset_9_removed_outliers[1,7], round(mean9, digits = 2), round(median9,digits = 2), round(mode9,digits = 2), round(min_max_9,digits = 2), count9))

names(base_values_df_1) <- c("Product Category","Mean","Median","Mode","Min","Max","Product Count")
names(base_values_df_2) <- c("Product Category","Mean","Median","Mode","Min","Max","Product Count")
names(base_values_df_3) <- c("Product Category","Mean","Median","Mode","Min","Max","Product Count")
names(base_values_df_4) <- c("Product Category","Mean","Median","Mode","Min","Max","Product Count")
names(base_values_df_5) <- c("Product Category","Mean","Median","Mode","Min","Max","Product Count")
names(base_values_df_6) <- c("Product Category","Mean","Median","Mode","Min","Max","Product Count")
names(base_values_df_7) <- c("Product Category","Mean","Median","Mode","Min","Max","Product Count")
names(base_values_df_9) <- c("Product Category","Mean","Median","Mode","Min","Max","Product Count")

#CREATED NEW HISTOGRAMES FOR NEW DATAFRAMES // INCLUDING VERTICAL LINES FOR MEAN, MEDIAN, AND MODE

```{r}
price_hist_1 <- ggplot(amazon_dataset_1_removed_outliers, aes(x=Product_Price)) + 
  geom_histogram(binwidth = 5, color = "white") +
  geom_vline(aes(xintercept = mean1, color = "mean")) + 
  geom_vline(aes(xintercept = median1, color = "median")) +
  geom_vline(aes(xintercept = mode1, color = "mode"))
```
```{r}                                 
price_hist_2 <- ggplot(amazon_dataset_2_removed_outliers, aes(x=Product_Price)) + geom_histogram(binwidth = 5,color = "white") + 
  geom_vline(aes(xintercept = mean2, color = "mean")) + 
  geom_vline(aes(xintercept = median2, color ="median")) +
  geom_vline(aes(xintercept = mode2, color = "mode"))

```
```{r}                     
price_hist_3 <- ggplot(amazon_dataset_3_removed_outliers, aes(x=Product_Price)) + geom_histogram(binwidth = 5,color = "white") + 
  geom_vline(aes(xintercept = mean3,color="mean")) + 
  geom_vline(aes(xintercept = median3,color="median")) +
  geom_vline(aes(xintercept = mode3, color = "mode"))
```
```{r}                      
price_hist_4 <- ggplot(amazon_dataset_4_removed_outliers, aes(x=Product_Price)) + geom_histogram(binwidth = 5,color = "white") + 
  geom_vline(aes(xintercept = mean4,color="mean")) + 
  geom_vline(aes(xintercept = median4,color="median")) +
  geom_vline(aes(xintercept = mode4, color = "mode"))
```
```{r}                      
price_hist_5 <- ggplot(amazon_dataset_5_removed_outliers, aes(x=Product_Price)) + geom_histogram(binwidth = 10,color = "white") + 
  geom_vline(aes(xintercept = mean5,color="mean")) + 
  geom_vline(aes(xintercept = median5,color="median")) +
  geom_vline(aes(xintercept = mode5,color = "mode"))
```
```{r}                     
price_hist_6 <- ggplot(amazon_dataset_6_removed_outliers, aes(x=Product_Price)) + geom_histogram(binwidth = 5,color = "white") + 
  geom_vline(aes(xintercept = mean6,color="mean")) + 
  geom_vline(aes(xintercept = median6,color="median")) +
  geom_vline(aes(xintercept = mode6, color = "mode"))
```
```{r}                      
price_hist_7 <- ggplot(amazon_dataset_7_removed_outliers, aes(x=Product_Price)) + geom_histogram(binwidth = 5,color = "white") + 
  geom_vline(aes(xintercept = mean7,color="mean")) + 
  geom_vline(aes(xintercept = median7,color="median")) +
  geom_vline(aes(xintercept = mode7, color = "mode"))
```
```{r}                      
price_hist_9 <- ggplot(amazon_dataset_9_removed_outliers, aes(x=Product_Price)) + geom_histogram(binwidth = 2,color = "white") + 
  geom_vline(aes(xintercept = mean9,color="mean")) + 
  geom_vline(aes(xintercept = median9,color="median"))+
  geom_vline(aes(xintercept = mode9, color = "mode"))
```

**Histograms And Base Values**

print(base_values_df_1)
print(price_hist_1)
print(base_values_df_2)
print(price_hist_2)
print(base_values_df_3)
print(price_hist_3)
print(base_values_df_4)
print(price_hist_4)
print(base_values_df_5)
print(price_hist_5)
print(base_values_df_6)
print(price_hist_6)
print(base_values_df_7)
print(price_hist_7)
print(base_values_df_9)
print(price_hist_9)

base_values_df <- cbind.data.frame(c(base_values_df_1_1, base_values_df_2_1, base_values_df_3_1, base_values_df_4_1, base_values_df_5_1, base_values_df_6_1, base_values_df_7_1, base_values_df_9_1 ))

view(base_values_df)

base_values_df_1_1 <- as.list(base_values_df_1)
base_values_df_2_1 <- as.list(base_values_df_2)
base_values_df_3_1 <-as.list(base_values_df_3)
base_values_df_4_1  <-as.list(base_values_df_4)
base_values_df_5_1  <-as.list(base_values_df_5)
base_values_df_6_1  <-as.list(base_values_df_6)
base_values_df_7_1  <-as.list(base_values_df_7)
base_values_df_9_1  <-as.list(base_values_df_9)

base_values_df <- cbind.data.frame(c(base_values_df_1, base_values_df_2, base_values_df_3, base_values_df_4, base_values_df_5, base_values_df_6, base_values_df_7, base_values_df_9 ))


##DATAFRAMES ARE RIGHT SKEWED // NORMALISING SKEWNESS USING LOG10

#FINDING SKEWNESS COEFFICENT
sc_1 <- skewness(amazon_dataset_1_removed_outliers$Product_Price)
sc_2 <- skewness(amazon_dataset_2_removed_outliers$Product_Price)
sc_3 <- skewness(amazon_dataset_3_removed_outliers$Product_Price)
sc_4 <- skewness(amazon_dataset_4_removed_outliers$Product_Price)
sc_5 <- skewness(amazon_dataset_5_removed_outliers$Product_Price)
sc_6 <- skewness(amazon_dataset_6_removed_outliers$Product_Price)
sc_7 <- skewness(amazon_dataset_7_removed_outliers$Product_Price)
sc_9 <- skewness(amazon_dataset_9_removed_outliers$Product_Price)
#SKEWNESS COES IN VECTOR
skewness_coes <- as.vector(c(sc_1,sc_2,sc_3,sc_4,sc_5,sc_6,sc_7,sc_9))
names(skewness_coes) <- c("df1","df2","df3","df4","df5","df6","df7","df9")

#SORTING COES IN ASCENDING ORDER
ordered_scs <- sort(skewness_coes)
print(ordered_scs)

print(skewness_coes)

#PERFORMING LOG10 NORMALIZATION ON PRODUCT PRICE FOR EACH DATAFRAME
ads_1_log <- log10(amazon_dataset_1_removed_outliers$Product_Price)
ads_2_log <- log10(amazon_dataset_2_removed_outliers$Product_Price)
ads_3_log <- log10(amazon_dataset_3_removed_outliers$Product_Price)
ads_4_log <- log10(amazon_dataset_4_removed_outliers$Product_Price)
ads_5_log <- log10(amazon_dataset_5_removed_outliers$Product_Price)
ads_6_log <- log10(amazon_dataset_6_removed_outliers$Product_Price)
ads_7_log <- log10(amazon_dataset_7_removed_outliers$Product_Price)
ads_9_log <- log10(amazon_dataset_9_removed_outliers$Product_Price)

ads_1_log <- as.data.frame(ads_1_log)
ads_2_log <- as.data.frame(ads_2_log)
ads_3_log <- as.data.frame(ads_3_log)
ads_4_log <- as.data.frame(ads_4_log)
ads_5_log <- as.data.frame(ads_5_log)
ads_6_log <- as.data.frame(ads_6_log)
ads_7_log <- as.data.frame(ads_7_log)
ads_9_log <- as.data.frame(ads_9_log)

names(ads_1_log) <- "Product_Price"
names(ads_2_log) <- "Product_Price"
names(ads_3_log) <- "Product_Price"
names(ads_4_log) <- "Product_Price"
names(ads_5_log) <- "Product_Price"
names(ads_6_log) <- "Product_Price"
names(ads_7_log) <- "Product_Price"
names(ads_9_log) <- "Product_Price"

##BASE VALUES AFTER NORMALIZATION FOR EACH DATAFRAME, INCLUDING VARIANCE, STANDARD DEVIATION AND STANDARD ERROR

##DF1
mean1_log <- mean(ads_1_log$Product_Price)
median1_log <- median(ads_1_log$Product_Price)
mode1_log <- getmode(ads_1_log$Product_Price)
count1_log <- as.numeric(nrow(ads_1_log))

ads_1_log_density <- ggdensity(ads_1_log, x = "Product_Price", fill = "lightgray", title = "Product Price, Home & Kitchen") +
                      stat_overlay_normal_density(color = "red", linetype = "dashed")
print(ads_1_log_density)

var_ads_1 <- var(ads_1_log$Product_Price)
sd_ads_1 <- sd(ads_1_log$Product_Price)
sqrt_count_ads1 <- sqrt(count1_log)
se_ads_1 <- sd_ads_1/sqrt_count_ads1


##DF2
mean2_log <- mean(ads_2_log$Product_Price)
median2_log <- median(ads_2_log$Product_Price)
mode2_log <- getmode(ads_2_log$Product_Price)
count2_log <- as.numeric(nrow(ads_2_log))

ads_2_log_density <- ggdensity(ads_2_log, x = "Product_Price", fill = "lightgray", title = "Product Price, Health & Household") +
                     stat_overlay_normal_density(color = "red", linetype = "dashed")

print(ads_2_log_density)

var_ads_2 <- var(ads_2_log$Product_Price)
sd_ads_2 <- sd(ads_2_log$Product_Price)
sqrt_count_ads2 <- sqrt(count2_log)
se_ads_2 <- sd_ads_2/sqrt_count_ads2

##DF3
mean3_log <- mean(ads_3_log$Product_Price)
median3_log <- median(ads_3_log$Product_Price)
mode3_log <- getmode(ads_3_log$Product_Price)
count3_log <- as.numeric(nrow(ads_3_log))

ads_3_log_density <- ggdensity(ads_3_log, x = "Product_Price", fill = "lightgray", title = "Product Price, Beauty & Personal Care") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

print(ads_3_log_density)

var_ads_3 <- var(ads_3_log$Product_Price)
sd_ads_3 <- sd(ads_3_log$Product_Price)
sqrt_count_ads3 <- sqrt(count3_log)
se_ads_3 <- sd_ads_3/sqrt_count_ads3

##DF4

mean4_log <- mean(ads_4_log$Product_Price)
median4_log <- median(ads_4_log$Product_Price)
mode4_log <- getmode(ads_4_log$Product_Price)
count4_log <- as.numeric(nrow(ads_4_log))

ads_4_log_density <- ggdensity(ads_4_log, x = "Product_Price", fill = "lightgray", title = "Product Price, Tools & Home Improvement") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

print(ads_4_log_density)

var_ads_4 <- var(ads_4_log$Product_Price)
sd_ads_4 <- sd(ads_4_log$Product_Price)
sqrt_count_ads4 <- sqrt(count4_log)
se_ads_4 <- sd_ads_4/sqrt_count_ads4

##DF5

mean5_log <- mean(ads_5_log$Product_Price)
median5_log <- median(ads_5_log$Product_Price)
mode5_log <- getmode(ads_5_log$Product_Price)
count5_log <- as.numeric(nrow(ads_5_log))

ads_5_log_density <- ggdensity(ads_5_log, x = "Product_Price", fill = "lightgray", title = "Product Price, Toys & Games") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

print(ads_5_log_density)

var_ads_5 <- var(ads_1_log$Product_Price)
sd_ads_5 <- sd(ads_1_log$Product_Price)
sqrt_count_ads5 <- sqrt(count1_log)
se_ads_5 <- sd_ads_5/sqrt_count_ads5

##DF6

mean6_log <- mean(ads_6_log$Product_Price)
median6_log <- median(ads_6_log$Product_Price)
mode6_log <- getmode(ads_6_log$Product_Price)
count6_log <- as.numeric(nrow(ads_6_log))

ads_6_log_density <- ggdensity(ads_6_log, x = "Product_Price", fill = "lightgray", title = "Product Price, Baby Products") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

print(ads_6_log_density)

var_ads_6 <- var(ads_6_log$Product_Price)
sd_ads_6 <- sd(ads_6_log$Product_Price)
sqrt_count_ads6 <- sqrt(count6_log)
se_ads_6 <- sd_ads_6/sqrt_count_ads6

##DF7

mean7_log <- mean(ads_7_log$Product_Price)
median7_log <- median(ads_7_log$Product_Price)
mode7_log <- getmode(ads_7_log$Product_Price)
count7_log <- as.numeric(nrow(ads_7_log))

ads_7_log_density <- ggdensity(ads_7_log, x = "Product_Price", fill = "lightgray", title = "Product Price, Office Products") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

print(ads_7_log_density)

var_ads_7 <- var(ads_7_log$Product_Price)
sd_ads_7 <- sd(ads_7_log$Product_Price)
sqrt_count_ads7 <- sqrt(count7_log)
se_ads_7 <- sd_ads_7/sqrt_count_ads7

#DF9

mean9_log <- mean(ads_9_log$Product_Price)
median9_log <- median(ads_9_log$Product_Price)
mode9_log <- getmode(ads_9_log$Product_Price)
count9_log <- as.numeric(nrow(ads_9_log))

ads_9_log_density <- ggdensity(ads_9_log, x = "Product_Price", fill = "lightgray", title = "Product Price, Clothing, Shoes & Jewelry") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

print(ads_9_log_density)

var_ads_9 <- var(ads_9_log$Product_Price)
sd_ads_9 <- sd(ads_9_log$Product_Price)
sqrt_count_ads9 <- sqrt(count9_log)
se_ads_9 <- sd_ads_9/sqrt_count_ads9

##FINAL EDA VALUES AS A VECTOR

EDA_VALUES_ADS_1 <- c(amazon_dataset_1_removed_outliers[1,7], round(mean1_log, digits = 2), round(median1_log, digits = 2), round(mode1_log, digits = 2), count1_log, round(var_ads_1, digits = 2), round(sd_ads_1, digits = 2), se_ads_1)
EDA_VALUES_ADS_2 <- c(amazon_dataset_2_removed_outliers[1,7], round(mean1_log, digits = 2), round(median2_log, digits = 2), round(mode2_log, digits = 2), count2_log, round(var_ads_2, digits = 2), round(sd_ads_2, digits = 2), se_ads_2)
EDA_VALUES_ADS_3 <- c(amazon_dataset_3_removed_outliers[1,7], round(mean1_log, digits = 2), round(median3_log, digits = 2), round(mode3_log, digits = 2), count3_log, round(var_ads_3, digits = 2), round(sd_ads_3, digits = 2), se_ads_3)
EDA_VALUES_ADS_4 <- c(amazon_dataset_4_removed_outliers[1,7], round(mean1_log, digits = 2), round(median4_log, digits = 2), round(mode4_log, digits = 2), count4_log, round(var_ads_4, digits = 2), round(sd_ads_4, digits = 2), se_ads_4)
EDA_VALUES_ADS_5 <- c(amazon_dataset_5_removed_outliers[1,7], round(mean1_log, digits = 2), round(median5_log, digits = 2), round(mode5_log, digits = 2), count5_log, round(var_ads_5, digits = 2), round(sd_ads_5, digits = 2), se_ads_5)
EDA_VALUES_ADS_6 <- c(amazon_dataset_6_removed_outliers[1,7], round(mean1_log, digits = 2), round(median6_log, digits = 2), round(mode6_log, digits = 2), count6_log, round(var_ads_6, digits = 2), round(sd_ads_6, digits = 2), se_ads_6)
EDA_VALUES_ADS_7 <- c(amazon_dataset_7_removed_outliers[1,7], round(mean1_log, digits = 2), round(median7_log, digits = 2), round(mode7_log, digits = 2), count7_log, round(var_ads_7, digits = 2), round(sd_ads_7, digits = 2), se_ads_7)
EDA_VALUES_ADS_9 <- c(amazon_dataset_9_removed_outliers[1,7], round(mean1_log, digits = 2), round(median9_log, digits = 2), round(mode9_log, digits = 2), count9_log, round(var_ads_9, digits = 2), round(sd_ads_9, digits = 2), se_ads_9)


names(EDA_VALUES_ADS_1) <- c("Product Category", "Mean", "Median", "Mode", "Product Count", "Variance", "Standard Deviation", "Standard Error")
names(EDA_VALUES_ADS_2) <- c("Product Category", "Mean", "Median", "Mode", "Product Count", "Variance", "Standard Deviation", "Standard Error")
names(EDA_VALUES_ADS_3) <- c("Product Category", "Mean", "Median", "Mode", "Product Count", "Variance", "Standard Deviation", "Standard Error")
names(EDA_VALUES_ADS_4) <- c("Product Category", "Mean", "Median", "Mode", "Product Count", "Variance", "Standard Deviation", "Standard Error")
names(EDA_VALUES_ADS_5) <- c("Product Category", "Mean", "Median", "Mode", "Product Count", "Variance", "Standard Deviation", "Standard Error")
names(EDA_VALUES_ADS_6) <- c("Product Category", "Mean", "Median", "Mode", "Product Count", "Variance", "Standard Deviation", "Standard Error")
names(EDA_VALUES_ADS_7) <- c("Product Category", "Mean", "Median", "Mode", "Product Count", "Variance", "Standard Deviation", "Standard Error")
names(EDA_VALUES_ADS_9) <- c("Product Category", "Mean", "Median", "Mode", "Product Count", "Variance", "Standard Deviation", "Standard Error")

print(as.data.frame(EDA_VALUES_ADS_1))
print(as.data.frame(EDA_VALUES_ADS_2)) 
print(as.data.frame(EDA_VALUES_ADS_3))
print(as.data.frame(EDA_VALUES_ADS_4))
print(as.data.frame(EDA_VALUES_ADS_5))
print(as.data.frame(EDA_VALUES_ADS_6))
print(as.data.frame(EDA_VALUES_ADS_7))
print(as.data.frame(EDA_VALUES_ADS_9))

##FINAL OBSERVATIONS FOR VARIABLES TO TEST/ MORE DESCRIPTIVE STATISTICS

## PART 6 - PERFORMING CONFIDENCE INTERVAL TESTING

## DETERMINING ALPHA - 95% CI
alpha <- 0.05

#DEGREES OF FREEDOM
degrees_freedom_1 <- count1_log - 1
degrees_freedom_2 <- count2_log - 1
degrees_freedom_3 <- count3_log - 1
degrees_freedom_4 <- count4_log - 1
degrees_freedom_5 <- count5_log - 1
degrees_freedom_6 <- count6_log - 1
degrees_freedom_7 <- count7_log - 1
degrees_freedom_9 <- count9_log - 1

#T-SCORE
t_score_1 <- qt(p=alpha/2, df=degrees_freedom_1, lower.tail = FALSE)
t_score_2 <- qt(p=alpha/2, df=degrees_freedom_2, lower.tail = FALSE)
t_score_3 <- qt(p=alpha/2, df=degrees_freedom_3, lower.tail = FALSE)
t_score_4 <- qt(p=alpha/2, df=degrees_freedom_4, lower.tail = FALSE)
t_score_5 <- qt(p=alpha/2, df=degrees_freedom_5, lower.tail = FALSE)
t_score_6 <- qt(p=alpha/2, df=degrees_freedom_6, lower.tail = FALSE)
t_score_7 <- qt(p=alpha/2, df=degrees_freedom_7, lower.tail = FALSE)
t_score_9 <- qt(p=alpha/2, df=degrees_freedom_9, lower.tail = FALSE)

#MARGIN OF ERROR                            
margin_error_1 <- t_score_1 * se_ads_1
margin_error_2 <- t_score_2 * se_ads_2
margin_error_3 <- t_score_3 * se_ads_3
margin_error_4 <- t_score_4 * se_ads_4
margin_error_5 <- t_score_5 * se_ads_5
margin_error_6 <- t_score_6 * se_ads_6
margin_error_7 <- t_score_7 * se_ads_7
margin_error_9 <- t_score_9 * se_ads_9

#LOWER/UPPER BOUNDS
lower_bound_1 <- mean1_log - margin_error_1
upper_bound_1 <- mean1_log + margin_error_1

lower_bound_2 <- mean2_log - margin_error_2
upper_bound_2 <- mean2_log + margin_error_2

lower_bound_3 <- mean3_log - margin_error_3
upper_bound_3 <- mean3_log + margin_error_3

lower_bound_4 <- mean4_log - margin_error_4
upper_bound_4 <- mean4_log + margin_error_4

lower_bound_5 <- mean5_log - margin_error_5
upper_bound_5 <- mean5_log + margin_error_5

lower_bound_6 <- mean6_log - margin_error_6
upper_bound_6 <- mean6_log + margin_error_6

lower_bound_7 <- mean7_log - margin_error_7
upper_bound_7 <- mean7_log + margin_error_7

lower_bound_9 <- mean9_log - margin_error_9
upper_bound_9 <- mean9_log + margin_error_9

#DIFFERENCE BETWEEN LOWER AND UPPER BOUND
diff1 <- upper_bound_1 - lower_bound_1
diff2 <- upper_bound_2 - lower_bound_2
diff3 <- upper_bound_3 - lower_bound_3
diff4 <- upper_bound_4 - lower_bound_4
diff5 <- upper_bound_5 - lower_bound_5
diff6 <- upper_bound_6 - lower_bound_6
diff7 <- upper_bound_7 - lower_bound_7
diff9 <- upper_bound_9 - lower_bound_9

#VECTORS FOR ALL DATA
data_frames <- c(EDA_VALUES_ADS_1[1],EDA_VALUES_ADS_2[1],EDA_VALUES_ADS_3[1],EDA_VALUES_ADS_4[1],EDA_VALUES_ADS_5[1],EDA_VALUES_ADS_6[1],EDA_VALUES_ADS_7[1],EDA_VALUES_ADS_9[1])
lower_bound <- c(lower_bound_1,lower_bound_2,lower_bound_3,lower_bound_4,lower_bound_5,lower_bound_6,lower_bound_7,lower_bound_9)
upper_bound <- c(upper_bound_1,upper_bound_2,upper_bound_3,upper_bound_4,upper_bound_5,upper_bound_6, upper_bound_7, upper_bound_9)
sample_mean_normal <- c(mean1_log,mean2_log,mean3_log,mean4_log,mean5_log,mean6_log, mean7_log,mean9_log)
sample_mean_orig <- c(mean1,mean2,mean3,mean4,mean5,mean6,mean7,mean9)
diff_up_low <- c(diff1,diff2,diff3,diff4,diff5,diff6,diff7,diff9)
product_count <- c(count1_log,count2_log,count3_log,count4_log,count5_log,count6_log,count7_log,count9_log)

print(diff_up_low)
#DATAFRAME FOR VALUES
CIs_ADS_95 <- data.frame(data_frames,lower_bound,upper_bound,sample_mean_normal, diff_up_low, sample_mean_orig, product_count)

#SORTING BY DIFFERENCE BETWEEN UPPER AND LOWER BOUND
CI_sort <- CIs_ADS_95[order(CIs_ADS_95$diff_up_low),]

## DETERMINING ALPHA - 99% CI
alpha_99 <- 0.01

#T-SCORE
t_score_1_99 <- qt(p=alpha_99/2, df=degrees_freedom_1, lower.tail = FALSE)
t_score_2_99 <- qt(p=alpha_99/2, df=degrees_freedom_2, lower.tail = FALSE)
t_score_3_99 <- qt(p=alpha_99/2, df=degrees_freedom_3, lower.tail = FALSE)
t_score_4_99 <- qt(p=alpha_99/2, df=degrees_freedom_4, lower.tail = FALSE)
t_score_5_99 <- qt(p=alpha_99/2, df=degrees_freedom_5, lower.tail = FALSE)
t_score_6_99 <- qt(p=alpha_99/2, df=degrees_freedom_6, lower.tail = FALSE)
t_score_7_99 <- qt(p=alpha_99/2, df=degrees_freedom_7, lower.tail = FALSE)
t_score_9_99 <- qt(p=alpha_99/2, df=degrees_freedom_9, lower.tail = FALSE)

#MARGIN OF ERROR                            
margin_error_1_99  <- t_score_1_99  * se_ads_1
margin_error_2_99  <- t_score_2_99  * se_ads_2
margin_error_3_99  <- t_score_3_99  * se_ads_3
margin_error_4_99  <- t_score_4_99  * se_ads_4
margin_error_5_99  <- t_score_5_99  * se_ads_5
margin_error_6_99  <- t_score_6_99  * se_ads_6
margin_error_7_99  <- t_score_7_99  * se_ads_7
margin_error_9_99  <- t_score_9_99  * se_ads_9

#LOWER/UPPER BOUNDS
lower_bound_1_99  <- mean1_log - margin_error_1_99 
upper_bound_1_99  <- mean1_log + margin_error_1_99 

lower_bound_2_99  <- mean2_log - margin_error_2_99 
upper_bound_2_99  <- mean2_log + margin_error_2_99 

lower_bound_3_99  <- mean3_log - margin_error_3_99 
upper_bound_3_99  <- mean3_log + margin_error_3_99 

lower_bound_4_99  <- mean4_log - margin_error_4_99 
upper_bound_4_99  <- mean4_log + margin_error_4_99 

lower_bound_5_99  <- mean5_log - margin_error_5_99 
upper_bound_5_99  <- mean5_log + margin_error_5_99 

lower_bound_6_99  <- mean6_log - margin_error_6_99 
upper_bound_6_99  <- mean6_log + margin_error_6_99 

lower_bound_7_99  <- mean7_log - margin_error_7_99 
upper_bound_7_99  <- mean7_log + margin_error_7_99 

lower_bound_9_99  <- mean9_log - margin_error_9_99 
upper_bound_9_99  <- mean9_log + margin_error_9_99 

#DIFFERENCE BETWEEN LOWER AND UPPER BOUND
diff1_99  <- upper_bound_1_99  - lower_bound_1_99 
diff2_99  <- upper_bound_2_99  - lower_bound_2_99 
diff3_99  <- upper_bound_3_99  - lower_bound_3_99 
diff4_99  <- upper_bound_4_99  - lower_bound_4_99 
diff5_99  <- upper_bound_5_99  - lower_bound_5_99 
diff6_99  <- upper_bound_6_99  - lower_bound_6_99 
diff7_99  <- upper_bound_7_99  - lower_bound_7_99 
diff9_99  <- upper_bound_9_99  - lower_bound_9_99 

#VECTORS FOR ALL DATA
data_frames_99 <- c(EDA_VALUES_ADS_1[1],EDA_VALUES_ADS_2[1],EDA_VALUES_ADS_3[1],EDA_VALUES_ADS_4[1],EDA_VALUES_ADS_5[1],EDA_VALUES_ADS_6[1],EDA_VALUES_ADS_7[1],EDA_VALUES_ADS_9[1])
lower_bound_99  <- c(lower_bound_1_99 ,lower_bound_2_99 ,lower_bound_3_99 ,lower_bound_4_99 ,lower_bound_5_99 ,lower_bound_6_99 ,lower_bound_7_99 ,lower_bound_9_99)
upper_bound_99  <- c(upper_bound_1_99 ,upper_bound_2_99 ,upper_bound_3_99 ,upper_bound_4_99 ,upper_bound_5_99 ,upper_bound_6_99 , upper_bound_7_99 , upper_bound_9_99 )
sample_mean_normal <- c(mean1_log,mean2_log,mean3_log,mean4_log,mean5_log,mean6_log, mean7_log,mean9_log)
sample_mean_orig <- c(mean1,mean2,mean3,mean4,mean5,mean6,mean7,mean9)
diff_up_low_99  <- c(diff1_99 ,diff2_99 ,diff3_99 ,diff4_99 ,diff5_99 ,diff6_99 ,diff7_99 ,diff9_99 )
product_count <- c(count1_log,count2_log,count3_log,count4_log,count5_log,count6_log,count7_log,count9_log)

#DATAFRAME FOR VALUES
CIs_ADS_99 <- data.frame(data_frames_99 ,lower_bound_99 ,upper_bound_99 ,sample_mean_normal, diff_up_low_99 , sample_mean_orig, product_count)

view(CIs_ADS_99)
view(CIs_ADS_95)

#SORTING BY DIFFERENCE BETWEEN UPPER AND LOWER BOUND
CI_sort <- CIs_ADS_95[order(CIs_ADS_95$diff_up_low),]

#CONFINDENCE INTERVAL PLOTS

CI_95_PLOT <- plotCI(x=CIs_ADS_95[,4],ui=CIs_ADS_95[,3],li=CIs_ADS_95[,2], main = "CI, 95%", xlab = "Dataframes 1-7,9 (8 = 9)", ylab= "mean")
CI_99_PLOT <- plotCI(x=CIs_ADS_99[,4],ui=CIs_ADS_99[,3],li=CIs_ADS_99[,2], main = "CI, 99%", xlab = "Dataframes 1-7,9 (8 = 9)", ylab= "mean")

## I DECIDE TO MOVE FORWARD WITH THE DATAFRAMES THAT HAVE THE SMALLEST GAP BETWEEN THE LOWER/UPPER BOUND FOR THE MEAN WITH THE HIGHEST COUNTS
##DF1 DF2 DF3

names(ads_1_log) <- "Product_Price_Norm"
names(ads_2_log) <- "Product_Price_Norm"
names(ads_3_log) <- "Product_Price_Norm"

ads_1_df_norm_values <- cbind.data.frame(amazon_dataset_1_removed_outliers, ads_1_log)
ads_2_df_norm_values <- cbind.data.frame(amazon_dataset_2_removed_outliers, ads_2_log)
ads_3_df_norm_values <- cbind.data.frame(amazon_dataset_3_removed_outliers, ads_3_log)


#PART 7 -- COMPARING THE INDEPENDENT MEANS

#F TEST TO CHECK IF VARIANCE IS EQUAL TO PERFORM STUDENT T TEST

var_test_1and2 <- var.test(ads_1_log_unlist ,ads_2_log_unlist, alternative = "two.sided")
var_test_2and3 <- var.test(ads_2_log_unlist ,ads_3_log_unlist, alternative = "two.sided")
var_test_1and3 <- var.test(ads_1_log_unlist ,ads_3_log_unlist, alternative = "two.sided")

ads_1_log_unlist <- unlist(ads_1_log)
ads_2_log_unlist <- unlist(ads_2_log)
ads_3_log_unlist <- unlist(ads_3_log)

print(var_test_1and2)
print(var_test_1and3)
print(var_test_2and3)

##THERE IS A SIGNIFICANT DIFFERENCE BETWEEN THE VARIANCE OF THE TWO POPULATIONG MEANS between 1&2, 1&3, and 2&3
## STUDENT T TEST CAN NOT BE PERFORMED
## I DECIDE TO PERFORM WELCH'S T TEST INSTEAD

##H-NULL  true difference in means is = 0
##H-ALT   true difference in means is =/= 0
##alpha is 0.05

welch_means1and2 <- t.test(ads_1_log_unlist, ads_2_log_unlist)
welch_means2and3 <- t.test(ads_2_log_unlist, ads_3_log_unlist)
welch_means1and3 <- t.test(ads_1_log_unlist, ads_3_log_unlist)

print(welch_means1and2)
print(welch_means1and3)
print(welch_means2and3)

## p.value for means 1 and 2, means 2 and 3, and means 1 and 3 are > than alpha

print(welch_means1and2$p.value)
print(welch_means2and3$p.value)
print(welch_means1and3$p.value)

## We fail to reject the null hypothesis
## There is probably no statistically significant difference between the means
## THere is probably no mean of product price that is more profitable than the other means

## PART 8 - IS THERE A CORRELATION BETWEEN PRODUCT RATING AND PRODUCT PRICE?

##OBSERVED HISTOGRAM -- DECIDED TO REMOVE LINES WITH 0 REVIEWS
product_rating_hist_1 <- ggplot(ads_1_df_norm_values, aes(x=Product_Rating)) + 
                            geom_histogram(binwidth = 1, color = "white")
print(product_rating_hist_1)

##LOOKING AT COUNT OF EACH RATING
product_rating_1 <- ads_1_df_norm_values %>%
                      group_by(product_rating = ads_1_df_norm_values$Product_Rating) %>%
                      tally()
view(product_rating_1)

##FILTERED OUT PRODUCT WITH NO REVIEWS
ads_1_df_reviewed <- ads_1_df_norm_values[!(ads_1_df_norm_values$Product_Reviews_Count == 0),]
ads_2_df_reviewed <- ads_2_df_norm_values[!(ads_2_df_norm_values$Product_Reviews_Count == 0),]
ads_3_df_reviewed <- ads_3_df_norm_values[!(ads_3_df_norm_values$Product_Reviews_Count == 0),]

str(ads_1_df_reviewed)
str(ads_2_df_reviewed)
str(ads_3_df_reviewed)

##REPLOTTED HISTOGRAMS
product_rating_hist_1_rvd <- ggplot(ads_1_df_reviewed, aes(x=Product_Rating)) + 
                              geom_histogram(binwidth = 0.25, color = "white")

product_rating_hist_2_rvd <- ggplot(ads_2_df_reviewed, aes(x=Product_Rating)) + 
                              geom_histogram(binwidth = 0.25, color = "white")

product_rating_hist_3_rvd <- ggplot(ads_3_df_reviewed, aes(x=Product_Rating)) + 
                              geom_histogram(binwidth = 0.25, color = "white")

print(product_rating_hist_1_rvd)
print(product_rating_hist_2_rvd)
print(product_rating_hist_3_rvd)

#DECIDED TO NORMALIZE THE VALUES USING LOG10
rating_normal_1 <- log10(ads_1_df_reviewed$Product_Rating)
rating_normal_2 <- log10(ads_2_df_reviewed$Product_Rating)
rating_normal_3 <- log10(ads_3_df_reviewed$Product_Rating)

#BOUND NORMALIZED VALUES TO DFs
ads_1_df_reviewed <- cbind.data.frame(ads_1_df_reviewed, rating_normal_1)
ads_2_df_reviewed <- cbind.data.frame(ads_2_df_reviewed, rating_normal_2)
ads_3_df_reviewed <- cbind.data.frame(ads_3_df_reviewed, rating_normal_3)

#LOOKED AT DENSITY OF PRODUCT RATING
ads_1_rating_log_density <- ggdensity(rating_normal_1,fill = "lightgray", title = "Product Rating, Home & Kitchen") +
                                                    stat_overlay_normal_density(color = "red", linetype = "dashed")
ads_2_rating_log_density <- ggdensity(rating_normal_2,fill = "lightgray", title = "Product Rating, Health & Household") +
                                                    stat_overlay_normal_density(color = "red", linetype = "dashed")
ads_3_rating_log_density <- ggdensity(rating_normal_3,fill = "lightgray", title = "Product Rating, Beauty And Personal Care") +
                                                   stat_overlay_normal_density(color = "red", linetype = "dashed")
print(ads_1_rating_log_density)
print(ads_2_rating_log_density)
print(ads_3_rating_log_density)

#RAN COORELATION COEFFICENT BETWEEN PRODUCT PRICE AND PRODUCT RATING
cor_ads_1 <- cor(ads_1_df_reviewed$Product_Price, ads_1_df_reviewed$Product_Rating)
cor_ads_2 <- cor(ads_2_df_reviewed$Product_Price, ads_2_df_reviewed$Product_Rating)
cor_ads_3 <- cor(ads_3_df_reviewed$Product_Price, ads_3_df_reviewed$Product_Rating)

cor_ads_1_norm <- cor(ads_1_df_reviewed$Product_Price_Norm, ads_1_df_reviewed$rating_normal_1)
cor_ads_2_norm <- cor(ads_2_df_reviewed$Product_Price_Norm, ads_2_df_reviewed$rating_normal_2)
cor_ads_3_norm <- cor(ads_3_df_reviewed$Product_Price_Norm, ads_3_df_reviewed$rating_normal_3)

## COORELATION COEEFICENT SHOWS NO/NEGLIBLE RELATIONSHIP BETWEEN PRODUCT PRICE AND PRODUCT RATING

print(cor_ads_1)
print(cor_ads_1_norm)
print(cor_ads_2)
print(cor_ads_2_norm)
print(cor_ads_3)
print(cor_ads_3_norm)


## PART 9 - IS THERE A CORRELATION BETWEEN PRODUCT PRICE AND PRODUCT RANKING?
## I DECIDED TO LOOK AT THE INDIVIDUAL CATEGORY RANKING AND THE OVERALL PRODUCT CATEGORY RANKING

##WROTE CSVS FOR DF 1,2, & 3 WITHOUT OUTLIERS TO DELIMIT THE BSR VALUES IN THE DF

write.csv(amazon_dataset_1_removed_outliers, "G:\\My Drive\\1_SPRING 2021\\2_STATISTICS AND ANALYSIS\\Final Project\\ADS_123_CSV\\ads_ros_1.csv", row.names = FALSE)
write.csv(amazon_dataset_2_removed_outliers, "G:\\My Drive\\1_SPRING 2021\\2_STATISTICS AND ANALYSIS\\Final Project\\ADS_123_CSV\\ads_ros_2.csv", row.names = FALSE)
write.csv(amazon_dataset_3_removed_outliers, "G:\\My Drive\\1_SPRING 2021\\2_STATISTICS AND ANALYSIS\\Final Project\\ADS_123_CSV\\ads_ros_3.csv", row.names = FALSE)

##LOAD EXCEL FILES

setwd("G:\\My Drive\\1_SPRING 2021\\2_STATISTICS AND ANALYSIS\\Final Project\\ADS_123_CSV")

bsr_cleaned_files <- as.vector(list.files(path = "G:\\My Drive\\1_SPRING 2021\\2_STATISTICS AND ANALYSIS\\Final Project\\ADS_123_CSV", pattern = "*.xlsx"))

view(bsr_cleaned_files)

amazon_dataset_1_delim_bsr <- as.data.frame(lapply(bsr_cleaned_files[1], read_xlsx))
amazon_dataset_2_delim_bsr <- as.data.frame(lapply(bsr_cleaned_files[2], read_xlsx))
amazon_dataset_3_delim_bsr <- as.data.frame(lapply(bsr_cleaned_files[3], read_xlsx))

amazon_dataset_1_delim_bsr <- select(amazon_dataset_1_delim_bsr,, -37:-47)
amazon_dataset_2_delim_bsr <- select(amazon_dataset_2_delim_bsr,, -37:-42)
amazon_dataset_3_delim_bsr <- select(amazon_dataset_3_delim_bsr,, -37:-38)

names_adsbrs2 <- names(amazon_dataset_2_delim_bsr)
names_adsbrs2[32] <- "BSR_1_CAT"
setnames(amazon_dataset_2_delim_bsr, old = names(amazon_dataset_2_delim_bsr), new = names_adsbrs2)

str(amazon_dataset_3_delim_bsr)

#SUBSET INTO 1 AND 2 RANKINGS

#DF1

str(amazon_dataset_2_delim_bsr_2nd)
amazon_dataset_1_delim_bsr_1st <- select(amazon_dataset_1_delim_bsr,, -31:-36)
amazon_dataset_1_delim_bsr_2nd <- select(amazon_dataset_1_delim_bsr,,-29:-30)
amazon_dataset_1_delim_bsr_2nd <- select(amazon_dataset_1_delim_bsr_2nd,,-31:-34)
amazon_dataset_1_delim_bsr_3rd <- select(amazon_dataset_1_delim_bsr,,-35:-36)
amazon_dataset_1_delim_bsr_3rd <- select(amazon_dataset_1_delim_bsr_3rd,,-29:-32)


#DF2
amazon_dataset_2_delim_bsr_1st <- select(amazon_dataset_2_delim_bsr,, -33:-36)
amazon_dataset_2_delim_bsr_2nd <- select(amazon_dataset_2_delim_bsr,, -35:-36)
amazon_dataset_2_delim_bsr_2nd <- select(amazon_dataset_2_delim_bsr_2nd,, -31:-32)


#DF3
amazon_dataset_3_delim_bsr_1st <- select(amazon_dataset_3_delim_bsr,, -33:-36)
amazon_dataset_3_delim_bsr_2nd <- select(amazon_dataset_3_delim_bsr,, -35:-36)
amazon_dataset_3_delim_bsr_2nd <- select(amazon_dataset_3_delim_bsr_2nd,, -31:-32)

str(amazon_dataset_3_delim_bsr_2nd)

##DROP NA VALUES

amazon_dataset_1_delim_bsr_1st <- drop_na(amazon_dataset_1_delim_bsr_1st, "BSR_1_CAT")
amazon_dataset_1_delim_bsr_2nd <- drop_na(amazon_dataset_1_delim_bsr_2nd, "BSR_1.2_CAT")
amazon_dataset_1_delim_bsr_3rd <- drop_na(amazon_dataset_1_delim_bsr_3rd, "BSR_2_CAT")

amazon_dataset_2_delim_bsr_1st <- drop_na(amazon_dataset_2_delim_bsr_1st, "BSR_1_CAT")
amazon_dataset_2_delim_bsr_2nd <- drop_na(amazon_dataset_2_delim_bsr_2nd, "BSR_2_CAT")

amazon_dataset_3_delim_bsr_1st <- drop_na(amazon_dataset_3_delim_bsr_1st, "BSR_1_CAT")
amazon_dataset_3_delim_bsr_2nd <- drop_na(amazon_dataset_3_delim_bsr_2nd, "BSR_2_CAT")

names_adsbrs2 <- names(amazon_dataset_2_delim_bsr)

names_adsbrs2[32] <- "BSR_1_CAT"

setnames(amazon_dataset_2_delim_bsr, old = names(amazon_dataset_2_delim_bsr), new = names_adsbrs2)

str(amazon_dataset_3_delim_bsr)

#SIMPLE PLOTS// COR.COE

##ADS_1_1
product_rank_hist_1_1 <- ggplot(amazon_dataset_1_delim_bsr_1st, aes(x=BSR_1_VALUE)) + 
                                    geom_histogram(binwidth = 100000, color = "white"

product_rank_dot_1_1 <- plot(Product_Price ~ BSR_1_VALUE, data = amazon_dataset_1_delim_bsr_1st)

coe_1_1 <- cor(amazon_dataset_1_delim_bsr_1st$Product_Price, amazon_dataset_1_delim_bsr_1st$BSR_1_VALUE)
print(coe_1_1)

##NO/NEGLIGIBLE RELATIONSHIP BETWEEN PRODUCT PRICE AND PRODUCT RANK IN ADS1

##ADS 2_1

product_rank_hist_2_1 <- ggplot(amazon_dataset_2_delim_bsr_1st, aes(x=BSR_1_VALUE)) +
                                  geom_histogram(binwidth = 100000, color = "white")

print(product_rank_hist_2_1)

product_rank_dot_2_1 <- plot(Product_Price ~ BSR_1_VALUE, data = amazon_dataset_2_delim_bsr_1st)

coe_2_1 <- cor(amazon_dataset_2_delim_bsr_1st$Product_Price, amazon_dataset_2_delim_bsr_1st$BSR_1_VALUE)

print(coe_2_1)

##NO/NEGLIGIBLE RELATIONSHIP BETWEEN PRODUCT PRICE AND PRODUCT RANK IN ADS2

##IT IS SAFE TO ASSUME THAT WITHIN THE ENTIRE DATASET, THERE IS NO RELATIONSHIP BETWEEN PRODUCT RANK AND PRODUCT PRICE

##PART 10 - RELATIONSHIP BETWEEN PRODUCT RATING AND PRODUCT RANK

##SUBSET OF VALUES WITH PRODUCT RATING AND RANK (REMOVE VALUES WITH NO RATINGS)

amazon_dataset_1_delim_bsr_rating <- drop_na(amazon_dataset_1_delim_bsr_1st, "Product_Reviews_Count")

zero_lines <- subset(amazon_dataset_1_delim_bsr_rating, amazon_dataset_1_delim_bsr_rating[,28] == 0)

amazon_dataset_1_delim_bsr_rating <- amazon_dataset_1_delim_bsr_rating[which(amazon_dataset_1_delim_bsr_rating$Product_Reviews_Count > 0),]

hist_ratings <- ggplot(amazon_dataset_1_delim_bsr_rating, aes(x=Product_Rating)) +
                            geom_histogram(binwidth = 0.25, color = "white")

plot(Product_Rating ~ BSR_1_VALUE, data = amazon_dataset_1_delim_bsr_rating)

coe_rating_rank <- cor(amazon_dataset_1_delim_bsr_rating$Product_Rating, amazon_dataset_1_delim_bsr_rating$BSR_1_VALUE)

print(coe_rating_rank)

## THE RELATIONSHIP BETWEEN PRODUCT RATING AND PRODUCT RANK IS STATISTICALLY NEGLIGIBLE, BUT OUR DATASET MAY NOT BE LARGE ENOUGH
## ACCORDING TO VARIOUS SOURCES, THERE ARE OVER 20 FACTORS THAT GO INTO PRODUCT RANKING, INCLUDING RATINGS, SALES, ANSWERED QUESTIONS, ETC.
  
  # FINAL OBSERVATIONS
  
## WE CAN SAY THAT THE SAMPLE MEANS OF THESE PRODUCT CATEGORIES PROBABLY REFLECT THE POPULATION MEANS WITHIN A VERY THIN MARGIN, PARTICULARY FOR THE FIRST 4 CATEGORIES**

## THE QUANTITATIVE OBSERVATIONS THAT WERE MADE HAD CLOSE TO NO CORRELATION BUT WE CAN SAY WITH SOME CERTAINTY THAT:**
## THE LACK OF RELATIONSHIP BETWEEN PRODUCT PRICE AND PRODUCT RATING IS ONE OF MANY INDICATORS THAT LOWER PRICING IS HIGHLY PRIORITIZED ON AMAZON AS OPPOSED TO PRODUCT QUALITY, BY BOTH CONSUMER AND VENDOR**
## THAT PRODUCT PRICE AND RATING ARE ONLY TWO OF MANY RELATIONSHIPS THAT CONTRIBUTE TO PRODUCT RANKING ON AMAZON**