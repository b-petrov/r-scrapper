################### Imported Libraries ####################

library(XML)
library(rvest)
library(stringr)
library(RSelenium)

################### Product Information Variables #####################

category <<- ""
subcategory <<- ""
categories <<- ""
subcategories <<- ""

modelnumber <<- ""
modelname <<- ""
colour <<- ""
productdimension <<- ""
packagedimension <<- ""
material <<- ""
similarproducts <<- ""

category_from <<- 1
subcategory_from <<- 1
productgroup_from <<- 1
products_from <<- 1
is_start <<- 1
counter <<- 1
total_count <<- 0

if (file.exists("log.csv")){
	log_data <- read.csv("log.csv", header = F)
	category_from <- strtoi(log_data[1])
	subcategory_from <- strtoi(log_data[2])
	productgroup_from <- strtoi(log_data[3])
	products_from <- strtoi(log_data[4])
	counter <- strtoi(log_data[5]) + 1
} else {

	################### Columns of result table ########################

	columns <- data.frame("Model_Name" = "", "Model_Number" = "", "Price" = "", "Material" = "", "Colour" = "", 
			"Product_Dimensions" = "", "Package_Dimensions" = "", "SimilarProduct_A" = "", "SimilarProduct_B" = "", 
			"SimilarProduct_C" = "", "SimilarProduct_D" = "", "SimilarProduct_E" = "")

	################### Make result table #####################

	write.table(columns, "product.csv", row.names = FALSE, na = "", sep = ",")
}

################### Scrape first page of the site #################

visitHomepage <- function(){
	print("Visiting Homepage")
	htmlpage <- read_html("http://www.bunnings.com.au")
	category_names <<- html_text(html_nodes(htmlpage, ".horizontal-nav ul li a"))

	doc <- htmlParse("http://www.bunnings.com.au")
	categories <<- as.character(doc['//div[@class="horizontal-nav-dropdown-column"]/strong/a/@href'])
}

################### Scrape each categories page ###################

visitCategories <- function(i){
	Sys.sleep(1)
	print("Visiting Category")
	doc <- htmlParse(paste0("http://www.bunnings.com.au", categories[i]))
	subcategories <<- as.character(doc['//li[@class="sidebar-dropdown-nav current"]/ul/li/a/@href'])
}

################### Scrape each subcategories page #####################

visitProductGroup <- function(i){
	Sys.sleep(2)
	print("Visiting ProductGroup")
	doc <- htmlParse(paste0("http://www.bunnings.com.au", subcategories[i]))
	print(paste0("http://www.bunnings.com.au", subcategories[i]))
	productgroup <<- as.character(doc['//li[@class="sidebar-dropdown-nav current"]/ul/li/a/@href'])
	htmlpage <<- read_html(paste0("http://www.bunnings.com.au", subcategories[i]))
	total_count <<- html_text(html_nodes(htmlpage, ".total-count"))
	print(total_count)
}

################## Scrape products group page #######################

visitProducts <- function(i){
	Sys.sleep(2)
	print("Visiting Products")
	doc <- htmlParse(paste0("http://www.bunnings.com.au", productgroup[i]))

	count <- strtoi(total_count[i])
	print(count)

	if (count > 48)
	{
		checkForServer()
		startServer()
		remDr <- remoteDriver$new()
		remDr$open(silent = TRUE)
		remDr$navigate(paste0("http://www.bunnings.com.au", productgroup[i]))

		while(count > 48)
		{
			count <- count - 48
			searchID<-'//*[@class="view-more-icon"]'
			webElem<-remDr$findElement(value = searchID)
			webElem$clickElement()
			Sys.sleep(10)
		}	
		
		searchID <- '//*/a[@class="product-list__link"]'
		products <<- remDr$findElements(using = 'xpath', searchID)
		products <<- sapply(products, function(x){x$getElementAttribute("href")})
		remDr$close()
	}
	else
	{
		products <<- as.character(doc['//article/a/@href'])
		for (j in 1 : length(products))
		{
			products[j] <<- paste0("http://www.bunnings.com.au", products[j])
		}
	}
}

################## Scrape products information page #######################

getProductInformation <- function(i){
	
	######## Scrape product name ############

	Sys.sleep(1)

	print(paste("Now scrapping", products[[i]]))
	htmlpage <- read_html(products[[i]])
	product <- html_text(html_nodes(htmlpage, ".product-details__info h1.fn"))

	######## Scrape price of the product ############
	tmpprice <- c(html_text(html_nodes(htmlpage, ".product-details__info .product-list__price .price-value")))
	price <- str_extract(tmpprice, "([0-9]|[.])+")
	price <- paste0(price, "$")

	######## Scrape detailed information of the product #############

	specs <- html_text(html_nodes(htmlpage, "#tab-specs dl div dt"))
	values <- html_text(html_nodes(htmlpage, "#tab-specs dl div dd"))
	if (length(specs) > 0)
	{

		for (j in 1 : length(specs)){
			
			######## Scrape Model Number #############
	
			if (specs[j] == "Model Number"){
				modelnumber <<- values[j]
			}

			######## Scrape Model Name #############

			if (specs[j] == "Model Name"){
				length <- nchar(products[[i]])
				modelname <<- paste(substr(products[[i]], length - 6, length), "   ",values[j])
			}

			######## Scrape Colour #############

			if (specs[j] == "Colour"){
				colour <<- values[j]
			}

			######## Scrape Product Dimensions #############

			if (specs[j] == "Product Dimensions (mm)"){
				productdimension <<- values[j]
			}

			######## Scrape Package Dimensions #############

			if (specs[j] == "Package Dimensions (mm)"){
				packagedimension <<- values[j]
			}

			######## Scrape Material #############

			if (specs[j] == "Material"){
				material <<- values[j]
			}
		}

	}
	
	######## Scrape Similar Products #############
	similarproducts <<- html_text(html_nodes(htmlpage, ".similar-products-list article a .product-list__details .product-list__prodname"))
	######## Write product information to csv file #############

	datas <- data.frame("Model_Name" = modelname, "Model_Number" = modelnumber, "Price" = price, 
		"Material" = material, "Colour" = colour, "Product_Dimensions" = productdimension, 
		"Package_Dimensions" = packagedimension, "SimilarProduct_A" = similarproducts[1], 
		"SimilarProduct_B" = similarproducts[2], "SimilarProduct_C" = similarproducts[3], 
		"SimilarProduct_D" = similarproducts[4], "SimilarProduct_E" = similarproducts[5])
	write.table(datas, "product.csv", row.names = FALSE, col.names = FALSE, na = "", sep = ",", append = TRUE)
}

logData <- function(a, b, c, d)
{
	log_data <- data.frame("1" = as.character(a), "2" = as.character(b), "3" = as.character(c), "4" = as.character(d), "5" = as.character(counter))
	write.table(log_data, "log.csv", row.names = FALSE, col.names = FALSE, na = "", sep = ",")
}


visitHomepage()

if (length(categories) > 0)
{
	for (i in category_from : length(categories))
	{
		visitCategories(i)
		if (is_start == 1)
			sub_from <- subcategory_from
		else
			sub_from <- 1
		
		if (length(subcategories) > 0)
		{
			for (j in sub_from : length(subcategories))
			{
				tmp <- str_extract(subcategories[j], "([0-9]|[a-z]|[A-Z]|[/]|[-]|[_]|[.])+")
	
				visitProductGroup(j)
				
				if (length(total_count) < 1)
				{
					#if (j + 1 > length(subcategories))
					#	logData(i, j + 1, 1, 1)
					#else
					#	logData(i + 1, j, 1, 1)
					#logData(i, j + 1, 1, 1)
					next
				}	
				if (is_start == 1)
					prog_from <- productgroup_from
				else
					prog_from <- 1
	
				if (length(productgroup) > 0)
				{
					for (k in prog_from : length(productgroup))
					{
						visitProducts(k)
						if (is_start == 1)
							pro_from <- products_from
						else
							pro_from <- 1
			
						if (length(products) > 0)
						{
							for (l in pro_from : length(products))
							{
								getProductInformation(l)
			
								a <- i
								b <- j
								c <- k
								d <- l + 1
							
								if (d > length(products))
								{
									d <- 1
									c <- c + 1
									if (c > length(productgroup))
									{
										c <- 1
										b <- b + 1
										if (b > length(subcategories))
										{
											b <- 1
											a <- a + 1
										}
									}
								}
								log_data <- data.frame("1" = as.character(a), "2" = as.character(b), "3" = as.character(c), 
												"4" = as.character(d), "5" = as.character(counter))
								write.table(log_data, "log.csv", row.names = FALSE, col.names = FALSE, na = "", sep = ",")
								print(paste(as.character(counter), "urls", "completed"))
								counter <- counter + 1
								is_start = 0
							}	
						}
					}	
				}
			}	
		}
	}
	
}


gifts <- c("http://www.bunnings.com.au/our-range/lighting-electrical/cooling/ceiling-fans",
		"http://www.bunnings.com.au/gift-ideas/interest/diyer",
		"http://www.bunnings.com.au/gift-ideas/interest/entertainer",
		"http://www.bunnings.com.au/gift-ideas/interest/decorator",
		"http://www.bunnings.com.au/gift-ideas/interest/gardener",
		"http://www.bunnings.com.au/gift-ideas/interest/kids",
		"http://www.bunnings.com.au/gift-ideas/interest/gifts-under-20",
		"http://www.bunnings.com.au/gift-ideas/interest/gifts-under-50",
		"http://www.bunnings.com.au/gift-ideas/interest/gifts-under-100",
		"http://www.bunnings.com.au/gift-ideas/interest/gifts-over-100")
counts <- c("73", "186", "126", "48", "50", "68", "37", "42", "61", "51")

for (i in 7 : length(gifts))
{
	print("Visiting Products")
	doc <- htmlParse(gifts[i])
	count = strtoi(counts[length(gifts) - i + 1])
	products <- c("")
	if (count > 48)
	{
		checkForServer()
		startServer()
		remDr <- remoteDriver$new()
		remDr$open(silent = TRUE)
		remDr$navigate(gifts[i])

		while(count > 48)
		{
			count <- count - 48
			searchID<-'//*[@class="view-more-icon"]'
			webElem<-remDr$findElement(value = searchID)
			webElem$clickElement()
			Sys.sleep(10)
		}	
		
		searchID <- '//*/a[@class="product-list__link"]'
		products <<- remDr$findElements(using = 'xpath', searchID)
		products <<- sapply(products, function(x){x$getElementAttribute("href")})
		remDr$close()
	}
	else
	{
		products <- as.character(doc['//article/a/@href'])
			
		for (j in 1 : length(products))
		{
			products[j] <- paste0("http://www.bunnings.com.au", products[j])
		}
	}

	for (j in 1 : length(products))
	{
		getProductInformation(j)
	}
}



print("Finished scrapping")

