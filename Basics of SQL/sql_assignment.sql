#    Task 1: Understanding the data in hand
#The data given describes the information stored by a superstore across 5 different tables .
#------------------------------------------------------------------------------------------------
#Table 1: cust_dimen - Stores customer information 
#PK  - Cust_id(text)       - Uniquely identifies customer 
#FK  - NA
#
#Other Columns :
#Customer_Name -Name of the Customer
#Province
#Region
#Customer Segment
#------------------------------------------------------------------------------------------------ 
#
#Table 2: order_dimen - Stores order information Consisting Date of order placed  and the priority of the order
#PK - Ord_id (text)- Uniquely identifies order
#FK - NA
#
#Other Columns :
#Order_ID (int): As it has duplicate values, it cannot be primary key or candidate key
#Order_date:Date of order placed
#Order_Priority:Priority of each order as specified.
#------------------------------------------------------------------------------------------------
#Table 3:prod_dimen: Stores various Product categories and sub-categories of the products available at the superstore
#
#PK - Prod_id(text)-Uniquely identifies a product
#FK - NA
#
#Other Columns:
#Product Category,Product_Sub_Category for each uniquely identified product.
#------------------------------------------------------------------------------------------------
#Table 4:
#PK - Ship_id - Uniquely identifies a shipment 
#FK- NA
#
#Other Columns:
#Order_ID - Refers to Order_ID of order_dimen.It cannot be FK as it has duplicate values in Order table.
#Ship_Mode,Ship_Date are other two columns which store the shipment mode  and the shipment date 
#------------------------------------------------------------------------------------------------
#Table 5:
#Pk - Composite Key -(Ord_id,Prod_id,Ship_id,Cust_id)
#FK - Prod_id 
#FK - Ord_id
#FK - Ship_id
#FK - Cust_id
#
#Other Columns:
#Sales,Discount ,Profit,Product_base_margin,Discount,Shipping_Cost
#------------------------------------------------------------------------------------------------
#
#2.Product information  - Various Product categories and sub-categories of the products available at the superstore
#3.Order information - 
#4.Shipping information-Shipping mode, ship date for every order placed by the customer
#5.Market information - Sales, Order quantity, Profit, Shipping_Cost etc for each order placed.

#Task 2: Basic Analysis

#A. Find the total and the average sales (display total_sales and avg_sales) 
select sum(Sales) as total_sales ,avg(Sales) as avg_sales
from market_fact;



#B. Display the number of customers in each region in decreasing order of
#no_of_customers. The result should contain columns Region, no_of_customers

select Region , count(*) as no_of_customers
from cust_dimen
group by Region 
order by no_of_customers desc;

#C. Find the region having maximum customers (display the region name and
#max(no_of_customers)
select Region , count(*) as max_no_of_customers
from cust_dimen
group by Region 
order by max_no_of_customers desc
Limit 1;

#D.Find the number and id of products sold in decreasing order of products sold (display
#product id, no_of_products sold)

select Prod_id as product_id,sum(Order_Quantity)as no_of_products
from market_fact
group by product_id
order by no_of_products desc;

#E. Find all the customers from Atlantic region who have ever purchased ‘TABLES’ and
#the number of tables purchased (display the customer name, no_of_tables
#purchased) 

select c.Customer_Name,sum(m.Order_Quantity) as no_of_tables from market_fact as m inner join prod_dimen as p 
on m.Prod_id = p.Prod_id 
inner join cust_dimen as c
on m.Cust_id = c.Cust_id 
where c.Region = 'ATLANTIC'
and p.Product_Sub_Category='TABLES'
group by c.Cust_id,c.Customer_Name;

#Task 3: Advanced Analysis

#A. Display the product categories in descending order of profits (display the product
#category wise profits i.e. product_category, profits)?

select p.Product_Category as product_category,sum(m.Profit) as profits 
from  prod_dimen as p inner join market_fact as m
on p.Prod_id = m.Prod_id
group by product_category
order by profits desc;

#B. Display the product category, product sub-category and the profit within each subcategory
#in three columns.

 
select p.Product_Category as product_category,p.Product_Sub_Category as product_sub_category,sum(m.Profit) as profits 
from  prod_dimen as p inner join market_fact as m
on p.Prod_id = m.Prod_id
group by product_category,product_sub_category;

#C. Where is the least profitable product subcategory shipped the most? For the least
#profitable product sub-category, display the region-wise no_of_shipments and the
#profit made in each region in decreasing order of profits (i.e. region,
#no_of_shipments, profit_in_each_region)
#Note: You can hardcode the name of the least profitable product subcategory

select c.Region,count(*)as no_of_shipments,sum(m.Profit) as profit_in_each_region 
from market_fact as m inner join shipping_dimen as s
on  m.ship_id = s.ship_id
inner join prod_dimen as p
on p.Prod_id=m.Prod_id
inner join cust_dimen as c
on c.Cust_id=m.Cust_id
where p.Product_Sub_Category='TABLES'
group by c.Region
order by profit_in_each_region desc;
