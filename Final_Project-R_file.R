library(completejourney)
library(tidyverse)
library(lubridate)


transactions<-get_transactions()
promotions<-get_promotions()


## going to be only looking at transactions for households whose data we have to make it easier for inferring



## calculating demographic factors

demographics_income<-demographics %>% group_by(income) %>% tally() %>% mutate(income_factor=1/n)
demographics_age<-demographics %>% group_by(age) %>% tally() %>% mutate(age_factor=1/n)
demographics_kidscount<-demographics %>% group_by(kids_count) %>% tally() %>% mutate(kids_count_factor=1/n)
demographics_householdsize<-demographics %>% group_by(household_size) %>% tally() %>% mutate(household_size_factor=1/n)
demographics_maritalstatus<-demographics %>% group_by(marital_status) %>% tally() %>% mutate(marital_status_factor=1/n)

## all discounted items sales and normal sales by demographics

## By income - discounted and normal

transactions %>% 
  filter(retail_disc>0|coupon_disc>0|coupon_match_disc>0) %>% 
  inner_join(demographics,by='household_id') %>% 
  group_by(income) %>% 
  summarise(totalsales=sum(sales_value)) %>%inner_join(demographics_income) %>%
  mutate(avgsales=totalsales*income_factor) %>% 
  select(income,totalsales,avgsales) %>% 
  arrange(desc(avgsales)) %>% 
  ggplot(aes(x = income, y = avgsales)) +
  geom_bar(stat="identity",fill="lightblue") + 
  scale_y_continuous(label = scales::dollar) +
  geom_text(aes(label =round(totalsales)), vjust = -1, colour = "black") +
  labs(title = "Discounted item sales by demographics income",
       subtitle = "Sales for discounted items based on their income range",
       x = "Income Range of Households",
       y = "Average sales in $")

transactions %>% 
  inner_join(demographics,by='household_id') %>% 
  group_by(income) %>% 
  summarise(totalsales=sum(sales_value)) %>% inner_join(demographics_income) %>%
  mutate(avgsales=totalsales*income_factor) %>% 
  select(income,totalsales,avgsales) %>% 
  arrange(desc(avgsales)) %>% 
  ggplot(aes(x = income, y = avgsales)) +
  geom_bar(stat="identity",fill="lightgreen") + 
  scale_y_continuous(label = scales::dollar) +
   geom_text(aes(label =round(totalsales)), vjust = -1, colour = "black")+
  labs(title = "Normal item sales by demographics income",
       subtitle = "Sales of items based on their income range",
       x = "Income Range of Households",
       y = "Average sales in $")


 




## a lot of demographics whose sales differ compared to their discounted sales.highlight them in report and presentation 
## Most peculiar things is that- avg total sales for 200-249k income group is very low. which means there is a lack of awareness here as inferred previously. 


## By age - discounted and normal

transactions %>% 
  filter(retail_disc>0|coupon_disc>0|coupon_match_disc>0) %>% 
  inner_join(demographics,by='household_id') %>% 
  group_by(age) %>% 
  summarise(totalsales=sum(sales_value)) %>%inner_join(demographics_age) %>%
  mutate(avgsales=totalsales*age_factor) %>% 
  select(age,totalsales,avgsales) %>% 
  arrange(desc(avgsales))

transactions %>% 
  inner_join(demographics,by='household_id') %>% 
  group_by(age) %>% 
  summarise(totalsales=sum(sales_value)) %>%inner_join(demographics_age) %>%
  mutate(avgsales=totalsales*age_factor) %>% 
  select(age,totalsales,avgsales) %>% 
  arrange(desc(avgsales))


## By marital_status - discounted and normal

transactions %>% 
  filter(retail_disc>0|coupon_disc>0|coupon_match_disc>0) %>% 
  inner_join(demographics,by='household_id') %>% 
  group_by(marital_status) %>% 
  summarise(totalsales=sum(sales_value)) %>%inner_join(demographics_maritalstatus) %>%
  mutate(avgsales=totalsales*marital_status_factor) %>% 
  select(marital_status,totalsales,avgsales) %>% 
  arrange(desc(avgsales))

transactions %>% 
  inner_join(demographics,by='household_id') %>% 
  group_by(marital_status) %>% 
  summarise(totalsales=sum(sales_value)) %>%inner_join(demographics_maritalstatus) %>%
  mutate(avgsales=totalsales*marital_status_factor) %>% 
  select(marital_status,totalsales,avgsales) %>% 
  arrange(desc(avgsales))

## By household_size - discounted and normal

transactions %>% 
  filter(retail_disc>0|coupon_disc>0|coupon_match_disc>0) %>% 
  inner_join(demographics,by='household_id') %>% 
  group_by(household_size) %>% 
  summarise(totalsales=sum(sales_value)) %>%inner_join(demographics_householdsize) %>%
  mutate(avgsales=totalsales*household_size_factor) %>% 
  select(household_size,totalsales,avgsales) %>% 
  arrange(desc(avgsales))

transactions %>% 
  inner_join(demographics,by='household_id') %>% 
  group_by(household_size) %>% 
  summarise(totalsales=sum(sales_value)) %>%inner_join(demographics_householdsize) %>%
  mutate(avgsales=totalsales*household_size_factor) %>% 
  select(household_size,totalsales,avgsales) %>% 
  arrange(desc(avgsales))

## By kids_count - discounted and normal

transactions %>% 
  filter(retail_disc>0|coupon_disc>0|coupon_match_disc>0) %>% 
  inner_join(demographics,by='household_id') %>% 
  group_by(kids_count) %>% 
  summarise(totalsales=sum(sales_value)) %>%inner_join(demographics_kidscount) %>%
  mutate(avgsales=totalsales*kids_count_factor) %>% 
  select(kids_count,totalsales,avgsales) %>% 
  arrange(desc(avgsales)) %>% 
  ggplot(aes(x =kids_count , y = avgsales)) +
  geom_bar(stat="identity",fill="lightblue") + 
  scale_y_continuous(label = scales::dollar) +
  geom_text(aes(label =round(totalsales)), vjust = -1, colour = "black")+
  labs(title = "Discounted item sales by demographic kids_count",
       subtitle = "Sales of discounted items based on Kids in every household",
       x = "Kids_count of Households",
       y = "Average sales in $")

transactions %>% 
  inner_join(demographics,by='household_id') %>% 
  group_by(kids_count) %>% 
  summarise(totalsales=sum(sales_value)) %>%inner_join(demographics_kidscount) %>%
  mutate(avgsales=totalsales*kids_count_factor) %>% 
  select(kids_count,totalsales,avgsales) %>% 
  arrange(desc(avgsales)) %>%  
  ggplot(aes(x =kids_count , y = avgsales)) +
  geom_bar(stat="identity",fill="lightgreen") + 
  scale_y_continuous(label = scales::dollar) +
  geom_text(aes(label =round(totalsales)), vjust = -1, colour = "black") +
  labs(title = "Normal item sales by demographic kids_count",
       subtitle = "Sales of items based on Kids in every household",
       x = "Kids_count of Households",
       y = "Average sales in $")

## people with 1 kid are buying more items with discounted sales compared to total sales. 

####-------------- How many customers are spending more over time? Less over time? Describe these customers.
## summarise all the demographics which are not increasing over time and tell that we need to market to these demographics in a diff way than we've been doing so far as they're not following the general trend


transactions %>% mutate(Month=month(transaction_timestamp)) %>% 
  group_by(Month) %>% summarise(monthlysales=sum(sales_value))  %>% 
  ggplot(aes(Month,monthlysales))+geom_path() +
  geom_point(size = 2)+ geom_text(aes(label = monthlysales), vjust = -1, colour = "black")+
  scale_x_continuous(breaks = 1:12, minor_breaks = NULL, labels = month.name[1:12]) +
  scale_y_continuous(label = scales::dollar) +
  labs(title = "Month wise transactions of customers",
       subtitle = "Customer spending range over months in year 2017",
       x = "Months of transaction",
       y = "Monthly sales in $") 

## overall transaction data is increasing month to month . 

## monthly avg sales per age

transactions %>% inner_join(demographics,by='household_id') %>% 
  mutate(Monthoftrans=month(transaction_timestamp)) %>% 
  group_by(age,Monthoftrans) %>% 
  summarise(Monthly_sales_asperage=sum(sales_value)) %>% 
  inner_join(demographics_age) %>%
  mutate(avgmonthlysales=Monthly_sales_asperage*age_factor) %>% 
  ggplot(aes(Monthoftrans,avgmonthlysales))+
  facet_wrap(~age)+geom_path(linetype = "dashed",color="RED",size = 1) + geom_point(color="blue",size = 1)+
  scale_x_continuous(breaks = 1:12, minor_breaks = NULL, labels = month.name[1:12]) +
  scale_y_continuous(label = scales::dollar) +
  labs(title = "Age group wise monthly transactions over time",
       subtitle = "Average montly transations of different age groups over period of time",
       x = "Month of transaction",
       y = "Average monthly sales in $") + 
  theme(axis.text.x = element_text(angle = 90,hjust = 1))
  

## looks like 19-24 age group avg spending is decreasing over time- compound that with marketing. 

## monthly avg sales per income

transactions %>% inner_join(demographics,by='household_id') %>% 
  mutate(Monthoftrans=month(transaction_timestamp)) %>% 
  group_by(income,Monthoftrans) %>% 
  summarise(Monthly_sales_asperincome=sum(sales_value)) %>% 
  inner_join(demographics_income) %>%
  mutate(avgmonthlysales=Monthly_sales_asperincome*income_factor) %>% 
  ggplot(aes(Monthoftrans,avgmonthlysales))+facet_wrap(~income)+geom_path(linetype = "dashed",color="RED",size = 1) + 
  geom_point(color="blue",size = 1)+
  scale_x_continuous(breaks = 1:12, minor_breaks = NULL, labels = month.name[1:12]) +
  scale_y_continuous(label = scales::dollar) +
  labs(title = "Income range wise monthly transactions over time",
       subtitle = "Average montly transations of different income ranges over period of time",
       x = "Month of transaction",
       y = "Average monthly sales in $") + 
  theme(axis.text.x = element_text(angle = 90,hjust = 1))

## a lot of income groups are flat and decreasing . need specific marketing for each income group as it looks like the current campaigns haven't worked in increasing their monthly sales. 

## monthly avg sales per kids_count

transactions %>% inner_join(demographics,by='household_id') %>% 
  mutate(Monthoftrans=month(transaction_timestamp)) %>% 
  group_by(kids_count,Monthoftrans) %>% 
  summarise(Monthly_sales_asperkids=sum(sales_value)) %>% 
  inner_join(demographics_kidscount) %>%
  mutate(avgmonthlysales=Monthly_sales_asperkids*kids_count_factor)%>% 
  ggplot(aes(Monthoftrans,avgmonthlysales))+facet_wrap(~kids_count)+geom_path(linetype = "dashed",color="RED",size = 1)+ 
  geom_point(color="blue",size = 1) +
  scale_x_continuous(breaks = 1:12, minor_breaks = NULL, labels = month.name[1:12]) +
  scale_y_continuous(label = scales::dollar) +
  labs(title = "Kids_count wise monthly transactions over time",
       subtitle = "Average montly transations based on kids_count over period of time",
       x = "Month of transaction",
       y = "Average monthly sales in $") + 
  theme(axis.text.x = element_text(angle = 90,hjust = 1))

## only people with 1 kid haven't been increasing sales as per month to month. 

## monthly avg sales per household_size

transactions %>% inner_join(demographics,by='household_id') %>% 
  mutate(Monthoftrans=month(transaction_timestamp)) %>% 
  group_by(household_size,Monthoftrans) %>% 
  summarise(Monthly_sales_asperhousehold_size=sum(sales_value)) %>% 
  inner_join(demographics_householdsize) %>%
  mutate(avgmonthlysales=Monthly_sales_asperhousehold_size*household_size_factor)%>% 
  ggplot(aes(Monthoftrans,avgmonthlysales))+facet_wrap(~household_size)+
  geom_path(linetype = "dashed",color="RED",size = 1) +
  geom_point(color="blue",size = 1) +
  scale_x_continuous(breaks = 1:12, minor_breaks = NULL, labels = month.name[1:12]) +
  scale_y_continuous(label = scales::dollar) +
  labs(title = "Household_size wise monthly transactions over time",
       subtitle = "Average montly transations based on household_size over period of time",
       x = "Month of transaction",
       y = "Average monthly sales in $") + 
  theme(axis.text.x = element_text(angle = 90,hjust = 1))

## monthly avg sales per marital status

transactions %>% inner_join(demographics,by='household_id') %>% 
  mutate(Monthoftrans=month(transaction_timestamp)) %>% 
  group_by(marital_status,Monthoftrans) %>% 
  summarise(Monthly_sales_aspermarital_status=sum(sales_value)) %>% 
  inner_join(demographics_maritalstatus) %>%
  mutate(avgmonthlysales=Monthly_sales_aspermarital_status*marital_status_factor)%>% 
  ggplot(aes(Monthoftrans,avgmonthlysales))+facet_wrap(~marital_status)+geom_path(linetype = "dashed",color="RED",size = 1) + 
  geom_point(color="blue",size = 1)+
  scale_x_continuous(breaks = 1:12, minor_breaks = NULL, labels = month.name[1:12]) +
  scale_y_continuous(label = scales::dollar) +
  labs(title = "Marital status wise monthly transactions over time",
       subtitle = "Average montly transations based on Marital status over period of time",
       x = "Month of transaction",
       y = "Average monthly sales in $")+ 
  theme(axis.text.x = element_text(angle = 90,hjust = 1))


########---------------------
# Maybe a certain demographic group is generating large amounts of revenue for a particular product and we could invest to capture market share in this area?
#   Is there a certain demographic group that we notice is not buying particular products? This could be for valid reasons or it could be because there is insufficient marketing to this group. This could be an opportunity to increase awareness of these products for this demographic group to gain more interest.


 # marital status top products 
transactions %>% inner_join(products) %>% inner_join(demographics) %>% 
  group_by(marital_status,product_category) %>% summarise(totalsales=sum(sales_value)) %>% 
  arrange(desc(totalsales)) %>% head(30) %>%
  ggplot(aes(product_category,totalsales))+facet_wrap(~marital_status,ncol=1)+geom_point(aes(colour=totalsales))+theme(axis.text.x = element_text(angle = 90,hjust = 1))+ labs(title = "Total sales value of top products purchased based on Marital status", x = "Top Products",
                                                                                                                                                         y = "Total sales in $")+scale_y_continuous(label = scales::dollar)
                                                                                                                                                    
# age top products
transactions %>% inner_join(products) %>% inner_join(demographics) %>% 
  group_by(age,product_category) %>% summarise(totalsales=sum(sales_value)) %>% 
  arrange(desc(totalsales)) %>% head(30) %>%
  ggplot(aes(product_category,totalsales))+facet_wrap(~age,ncol=1)+geom_point(aes(colour=product_category))+theme(axis.text.x = element_text(angle = 90,hjust = 1))+ labs(title = "Total sales value of top products purchased based on Age Group", x = "Top Products",
                                                                                                                                                                    y = "Total sales in $")+theme(axis.text.x = element_text(angle = 90,hjust = 1))+scale_y_continuous(label = scales::dollar)

# household size top products  

transactions %>% inner_join(products) %>% inner_join(demographics) %>% 
  group_by(household_size,product_category) %>% summarise(totalsales=sum(sales_value)) %>% 
  arrange(desc(totalsales)) %>% head(30) %>%
  ggplot(aes(product_category,totalsales))+facet_wrap(~household_size,ncol=1)+geom_point(aes(colour=product_category))+theme(axis.text.x = element_text(angle = 90,hjust = 1))+ labs(title = "Total sales value of top products purchased based on Household Size", x = "Top Products",
                                                                                                                                                                                     y = "Total sales in $")+theme(axis.text.x = element_text(angle = 90,hjust = 1))+scale_y_continuous(label = scales::dollar)


# income top products  

transactions %>% inner_join(products) %>% inner_join(demographics) %>% 
  group_by(income,product_category) %>% summarise(totalsales=sum(sales_value)) %>% 
  arrange(desc(totalsales)) %>% head(30) %>%
  ggplot(aes(product_category,totalsales))+facet_wrap(~income,ncol=1)+geom_point(aes(colour=totalsales))+theme(axis.text.x = element_text(angle = 90,hjust = 1))+ labs(title = "Total sales value of top products purchased based on Income", x = "Top Products",
                                                                                                                                                                             y = "Total sales in $")+theme(axis.text.x = element_text(angle = 90,hjust = 1))+scale_y_continuous(label = scales::dollar)


###------------------ coupons data - 

# campaign id and household

households_per_campaign<-campaigns %>% group_by(campaign_id) %>% tally() %>%
  mutate(households_per_campaign_factor=1/n)

coupon_redemptions %>% 
  inner_join(transactions,by='household_id') %>% 
  filter(yday(transaction_timestamp)==yday(redemption_date)) %>%
  inner_join(coupons,by = c("coupon_upc", "campaign_id", "product_id")) %>% 
  group_by(campaign_id) %>% 
  summarise(totalsales=sum(sales_value)) %>% 
  inner_join(households_per_campaign) %>% 
  mutate(income_per_household=totalsales*households_per_campaign_factor) %>% 
  arrange(desc(income_per_household)) %>% head(10)  %>%       #Top 10 campaign id
  ggplot(aes(x="",fill=campaign_id, y = reorder(income_per_household, campaign_id))) +
  geom_bar(stat="identity", width=1,color='black') +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette = 'Set3') +
  geom_text(aes(x = 1.7, label = scales::percent(income_per_household/10, accuracy = .1)), position = position_stack(vjust = .5)) +
  theme_void() +
  labs(title = "Most successfull campaigns with respect to households",
       subtitle = "Percentage wise successfull campaigns in year 2017",
       x = NULL,
       y = NULL)


  









