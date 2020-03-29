import pandas as pd
import numpy as np
from plotnine import *
from adjustText import adjust_text
from statistics import variance 
from mizani.formatters import dollar_format

# read in data
income = pd.read_csv("tuition_income.csv", 
                     encoding = "ISO-8859-1")
tuition = pd.read_csv("tuition_cost.csv").filter(["name", "type", "degree_length"]) \
              .query("degree_length == '4 Year'")


income_2 = (income.filter(["name", "total_price", "year"]) \
               .drop_duplicates() \
                   
                # group by year and name to aggregate multiple rows with same year and name
               .groupby(["name", "year"], as_index = False) \
               .agg({"total_price": "mean"}) \
               .sort_values(["name", "year"]) \
                   
                # claculate the difference for each year in college tuition
               .assign(count_name = lambda x: x.groupby("name")["name"].transform("count"),
                       difference = lambda x: x.groupby("name")["total_price"].transform(difference_income)) \
               
               # filter out tuition where there has not been an increase in tuition each year 
               .query("(difference >= 0) or (difference != difference)") \
               .assign(count_name_2 = lambda x: x.groupby("name")["name"].transform("count")) \
                   
               # throw out rows where there has not been a consistent increase intuition over the years
               .query("(count_name_2 == count_name) and (count_name_2 >=4)") \
               .assign(var = lambda x: x.groupby("name")["total_price"].transform(variance)))
               
# inner join to get degree length and type of university
tuition_income = pd.merge(income_2,
                 tuition,
                 on = "name")


diff_types = np.unique(tuition_income["type"])
length_type = len(diff_types)

# initializing empty data frame
df = pd.DataFrame(columns = tuition_income.columns)
for i in range(length_type):
    
    # get top 3 universities for each type (public, private, for profit)
    type_college = diff_types[i]
    tu_in = tuition_income.groupby("type") \
        .get_group(type_college) \
        .sort_values("var", ascending = False) 
  
    top_3 = tu_in.drop_duplicates("name")["name"][0:3].reset_index() 
    tu_in = (tu_in[tu_in["name"].isin(top_3['name'])] \
        .sort_values(["name", "year"]) \
            
         # calculate the percentage change in tuition from the first year to the last year
        .assign(perc_change = lambda x: x.groupby('name')['total_price'].transform(lambda x: (x.iloc[-1] - x.iloc[0]) / x.iloc[0])))
    
    df = df.append(tu_in)
  
df_text = df.sort_values(["name", "type", "year"], ascending = False) \
    .drop_duplicates("name") \
    .assign(perc_change = lambda x: round(x["perc_change"] * 100, 2))


(ggplot(df, aes(x = "factor(year)", y = "total_price", color = "factor(name)", group = "name")) + 
  geom_point() +
  geom_line() +
  geom_label(data = df_text, mapping = aes(x = "factor(year)", y = "total_price", 
                                          label = "perc_change"), color = "black", 
            format_string='{}%') +
  theme_light() +
  theme(legend_position = (.5, -.1),
        legend_direction = 'horizontal',
        plot_title = element_text(hjust = 0.5, size = 20),
        legend_title = element_blank(),
        axis_title = element_text(size = 15),
        axis_text = element_text(size = 12),
        legend_text = element_text(size = 12)) +
  scale_y_continuous(labels = dollar_format()) +
  ylab("Tuition") +
  xlab("Year") +
  ggtitle("Top 9 Universities With the Most Tuition Increase Over Time"))
                   

def difference_income(column):
    d = np.diff(column)
    result = np.append(np.nan, d)         
    return(result) 
