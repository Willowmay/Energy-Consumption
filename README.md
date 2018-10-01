# Energy-Consumption


This project is one of my course mideterm project, cooperated with my classmate Adam Kirstein. Thanks to our team effort! Hope you could enjor data analysis world :)


1. Dataset Description & Data Source

The dataset that we have chosen to use is an historical representation of consumed energy transition by country from 1800. 
The dataset is broken into columns of various energy sources, such as fuelwood, and electricity, 
and then shows each year what percentage of each energy source is used by each country. 

For example, in 1800, Canada was primarily using wood as a fuel source, accounting for 88% of its main source of energy. 
As time moves on, we can see that Canada's percentage use of Fuelwood decreases, and its percentage use of Coal increases. 

The energy transition data was published on the website Our World In Data, 
and boasts data collected by sources including the Joint Center for History and Economics, 
BP Statistical Review, The World Bank, and The International Renewable Energy Agency, amongst others. 

One method employed by The World Bank includes 
sourcing related data from the Sustainable Energy for All (SE4ALL) database from the SE4ALL Global Tracking Framework. 
All of the data were then compiled into a single dataset found at https://ourworldindata.org. 

The first three columns of the data are Entity(country), Code, and Year. The remaining columns show “the shared of energy by source over the long-term, measured as the percentage of total energy consumption. Primary electricity includes: hydro power, nuclear power, wind, photovoltaics, tidal, wave, solar thermal and geothermal. (Only figures for electricity productions are included). Non-energy uses of oil and natural gas are excluded from the totals . 

This data frame consists of 1816 observations of 11 variables (97 KB). 

2. Questions we want to ask

1) What is the long-term trend of Natural Gas, Oil and Coal ( non-renewables) consumption amongst countries? 
2) What is the trend of Fuelwood consumption by Canada from 1800 -2000? 
3) What percentage of human power in total energy consumption are used average in this time period for each country? 
4) What is the coal consumption trend from 1800 in Spain? 

All above questions have been solved in main_code file. We used R as analytic tool to do the data cleaning and analysis work.
Feel free to check out!
