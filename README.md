# Restaurant-Analysis

Running a thriving local restaurant isn't always as charming as first impressions appear. There are often all sorts of unexpected troubles popping up that could hurt business.

One common predicament is that restaurants need to know how many customers to expect each day to effectively purchase ingredients and schedule staff members. This forecast isn't easy to make because many unpredictable factors affect restaurant attendance, like weather and local competition. It's even harder for newer restaurants with little historical data.

Recruit Holdings has unique access to key datasets that could make automated future customer prediction possible. Specifically, Recruit Holdings owns Hot Pepper Gourmet (a restaurant review service), AirREGI (a restaurant point of sales service), and Restaurant Board (reservation log management software).

The data comes in the shape of 8 relational files which are derived from two separate Japanese websites that collect user information: “Hot Pepper Gourmet (hpg): similar to Yelp” (search and reserve) and “AirREGI / Restaurant Board (air): similar to Square” (reservation control and cash register). The training data is based on the time range of Jan 2016 - most of Apr 2017, while the test set includes the last week of Apr plus May 2017. The test data “intentionally spans a holiday week in Japan called the ‘Golden Week.’ The data description further notes that:”There are days in the test set where the restaurant were closed and had no visitors. These are ignored in scoring. The training set omits days where the restaurants were closed."

Those are the individual files:

air_visit_data.csv: historical visit data for the air restaurants. This is essentially the main training data set.

air_reserve.csv / hpg_reserve.csv: reservations made through the air / hpg systems.

air_store_info.csv / hpg_store_info.csv: details about the air / hpg restaurants including genre and location.

store_id_relation.csv: connects the air and hpg ids

date_info.csv: essentially flags the Japanese holidays.

sample_submission.csv: serves as the test set. The id is formed by combining the air id with the visit date.

I have created a shiny R application which demonstrates restaurant visitors and reservation analysis. Please find below Shiny R app link to access it: https://nikhil91.shinyapps.io/Restaurant_analysis/

