# nutrientfootprint
An analysis of the water-footprints of foods with respect to their protein content.
Author: Stephen Franklin

## Background
In February 2014, the [California drought](http://en.wikipedia.org/wiki/Climate_change_in_California#Drought) reached for the first time in the 54-year history of the State Water Project to shortages of water supplies. 
on April 1, 2015, lack of water due to low snowpack prompted California governor Jerry Brown to order mandatory water restrictions that would reduce municipal water use by 25%.
Those restrictions focused on municipal water use. 
http://www.waterboards.ca.gov/waterrights/water_issues/programs/drought/docs/040115_executive_order.pdf  
http://www.waterboards.ca.gov/waterrights/water_issues/programs/drought/emergency_regulations_waterconservation.shtml

Articles explaining how agricultural water use is much higher than municipal use:
http://www.sacbee.com/opinion/op-ed/soapbox/article17332904.html  
http://www.thedailybeast.com/articles/2015/03/30/how-growers-gamed-california-s-drought.html

Articles that showed the almond as symbolic of ineffective water restrictions:  
http://www.thedailybeast.com/articles/2015/03/30/how-growers-gamed-california-s-drought.html  
http://www.splendidtable.org/story/farming-in-californias-drought-almonds-take-more-total-water-than-any-other-crop  
http://www.motherjones.com/environment/2015/01/california-drought-almonds-water-use  
http://www.motherjones.com/environment/2014/02/wheres-californias-water-going  
http://www.motherjones.com/environment/2015/04/real-problem-almonds  
http://gizmodo.com/how-the-drought-is-devastating-californias-1-food-exp-1517881858  
http://www.bloombergview.com/articles/2015-04-07/california-s-almond-farmers-have-become-a-target

Articles denouncing the over-emphasis of almonds as a water-intensive crop in favor of alfalfa:
http://gizmodo.com/seriously-stop-demonizing-almonds-1696065939  
http://www.alternet.org/environment/you-boycott-all-california-almonds-read  
http://www.slate.com/articles/business/moneybox/2015/04/almonds_in_california_they_use_up_a_lot_of_water_but_they_deserve_a_place.html  
http://news.nationalgeographic.com/2015/04/150409-water-agriculture-cattle-dairy-conservation-ngfood/

## Data

Plants
http://waterfootprint.org/media/downloads/Report47-Appendix-II.zip

Animals
http://waterfootprint.org/media/downloads/Report48-Appendix-V.zip

Nutrition
http://ndb.nal.usda.gov/ndb/doc/index

## Works
`water_footprint.R`: An R script which downloads the datasets and returns the data table: `water_footprint_table.RData`.
`water_footprint_analysis.Rmd`: An R Markdown document which explores data models.
`water_nutritional_efficiency.Rmd`: The published report.





