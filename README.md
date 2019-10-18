# FunStats2019 #

This is a R Shiny app used on the 2019 BGS Open Day for a simple demostration of collecting and analysing data. 
It takes basic travel information from visitors, and then produce a rough estimation of carbon footprint of the visitor's journey to the BGS head office in Keyworth, Nottingham.

* The app can be accessed through the link https://rapp-m.shinyapps.io/FunStats/

* To run the app locally in R, you will need the following R packages

  shiny, leaflet, maps, rgeos, geosphere, gmapsdistance, stringr, rdrop2

* You will also need a Google map API key. You can request this from Google (https://developers.google.com/maps/gmp-get-started). 
  The free version should cover enough enquires of travel distance (thousands of euquires a month) for general useage. 

* After all this is done, type runGitHub(repo='FunStats2019', username='GMY2018') to run the app.
