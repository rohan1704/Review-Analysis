# Review Analysis

# What is Text analytics.
Internet is full of unstructured textual data. Text analytics is the way to unlock the meaning from all of this unstructured text. It lets you uncover patterns and topics. The traditional TRP rating system is used to measure the popularity of a show or channel, but in this busy and fast moving world people don't sit in front of television to watch their favourite show when they can watch them online, anywhere, anytime.Here, our traditional TRP system fails. Since lot of users like to post what they like and what they don't, online as review on various websites, we can get a lot of information from these reviews only if we can derieve meaning from these unstructured text. Review analysis is a part of text analysis, here meaningful patterns and sentiments is extracted from unstructured textual reviews to make proper decision.

## 1. Data Collection
The data for analysis has been collected from imdb.com website.
The data is collected real time from these websites to have a real time ratings of the shows.
[Arrow](http://www.imdb.com/title/tt2193021/)
[The Flash](http://www.imdb.com/title/tt3107288/)
[Constantine ](http://www.imdb.com/title/tt3489184/)
[Lucifer ](http://www.imdb.com/title/tt4052886/)
[Legends of Tomorrow ](http://www.imdb.com/title/tt4532368/)
[Supergirl](http://www.imdb.com/title/tt4016454/)

To gather data from these websites, web scraper was used and stored in csv format.

Considering that the users had given these reviews in the right frame of mind and there is no sarcasm in them, data was merged together in a single file 
with the following headings:

"Show"		: Show's name
"Date"		: Date of Review
"Review"	: Actual textual description of review
"Rating"	: User provided ratings.