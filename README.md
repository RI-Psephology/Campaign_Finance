# Campaign_Finance

App to disseminate publicly available Rhode Island campaign finance data.

Data provided by the RI Board of Elections http://www.elections.state.ri.us/ and dates back to 2002.

RI campaign contributions located here, http://ricampaignfinance.com/RIPublic/Contributions.aspx

RI campaign expenditures, http://ricampaignfinance.com/RIPublic/Expenditures.aspx

A computer interprets "Smith, Susan A" differently than "Smith, Susan", or "Smith, Ms Susan", which creates difficulty in obtaining meaningful results.  At least one employer had over 65 unique spelling variations.

Considerable effort was made to consolidate duplicate names.  Code used to clean the data is provided.  In some cases, mistakes may have been made.  Please contact the author to report any issues.  Every effort will be made to promptly fix any mistakes.  

In assigning employers to an industry, the North American Industry Classification System (NAICS), https://www.census.gov/cgi-bin/sssd/naics/naicsrch?chart=2017 and the Bureau of Labor Statistics https://www.bls.gov/iag/tgs/iag_index_naics.htm were used as guides.  Final assignment is a hodgepodge of the author's best guess and leaves room for considerable improvement.  

