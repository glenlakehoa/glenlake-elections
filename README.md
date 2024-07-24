# glenlake-elections
Tracking the votes for the Glenlake Upstate Homeowners Association, Inc Board elections


Usage to add new election years:

sources/votes-**%YYYY%**.json:
    start filling in the dates and votes received at those dates. **%YYYY%** is the current year, *e.g.* 2023

graphs/vote-tracking-**%YYYY%**.png:
    the vote tracker for the year **%YYYY%**, *e.g.* 2023. This files don't need to be handled on GitHub.

graphs/vote-tracking.png:
    the vote tracker for the current year, *e.g.* 2023. This files needs to be excluded from the .gitignore file if it is to be shared from GitHub
