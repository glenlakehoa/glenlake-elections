# glenlake-elections
Tracking the votes for the Glen Lake Board elections


Usage to add new election years:

config.csv:
    add new meeting date and the number of homes for that particular year (usually 482)

sources/vote-tracking-**%YYYY%**.csv:
    start filling in the dates and votes received at those dates. **%YYYY%** is the current year, *e.g.* 2023

graphs/vote-tracking-**%YYYY%**.png:
    the vote tracker for the year **%YYYY%**, *e.g.* 2023. This files needs to be excluded from the .gitignore file if it is to be shared from GitHub

.gitignore:
    update `!graphs/vote-tracking-**%YYYY%**.png` for the current year **%YYYY%**, *e.g.* 2023