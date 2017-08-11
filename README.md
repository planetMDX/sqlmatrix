# How different approaches for matrices in databases fare in dependency of the databasesize

Comparison between diiferent approaches of implemetation of matrices in a database: The serialization approach in SQLite, the read and write table from DBI approach in SQLite and the read and write table from DBI approach in PostgreSQL.
![barplot1](https://raw.githubusercontent.com/planetMDX/sqlmatrix/gh-pages/website/index_files/figure-html/unnamed-chunk-2-1.png)
![barplot2](https://raw.githubusercontent.com/planetMDX/sqlmatrix/gh-pages/website/index_files/figure-html/unnamed-chunk-3-1.png)
One can see, that both approaches, that aren't over PostgreSQL, suffer greatly from a big database.
Meanwhile PostgreSQL
![barplot3](https://raw.githubusercontent.com/planetMDX/sqlmatrix/gh-pages/website/index_files/figure-html/unnamed-chunk-4-1.png)
isn't affected by that.
![barplot4](https://raw.githubusercontent.com/planetMDX/sqlmatrix/gh-pages/website/index_files/figure-html/unnamed-chunk-5-1.png)
This is a close look on the connection times for the SQLite and PostgreSQL.

![barplot5](https://raw.githubusercontent.com/planetMDX/sqlmatrix/gh-pages/website/index_files/figure-html/unnamed-chunk-6-1.png)
If the connection to the database doesn't get opened and closed every time, the difference between a big and a small database becom smaller and the difference between PostgreSQL and SQLite also becomes smaller.
