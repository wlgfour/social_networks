# Polarity in social networks on Reddit

This repository scrapes data from Reddit, processes it, labels comments with sentiment scores, creates graphical representations, and fits statistical models to the data. The associated report uses the data obtained to examine polarity as the tendency for positively or negatively labelled posts to incur like-minded responses.

The repository is written completely using the R programming language, and is self-contained. The data that was used for the report is provided, but in order to scrape more data, all of the scripts that were used for this project are included under `scripts/`:

1. `data_simulation.R`
2. `scrape_comments.R`
3. `clean_data.R`
4. `fit_model.R`

The only file that is not included in this repository is the `.rdb` file that contains the models generated by `fir_model.R`, which was too large to upload to GitHub (600Mb).

There were several issues with computational tractability, as well as memory management when it came to running the scripts for this project, so I recommend that if you are running the scripts on different data, try to keep under 200,000 comments, or find a way to break the scripts into chunks. The cleaning and fitting scripts are also fairly slow, as they are responsible for calculating sentiment labels and fitting models, respectively. They could both benefit from parallelization, but this comes second to the memory management issues that I mentioned before where R seems to crash on difficult or long operations.