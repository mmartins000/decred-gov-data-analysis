# Decred Governance Data Analysis

This project holds two scripts to extract and analyse data from Decred blockchain, along with Decred and Bitcoin USD prices.

### Folders

- csv: comma-separated value files
    - the majority of the files were generated with the execution of `dcr.sql`
    - `BTC-USD.csv` and `DCR-USD.csv` were obtained from Yahoo Finance at https://finance.yahoo.com/quote/DCR-USD/history?period1=1454716800&period2=1586131200&interval=1d&filter=history&frequency=1d and https://finance.yahoo.com/quote/BTC-USD/history?period1=1454716800&period2=1586131200&interval=1d&filter=history&frequency=1d
    - `difficulty.csv` was obtained from blockchain.com at https://www.blockchain.com/charts/difficulty
    - Privacy data was obtained from https://dcrdata.decred.org/api/chart/privacy-participation?axis=time&bin=day
    - Circulation data was obtained from https://dcrdata.decred.org/api/chart/stake-participation?axis=time&bin=day
- figures: figures and text file generated with the execution of `dcr.r`

### Pre-requisites

- To run `dcr.sql`:
    - A running version of dcrdata, Decred block explorer. Instructions to install dcrdata are available at [dcrdata: running your own block explorer](https://stakey.club/en/dcrdata-running-your-own-block-explorer/).
    - dcrdata depends on establishing a connection to a dcrd, Decred blockchain server. Instructions to install dcrd are available a [Installing dcrd](https://stakey.club/en/installing-dcrd/).
    - dcrd and dcrdata (and the underlying PostgreSQL) can be installed and executed from the same computer.
    - Start on step 1 of the next section

- To run only `dcr.r`:
    - Clone this repository on your local computer
    - Unpack `csv/psql_ticket-time-to-draw.csv.zip` (Github won't allow the upload of files with more than 100 MB)
    - Skip to step 3 of the next section

### Execution

0) Read the previous section before continuing.

1) To extract the data from the database: `dcr.sql`

- Login to the dcrdata server (via SSH), and run `dcr.sql` as postgres user. The last lines of article [Querying dcrdata](https://stakey.club/en/querying-dcrdata/) show instructions on how to do it.

2) To download the files to a local computer

- Download the generated csv files (default location points to `/tmp` folder) to a local computer using `scp` or a FTP tool, for example.

3) To execute the analysis on extracted data: `dcr.r.`

- On the local computer, open R Console or R Studio.
- Load or paste the contents of `dcr.r`.
- Execute `main()` function from the command prompt on R Console.
- The `figures` folder holds the files generated with the execution of `dcr.r`.

### Additional information

- About Decred Project: [https://decred.org](https://decred.org)
- About Stakey Club: [https://stakey.club](https://stakey.club)
