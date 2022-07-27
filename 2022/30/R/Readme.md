## Steps to create the IMDB ratings plot

1. Download the IMDB datasets from https://datasets.imdbws.com/. The datasets are documented at https://www.imdb.com/interfaces/
2. Place the tsv.gz archives in a folder named /data/imdb_input in your project directory.
3. Run **01_write_to_db.R** - adapt the base_path if needed
4. Run **02_episodes_plot.R** to create the chart
