import os
import pandas as pd
import requests
from bs4 import BeautifulSoup

# Function to scrape a specific table from a webpage
def scrape_table(url, table_id):
    # setting up headers to mimic a browser request
    headers = {'User-Agent': 'Mozilla/5.0'}
    response = requests.get(url, headers=headers)  # send GET request to the URL
    
    if not response.ok:  # check request status
        print(f"Failed to fetch URL: {url}")
        return None

    # parsing HTML content of the response
    soup = BeautifulSoup(response.content, 'lxml')

    table = None
    # searching for table using the specified table_id
    for comment in soup.find_all(string=lambda text: isinstance(text, str) and table_id in text):
        comment_soup = BeautifulSoup(comment, 'lxml')
        table = comment_soup.find('table', {'id': table_id})  # find the table by id
        if table:
            break

    # if no table is found in comments, try searching directly in the body of the page
    if not table:
        table = soup.find('table', {'id': table_id})

    # if no table is found at all, print an error and return None
    if not table:  
        print(f"Table '{table_id}' not found")
        return None

    # converting HTML table into pandas data frame
    df = pd.read_html(str(table))[0]
    # removing rows where the 'Rk' column has the value 'Rk'
    df = df[df['Rk'] != 'Rk']
    # resetting the data frame index to clean up
    df.reset_index(drop=True, inplace=True)
    return df

# Function to scrape and merge per-game and advanced stats for a given NBA season
def scrape_and_merge_stats(season: int, save_dir='nba_merged_stats'):
    os.makedirs(save_dir, exist_ok=True)  # Create the directory if it doesn't exist

    # URLs for per-game and advanced statistics
    per_game_url = f'https://www.basketball-reference.com/leagues/NBA_{season}_per_game.html'
    advanced_url = f'https://www.basketball-reference.com/leagues/NBA_{season}_advanced.html'

    # scraping the two tables given URLs
    per_game_df = scrape_table(per_game_url, 'per_game_stats')
    advanced_df = scrape_table(advanced_url, 'advanced')

    # checking if any of the tables failed to scrape data
    if per_game_df is None or advanced_df is None:
        print(f"Skipping season {season} due to missing data.")
        return

    # merging the per-game and advanced stats on common columns
    merged_df = pd.merge(per_game_df, advanced_df, on=['Player', 'Team', 'Age', 'Pos', 'G', 'GS', 'Awards'], how='inner')
    # dropping unnecessary columns from the merged data frame
    merged_df = merged_df.drop(columns=["Rk_y", "MP_y"])
    # adding the season column in the format "2021-22" for the given year
    merged_df['Season'] = f'{season-1}-{str(season)[-2:]}'

    # saving the merged data frame as a CSV file
    merged_df.to_csv(f"{save_dir}/nba_merged_{season}.csv", index=False)
    print(f"Merged stats saved for {season}: nba_merged_{season}.csv")

# looping through the range of years (2002 to 2024) and scraping/merging stats for each season
# 2002 was Lebron's rookie season, used to calculate Experience in NBA feature
for year in range(2002, 2025):
    scrape_and_merge_stats(year)

# Function to scrape player salaries data from a given URL and table ID
def scrape_salaries(url, table_id):
    headers = {'User-Agent': 'Mozilla/5.0'}
    response = requests.get(url, headers=headers)
    
    if not response.ok:  # check status request
        print(f"Failed to fetch URL: {url}")
        return None
    
    # Parse the HTML content
    soup = BeautifulSoup(response.content, 'lxml') 

    table = None
    # searching for the table in the HTML comments by table_id
    for comment in soup.find_all(string=lambda text: isinstance(text, str) and table_id in text):
        comment_soup = BeautifulSoup(comment, 'lxml')
        table = comment_soup.find('table', {'id': table_id})
        if table:
            break

    # if no table is found in comments, try searching directly in the body of the page
    if not table:
        table = soup.find('table', {'id': table_id})

    # if no table is found at all, print an error and return None
    if not table:
        print(f"Table '{table_id}' not found")
        return None

    # reading table into a data frame
    df = pd.read_html(str(table), skiprows=0)[0]
    # filtering rows where the 'Rk' column has the value 'Rk' (header rows)
    df = df[(df.iloc[:, 0] != 'Rk') & (df.iloc[:, 0].notna())]
    # dropping the level-0 column if it's a MultiIndex object
    df.columns = df.columns.droplevel(0) if isinstance(df.columns, pd.MultiIndex) else df.columns
    df.reset_index(drop=True, inplace=True)

    # converting 'Rk' to numeric and removing rows with NaN in 'Rk'
    df['Rk'] = pd.to_numeric(df['Rk'], errors='coerce')
    df.dropna(subset=['Rk'], inplace=True)
    df['Rk'] = df['Rk'].astype(int)

    return df

# player salary data URLs for 2023, 2024, 2025 seasons
salaries_2223_url = "https://web.archive.org/web/20230516051257/https://www.basketball-reference.com/contracts/players.html"
salaries_2324_url = "https://web.archive.org/web/20240605124914/https://www.basketball-reference.com/contracts/players.html"
salaries_2425_url = "https://www.basketball-reference.com/contracts/players.html"

# scraping salary data for the three seasons and saving them as CSVs
os.makedirs('nba_player_contracts', exist_ok=True)
salaries_2223_df = scrape_salaries(salaries_2223_url, "player-contracts")
salaries_2223_df.to_csv("nba_player_contracts/nba_contracts_2023.csv", index=False)
salaries_2324_df = scrape_salaries(salaries_2324_url, "player-contracts")
salaries_2324_df.to_csv("nba_player_contracts/nba_contracts_2024.csv", index=False)
salaries_2425_df = scrape_salaries(salaries_2425_url, "player-contracts")
salaries_2425_df.to_csv("nba_player_contracts/nba_contracts_2025.csv", index=False)