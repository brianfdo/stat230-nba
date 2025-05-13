import pandas as pd
import numpy as np

# function to process player contracts
def player_contracts(filepath):
    # read the csv file into a dataframe
    df = pd.read_csv(filepath)
    # fill any missing values with 0.0 (assuming missing salary data)
    df = df.fillna(0.0)
    
    # iterate over specific columns (columns 3 to 10) that hold salary data
    for col in df.iloc[:, 3:10].columns:
        # remove dollar signs and commas, and convert to integer
        df[col] = df[col].replace('[\$,]', '', regex=True).astype(int)

    # get the column names for the current and next season
    current_season = df.iloc[:, 3:10].columns[0]
    next_season = df.iloc[:, 3:10].columns[1]

    # create a new column for contract year indicator
    indicator_col = f"{current_season}_contract_year"

    # contract year indicator: set to 1 if salary > 0 in current season, 0 in next season
    df[indicator_col] = (df[current_season].astype(float) > 0) & (df[next_season].astype(float) == 0)
    
    # convert the indicator column to integer type
    df[indicator_col] = df[indicator_col].astype(int)
    return df

# load player contracts for different seasons (2023, 2024, 2025)
salaries23 = player_contracts('nba_player_contracts/nba_contracts_2023.csv')
salaries24 = player_contracts('nba_player_contracts/nba_contracts_2024.csv')
salaries25 = player_contracts('nba_player_contracts/nba_contracts_2025.csv')

# create a dictionary to store the first year a player appeared in the league
seasons = list(range(2002, 2025))
player_first_year = {}

# iterate through each season's data
for year in seasons:
    # load merged stats data for each season
    df = pd.read_csv(f"nba_merged_stats/nba_merged_{year}.csv")
    # get the list of unique players in the current season
    players = df['Player'].dropna().unique()
    # store the first year a player appeared in the league
    for player in players:
        if player not in player_first_year:
            player_first_year[player] = year

# create 'Experience' feature for 2023 and 2024 seasons
target_year = 2023
df_2023 = pd.read_csv(f"nba_merged_stats/nba_merged_2023.csv")
df_2023['Experience'] = df_2023['Player'].map(lambda x: (2023 + 1) - player_first_year.get(x, 2023))

target_year = 2024
df_2024 = pd.read_csv(f"nba_merged_stats/nba_merged_2024.csv")
df_2024['Experience'] = df_2024['Player'].map(lambda x: (2024 + 1) - player_first_year.get(x, 2024))

# function to check if a player has won any major awards
def check_award_winner(x):
    awards = ["MVP-1", "DPOY-1", "MIP-1", "CPOY-1", "ROY-1", "6MOY-1"]
    # return 1 if the player has any of the specified awards
    for a in awards:
        if a in x.split(','):
            return 1
    return 0

# functions to check for specific all-NBA or all-defensive team appearances
def all_nba_team_1(x):
    award = 'NBA1'
    if award in x.split(','):
        return 1
    return 0

def all_nba_team_2(x):
    award = 'NBA2'
    if award in x.split(','):
        return 1
    return 0

def all_nba_team_3(x):
    award = 'NBA3'
    if award in x.split(','):
        return 1
    return 0

def all_defensive_1(x):
    award = 'DEF1'
    if award in x.split(','):
        return 1
    return 0

def all_defensive_2(x):
    award = 'DEF2'
    if award in x.split(','):
        return 1
    return 0

# create award-related features for the 2024 season
df_2024['Season'] = 2024
df_2024['NumOfAwards'] = df_2024['Awards'].apply(lambda x: len(x.split(',')) if pd.notna(x) else 0)
df_2024["All-Star"] = df_2024['Awards'].apply(lambda x: ("AS" in x) + 0 if pd.notna(x) else 0)
df_2024['AwardWinner'] = df_2024['Awards'].apply(lambda x: check_award_winner(x) if pd.notna(x) else 0)
df_2024['FirstTeam'] = df_2024['Awards'].apply(lambda x: all_nba_team_1(x) if pd.notna(x) else 0)
df_2024['SecondTeam'] = df_2024['Awards'].apply(lambda x: all_nba_team_2(x) if pd.notna(x) else 0)
df_2024['ThridTeam'] = df_2024['Awards'].apply(lambda x: all_nba_team_3(x) if pd.notna(x) else 0)
df_2024['DefTeam1'] = df_2024['Awards'].apply(lambda x: all_defensive_1(x) if pd.notna(x) else 0)
df_2024['DefTeam2'] = df_2024['Awards'].apply(lambda x: all_defensive_2(x) if pd.notna(x) else 0)

# repeat the same award-related feature creation for 2023 season
df_2023['Season'] = 2023
df_2023['NumOfAwards'] = df_2023['Awards'].apply(lambda x: len(x.split(',')) if pd.notna(x) else 0)
df_2023["All-Star"] = df_2023['Awards'].apply(lambda x: ("AS" in x) + 0 if pd.notna(x) else 0)
df_2023['AwardWinner'] = df_2023['Awards'].apply(lambda x: check_award_winner(x) if pd.notna(x) else 0)
df_2023['FirstTeam'] = df_2023['Awards'].apply(lambda x: all_nba_team_1(x) if pd.notna(x) else 0)
df_2023['SecondTeam'] = df_2023['Awards'].apply(lambda x: all_nba_team_2(x) if pd.notna(x) else 0)
df_2023['ThridTeam'] = df_2023['Awards'].apply(lambda x: all_nba_team_3(x) if pd.notna(x) else 0)
df_2023['DefTeam1'] = df_2023['Awards'].apply(lambda x: all_defensive_1(x) if pd.notna(x) else 0)
df_2023['DefTeam2'] = df_2023['Awards'].apply(lambda x: all_defensive_2(x) if pd.notna(x) else 0)

# function to check if the team name indicates multiple teams (for free agents)
import re

def is_multi_team(team):
    return bool(re.match(r'\d+TM', str(team)))

# function to fix team labels for players who played for multiple teams
def fix_team_labels(df):
    cleaned_rows = []
    
    # group by player and season
    grouped = df.groupby(['Player'])
    
    for (player), group in grouped:
        # handle players who have 'TM' in their team name (free agents or multiple teams)
        multi_team_row = group[group['Team'].apply(is_multi_team)]
        if not multi_team_row.empty:
            other_teams = group[~group['Team'].apply(is_multi_team)]
            if not other_teams.empty:
                last_team = other_teams.iloc[-1]['Team']
            else:
                last_team = None
            
            row = multi_team_row.iloc[0].copy()
            row['Team'] = last_team
            cleaned_rows.append(row)
        else:
            cleaned_rows.append(group.iloc[0])
    
    return pd.DataFrame(cleaned_rows)

# apply the team fixing function to the 2024 and 2023 dataframes
df_cleaned_2024 = fix_team_labels(df_2024)
df_cleaned_2023 = fix_team_labels(df_2023)

# merge player data with salary data for the 2024 season
full2024 = pd.merge(df_cleaned_2024, salaries24, on=['Player'])
# drop unnecessary columns from the merged dataframe
full2024 = full2024.drop(columns=['Rk', 'Tm', '2024-25', '2025-26', '2026-27', '2027-28', '2028-29'])
# rename the salary column
full2024 = full2024.rename(columns={'2023-24':"Salary"})
# update the salaries for 2024-2025 season and merge with 2024 data
salaries25_updated = salaries25.loc[:, ['Player', '2024-25', 'Guaranteed']]
salaries25_updated = salaries25_updated.rename(columns={'2024-25':"Next_Year_Salary", "Guaranteed":"Next_Year_Guaranteed"})
full2024_updated = pd.merge(full2024, salaries25_updated, on=['Player'], how='inner')

# save the updated data to a CSV file
full2024_updated.to_csv("final_2024_player.csv", index=False)

# repeat the same merging and cleaning process for the 2023 season
full2023 = pd.merge(df_cleaned_2023, salaries23, on=['Player'])
full2023 = full2023.drop(columns=['Rk', 'Tm', '2023-24', '2024-25', '2025-26', '2026-27', '2027-28'])
full2023 = full2023.rename(columns={'2022-23':"Salary"})
full2023.to_csv("final_2023_player.csv", index=False)
# update the salaries for 2023-2024 season and merge with 2023 data
salaries24_updated = salaries24.loc[:, ['Player', '2023-24', 'Guaranteed']]
salaries24_updated = salaries24_updated.rename(columns={'2023-24':"Next_Year_Salary", "Guaranteed":"Next_Year_Guaranteed"})
full2023_updated = pd.merge(full2023, salaries24_updated, on=['Player'], how='inner')

# save the updated data to a CSV file
full2023_updated.to_csv("final_2023_player.csv", index=False)
print('done')