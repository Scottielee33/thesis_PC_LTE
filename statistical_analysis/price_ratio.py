import pandas as pd
import scipy.stats as stats

# Load the datasets
cpu_df = pd.read_csv('final_data/CPU.csv')
disk_df = pd.read_csv('final_data/Disk.csv')
gpu_df = pd.read_csv('final_data/GPU.csv')
memory_df = pd.read_csv('final_data/Memory.csv')

# Preprocess the data and ensure date formats are consistent
cpu_df['ReleaseDate'] = pd.to_datetime(cpu_df['ReleaseDate'])
disk_df['ReleaseDate'] = pd.to_datetime(disk_df['ReleaseDate'])
gpu_df['ReleaseDate'] = pd.to_datetime(gpu_df['ReleaseDate'])
memory_df['ReleaseDate'] = pd.to_datetime(memory_df['ReleaseDate'])

# Sort the data by release date
cpu_df = cpu_df.sort_values('ReleaseDate')
disk_df = disk_df.sort_values('ReleaseDate')
gpu_df = gpu_df.sort_values('ReleaseDate')
memory_df = memory_df.sort_values('ReleaseDate')

# Check the unique quantiles in each dataframe
cpu_quantiles = cpu_df['Quantile'].unique()
disk_quantiles = disk_df['Quantile'].unique()
gpu_quantiles = gpu_df['Quantile'].unique()
memory_quantiles = memory_df['Quantile'].unique()

# Function to perform ANOVA test for a given dataframe and quantiles
def perform_anova(df, quantiles, component_name):
    results = []
    for quantile in quantiles:
        # Filter data for the current quantile
        quantile_data = df[df['Quantile'] == quantile]
        # Group data by release date and extract release prices
        grouped_data = quantile_data.groupby('ReleaseDate')['ReleasePrice'].apply(list)
        # Perform ANOVA
        f_val, p_val = stats.f_oneway(*grouped_data)
        results.append({'Component': component_name, 'Quantile': quantile, 'F-Value': f_val, 'P-Value': p_val})
    return results

# Perform ANOVA test for each component
cpu_anova_results = perform_anova(cpu_df, cpu_quantiles, 'CPU')
disk_anova_results = perform_anova(disk_df, disk_quantiles, 'Disk')
gpu_anova_results = perform_anova(gpu_df, gpu_quantiles, 'GPU')
memory_anova_results = perform_anova(memory_df, memory_quantiles, 'Memory')

# Combine results into one dataframe
anova_results = pd.DataFrame(cpu_anova_results + disk_anova_results + gpu_anova_results + memory_anova_results)

# Save the results to a CSV file
anova_results.to_csv('anova_results.csv', index=False)

anova_results.head()