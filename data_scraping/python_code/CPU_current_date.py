import pandas as pd

from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium import webdriver
import time

df = pd.read_csv("data_scraping/scraped_data/cpu_data.csv")
df = df[['CPU', 'URL']]

# Initialize the Firefox driver
driver = webdriver.Firefox()

# Initialize the WebDriverWait
wait = WebDriverWait(driver, 10)

release_date = []
release_price = []
current_date = []
current_price = []

for index, row in df.iterrows():
# Open the webpage
    try:
        driver.get(row['URL'])

        if index==0:
            # Use the provided XPath to locate the "Accept" button
            accept_button_xpath = '/html/body/div[1]/div/div/div/div[2]/div/button[2]'
            time.sleep(0.2)
            wait.until(EC.presence_of_element_located((By.XPATH, accept_button_xpath)))

            # Locate the button again right before clicking it
            accept_button = driver.find_element(By.XPATH, accept_button_xpath)
            accept_button.click()

            # Wait a moment for the page to adjust after accepting cookies
            time.sleep(0.2)

        # Find the graph element
        graph_element = driver.find_element(By.ID, "price-history")

        # Scroll the element into view
        driver.execute_script("arguments[0].scrollIntoView(); window.scrollBy(0, -100);", graph_element)
        canvas = graph_element.find_elements(By.CLASS_NAME, "canvasjs-chart-canvas")

        width = canvas[-1].size['width']
        height = canvas[-1].size['height']
        viewport_width = driver.execute_script("return document.documentElement.clientWidth")
        viewport_height = driver.execute_script("return document.documentElement.clientHeight")

        graph_position = driver.execute_script("""
            var rect = arguments[0].getBoundingClientRect();
            return { top: rect.top, left: rect.left, bottom: rect.bottom, right: rect.right };
        """, graph_element)

        # Calculate the middle position between top and bottom
        middle_y = 175

        body = driver.find_element(By.TAG_NAME, "body")
        data_dict = {}
        time.sleep(0.5)
        # Ensure the x offsets are within the graph's left and right positions
        for x in [int((-(width/2) + 70) ), int((-(width/2) + 60) ), int((width/2)+30)]:
            action = ActionChains(driver)
            action.move_to_element_with_offset(graph_element, x, middle_y).perform()
            time.sleep(0.5)  # Add a delay here

            try:
                tooltip = graph_element.find_element(By.CLASS_NAME, "canvasjs-chart-tooltip")
                # Select the div element inside the tooltip
                div_element = tooltip.find_element(By.XPATH, "./div")
                inner_html = div_element.get_attribute('innerHTML')
                # Extract date and price from the string
                if inner_html:
                    date, price = inner_html.split("&nbsp;&nbsp;")
                    date = date[date.find(">")+1:date.rfind(":")]

                    # Create a dictionary
                    data_dict[date] = price
            except Exception as e:
                continue
        keys = list(data_dict.keys())
        print(data_dict)
        if len(keys) > 1:
            release_date.append(keys[0])
            release_price.append(data_dict[keys[0]])
            current_date.append(keys[1])
            current_price.append(data_dict[keys[1]])
            print(f"Date: {keys[0]} Price: {data_dict[keys[0]]}")
            print(f"Date: {keys[1]} Price: {data_dict[keys[1]]}")
        else:
            release_date.append(None)
            release_price.append(None)
            current_date.append(None)
            current_price.append(None)
        print(f"Finished {index+1} of {len(df)}")
        
    except Exception as e:
            release_date.append(None)
            release_price.append(None)
            current_date.append(None)
            current_price.append(None)

df['ReleaseDate'] = release_date
df['ReleasePrice'] = release_price
df['CurrentDate'] = current_date
df['CurrentPrice'] = current_price
df.to_csv('data_scraping/scraped_data/CPU_price_data.csv', index=False)
driver.quit()