import csv
import re
from pathlib import Path
from typing import Callable

import requests
from bs4 import BeautifulSoup
from wordcloud import WordCloud, STOPWORDS
import matplotlib.pyplot as plt


def scrape_module_webpage(webpage: str) -> str:
    # Send a GET request to the webpage
    response = requests.get(webpage)

    # Parse the HTML content using BeautifulSoup
    soup = BeautifulSoup(response.content, "html.parser")

    if "warwick" in webpage:
        return extract_warwick_module(soup)
    return extract_edinburgh_module(soup)


def extract_warwick_module(soup: BeautifulSoup) -> str:
    desired_headings = [
        "Introductory description",
        "Outline syllabus",
        "Subject specific skills",
        "Transferable skills",
    ]

    # Find all h5 headings
    h5_headings = soup.find_all("h5")

    # Extract the content for desired h5 headings
    content = []
    for heading in h5_headings:
        if heading.text.strip() in desired_headings:
            # Find all siblings after the current heading until the next h5 heading
            siblings = []
            sibling = heading.next_sibling
            while sibling and sibling.name != "h5":
                siblings.append(sibling)
                sibling = sibling.next_sibling

            # Join the sibling texts and remove HTML tags
            content_text = " ".join(str(sibling) for sibling in siblings)
            content_text = re.sub("<.*?>", " ", content_text)
            content.append(content_text)

    # Print the extracted content
    return " ".join([c.strip() for c in content])


def extract_edinburgh_module(soup: BeautifulSoup) -> str:
    out = []
    tag = ["Course description", "Summary", "Other requirements"]

    for t in tag:
        td = soup.find("td", class_="rowhead1", text=t)

        content_td = td.find_next_sibling("td")
        description = content_td.get_text(strip=True)
        out.append(description)
    return " ".join(out)


def extract_columns(file: Path, id_col: str, field: str):
    with open(file) as csv_file:
        reader = csv.DictReader(csv_file)
        reader.fieldnames = [name.strip("\ufeff") for name in reader.fieldnames]

        extracted_data = {}
        for row in reader:
            extracted_data[row[id_col]] = row[field]
    return extracted_data
