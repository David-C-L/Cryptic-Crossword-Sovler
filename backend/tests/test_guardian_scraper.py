import json

from utils.crossword_digitiser.json_to_crossword import CrosswordJsonProcessor
from utils.crossword_scrapers.guardian.guardian_scraper import scrape_everyman_crossword

TEST_DATA = {
    "grid": [
        ["1", "1", "1", "1", "1", "0", "1", "1", "1", "1", "1", "1", "1", "1", "1"],
        ["1", "0", "1", "0", "1", "0", "1", "0", "1", "0", "1", "0", "1", "0", "1"],
        ["1", "1", "1", "1", "1", "1", "1", "1", "1", "0", "1", "1", "1", "1", "1"],
        ["1", "0", "1", "0", "1", "0", "1", "0", "1", "0", "1", "0", "1", "0", "1"],
        ["0", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1"],
        ["1", "0", "1", "0", "1", "0", "0", "0", "1", "0", "1", "0", "1", "0", "1"],
        ["1", "1", "1", "1", "1", "1", "1", "0", "1", "1", "1", "1", "1", "1", "1"],
        ["1", "0", "1", "0", "0", "0", "1", "0", "1", "0", "0", "0", "1", "0", "1"],
        ["1", "1", "1", "1", "1", "1", "1", "0", "1", "1", "1", "1", "1", "1", "1"],
        ["1", "0", "1", "0", "1", "0", "1", "0", "0", "0", "1", "0", "1", "0", "1"],
        ["1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "0"],
        ["1", "0", "1", "0", "1", "0", "1", "0", "1", "0", "1", "0", "1", "0", "1"],
        ["1", "1", "1", "1", "1", '0', "1", "1", "1", "1", "1", "1", "1", "1", "1"],
        ["1", "0", "1", "0", "1", "0", "1", "0", "1", "0", "1", "0", "1", "0", "1"],
        ["1", "1", "1", "1", "1", "1", "1", "1", "1", "0", "1", "1", "1", "1", "1"]
    ],
    "across": {
        "1": {
            "clue": "Very fine one, a capital city",
            "length": [5]
        },
        "4": {
            "clue": "Occasionally available in a little Square in New York City?",
            "length": [9]
        },
        "9": {
            "clue": "Spoil artist put in lodgings for festival",
            "length": [5, 4]
        },
        "10": {
            "clue": "Element causing trouble among our sailors",
            "length": [5]
        },
        "11": {
            "clue": "End endless pain, getting treatment for complaint",
            "length": [4, 3, 7]
        },
        "13": {
            "clue": "Poem and piece of music heard",
            "length": [7]
        },
        "15": {
            "clue": "Get the better of other ranks in the open air",
            "length": [7]
        },
        "16": {
            "clue": "One used by downhill racers on their way to the top?",
            "length": [3, 4]
        },
        "18": {
            "clue": "Most of team must, suffering changes",
            "length": [7]
        },
        "20": {
            "clue": "Film barrister attending hostile meeting",
            "length": [5, 9]
        },
        "23": {
            "clue": "Well-known record, Dylan's first",
            "length": [5]
        },
        "24": {
            "clue": "Dance master, a bright star",
            "length": [5, 4]
        },
        "25": {
            "clue": "More than one itinerant in street waves",
            "length": [9]
        },
        "26": {
            "clue": "Western city to avoid",
            "length": [5]
        }
    },
    "down": {
        "1": {
            "clue": "House over in Lime Street",
            "length": [4]
        },
        "2": {
            "clue": "Senior politician from abroad, clergyman",
            "length": [7, 8]
        },
        "3": {
            "clue": "A series under discussion",
            "length": [2, 5]
        },
        "4": {
            "clue": "Father's name for a fascinating woman",
            "length": [5]
        },
        "5": {
            "clue": "Give incorrect gen to short schoolgirl in class",
            "length": [9]
        },
        "6": {
            "clue": "Una drops out of sporting competition in agony",
            "length": [7]
        },
        "7": {
            "clue": "Moderate fielder had to do, running after last of them",
            "length": [6, 2, 3, 4]
        },
        "8": {
            "clue": "Female singer showing tension, appearing after boy, leader of group",
            "length": [10]
        },
        "12": {
            "clue": "A warning sign? Meet with doctor",
            "length": [10]
        },
        "14": {
            "clue": "Let a nun be maltreated? That's indefensible",
            "length": [9]
        },
        "17": {
            "clue": "Sceptical in chess federation over Lasker's opening",
            "length": [7]
        },
        "19": {
            "clue": "Terriers drank rum with pot",
            "length": [7]
        },
        "21": {
            "clue": "Drop of ouzo when one's in watering hole",
            "length": [5]
        },
        "22": {
            "clue": "About to take off cloak",
            "length": [4]
        }
    }
}


def test_can_scrape_guardian():
    expected = CrosswordJsonProcessor.crossword_from_json(json.dumps(TEST_DATA))

    actual = scrape_everyman_crossword(3069)

    assert actual == expected
