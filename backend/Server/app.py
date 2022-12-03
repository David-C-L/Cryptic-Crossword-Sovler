import re
import time

import config
import cv2
import io
import json
import numpy as np
import os
import utils.backtracker.conversion_utils as conversion_utils
import utils.MorseRequests as MorseRequests
import utils.sample_data.test_dataset as sample_data

from flask import Flask, request, Response
from flask_cors import CORS, cross_origin
from flask_sse import sse
from dotenv import load_dotenv

from utils.hint_parser import hint_parser
from utils.hint_parser.hint_parser import MorseHintParser
from utils.backtracker.neighbour_backtracker import solve_crossword
from utils.crossword_digitiser import image_to_crossword
from utils.crossword_scrapers.guardian.guardian_scraper import scrape_everyman_crossword, \
    scrape_quiptic_crossword, scrape_cryptic_crossword

app = Flask(__name__)
cors = CORS(app)

app.config["REDIS_URL"] = "redis://localhost:7777" if os.environ.get("REDIS_URL") is None else os.environ.get(
    "REDIS_URL")

print("Redis URL: " + str(app.config["REDIS_URL"]))

app.register_blueprint(sse, url_prefix='/stream')
MorseRequests.set_config(config.Config())
conversion_utils.set_config(config.Config())

# Acceptable file formats for image upload.
FILE_FORMATS = {'png', 'jpg', 'jpeg'}
TESSERACT_PATH = "/usr/bin/tesseract"


# To read json element
# r_dict = json.loads(request.data)


@app.route("/")
@app.route("/healthcheck")
@cross_origin()
def home():
    return "<h1>Server is running</h1>"


# CLUE is the string we pass to Morse
# LENGTH is the length of answer required
# returns first answer given by Morse

# JSON sent to us has keys clue and length
@app.route("/solve", methods=['POST'])
@cross_origin()
def solve():
    try:
        r_dict = json.loads(request.data)
    except json.decoder.JSONDecodeError:
        return Response("{'error': 'No JSON was provided'}", status=400, mimetype='application/json')

    clue = r_dict['clue']
    length = r_dict['length']
    if not config.Config.PRODUCTION:
        ans = sample_data.findClue(clue, length)
        if ans == sample_data.CLUE_NOT_FOUND:
            body = {'morse_answered': False,
                    'results': []}
        else:
            body = {'morse_answered': True,
                    'results': [{"answer": ans['answers'][0], "explanation": ans['explanations'][0]}]}
        return json.dumps(body)
    else:
        lens = [int(x) for x in re.split(r"\s*,\s*", length)]
        query_res = MorseRequests.solve_single_clue(clue, lens)
        morse_hint_parser = MorseHintParser()
        for res in query_res['results']:
            exp = res['explanation']
            if "\n" not in exp:
                res["hints"] = morse_hint_parser.parse(exp)
            else:
                res["hints"] = hint_parser.unlikely_hint_parser(exp)

        return query_res


@app.route("/everyman/<num>", methods=['GET', 'POST'])
@cross_origin()
def getEveryman(num):
    crossword = scrape_everyman_crossword(num)
    return crossword_response(crossword)


@app.route("/quiptic/<num>", methods=['GET', 'POST'])
@cross_origin()
def getQuiptic(num):
    crossword = scrape_quiptic_crossword(num)
    return crossword_response(crossword)


@app.route("/cryptic/<num>", methods=['GET', 'POST'])
@cross_origin()
def getCryptic(num):
    crossword = scrape_cryptic_crossword(num)
    return crossword_response(crossword)


def crossword_response(crossword):
    sse.publish({}, type="change_screen")
    time.sleep(2)
    print(crossword.export_crossword())
    sse.publish(crossword.export_crossword(), type="show_grid")
    print("published")
    solve_crossword(crossword)
    crossword.empty_last_filled()
    sse.publish(crossword.export_crossword(True), type="show_grid")
    return Response(json.dumps(crossword.export_crossword()), status=200, mimetype="application/json")


def correct_format(filename: str) -> bool:
    return '.' in filename and filename.rsplit('.', 1)[1].lower() in FILE_FORMATS


def check_file(files, key):
    if key not in files:
        print("{} not in files".format(key))
        return Response("error: No {} image was received".format(key),
                        status=400, mimetype='application/json')
    if not (correct_format(files[key].filename)):
        return Response("error: {} image was not in the correct format".format(files[key].filename),
                        status=400, mimetype='application/json')
    return None


def extract_images(files):
    image_files = ['grid', 'across', 'down']
    cv2_images = []

    for filename in image_files:
        error = check_file(files, filename)
        if error is not None:
            return None, None, None, error
        image = files[filename]
        in_memory_file = io.BytesIO()
        image.save(in_memory_file)
        data = np.frombuffer(in_memory_file.getvalue(), dtype=np.uint8)
        color_image_flag = 1
        cv2_images.append(cv2.imdecode(data, color_image_flag))

    return *cv2_images, None


def check_form(form, key):
    if key not in form:
        return Response("error: {} value not present in request".format(key),
                        status=400, mimetype='application/json')
    return None


def extract_shape(form):
    col_error = check_form(form, 'noOfColumns')
    row_error = check_form(form, 'noOfRows')
    if (col_error is not None) or (row_error is not None):
        return 0, 0, (col_error if col_error is not None else row_error)

    n_cols = int(request.form['noOfColumns'])
    n_rows = int(request.form['noOfRows'])
    return n_cols, n_rows, None


@app.route("/uploadImage", methods=['POST'])
@cross_origin()
def upload_image():
    grid, across, down, error = extract_images(request.files)
    if error is not None:
        return error

    n_cols, n_rows, error = extract_shape(request.form)
    if error is not None:
        return error

    crossword_puzzle = image_to_crossword.CrosswordImageProcessor.crossword_from_images(
        TESSERACT_PATH, grid, across, down, n_rows, n_cols)

    solve_crossword(crossword_puzzle)
    crossword = crossword_puzzle.export_crossword()
    return json.dumps(crossword)


if __name__ == "__main__":
    load_dotenv('.env')
    MorseRequests.set_config(config.Config())
    conversion_utils.set_config(config.Config())
    port = int(os.environ.get('PORT', 5000))
    app.run(host='0.0.0.0', port=port)
