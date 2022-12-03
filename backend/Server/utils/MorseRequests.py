import json

import requests

from utils.sample_data.test_dataset import test_data

baseURL = "http://146.169.42.182"
query_morse = True


def set_config(config, query=True):
    global baseURL, query_morse
    baseURL = config.SOLVER if config.PRODUCTION else config.DUMMY
    query_morse = query


def solve_single_clue(clue, length, pattern=None):
    endpoint = '/solve'
    body = {'clue': clue, 'length': length}
    if pattern:
        body['pattern'] = pattern
    return query(endpoint, body)


def solve_single_clue_all(clue, length, pattern=None):
    endpoint = '/solveAll'
    body = {'clue': clue, 'length': length}
    if pattern:
        body['pattern'] = pattern
    return query(endpoint, body)


def query(endpoint, body, headers=None):
    global query_morse
    if query_morse:
        return query_solver(endpoint, body, headers)
    return query_test_data(body)


def query_test_data(body):
    result = [
        data
        for data in test_data
        if data['clue'].lower() == body['clue'].lower() and data['length'] == body['length']
    ][0]

    return {
        'hints': [],
        'results': [{
            'answer': result['answers'][0],
            'confidence': 1,
            'explanation': result['explanations'][0]
        }],
        'unlikely_answered': False
    }


def query_solver(endpoint, body, headers=None):
    print(body)
    if headers is None:
        headers = {'content-type': 'application/json'}
    global baseURL
    return requests.post(baseURL + endpoint, data=json.dumps(body), headers=headers).json()
