#!/usr/bin/env python3
"""Replays real API answers for the package's tests.

The files in tests/fixtures/ were recorded from a running Sondavi instance over
real HTTP, so this package is tested against shapes the API actually produces
rather than against invented ones.

Adds only the two behaviours a static file cannot express: the bearer check, and
a 429 with Retry-After — which is the one thing a hand-written client gets wrong.

    python3 tests/fixture-server.py [port]
"""
import json
import os
import re
import sys
from http.server import BaseHTTPRequestHandler, HTTPServer

HERE = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'fixtures')
TOKEN = 'sdv_' + 'T' * 48
SURVEY_ID = '135'
# The snapshot whose set lost a response after it was recorded.
ERASED_ID = '00000000-0000-4000-8000-000000000001'
state = {'flaky_left': 2, 'erased': False}


def load(name):
    with open(os.path.join(HERE, name), encoding='utf-8') as f:
        return json.load(f)


class Handler(BaseHTTPRequestHandler):
    def log_message(self, *a):
        pass

    def send_json(self, obj, status=200, headers=None):
        body = json.dumps(obj).encode()
        self.send_response(status)
        self.send_header('Content-Type', 'application/json')
        for k, v in (headers or {}).items():
            self.send_header(k, v)
        self.send_header('Content-Length', str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def _auth_ok(self):
        return self.headers.get('Authorization') == f'Bearer {TOKEN}'

    def _split(self):
        path, _, qs = self.path.partition('?')
        query = dict(p.split('=', 1) for p in qs.split('&') if '=' in p)
        return path, query

    def do_POST(self):
        if not self._auth_ok():
            return self.send_json({'message': 'Invalid or expired token.'}, 401)

        path, _ = self._split()
        if re.match(rf'^/api/v1/surveys/{SURVEY_ID}/snapshots$', path):
            return self.send_json(load('snapshot.json'), 201)
        return self.send_json({'message': 'Not found.'}, 404)

    def do_GET(self):
        if not self._auth_ok():
            return self.send_json({'message': 'Invalid or expired token.'}, 401)

        path, query = self._split()

        # `?flaky=1` answers 429 twice, then normally.
        if 'flaky' in query and state['flaky_left'] > 0:
            state['flaky_left'] -= 1
            return self.send_json({'message': 'Too many requests.'}, 429, {'Retry-After': '1'})

        if path == '/api/v1/surveys':
            return self.send_json(load('surveys.json'))

        if path == '/api/v1/snapshots':
            return self.send_json(load('snapshots.json'))

        m = re.match(r'^/api/v1/snapshots/([0-9a-f-]+)/responses$', path)
        if m:
            # A SECOND id, not a query parameter: a client builds the path from the
            # id alone, so anything else would only be reachable by a test that
            # misuses the client — and such a test passes for the wrong reasons.
            # This one replays the captured answer from AFTER a response was
            # deleted, which is the case the whole design exists for.
            name = ('snapshot_responses_after_erasure.json' if m.group(1) == ERASED_ID
                    else 'snapshot_responses.json')
            return self.send_json(load(name))

        m = re.match(r'^/api/v1/snapshots/([0-9a-f-]+)$', path)
        if m:
            return self.send_json(load('snapshot.json'))

        m = re.match(rf'^/api/v1/surveys/{SURVEY_ID}/codebook$', path)
        if m:
            return self.send_json(load('codebook.json'))

        m = re.match(rf'^/api/v1/surveys/{SURVEY_ID}/responses$', path)
        if m:
            page = load('responses_page1.json')
            if 'cursor' in query:
                page = {'data': page['data'][:1],
                        'meta': dict(page['meta'], count=1, remaining=0, next_cursor=None)}
                page['data'][0] = dict(page['data'][0], response_id=999, respondent_id='PNL-3')
            return self.send_json(page)

        return self.send_json({'message': 'Not found.'}, 404)


if __name__ == '__main__':
    port = int(sys.argv[1]) if len(sys.argv) > 1 else 8765
    HTTPServer(('127.0.0.1', port), Handler).serve_forever()
