#!/usr/bin/env python3
"""Perform dependency-free Phase 0 checks on elm.json."""

import json
import pathlib
import sys


path = pathlib.Path(sys.argv[1])
project = json.loads(path.read_text())
assert project["type"] == "application"
assert project["elm-version"] == "0.19.1"
for source_dir in project["source-directories"]:
    assert (path.parent / source_dir).is_dir(), source_dir
