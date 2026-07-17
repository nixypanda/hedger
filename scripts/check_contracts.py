#!/usr/bin/env python3
"""Validate the Phase 0 OpenAPI shape and representative JSON goldens."""

import json
import pathlib
import sys


contract_path = pathlib.Path(sys.argv[1])
golden_dir = pathlib.Path(sys.argv[2])
contract = json.loads(contract_path.read_text())

assert contract["openapi"].startswith("3.1.")
assert contract["servers"] == [{"url": "/api/v1"}]
assert {"/health/live", "/health/ready", "/bootstrap", "/transactions"} <= set(contract["paths"])
assert "parameters" not in contract["paths"]["/transactions"]["get"]

schemas = contract["components"]["schemas"]
for path in contract["paths"].values():
    for operation in path.values():
        assert operation["operationId"]
        for response in operation["responses"].values():
            schema = response.get("content", {}).get("application/json", {}).get("schema", {})
            if "$ref" in schema:
                name = schema["$ref"].removeprefix("#/components/schemas/")
                assert name in schemas, name

goldens = {path.name: json.loads(path.read_text()) for path in golden_dir.glob("*.json")}
assert goldens["bootstrap.json"]["fiscalYearStartMonth"] == 4
transactions = goldens["transactions.json"]
assert len(transactions["transactions"]) == 13
assert transactions["metadata"]["revision"] == goldens["bootstrap.json"]["metadata"]["revision"]
assert all("id" not in transaction and "sourceIndex" not in transaction for transaction in transactions["transactions"])
error = goldens["error-journal-unavailable.json"]["error"]
assert set(error) == {"code", "message", "requestId"}
