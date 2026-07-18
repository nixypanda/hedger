#!/usr/bin/env python3
"""Validate the Phase 0 OpenAPI shape."""

import json
import pathlib
import sys


contract_path = pathlib.Path(sys.argv[1])
contract = json.loads(contract_path.read_text())

assert contract["openapi"].startswith("3.1.")
assert contract["servers"] == [{"url": "/api/v1"}]
assert {"/health/live", "/health/ready", "/bootstrap", "/transactions"} <= set(contract["paths"])
transaction_operation = contract["paths"]["/transactions"]["get"]
assert {parameter["name"] for parameter in transaction_operation["parameters"]} == {
    "query",
    "limit",
    "cursor",
}

schemas = contract["components"]["schemas"]
for path in contract["paths"].values():
    for operation in path.values():
        assert operation["operationId"]
        for response in operation["responses"].values():
            schema = response.get("content", {}).get("application/json", {}).get("schema", {})
            if "$ref" in schema:
                name = schema["$ref"].removeprefix("#/components/schemas/")
                assert name in schemas, name
