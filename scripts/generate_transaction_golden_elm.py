#!/usr/bin/env python3
"""Generate or verify the Elm decoder fixture sourced from the backend golden."""

import argparse
import pathlib


ROOT = pathlib.Path(__file__).resolve().parent.parent
SOURCE = ROOT / "backend/test/golden/transactions.json"
TARGET = ROOT / "frontend-web/tests/TransactionGolden.elm"


def rendered_module() -> str:
    golden = SOURCE.read_text().strip().replace("\\", "\\\\")
    if '"""' in golden:
        raise ValueError("The transaction golden cannot be represented as an Elm multiline string.")
    return (
        "module TransactionGolden exposing (json)\n\n\n"
        "json : String\n"
        "json =\n"
        f'    """{golden}"""\n'
    )


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--check", action="store_true")
    arguments = parser.parse_args()
    expected = rendered_module()
    if arguments.check:
        if not TARGET.exists() or TARGET.read_text() != expected:
            raise SystemExit(
                "frontend-web/tests/TransactionGolden.elm is out of date; "
                "run scripts/generate_transaction_golden_elm.py"
            )
    else:
        TARGET.write_text(expected)


if __name__ == "__main__":
    main()
