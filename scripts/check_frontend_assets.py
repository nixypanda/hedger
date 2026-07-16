#!/usr/bin/env python3
"""Check Phase 3 PWA identity and API-cache separation."""

import json
import pathlib


root = pathlib.Path(__file__).resolve().parents[1]
frontend = root / "frontend-web"
manifest = json.loads((frontend / "public/manifest.webmanifest").read_text())
service_worker = (frontend / "public/sw.js").read_text()
runtime = (frontend / "src/main.js").read_text()

assert manifest["name"] == "Hedger"
assert manifest["short_name"] == "Hedger"
assert manifest["start_url"] == "/net-worth"
assert "iledger" not in json.dumps(manifest).lower()
assert 'url.pathname.startsWith("/api/")' in service_worker
assert 'url.pathname === "/api"' in service_worker
assert "hedger-app-shell-" in service_worker
assert "iledger" not in service_worker.lower()
assert 'const CONFIG_KEY = "hedger-preferences-v1"' in runtime
