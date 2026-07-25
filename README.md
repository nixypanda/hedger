# Hedger

Hedger is a backend-backed, read-only replica of eLedger. The repository has
completed Phase 2 of the implementation roadmap.

```sh
nix develop
just check
just serve
```

The API contract is in `contracts/openapi.json`; Release 1 decisions are in
`docs/api-decisions.md`; and the shared synthetic journal is in
`backend/test/fixtures/shared.journal`.

Pass a YAML configuration with `hedger-server --config hedger.yaml`; the
supported shape is documented in `hedger.example.yaml`. Journal, server, and
display settings may also be passed as command-line options, which take
precedence over YAML. Investment gain mappings are YAML-only. Run
`hedger-server --help` for the command-line options. The journal is always
supplied as a runtime path and is never embedded in the Nix package.

Release 1 uses the stock WAI access logger. Possible future structured logging
requirements are recorded in `docs/future-logging.md`.

The backend parses the configured journal into an immutable hledger snapshot
containing complete transactions and their postings. Reload failures retain
the last usable snapshot.
Send the server `SIGHUP` to parse, validate, and atomically publish a replacement.

Internally the backend follows a functional-core / imperative-shell design: all
ledger calculations live as pure functions in `Hedger.Domain.*` (net worth,
cashflow, activity, investment performance, valuation), behind a thin
application/query layer and a Servant HTTP layer. See `AGENTS.md` for the module
layout and layering rules.

## NixOS deployment

The flake exports:

- `packages.<system>.hedger-backend` — the Haskell server;
- `packages.<system>.hedger-frontend` — the production PWA in
  `share/hedger/`;
- `nixosModules.default` — the `services.hedger` NixOS module.

The module runs each ledger as a dedicated system user and serves its frontend
and `/api/*` backend under one nginx origin. It does not copy journals into the
Nix store. Add the flake and module to a NixOS host:

```nix
{
  inputs.hedger.url = "github:nixypanda/hedger";
  inputs.hedger.inputs.nixpkgs.follows = "nixpkgs";

  outputs = { nixpkgs, hedger, ... }: {
    nixosConfigurations.homelab = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        hedger.nixosModules.default
        ./hosts/homelab
      ];
    };
  };
}
```

Configure one instance per ledger:

```nix
{
  services.hedger = {
    openFirewall = true;
    instances.personal = {
      enable = true;
      hostName = "ledger.example.net";
      journalPath = "/var/lib/hedger/personal/main.journal";
      port = 8080;
      defaultCommodity = "INR";
      fiscalYearStartMonth = 4;
      enableACME = true;
      forceSSL = true;
    };
  };
}
```

The module creates `/var/lib/hedger/personal` as `root:hedger-personal` with
mode `0750`. Provision the journal tree there with group-readable files. For a
journal managed elsewhere, set `supplementaryGroups` to a group that can read
the complete include tree. `configFile` may point to a version-1 YAML file when
investment gain mappings are needed; the instance options still control the
journal, listener, and display defaults.

The backend listens only on `127.0.0.1`. nginx preserves `/api/*`, prevents API
errors from falling through to the PWA, provides SPA route fallback elsewhere,
and marks hashed assets immutable. Set `openFirewall = false` when an upstream
proxy owns ingress. Authentication remains a deployment concern; merge access
control into `services.nginx.virtualHosts.<host>` or use `nginxExtraConfig`.

Useful operations for the example instance are:

```sh
systemctl status hedger-personal
systemctl reload hedger-personal
journalctl -u hedger-personal
curl --fail https://ledger.example.net/api/v1/health/ready
```
