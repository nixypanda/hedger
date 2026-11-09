# Deployment

Hedger is deployed with Nix. The flake exports:

- `packages.<system>.hedger-backend` — the Haskell server (statically linked
  executable only);
- `packages.<system>.hedger-frontend` — the production PWA in
  `share/hedger/`;
- `nixosModules.default` — the `services.hedger` NixOS module.

The module runs each deployment instance as a dedicated system user and serves
its frontend and `/api/*` backend under one nginx origin. An instance can list
several ledgers. Journal files remain outside the Nix store.

## Add the module

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

## Configure an instance

Configure named ledgers within an instance:

```nix
{
  services.hedger = {
    openFirewall = true;
    instances.personal = {
      enable = true;
      hostName = "ledger.example.net";
      port = 8080;
      defaultLedger = "personal";
      ledgers.personal = {
        label = "Personal";
        journalPath = "/var/lib/hedger/personal/main.journal";
        configPath = "/var/lib/hedger/personal/settings.yaml";
      };
      ledgers.business = {
        label = "Business";
        journalPath = "/var/lib/hedger/personal/business/main.journal";
        configPath = "/var/lib/hedger/personal/business/settings.yaml";
      };
      enableACME = true;
      forceSSL = true;
    };
  };
}
```

The module creates `/var/lib/hedger/personal` as `root:hedger-personal` with
mode `0750`. Grant that service user read access to every configured journal,
its include tree, and each referenced settings file. For journals managed by another group, set
`supplementaryGroups` accordingly.

Each ledger references a user-owned version-1 settings file through
`configPath`; display commodity, fiscal year, and investment mappings live in
that file (see [backend.md](backend.md)). The module generates version-3 JSON
configuration, which the backend reads as YAML. Only runtime paths, not journal
or settings contents, appear in that generated file. Omit `defaultLedger` to
show a ledger picker at the application root. Visitors can switch ledgers later
from Settings.

Existing NixOS configurations must move to `ledgers.<id>.journalPath` and
`ledgers.<id>.configPath`. Settings are no longer generated from
`defaultCommodity`, `fiscalYearStartMonth`, or `investments`; create the
referenced settings files before enabling the instance. `defaultLedger` should
be set only if the root should open one ledger automatically.

The backend listens only on `127.0.0.1`. nginx preserves `/api/*` (including the
ledger-scoped SSE streams), prevents API errors from falling through to the
PWA, provides SPA route fallback elsewhere, and marks hashed assets immutable.
Set `openFirewall = false` when an upstream proxy owns ingress.

All ledgers in one instance share its access policy and Unix process. Use
separate instances for ledgers with different audiences. Authentication is a
deployment concern. Merge access control into
`services.nginx.virtualHosts.<host>` or use `nginxExtraConfig`. In-application
authentication is on the roadmap; see [features.md](features.md).

See [`deploy/module.nix`](../deploy/module.nix) for every option and
[`deploy/example-ledger.nix`](../deploy/example-ledger.nix) for a complete
example.

## Reloads

The backend reloads each journal automatically when watched files change.
`SIGHUP` explicitly reloads every ledger:

```console
systemctl reload hedger-personal
```

The module wires that to `kill -HUP`. A failed reload keeps serving the last
successful snapshot.

## Operations

```console
systemctl status hedger-personal
systemctl reload hedger-personal
journalctl -u hedger-personal
curl --fail https://ledger.example.net/api/v1/health/ready
```

## Security posture

Each instance runs as its own unprivileged user with a hardened systemd unit:
no capabilities, `ProtectSystem = strict`, a read-only view of its data
directory and journal, private devices and `/tmp`, and restricted address
families and namespaces. It needs only network sockets and read access to its
journal.
