# NixOS deployment

The flake exports:

- `packages.<system>.hedger-backend` — the Haskell server;
- `packages.<system>.hedger-frontend` — the production PWA in
  `share/hedger/`;
- `nixosModules.default` — the `services.hedger` NixOS module.

The module runs each ledger as a dedicated system user and serves its frontend
and `/api/*` backend under one nginx origin. It does not copy journals into the
Nix store.

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
the complete include tree.

`configFile` may point to a version-1 YAML file for investment portfolio and
gain mappings. Instance options still control the journal, listener, and
display defaults. The configured `investments.assetClasses` account prefixes
define which accounts appear in the Investments view.

The backend listens only on `127.0.0.1`. nginx preserves `/api/*`, prevents API
errors from falling through to the PWA, provides SPA route fallback elsewhere,
and marks hashed assets immutable. Set `openFirewall = false` when an upstream
proxy owns ingress.

Authentication is a deployment concern. Merge access control into
`services.nginx.virtualHosts.<host>` or use `nginxExtraConfig`.

See [`deploy/module.nix`](../deploy/module.nix) for every option and
[`deploy/example-ledger.nix`](../deploy/example-ledger.nix) for a complete
example.

## Operations

```console
systemctl status hedger-personal
systemctl reload hedger-personal
journalctl -u hedger-personal
curl --fail https://ledger.example.net/api/v1/health/ready
```
