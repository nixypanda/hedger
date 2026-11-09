{ self }:
{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.hedger;
  enabledInstances = lib.filterAttrs (_: instance: instance.enable) cfg.instances;
  instanceNames = builtins.attrNames enabledInstances;

  ledgerType = lib.types.submodule (
    { name, ... }:
    {
      options = {
        label = lib.mkOption {
          type = lib.types.str;
          default = name;
          description = "Ledger label shown in the selector.";
        };

        journalPath = lib.mkOption {
          type = lib.types.str;
          description = "Absolute runtime path to this ledger's root journal.";
        };

        configPath = lib.mkOption {
          type = lib.types.str;
          description = "Absolute runtime path to this ledger's version-1 settings file.";
        };
      };
    }
  );

  instanceType = lib.types.submodule (
    {
      config,
      name,
      ...
    }:
    {
      options = {
        enable = lib.mkEnableOption "this Hedger deployment instance";

        dataDir = lib.mkOption {
          type = lib.types.str;
          default = "/var/lib/hedger/${name}";
          description = ''
            Runtime directory for this instance. It is created as root-owned and
            readable by the instance's dedicated group.
          '';
        };

        defaultLedger = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = "Optional ledger ID selected at the application root; without one, visitors choose a ledger.";
        };

        ledgers = lib.mkOption {
          type = lib.types.attrsOf ledgerType;
          default = { };
          description = "Named ledgers sharing this instance's access policy.";
        };

        hostName = lib.mkOption {
          type = lib.types.str;
          description = "Hostname served by nginx for this instance.";
          example = "ledger.example.net";
        };

        port = lib.mkOption {
          type = lib.types.port;
          default = 8080;
          description = "Unique localhost port for this instance's backend.";
        };

        supplementaryGroups = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ ];
          description = ''
            Additional groups granted to the dedicated service user. Use this
            when a journal tree is managed outside dataDir by another group.
          '';
        };

        enableACME = lib.mkEnableOption "automatic ACME certificates for this hostname";
        forceSSL = lib.mkEnableOption "HTTP-to-HTTPS redirects for this hostname";

        nginxExtraConfig = lib.mkOption {
          type = lib.types.lines;
          default = "";
          description = ''
            Extra nginx virtual-host configuration, for example access-control
            directives supplied by the consuming host.
          '';
        };
      };
    }
  );

  serviceName = name: "hedger-${name}";
  serviceUser = name: "hedger-${name}";

  backendConfig =
    name: instance:
    pkgs.writeText "hedger-${name}.json" (
      builtins.toJSON {
        version = 3;
        server = {
          listenAddress = "127.0.0.1";
          port = instance.port;
        };
        defaultLedger = instance.defaultLedger;
        ledgers = lib.mapAttrsToList (id: ledger: {
          inherit id;
          label = ledger.label;
          journalPath = ledger.journalPath;
          configPath = ledger.configPath;
        }) instance.ledgers;
      }
    );

  backendService =
    name: instance:
    lib.nameValuePair (serviceName name) {
      description = "Hedger backend for the ${name} instance";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];

      restartTriggers = [
        cfg.backendPackage
        (backendConfig name instance)
      ];

      serviceConfig = {
        Type = "exec";
        User = serviceUser name;
        Group = serviceUser name;
        SupplementaryGroups = instance.supplementaryGroups;
        WorkingDirectory = instance.dataDir;
        ExecStart = lib.escapeShellArgs ([
          "${cfg.backendPackage}/bin/hedger-server"
          "--config"
          (backendConfig name instance)
        ]);
        ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
        Restart = "on-failure";
        RestartSec = 5;
        UMask = "0077";

        # The service needs network sockets and read access to its journal, but
        # no writable filesystem or elevated kernel facilities.
        CapabilityBoundingSet = "";
        LockPersonality = true;
        MemoryDenyWriteExecute = true;
        NoNewPrivileges = true;
        PrivateDevices = true;
        PrivateTmp = true;
        ProtectClock = true;
        ProtectControlGroups = true;
        ProtectHome = true;
        ProtectHostname = true;
        ProtectKernelLogs = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        ProtectSystem = "strict";
        ReadOnlyPaths = [
          instance.dataDir
        ]
        ++ (lib.mapAttrsToList (_: ledger: ledger.journalPath) instance.ledgers)
        ++ (lib.mapAttrsToList (_: ledger: ledger.configPath) instance.ledgers);
        RestrictAddressFamilies = [
          "AF_INET"
          "AF_INET6"
          "AF_UNIX"
        ];
        RestrictNamespaces = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        SystemCallArchitectures = "native";
      };
    };

  nginxVirtualHost =
    _: instance:
    {
      root = "${cfg.frontendPackage}/share/hedger";

      locations = {
        "= /api" = {
          proxyPass = "http://127.0.0.1:${toString instance.port}";
          extraConfig = "proxy_intercept_errors off;";
        };

        "^~ /api/" = {
          proxyPass = "http://127.0.0.1:${toString instance.port}";
          extraConfig = "proxy_intercept_errors off;";
        };

        "~* ^/assets/.+-[0-9A-Za-z_-]+\\.(?:css|js|map|woff2?|png|svg|webp)$" = {
          extraConfig = ''
            expires 1y;
            add_header Cache-Control "public, max-age=31536000, immutable" always;
          '';
        };

        "= /index.html".extraConfig = ''
          expires -1;
          add_header Cache-Control "no-cache" always;
        '';

        "= /sw.js".extraConfig = ''
          expires -1;
          add_header Cache-Control "no-cache" always;
        '';

        "/".tryFiles = "$uri $uri/ /index.html";
      };
    }
    // lib.optionalAttrs instance.enableACME { enableACME = true; }
    // lib.optionalAttrs instance.forceSSL { forceSSL = true; }
    // lib.optionalAttrs (instance.nginxExtraConfig != "") {
      extraConfig = instance.nginxExtraConfig;
    };

  instanceValues = select: map (name: select name enabledInstances.${name}) instanceNames;
in
{
  options.services.hedger = {
    backendPackage = lib.mkOption {
      type = lib.types.package;
      default = self.packages.${pkgs.system}.hedger-backend;
      defaultText = lib.literalExpression "hedger.packages.\${pkgs.system}.hedger-backend";
      description = "Hedger backend package to deploy.";
    };

    frontendPackage = lib.mkOption {
      type = lib.types.package;
      default = self.packages.${pkgs.system}.hedger-frontend;
      defaultText = lib.literalExpression "hedger.packages.\${pkgs.system}.hedger-frontend";
      description = "Static Hedger frontend package served by nginx.";
    };

    openFirewall = lib.mkEnableOption "ports 80 and 443 as needed by enabled instances";

    instances = lib.mkOption {
      type = lib.types.attrsOf instanceType;
      default = { };
      description = "Named, isolated Hedger deployments, each with one or more ledgers.";
    };
  };

  config = lib.mkIf (enabledInstances != { }) {
    assertions = [
      {
        assertion = lib.all (name: builtins.match "^[a-z][a-z0-9_-]{0,23}$" name != null) instanceNames;
        message = ''
          services.hedger instance names must start with a lowercase letter,
          contain only lowercase letters, digits, underscores, or hyphens, and
          be no longer than 24 characters.
        '';
      }
      {
        assertion = lib.allUnique (instanceValues (_: instance: instance.port));
        message = "Every enabled services.hedger instance must use a unique port.";
      }
      {
        assertion = lib.allUnique (instanceValues (_: instance: instance.hostName));
        message = "Every enabled services.hedger instance must use a unique hostName.";
      }
      {
        assertion = lib.allUnique (instanceValues (_: instance: instance.dataDir));
        message = "Every enabled services.hedger instance must use a unique dataDir.";
      }
    ]
    ++ instanceValues (
      name: instance: {
        assertion =
          lib.hasPrefix "/" instance.dataDir
          && instance.hostName != ""
          && instance.ledgers != { }
          && (instance.defaultLedger == null || builtins.hasAttr instance.defaultLedger instance.ledgers)
          && lib.all (
            id:
            let
              ledger = instance.ledgers.${id};
            in
            builtins.match "^[a-z][a-z0-9-]{0,23}$" id != null
            && lib.hasPrefix "/" ledger.journalPath
            && builtins.match ".*[^[:space:]].*" ledger.label != null
            && lib.hasPrefix "/" ledger.configPath
          ) (builtins.attrNames instance.ledgers);
        message = ''
          services.hedger.instances.${name} requires valid
          ledger IDs, absolute journal and settings paths, and non-empty labels and hostName.
        '';
      }
    );

    users.groups = lib.mapAttrs' (name: _: lib.nameValuePair (serviceUser name) { }) enabledInstances;

    users.users = lib.mapAttrs' (
      name: instance:
      lib.nameValuePair (serviceUser name) {
        isSystemUser = true;
        group = serviceUser name;
        extraGroups = instance.supplementaryGroups;
        home = instance.dataDir;
      }
    ) enabledInstances;

    systemd.tmpfiles.rules = instanceValues (
      name: instance: "d ${instance.dataDir} 0750 root ${serviceUser name} -"
    );

    systemd.services = lib.mapAttrs' backendService enabledInstances;

    services.nginx = {
      enable = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedProxySettings = true;
      virtualHosts = lib.mapAttrs' (
        name: instance: lib.nameValuePair instance.hostName (nginxVirtualHost name instance)
      ) enabledInstances;
    };

    networking.firewall.allowedTCPPorts = lib.mkIf cfg.openFirewall (
      [ 80 ]
      ++ lib.optional (lib.any (
        _: instance: instance.enableACME || instance.forceSSL
      ) enabledInstances) 443
    );
  };
}
