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

  instanceType = lib.types.submodule (
    {
      config,
      name,
      ...
    }:
    {
      options = {
        enable = lib.mkEnableOption "this Hedger ledger instance";

        dataDir = lib.mkOption {
          type = lib.types.str;
          default = "/var/lib/hedger/${name}";
          description = ''
            Runtime directory for this ledger. It is created as root-owned and
            readable by the instance's dedicated group.
          '';
        };

        journalPath = lib.mkOption {
          type = lib.types.str;
          default = "${config.dataDir}/main.journal";
          description = ''
            Runtime path to the root journal. This is deliberately a string,
            not a Nix path, so the journal is never copied into the Nix store.
            Relative include paths are resolved by hledger from this file.
          '';
        };

        hostName = lib.mkOption {
          type = lib.types.str;
          description = "Hostname served by nginx for this ledger.";
          example = "ledger.example.net";
        };

        port = lib.mkOption {
          type = lib.types.port;
          default = 8080;
          description = "Unique localhost port for this instance's backend.";
        };

        defaultCommodity = lib.mkOption {
          type = lib.types.str;
          default = "INR";
          description = "Default display commodity exposed to the frontend.";
        };

        fiscalYearStartMonth = lib.mkOption {
          type = lib.types.ints.between 1 12;
          default = 4;
          description = "First month of the fiscal year, from 1 to 12.";
        };

        configFile = lib.mkOption {
          type = lib.types.nullOr lib.types.path;
          default = null;
          description = ''
            Optional version-1 Hedger YAML configuration, primarily for
            investment gain mappings. Instance options override its journal,
            listen address, port, display commodity, and fiscal-year values.
          '';
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

  backendArguments =
    instance:
    lib.optionals (instance.configFile != null) [
      "--config"
      (toString instance.configFile)
    ]
    ++ [
      "--journal"
      instance.journalPath
      "--listen-address"
      "127.0.0.1"
      "--port"
      (toString instance.port)
      "--display-commodity"
      instance.defaultCommodity
      "--fiscal-year-start"
      (toString instance.fiscalYearStartMonth)
    ];

  backendService =
    name: instance:
    lib.nameValuePair (serviceName name) {
      description = "Hedger backend for the ${name} ledger";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];

      restartTriggers = [
        cfg.backendPackage
      ]
      ++ lib.optional (instance.configFile != null) instance.configFile;

      serviceConfig = {
        Type = "exec";
        User = serviceUser name;
        Group = serviceUser name;
        SupplementaryGroups = instance.supplementaryGroups;
        WorkingDirectory = instance.dataDir;
        ExecStart = lib.escapeShellArgs (
          [ "${cfg.backendPackage}/bin/hedger-server" ] ++ backendArguments instance
        );
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
          instance.journalPath
        ];
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
      description = "Named, isolated Hedger deployments, one per ledger.";
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
          && lib.hasPrefix "/" instance.journalPath
          && instance.hostName != ""
          && builtins.match "^[^[:space:]]+$" instance.defaultCommodity != null;
        message = ''
          services.hedger.instances.${name} requires absolute dataDir and
          journalPath values, a non-empty hostName, and a whitespace-free
          defaultCommodity.
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
