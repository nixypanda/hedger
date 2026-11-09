{
  services.hedger = {
    # Leave this false when another firewall or reverse proxy fronts nginx.
    openFirewall = true;

    instances.personal = {
      enable = true;
      hostName = "ledger.example.net";

      port = 8080;
      defaultLedger = "personal";
      ledgers.personal = {
        label = "Personal";
        # The journal path stays a runtime string, outside the Nix store.
        journalPath = "/var/lib/hedger/personal/main.journal";
        defaultCommodity = "INR";
        fiscalYearStartMonth = 4;
      };

      # Enable these after the hostname resolves to this machine and ACME can
      # reach ports 80 and 443.
      enableACME = true;
      forceSSL = true;
    };
  };
}
