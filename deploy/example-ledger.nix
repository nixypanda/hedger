{
  services.hedger = {
    # Leave this false when another firewall or reverse proxy fronts nginx.
    openFirewall = true;

    instances.personal = {
      enable = true;
      hostName = "ledger.example.net";

      # The module creates /var/lib/hedger/personal for runtime data. The
      # journal remains a runtime string and is never copied into the store.
      journalPath = "/var/lib/hedger/personal/main.journal";
      port = 8080;
      defaultCommodity = "INR";
      fiscalYearStartMonth = 4;

      # Enable these after the hostname resolves to this machine and ACME can
      # reach ports 80 and 443.
      enableACME = true;
      forceSSL = true;
    };
  };
}
