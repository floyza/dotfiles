{ config, lib, pkgs, ... }:

let duckdns_hostname = "gdown";
in {
  systemd.services.duckdns = {
    description = "DuckDNS update job";
    startAt = "*:0/15";
    serviceConfig = { Type = "oneshot"; };
    script = ''
      PATH="${pkgs.curl}/bin:${pkgs.util-linux}/bin:$PATH"
      logger -t DuckDNS "Updating DuckDNS entries"
      EXITCODE=0
      TOKEN="$(cat /root/nix/duckdns-token.txt)"
      OUTPUT=$(curl -k -s "https://www.duckdns.org/update?domains=${duckdns_hostname}&token=$TOKEN&ip=")
      logger -t DuckDNS "$OUTPUT"
      if [ "$OUTPUT" == "KO" ]; then
          logger -t DuckDNS "You should check if your domain/token is correct because the server responded negatively!"
          EXITCODE=1
      fi

      exit $EXITCODE
    '';
  };
}
