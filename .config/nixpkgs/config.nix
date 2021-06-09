{
  allowUnfree = true;
  packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
      inherit pkgs;
    };
    factorio = pkgs.factorio.override {
      username = "gdown";
      token = "ba0ddd7629585d148e8faf94c24d37";
    };
  };
}
