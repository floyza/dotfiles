{ lib, stdenv, fetchgit, alsa-lib, curl, freeglut, freetype, gtk3, libjack2
, libusb1, mesa, pkg-config, python3Full, webkitgtk, winePackages, xorg, cmake
, gcc }:

stdenv.mkDerivation {
  name = "BespokeSynth";
  # NOTE: we specifically don't want the ZIP since the binary in there seems utterly broken.
  src = fetchgit {
    url = "http://github.com/BespokeSynth/BespokeSynth";
    rev = "46bff9be0d293d298e6230538c5af1adc467a0c5";
    # We need the JUCE library (TODO: make a separate package?)
    fetchSubmodules = true;
    sha256 = "0mngq9bly8qi4b9w5j63za5k14wn78rdv8ijxzprlv5m6m7assxd";
  };

  nativeBuildInputs = [ cmake gcc ];

  # Based on the bespoke_dependencies.sh file in the ZIP for linux (sorted alphabetically).
  # Except, cmake and g++ are in the nativeBuildInputs and there's an extra font package
  # since frabk.ttf (Franklin Gothic Book) doesn't seem to be available.
  buildInputs = [
    alsa-lib
    curl
    freeglut
    freetype
    gtk3
    libjack2
    libusb1
    mesa
    pkg-config
    python3Full
    webkitgtk
    # frabk.ttf notice me.
    winePackages.fonts
    xorg.libX11
    xorg.libXcomposite
    xorg.libXcursor
    xorg.libXinerama
    xorg.libXrandr
  ];
  configurePhase = "cmake -B ignore/build";
  buildPhase = "cmake --build ignore/build";
  installPhase = ''
    mv ignore/build/BespokeSynth_artefacts/Release $out
  '';
}
