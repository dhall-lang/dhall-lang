{ stdenv, fetchurl, unzip }:

stdenv.mkDerivation rec {
  name = "twitter-logo";

  src = fetchurl {
    name = "twitter-logos.zip";
    url = "https://about.twitter.com/content/dam/about-twitter/en/brand-toolkit/downloads/twitter-logo-01282021.zip";
    sha256 = "18ig7krfdqv8v77kiwig7fgz77zn692l4xky4xhlgfi3py86fn59";
  };

  sourceRoot = ".";

  buildInputs = [ unzip ];

  dontBuild = true;

  installPhase = ''
    mkdir $out
    cp -r * $out/
  '';
}
