{ stdenv, fetchurl, unzip }:

stdenv.mkDerivation rec {
  name = "twitter-logo";

  src = fetchurl {
    name = "twitter-logos.zip";
    url = "https://about.twitter.com/content/dam/about-twitter/company/brand-resources/en_us/Twitter-Logos.zip";
    sha256 = "1dhqmj3krhak10yrq0zm89ld40a32ndrfnl6ligaaphf5dkff5m6";
  };

  sourceRoot = ".";

  buildInputs = [ unzip ];

  dontBuild = true;

  installPhase = ''
    mkdir $out
    cp -r * $out/
  '';
}
