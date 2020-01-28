{ stdenv, fetchurl, unzip }:

stdenv.mkDerivation rec {
  name = "github-logo";

  src = fetchurl {
    url = "https://github-media-downloads.s3.amazonaws.com/GitHub-Mark.zip";
    sha256 = "11znjcl0kwvws0wk600hlhq5z0mp7a0234zwryn20x6ib8qqb9v9";
  };

  buildInputs = [ unzip ];

  dontBuild = true;

  installPhase = ''
    mkdir $out
    cp -r * $out/
  '';
}
