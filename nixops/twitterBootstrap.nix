{ stdenv, fetchurl, unzip }:

stdenv.mkDerivation rec {
  name = "bootstrap-${version}";
  version = "4.1.3";

  src = fetchurl {
    url = "https://github.com/twbs/bootstrap/releases/download/v${version}/bootstrap-${version}-dist.zip";
    sha256 = "0yr6mbqcb4mizpgi6nkzcb899q410dr30wd4wqj9w9pmn6jrrjgn";
  };

  sourceRoot = ".";

  buildInputs = [ unzip ];

  dontBuild = true;
  installPhase = ''
    mkdir $out
    cp -r {css,js} $out/
  '';

  meta = {
    description = "Front-end framework for faster and easier web development";
    homepage = http://getbootstrap.com/;
    license = stdenv.lib.licenses.mit;
  };

}
