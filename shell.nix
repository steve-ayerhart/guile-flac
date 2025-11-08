{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    # Build tools
    autoconf
    automake
    pkg-config

    # Guile and dependencies
    guile_3_0
    guile-gcrypt

    # Note: guile-bytestructures is not in nixpkgs
    # You may need to install it manually or add it to this environment
  ];

  shellHook = ''
    # Ensure pkg-config can find Guile
    export PKG_CONFIG_PATH="${pkgs.guile_3_0}/lib/pkgconfig:$PKG_CONFIG_PATH"

    # Set up Guile load paths including local bytestructures installation
    BYTESTRUCTURES_DIR="$HOME/Source/scheme-bytestructures/install"
    export GUILE_LOAD_PATH="$BYTESTRUCTURES_DIR/share/guile/site/3.0:${pkgs.guile-gcrypt}/share/guile/site/3.0:${pkgs.guile_3_0}/share/guile/site/3.0:$GUILE_LOAD_PATH"
    export GUILE_LOAD_COMPILED_PATH="$BYTESTRUCTURES_DIR/lib/guile/3.0/site-ccache:${pkgs.guile-gcrypt}/lib/guile/3.0/site-ccache:${pkgs.guile_3_0}/lib/guile/3.0/site-ccache:$GUILE_LOAD_COMPILED_PATH"

    echo "Guile FLAC development environment"
    echo "Guile version: $(guile --version | head -n1)"
    echo ""
    echo "To set up the build:"
    echo "  autoreconf -vif"
    echo "  ./configure"
    echo "  make"
  '';
}
