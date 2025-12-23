#! /bin/sh
#
# Builds and installs  Emacs on MacOS with X11 support.
# Run this in the root of the Emacs source tree.
#
# requires:
#
# brew install xquartz
# brew install d12frosted/emacs-plus/emacs-plus@30 -- for treesitter lib
# brew install dylibbundler

PREFIX="/opt/xemacs"

HB_PREFIX="$(brew --prefix)"
TS_PREFIX="$(brew --prefix tree-sitter@0.25)"

export PKG_CONFIG_PATH="$TS_PREFIX/lib/pkgconfig:$HB_PREFIX/lib/pkgconfig:$HB_PREFIX/share/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"

export CPPFLAGS="-I$TS_PREFIX/include ${CPPFLAGS:-}"
export LDFLAGS="-L$TS_PREFIX/lib ${LDFLAGS:-}"
export LDFLAGS="-Wl,-headerpad_max_install_names ${LDFLAGS:-}"

make distclean

./configure --with-x --with-x-toolkit=lucid --with-ns=no \
			--with-tree-sitter \
			--with-jpeg=ifavailable --with-gif=ifavailable --with-tiff=ifavailable \
			--prefix=$PREFIX

make -j$(sysctl -n hw.ncpu)

sudo rm -rf $PREFIX
sudo make install

# Rebuild the executable to bundle in HomeBrew installed shared libraries. This
# protects the build from HomeBrew upgrades.

rm -rf emacs-bundle
mkdir -p emacs-bundle/{bin,lib}
cp -f ${PREFIX}/bin/emacs emacs-bundle/bin/

dylibbundler \
  -x emacs-bundle/bin/emacs \
  -b \
  -d emacs-bundle/lib \
  -p @executable_path/../lib \
  -s "$HB_PREFIX/lib" \
  -s "$HB_PREFIX/opt" \
  -i /opt/X11/lib \
  -i /usr/lib \
  -od

pushd emacs-bundle && sudo tar -cf ${PREFIX}/bundle.tar .
cd $PREFIX
sudo tar -xf bundle.tar && rm -f bundle.tar
popd
