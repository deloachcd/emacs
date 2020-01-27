#!/bin/bash -e
EMACS_VERSION=26.3
xargs -a apt-packages sudo apt install
curl http://mirrors.kernel.org/gnu/emacs/emacs-$EMACS_VERSION.tar.xz \
    -o emacs.tar.xz
tar -xf emacs.tar.xz
rm emacs.tar.xz
cd emacs-$EMACS_VERSION
./configure --without-x
make
sudo make install
cd ..
