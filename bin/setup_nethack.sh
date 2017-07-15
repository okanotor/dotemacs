#!/bin/bash

# wget http://sourceforge.net/projects/nethack/files/nethack/3.4.3/nethack-343-src.tgz
# wget http://download.savannah.gnu.org/releases/nethack-el/nethack_el-0.9.5.tar.gz

# wget -> decompress -> patch -> make -> make install

BASE_DIR=${HOME}/.emacs.d

cd ${BASE_DIR}/tmp
if [[ ! -e ${BASE_DIR}/tmp/nethack-343-src.tgz ]]; then
    wget http://sourceforge.net/projects/nethack/files/nethack/3.4.3/nethack-343-src.tgz
fi

if [[ ! -e ${BASE_DIR}/tmp/nethack_el-0.9.5.tar.gz ]]; then
    wget http://download.savannah.gnu.org/releases/nethack-el/nethack_el-0.9.5.tar.gz
fi

cd ${BASE_DIR}/usr/local/src
rm -rf nethack-3.4.3
rm -rf nethack_el-0.9.5

tar zxvf ${BASE_DIR}/tmp/nethack-343-src.tgz
tar zxvf ${BASE_DIR}/tmp/nethack_el-0.9.5.tar.gz

cd nethack-3.4.3
patch -p1 < ../nethack_el-0.9.5/enh-343.patch

sh sys/unix/setup.sh

