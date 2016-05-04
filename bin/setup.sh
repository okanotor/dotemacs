#!/bin/bash
#
# Setup emacs environment.

################################################################################
# Constants
################################################################################
declare -r EMACS_USER_DIR=~/.emacs.d

declare -r EMACS_VERSION=24.5
declare -r EMACS_PATCH_VERSION=5.17

declare -r RCODETOOLS_VERSION=0.8.5.0

declare -r HOWM_VERSION=1.4.3

declare -r SQLITE3_YEAR=2016
declare -r SQLITE3_VERSION_X=12
declare -r SQLITE3_VERSION_Y=2
declare -r SQLITE3_VERSION_Z=0


################################################################################
# Functions
################################################################################

################################################################################
# Output error message to STDERR.
# Globals:
#   None
# Arguments:
#   message
# Returns:
#   None
################################################################################
err() {
  echo "[$(date +'%Y-%m-%dT%H:%M:%S%z')] $@" >&2
}

################################################################################
# Output error message and abort this script.
# Globals:
#   None
# Arguments:
#   message
# Returns:
#   None
################################################################################
abort() {
  err $@
  exit 1
}

################################################################################
# Check if command is installed. If not installed. abort this script.
# Globals:
#   None
# Arguments:
#   command
# Returns:
#   None
################################################################################
has_installed() {
  if [[ ! -x "$(which $1)" ]]; then
    abort "$1 is not installed. Please install $1 before executing this script."
  fi
}

################################################################################
# Clone project from Github.com.
# Globals:
#   None
# Arguments:
#   project
#   user
#   dir
# Returns:
#   None
################################################################################
clone_from_github() {
  local project="$1"
  local user="$2"
  local dir="$3"

  cd ${dir}
  if [[ -e ./${project} ]]; then
    echo "${project} has been checkouted already."
  else
    git clone https://github.com/${user}/${project}.git
  fi
}

################################################################################
# Install rcodetools
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
################################################################################
install_rcodetools() {
  # download .gem file
  cd ${EMACS_USER_DIR}/tmp
  if [[ -e ./rcodetools-${RCODETOOLS_VERSION}.gem ]]; then
    echo "rcodetools-${RCODETOOLS_VERSION}.gem has been downloaded already."
  else
    wget https://rubygems.org/downloads/rcodetools-${RCODETOOLS_VERSION}.gem
  fi

  # install
  cd ${EMACS_USER_DIR}/lib/elisp/other
  if [[ -e ./rcodetools ]]; then
    echo "rcodetools has been unpacked already."
  else
    if [[ -e ${EMACS_USER_DIR}/tmp/rcodetools-${RCODETOOLS_VERSION}.gem ]]; then
      gem unpack ${EMACS_USER_DIR}/tmp/rcodetools-${RCODETOOLS_VERSION}.gem
      mv -iv rcodetools-${RCODETOOLS_VERSION} rcodetools
    else
      abort "rcodetools-${RCODETOOLS_VERSION}.gem is not downloaded."
    fi
  fi
}

################################################################################
# Install howm
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
################################################################################
install_howm() {
  # download .tar.gz file
  cd ${EMACS_USER_DIR}/tmp
  if [[ -e howm-${HOWM_VERSION}.tar.gz ]]; then
    echo "howm-${HOWM_VERSION}.tar.gz has been downloaded already."
  else
    wget http://howm.sourceforge.jp/a/howm-${HOWM_VERSION}.tar.gz
  fi

  # install
  cd ${EMACS_USER_DIR}/lib/elisp/other
  if [ -e howm ]; then
    echo "howm has been extracted already."
  else
    if [[ -e ${EMACS_USER_DIR}/tmp/howm-${HOWM_VERSION}.tar.gz ]]; then
      # extract
      tar zxvf ${EMACS_USER_DIR}/tmp/howm-${HOWM_VERSION}.tar.gz
      mv -iv howm-${HOWM_VERSION} howm
      
      # convert character code
      cd howm
  
      # ISO-2022-JP -> UTF-8
      for f in howm-lang-ja.el \
               howm-menu-ja.el; do
      mv -iv ${f} ${f}.org
        nkf -Jw ${f}.org > ${f}
        rm -v ${f}.org
      done
  
      # EUC-JP -> UTF-8
      for f in doc/CL-HOWM.ja.rd \
               doc/index-j.html \
               doc/OLD.rd \
               doc/README-j.html \
               doc/README.ja.rd \
               doc/TUTORIAL.ja.rd \
               ext/hcal.rb \
               ext/howm2 \
               ext/howmkara \
               ext/hsplit.rb \
               howm-misc.el \
               ja/0000-00-00-000000.txt \
               sample/bug-report.txt \
               sample/search.txt \
               sample/top.txt; do
        mv -iv ${f} ${f}.org
        nkf -Ew ${f}.org > ${f}
        rm -v ${f}.org
      done
  
      # patch
      if [[ -e ${EMACS_USER_DIR}/usr/local/src/howm/howm-${HOWM_VERSION}.patch ]]; then
        patch -p1 < ${EMACS_USER_DIR}/usr/local/src/howm/howm-${HOWM_VERSION}.patch
      else
        echo "howm-${HOWM_VERSION}.patch does not exist."
      fi
  
    else
      abort "howm-${HOWM_VERSION}.tar.gz is not downloaded."
    fi
  fi
}

################################################################################
# Install highlight-sexps.el
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
################################################################################
install_highlight_sexps() {
  mkdir -p ${EMACS_USER_DIR}/lib/elisp/other/highlight-sexps
  cd ${EMACS_USER_DIR}/lib/elisp/other/highlight-sexps
  if [[ ! -e ./highlight-sexps.el ]]; then
    wget http://david.rysdam.org/src/emacs/highlight-sexps.el
    patch -p0 < ${EMACS_USER_DIR}/usr/local/src/highlight-sexps/highlight-sexps.el.patch
  else
    echo "highlight-sexps.el has been installed already."
  fi
}

################################################################################
# Install sqlite3
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
################################################################################
install_sqlite3() {
  local etc_dir=${EMACS_USER_DIR}/etc
  local opt_dir=${EMACS_USER_DIR}/opt
  local src_dir=${EMACS_USER_DIR}/usr/local/src
  local tmp_dir=${EMACS_USER_DIR}/tmp

  local package_name=sqlite-autoconf-3$(printf '%02d' ${SQLITE3_VERSION_X})$(printf '%02d' ${SQLITE3_VERSION_Y})$(printf '%02d' ${SQLITE3_VERSION_Z})
  local install_dir_name=sqlite3.${SQLITE3_VERSION_X}.${SQLITE3_VERSION_Y}
  if [[ "${SQLITE3_VERSION_Z}" != "0" ]]; then
    install_dir_name=${install_dir_name}.${SQLITE3_VERSION_Z}
  fi
  local prefix=${opt_dir}/${install_dir_name}
  local prefix_nover=${opt_dir}/sqlite3

  local homepage_url=http://sqlite.org/
  local ef_src_name=extension-functions.c
  local ef_dylib_name=libsqlitefunctions.dylib
  local init_file_name=sqliterc

  # download .tar.gz file
  cd ${tmp_dir}
  if [[ -e ${package_name}.tar.gz ]]; then
    echo "${package_name}.tar.gz has been downloaded."
  else
    wget ${homepage_url}/${SQLITE3_YEAR}/${package_name}.tar.gz
  fi
  
  # extract
  cd ${src_dir}
  if [[ -e ./${package_name} ]]; then
    echo "${package_name} has been extracted already."
  else
    tar zxvf ${tmp_dir}/${package_name}.tar.gz
  fi
  
  # configure & make
  if [[ ! -e ${prefix} ]]; then
    cd ${package_name}
    ./configure --prefix=${prefix} \
                --disable-dependency-tracking \
                --enable-dynamic-extensions \
                --disable-readline \
                --disable-shared  
    make
    make install
    cd ${opt_dir}
    rm -f sqlite3
    ln -s ${install_dir_name} sqlite3
  fi
  
  # make extension-functions
  mkdir -p ${src_dir}/sqlite3_extension_functions
  cd ${src_dir}/sqlite3_extension_functions
  if [[ -e ${ef_src_name} ]]; then
    echo "${ef_src_name} has been downloaded already."
  else
    wget -O ${ef_src_name} ${homepage_url}/contrib/download/${ef_src_name}?get=25
  fi
  if [[ -e ${ef_dylib_name} ]]; then
    rm -f ${ef_dylib_name}
  fi
  clang -fno-common \
        -dynamiclib \
        ${ef_src_name} \
        -o ${ef_dylib_name} \
        -I ${prefix_nover}/include \
        -L${prefix_nover}/lib \
        -lsqlite3
  install -m 0755 ${ef_dylib_name} ${prefix_nover}/lib/
  if [[ -e ${etc_dir}/${init_file_name} ]]; then
    echo "sqliterc exists."
  else
    echo ".load ${prefix_nover}/${ef_dylib_name}" > ${etc_dir}/${init_file_name}
  fi
}

################################################################################
# Install emacs
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
################################################################################
install_emacs() {
  # download emacs source
  cd ${EMACS_USER_DIR}/tmp
  if [[ -e emacs-${EMACS_VERSION}.tar.xz ]]; then
    echo "emacs-${EMACS_VERSION}.tar.xz has been downloaded already."
  else
    wget http://ftp.gnu.org/pub/gnu/emacs/emacs-${EMACS_VERSION}.tar.xz
  fi

  # download patch source
  if [[ -e emacs-${EMACS_VERSION}-mac-${EMACS_PATCH_VERSION}.tar.gz ]]; then
    echo "emacs-${EMACS_VERSION}-mac-${EMACS_PATCH_VERSION}.tar.gz has been downloaded already."
  else
    wget ftp://ftp.math.s.chiba-u.ac.jp/emacs/emacs-${EMACS_VERSION}-mac-${EMACS_PATCH_VERSION}.tar.gz
  fi

  # extract
  local src_dir=emacs-${EMACS_VERSION}-${EMACS_PATCH_VERSION}
  local patch_dir=emacs-${EMACS_VERSION}-${EMACS_PATCH_VERSION}-patch

  cd ${EMACS_USER_DIR}/usr/local/src
  if [[ -d ./${src_dir} ]]; then
    echo "${src_dir} has been extracted already."
  else
    tar Jxvf ${EMACS_USER_DIR}/tmp/emacs-${EMACS_VERSION}.tar.xz
    mv -i emacs-${EMACS_VERSION} ${src_dir}
    if [[ -d ./${patch_dir} ]]; then
      echo "${patch_dir} has been extracted already."
    else
      tar zxvf ${EMACS_USER_DIR}/tmp/emacs-${EMACS_VERSION}-mac-${EMACS_PATCH_VERSION}.tar.gz
      mv -i emacs-${EMACS_VERSION}-mac-${EMACS_PATCH_VERSION} ${patch_dir}

      # compile
      cd ${src_dir}
      patch -p1 < ../${patch_dir}/patch-mac
      cp -r ../${patch_dir}/mac mac
      cp ../${patch_dir}/src/* src
      cp ../${patch_dir}/lisp/term/mac-win.el lisp/term
      cp nextstep/Cocoa/Emacs.base/Contents/Resources/Emacs.icns mac/Emacs.app/Contents/Resources/Emacs.icns
      ./configure --with-mac --without-x --with-gnutls
      make
      make install
      rm -f ${EMACS_USER_DIR}/opt/Emacs.app
      ln -sv ${EMACS_USER_DIR}/usr/local/src/emacs-${EMACS_VERSION}-${EMACS_PATCH_VERSION}/mac/Emacs.app ${EMACS_USER_DIR}/opt/Emacs.app
    fi
  fi
}

################################################################################
# Main script.
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
################################################################################
main() {
  # if setup.sh is not at "~/.emacs.d/bin/", exit.
  if [[ ! -e "${EMACS_USER_DIR}/bin/setup.sh" ]]; then
    abort "Please check your .emacs.d."
  fi
  
  # create directories not under version controls.
  for dir in lib/elisp/auto-install lib/elisp/el-get lib/elisp/elpa lib/elisp/other etc/inits opt tmp usr/local/src var; do
    mkdir -pv ${EMACS_USER_DIR}/${dir}
  done

  # check if installed command.
  for command in git wget nkf patch; do
    has_installed ${command}
  done
  
  # install emacs
  install_emacs

  # clone projects from github
  clone_from_github init-loader emacs-jp ${EMACS_USER_DIR}/lib/elisp/other
  clone_from_github blgrep ataka ${EMACS_USER_DIR}/lib/elisp/other
  clone_from_github clmemo ataka ${EMACS_USER_DIR}/lib/elisp/other
  
  # install miscs.
  install_rcodetools
  install_howm
  install_highlight_sexps
  install_sqlite3
}

################################################################################
#  Execute sain script
################################################################################
main

# download from emacswiki
# TODO use auto-install?
# - anything-R.el
# - csv-mode.el  -> attach patch

# download by curl or wget
# - gas-mode (http://www.hczim.de/software/gas-mode.html)

# - skk (http://openlab.ring.gr.jp/skk/index-j.html)
