dotemacs
========
これは私の .emacs です。

install
=======
`git clone https://github.com/okanotor/dotemacs.git ~/.emacs.d`


description
===========
この .emacs.d 配下の構成です。
```
~/
  .emacs.d/
    .gitignore        # git 管理対象外ファイルの一覧。
    bin/              # 実行ファイル群。
      setup.sh        # Emacs 実行環境を構築するためのスクリプト。
    etc/              # Emacs Lisp が利用する設定ファイル群。
      inits/          # init-loader が読み込む設定ファイル群。必要な分だけ、my-inits からリンクを張る。git 管理対象外。
      inits-local/    # 各利用環境で個別に設定するための設定ファイル群。雛形のみ git 管理対象。
      my-inits/       # 設定ファイル群の本体。
    init.el           # 設定ファイルのエントリポイント。主に init-loader 関係の設定を行なっている。
    lib/              # Emacs で使用するライブラリファイル群。
      elisp/          # Emacs Lisp ファイル群。取得方法によってサブディレクトリを構築。
        auto-install/ # auto-install を使ってダウンロードするもの。git 管理対象外。
        el-get/       # el-get を使ってダウンロードするもの。git 管理対象外。
        elpa/         # ELPA からダウンロードするもの。git 管理対象外。
        original/     # 自作の Emacs Lisp。
        other/        # ELPA/el-get/auto-install 以外の方法でダウンロードするもの。git 管理対象外。
    opt/              # Emacs から利用するアプリケーション群。git 管理対象外。
    tmp/              # 作業用ディレクトリ。git 管理対象外。
    usr/              #
      local/          #
        src/          # ソースコードの展開先。一部 git 管理対象。
      share/          #
        snippets      # yasnippet のテンプレートファイル。
    var/              # キャッシュファイルやバックアップファイルなど。git 管理対象外。
    README.md         # このファイル。
```

ダウンロードしたら、 `bin/setup.sh` を実行して下さい。起動に必要な設定を行います。
特に、この環境は init-loader ありきで動きますが、
init-loader はこの方式でダウンロードしますので、
必ず Emacs 起動前に実施してください。
なお、実施にあたっては以下のコマンドを必要とします。事前にインストールしておいてください。
- `clang` (mac) or `gcc` (linux)
- `gem`
- `git`
- `nkf`
- `patch`
- `wget`

