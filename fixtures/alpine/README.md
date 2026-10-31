# Alpine x86 起動検証用入力

公式Alpine 3.22.7 x86のカーネルとminirootfsを使用する。
バイナリはGit管理対象外。以下はリポジトリのルートで実行する。
`curl`、Python 3.9以上、SHA256検証用の`shasum`が必要。
`shasum -a 256 -c`は`sha256sum -c`でも代用できる。

```sh
curl -fL https://dl-cdn.alpinelinux.org/alpine/v3.22/releases/x86/netboot/vmlinuz-lts \
  -o fixtures/alpine/vmlinuz-lts
curl -fL https://dl-cdn.alpinelinux.org/alpine/v3.22/releases/x86/alpine-minirootfs-3.22.7-x86.tar.gz \
  -o fixtures/alpine/alpine-minirootfs-3.22.7-x86.tar.gz
(cd fixtures/alpine && shasum -a 256 -c SHA256SUMS)
python3 scripts/make_initramfs.py fixtures/alpine/alpine-minirootfs-3.22.7-x86.tar.gz \
  fixtures/alpine/initramfs.cpio.gz
```

カーネルの配布URLは更新される。ハッシュが一致しなければ、保存済みログと
同じ入力ではないため、バージョンと起動結果を改めて確認する。
`SHA256SUMS`は検証に使用した入力の識別値。minirootfsのSHA256は取得時に
配布元の`.sha256`と照合したが、署名検証は行っていない。

変換スクリプトはホストへtarを展開せず、デバイスノードと`/init`を含む
newc initramfsを作成する。`/init`はproc/sysfsをマウントし、BusyBoxの
`/bin/sh -i`を起動する。

通常のbzImage経路でシェル起動と確認コマンドの実行に成功した。
[起動手順](../../README.md#シェル起動の再現と検証)と
[検証ログ](../../docs/validation/README.md)を参照。
