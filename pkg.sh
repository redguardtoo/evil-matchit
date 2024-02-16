#!/bin/bash
name=evil-matchit
version=3.0.4
pkg=$name-$version
mkdir $pkg
cp README.org $pkg
cp *.el $pkg
cat << EOF > $pkg/$name-pkg.el
(define-package "$name" "$version" "Vim matchit ported into Emacs (requires EVIL)")
EOF

if [[ `uname -s` == *Darwin* ]]; then
   COPYFILE_DISABLE="" tar cvf $pkg.tar $pkg/
else
   tar cvf $pkg.tar $pkg/
fi
rm -rf $pkg/
