set -e
unset PATH
for p in $buildInputs; do
  export PATH=$p/bin${PATH:+:}$PATH
done
source bluespec-setup
for f in $src; do
  bsc $f
done
mkdir -p $out/lib/bluespec/Libraries
for f in *.bo; do
  cp $f $out/lib/bluespec/Libraries
done
