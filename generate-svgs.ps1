$cwd = $PSScriptRoot
pushd (join-path $cwd .\graphviz)
ls *.dot | % {
  $cmd = if ($_.Name.contains("grid")) {"neato"} else {"dot"};
  write-host 'visualizing ' $_.Name;
  & $cmd $_.Name -Tsvg -o ("svg\" + $_.Name + ".svg")
}
popd