###### Strip comments
COMBY_M="$(cat <<"MATCH"
:[open~\(\*]:[any]:[close~\n*\*\)]
MATCH
)"
# Install comby with `bash <(curl -sL get.comby.dev)` or see github.com/comby-tools/comby && \
comby -in-place "$COMBY_M" ''  .ml -stats

######## Strip records
COMBY_M="$(cat <<"MATCH"
{:[any]}
MATCH
)"
# Install comby with `bash <(curl -sL get.comby.dev)` or see github.com/comby-tools/comby && \
comby "$COMBY_M" ''  .ml -stats -i my_typedtree.ml

######### Strip variants
COMBY_M="$(cat <<"MATCH"
| :[any\n]
MATCH
)"
# Install comby with `bash <(curl -sL get.comby.dev)` or see github.com/comby-tools/comby && \
comby "$COMBY_M" ''  .ml -stats -i
