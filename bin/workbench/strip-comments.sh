
COMBY_M="$(cat <<"MATCH"
:[open~\(\*]:[any]:[close~\n*\*\)]
MATCH
)"
# Install comby with `bash <(curl -sL get.comby.dev)` or see github.com/comby-tools/comby && \
comby "$COMBY_M" ''  .ml -stats
