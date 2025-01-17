

devClient:
  cs launch io.github.quafadas:sjsls_3:0.2.5 -- \
    --path-to-index-html {{justfile_directory()}}/flicflac/client/resources \
    --build-tool mill \
    --mill-module-name flicflac.client  \
    --port 3000 \
    --out-dir {{justfile_directory()}}/out/flicflac/client/fastLinkJS.dest \
    --proxy-prefix-path /api \
    --proxy-target-port 8080 \
    --client-routes-prefix /ui

update:
  mill mill.scalalib.Dependency/showUpdates

fmt:
  mill mill.scalalib.scalafmt.ScalafmtModule/

cleanClient:
  mill clean flicflac.client.__
  mill clean flicflac.shared.__

compileAll:
  mill __.compile



