

devFront:
  cs launch io.github.quafadas::sjsls:0.2.1 -- \
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

cleanFront:
  mill clean flicflac.client.__