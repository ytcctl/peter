````markdown

This project configures R as a Jupyter kernel in Codespaces/devcontainer and now uses a Dockerfile based on a pinned rocker image.

What changed

- The devcontainer now builds from `rocker/r-ver:4.3.2` (pinned) via `.devcontainer/Dockerfile`.
- The Dockerfile bakes R, common system libraries, Python, `jupyterlab`, and the R package `IRkernel` into the image. The IRkernel is registered system-wide.
- VS Code extensions (including Jupyter Powertoys) are still provisioned by the devcontainer and will be available in the Codespace.

How to apply the Dockerfile-based devcontainer

1. Commit & push the changes.
2. Recreate or rebuild the Codespace (or use VS Code: Command Palette → "Rebuild Container"). The Dockerfile will be built and the image will be used for the Codespace.

Verification

After the rebuild completes, open a terminal in the Codespace and run:

```bash
# list jupyter kernels
python3 -m jupyter kernelspec list
```

You should see an `ir` kernel listed (system-wide registration).

Open a notebook and choose the R kernel using the kernel picker (Jupyter Powertoys should be available).

Notes

- The Dockerfile uses `rocker/r-ver:4.3.2` for reproducibility; change the tag only when you intentionally upgrade R.
- Add any additional system libraries required by R packages into the Dockerfile's `apt-get install` step.
- Because the image includes IRkernel system-wide, you don't need to run kernel registration in `postCreateCommand` anymore.


### In-container browser (noVNC)

After rebuilding the devcontainer the image will run a headless X server, x11vnc, and a Chromium instance served through noVNC.

- noVNC web UI will be available at port 6080 (forwarded in `devcontainer.json`).
- To open: rebuild the container, then open `http://localhost:6080` in your local browser (or use Codespaces port forwarding UI).

Notes:

- This adds ~200-300MB to the image. If you only need a quick preview, use the `auchenberg.vscode-browser-preview` extension instead.
- Rebuild required: Command Palette → Dev Containers: Rebuild Container.

````

