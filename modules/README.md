# Modules

Though the structure of these modules loosely take after Spacemacs'
notion of layers, they are not meant to be interchangeable. Their
purpose is _almost_ purely organizational.

The structure of a module is as follows (all files are optional):

```bash
config.el      # loaded when module is enabled
packages.el    # loaded lazily by DOOM package management
+*.el          # extra config files (not automatically loaded)

# lazy-loaded functions
autoload.el
autoload/*.el
```
