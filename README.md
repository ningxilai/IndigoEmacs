Indigo Emacs
--------------------
Install:

```bash
git clone --depth 1 https://github.com/ningxilai/IndigoEmacs $XDG_CONFIG_HOME/emacs
git clone --depth 1 https://github.com/radian-software/straight.el $XDG_CONFIG_HOME/emacs/straight/repos/straight.el
```
--------------------
Acknowledge:

- IsaacGu .emacs.d
- seagle0128 Centaur
- snackon Witchmacs / pprobst yukimacs
- ltylty .emacs.d
- rougier nano-emacs
- EmacsChina forum Emacs-general
- ![Cold's World](https://coldnight.github.io/dump-brain-with-emacs/)
- etc..

--------------------
LSP Bridge(Emacs 30.1):

``` bash
uv pip install epc orjson sexpdata six setuptools paramiko rapidfuzz watchdog packaging
```

``` emacs-lisp
(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
            :build (:not compile))
  :init (global-lsp-bridge-mode))
```
