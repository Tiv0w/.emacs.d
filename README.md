# .emacs.d
**My personal .emacs.d**

:information_source: This config is only intended for use with Emacs 27+ (because of `early-init.el`).

## Key features
- :100: [xah-fly-keys](https://github.com/xahlee/xah-fly-keys): for powerful and ergonomics-based modal editing
- :zap: a super duper quick Emacs, with lots of package-loading deferred
- :jack_o_lantern: multiple tweaks inspired from [Doom Emacs](https://github.com/hlissner/doom-emacs)
- :package: a good amount of packages to help, but not too much
- :globe_with_meridians: a simple format for adding new languages configurations, thanks to [Emacs Bootstrap](https://github.com/editor-bootstrap/emacs-bootstrap/)


### Based on Emacs Bootstrap, but why ?
When I picked up Emacs, I hacked together a clunky `.emacs`, ~1500 lines of copy-pasted code that I didn't understand.
Then I found [Emacs Bootstrap](https://github.com/editor-bootstrap/emacs-bootstrap/), and I got an organized config in 3 clicks.
It is *easy, quick, simple and well organized*.
<br/>
Nowadays, my elisp-fu has graduated from white to yellow belt, so I maintain my config entirely myself.
But It was a good base config when I started.


### Xah-fly-keys
[Xah-fly-keys](https://github.com/xahlee/xah-fly-keys) is the most important part of the config.
It is a modal editing interface for Emacs.
You could use any modal editing system, but I really like the core defaults from xah-fly-keys.

I use my own version because I dislike some features, but like Emacs-bootstrap, it is a good base.


### Many programming languages
This config has evolved with my programming needs, and is now supporting multiple programming languages:
- JS and TS
- Web in general
- Clojure
- C/C++
- Rust

These are the main ones.


Markdown and LaTeX are also supported.


### Use-package, and deferred loading
I extensively use John Wiegley's [use-package](https://github.com/jwiegley/use-package).
With modes, hooks and commands, you can defer loading of many packages, and do it really easily.
In addition, packages configuration is much more easier with it.

Nice work John! :+1:


## Installation

### External programs needed
- ripgrep (`rg` command, `ripgrep` package on Arch)
- a Nerd font (probably `Symbols Nerd Fonts Mono`, available as `ttf-nerd-fonts-symbols-mono` on Arch)
- Blobmoji font (`noto-fonts-emoji-blob` on Arch)


---

:warning: If you intend to use this configuration, it is heavily opinionated, hacky in some ways, not optimal in others.
