![](https://raw.githubusercontent.com/Shinmera/lem-icon/gh-pages/icon-blue.svg)
# Lem: Editor Modules

![Build Status](https://github.com/lem-project/lem/workflows/CI/badge.svg)
[![Backers on Open Collective](https://opencollective.com/lem/backers/badge.svg)](#backers) [![Sponsors on Open Collective](https://opencollective.com/lem/sponsors/badge.svg)](#sponsors)
[![GitHub Sponsors](https://img.shields.io/badge/-Sponsor-fafbfc?logo=GitHub-Sponsors)](https://github.com/sponsors/cxxxr)
[![gitcgr](https://gitcgr.com/badge/lem-project/lem.svg)](https://gitcgr.com/lem-project/lem)



After installing lem, you can start developing and extend the editor while it runs.

## Install

### nightly build

Tested on Ubuntu 24.04 and macOS(apple silicon).

https://github.com/lem-project/lem/releases/tag/nightly-latest

### Nix

    $ nix profile add github:lem-project/lem#
    $ nix profile add github:lem-project/lem#lem-ncurses
    $ nix profile add github:lem-project/lem#lem-webview
    $ nix profile add github:lem-project/lem#lem-sdl2

Or run Lem temporarily (no install):

    $ nix run github:lem-project/lem#
    $ nix run github:lem-project/lem#lem-ncurses
    $ nix run github:lem-project/lem#lem-webview
    $ nix run github:lem-project/lem#lem-sdl2

Use the overlay when you already have a flake-based NixOS/home-manager config and
want `pkgs.lem-ncurses` (and `apps.lem-ncurses`) available from your `nixpkgs` set.
You can also consume `lem` as an overlay in `flake.nix`:

    {
      inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
        lem.url = "github:lem-project/lem";
      };

      outputs = { self, nixpkgs, lem, ... }: {
        nixosConfigurations.example = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            { nixpkgs.overlays = [ lem.overlays.default ]; }
            { environment.systemPackages = [ pkgs.lem-ncurses ]; }
          ];
        };
      };
    }

### docker

With Docker (terminal version):

    $ docker run --rm -it ghcr.io/lem-project/lem:latest

## Build

Instructions to build Lem on GNU/Linux, Macos and Windows.

### Terminal version
https://lem-project.github.io/installation/ncurses/

### Webview version
https://lem-project.github.io/installation/webview/

### SDL2 version
https://lem-project.github.io/installation/sdl2/

### Clysm version
Lem compiled to WebAssembly by [Clysm](https://github.com/cxxxr/clysm), as a
desktop application (Electron) or a page for a browser
([demo](https://cxxxr.github.io/lem/)): `make clysm`, `make clysm-web`.
See [frontends/clysm/README.md](frontends/clysm/README.md).

### Quick summary for Nix and NixOS users
If you have flakes enabled, you can run:

```
nix develop --command make webview-ncurses # webview AND ncurses version
```

or:

```
nix develop --command make ncurses # ncurses version (no webview)
```

Then you can run the executable with:

```
nix develop --command ./lem
```

You need to run the executable inside the Nix development environment so it can
find the required native libraries.
## Vision

Lem brings the distance between code and its execution state as close to zero as possible.  
Users can see the results of their program while editing, without breaking their flow, 
and visually follow the behavior of the running code in real time.

The interface is consistently simple and easy to use without confusion.  
Moreover, Lem is fully customizable, and any extensions take effect immediately as they are written.

### Goals
- Instantly reflect executed code inline
- Visualize running code in real time
- Provide a consistent UI that feels effortless to use
- Maintain documentation for beginners, extension developers, and core developers.
- Offer an intuitive and consistent API for extensions
- Deliver a comfortable environment right out of the box

### Non-goals
- Rather than imitating Emacs or Vim, Lem pursues its own unique approach

## Contributors

This project exists thanks to all the people who contribute. [[Contribute]](CONTRIBUTING.md).

<a href="https://github.com/lem-project/lem/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=lem-project/lem&max=24" />
</a>

## Backers

Thank you to all our backers! 🙏 [[Become a backer](https://opencollective.com/lem#backer)]

<a href="https://opencollective.com/lem#backers" target="_blank"><img src="https://opencollective.com/lem/backers.svg?width=890"></a>


## Sponsors

Support this project by becoming a sponsor. Your logo will show up here with a link to your website. [[Become a sponsor](https://opencollective.com/lem#sponsor)]

<a href="https://opencollective.com/lem/sponsor/0/website" target="_blank"><img src="https://opencollective.com/lem/sponsor/0/avatar.svg"></a>
<a href="https://opencollective.com/lem/sponsor/1/website" target="_blank"><img src="https://opencollective.com/lem/sponsor/1/avatar.svg"></a>
<a href="https://opencollective.com/lem/sponsor/2/website" target="_blank"><img src="https://opencollective.com/lem/sponsor/2/avatar.svg"></a>
<a href="https://opencollective.com/lem/sponsor/3/website" target="_blank"><img src="https://opencollective.com/lem/sponsor/3/avatar.svg"></a>
<a href="https://opencollective.com/lem/sponsor/4/website" target="_blank"><img src="https://opencollective.com/lem/sponsor/4/avatar.svg"></a>
<a href="https://opencollective.com/lem/sponsor/5/website" target="_blank"><img src="https://opencollective.com/lem/sponsor/5/avatar.svg"></a>
<a href="https://opencollective.com/lem/sponsor/6/website" target="_blank"><img src="https://opencollective.com/lem/sponsor/6/avatar.svg"></a>
<a href="https://opencollective.com/lem/sponsor/7/website" target="_blank"><img src="https://opencollective.com/lem/sponsor/7/avatar.svg"></a>
<a href="https://opencollective.com/lem/sponsor/8/website" target="_blank"><img src="https://opencollective.com/lem/sponsor/8/avatar.svg"></a>
<a href="https://opencollective.com/lem/sponsor/9/website" target="_blank"><img src="https://opencollective.com/lem/sponsor/9/avatar.svg"></a>



# License
[MIT](https://github.com/lem-project/lem/blob/master/LICENCE)
