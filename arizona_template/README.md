# Arizona Template

This directory provides a rebar3 template for easier creation of an Arizona application.

## Table of contents

- [Installation](#installation)
- [Usage](#usage)
- [Options](#options)
- [License](#license)

## Installation

To install, clone the entirely content of this directory to `~/.config/rebar3/templates`. If the rebar3 templates directory does not exist, create it via:

```console
mkdir -p ~/.config/rebar3/templates
```

## Usage

```console
$ rebar3 new arizona name=myarizona
$ cd myarizona
```

## Options

```console
$ rebar3 new help arizona
arizona:
	custom template (/home/williamthome/.config/rebar3/templates/arizona_template/arizona.template)
	Description: Create a basic live Arizona template
	Variables:
		name="myarizona" (Name of the Arizona application)
		date="2023-10-29"
		datetime="2023-10-29T21:33:38+00:00"
		author_name="William Fank Thomé"
		author_email="willegp88@gmail.com"
		copyright_year="2023"
		apps_dir="apps" (Directory where applications will be created if needed)
```

## License

Copyright (c) 2023 [William Fank Thomé](https://github.com/williamthome)

Arizona is 100% open source and community-driven. All components are available under the Apache 2 License on [GitHub](https://github.com/spawnfest/arizona).

See [LICENSE.md](LICENSE.md) for more information.
