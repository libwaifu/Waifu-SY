#!/usr/bin/env bash
rm raw/* -r && cd raw &&
git submodule add "https://github.com/norybaby/poet" &&
cd raw/poet