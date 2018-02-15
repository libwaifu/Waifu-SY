#!/usr/bin/env bash
rm raw/* -r && cd raw &&
git submodule add "https://github.com/chinese-poetry/chinese-poetry-zhCN" &&
./db_build.wls