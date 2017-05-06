#/usr/bin/env bash
rm -rf dist
elm-app build
gh-pages -d dist
echo "Deployed to http://shuheikagawa.com/elmbda/"
