#!/usr/bin/env bash

elm-app build
cp -R build/* docs
