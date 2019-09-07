#!/usr/bin/env bash

PUBLIC_URL=/circle-training/ elm-app build
cp -R build/* docs
