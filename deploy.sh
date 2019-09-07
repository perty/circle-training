#!/usr/bin/env bash

PUBLIC_URL=/circle-training/ elm-app build
rm -rf docs/*
cp -R build/* docs
