#!/usr/bin/env bash

rm -rf build/*
ELM_DEBUGGER=true elm-app build
elm-app start
