#!/bin/bash

docker run --rm \
    -v "/home/pi/chartbot/finR:/home/user" \
    base_armhf:latest \
    Rscript main.R
