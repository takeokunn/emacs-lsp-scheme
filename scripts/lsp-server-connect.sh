#!/bin/bash

PORT=$1

# wait a bit for server to initialize
sleep 2
nc localhost $PORT
