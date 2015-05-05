#!/bin/bash

ldd $1 | awk '/=>/{print $(NF-1)}' | sort -u | while read n; do dpkg -S $n; done | cut -d':' -f1 | sort -u
