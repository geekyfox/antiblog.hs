#!/bin/sh

if [ ! -e /tmp/supervisor.sock ]; then
    ~/antiblog/scripts/svd_run.sh
    echo "supervisord started"
fi

