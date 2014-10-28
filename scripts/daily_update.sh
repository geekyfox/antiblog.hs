#!/bin/sh

cd ~/antiblog/schema
echo "select nightly_maintenance();" | ./cli.sh

