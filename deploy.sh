#!/bin/bash

# Shell script to deploy the vlambda site to production server.
# Should be called using cabal:
#
#   $ cabal run -- deploy
#
# Author: Alexander Vandenbroucke <alexander.vandenbroucke@kuleuven.be>

# static configuration settings
REMOTE=ssh1.cs.kuleuven.be
REMOTE_DIR=/cw/w3people/alexander.vandenbroucke/public_html/
LOCAL_DIR=_site
USER=alexvdb

echo "Deploying as $USER..."
echo [local] from $LOCAL_DIR
echo [$REMOTE] to $REMOTE_DIR

read -r -p "Proceed with deployment? [y/N] " response
case $response in
    [yY][eE][sS]|[yY])

	rsync -rav $LOCAL_DIR/* $USER@$REMOTE:$REMOTE_DIR

	if [ $? -eq 0 ]
	then
	    echo Done.
	fi

        ;;
    *)
	echo "Abort."
        ;;
esac

