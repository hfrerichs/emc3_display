#!/bin/bash

#=======================================================================
# path to emc3_display files

EMC3_DISPLAY_DIR=~/tools/emc3_display
#=======================================================================



# load default configuration
RC=$EMC3_DISPLAY_DIR/config
. $RC

# user configuration
USR_CONF_DIR=$HOME/.emc3_display
if [ ! -d $USR_CONF_DIR ]; then mkdir $USR_CONF_DIR; fi

# source user defined configuration
RC=~/.emc3_display/config
if [ -f $RC ]; then . $RC; fi

# default settings for colors
xrdb $EMC3_DISPLAY_DIR/xdefaults

# start main program
idl << EOF
!path   = '$EMC3_DISPLAY_DIR' + ':' + !path
emc3_display, '$EMC3_DISPLAY_DIR'
exit
EOF
