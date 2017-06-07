#!/bin/bash

# this script
# backs up all the climate variable stuff from INLA models from wrfstore5-10 to wrfstore6-10

#@daily rsync -avz -e ssh rmp15@wrfproc1-10.sp.ph.ic.ac.uk:~/data/* rmp15@wrfstore6-10.sp.ph.ic.ac.uk:/net/wrfstore6-10/disk1/rmp15/data/
