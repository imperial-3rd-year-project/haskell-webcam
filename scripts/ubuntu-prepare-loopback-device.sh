#!/bin/sh

# Pass in the argument of the virtual video device location, the script device's 
# capabilities to ones v4l2output/input in our code works with 

# Packages required to be installed: v4l2loopack-dkms and v4l2loopack-utils

# The script to be run with elevated privledges because of modprobe, i.e. with sudo

# Creates virtual video device /dev/video9; should be changed in code if another number should be used
video_dev_id=9

modprobe v4l2loopback video_nr=$video_dev_id

v4l2loopback-ctl set-caps "video/x-raw, format=RGB, width=640, height=480" /dev/video$video_dev_id
