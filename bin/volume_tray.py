#!/usr/bin/env python
"A simple volume control applet."

import gtk
import subprocess

__author__  = "Joakim \"JockeTF\" Soderlund"
__date__    = "2008-08-22"
__license__ = "Public domain."

# Binaries to execute.
OSSMIX  = "ossmix"
OSSXMIX = "ossxmix"

# The device to control.
DEVICE = "vmix0-outvol"

# The maximum outvol volume in decibels.
MAX_DB = 25.0

# Sets an icon for different volume levels.
LEVELS = (
	(22, "audio-volume-high"),
	(18, "audio-volume-medium"),
	(0,  "audio-volume-low"),
	(-1, "audio-volume-muted"),
)

class StatusIcon():
	"The status icon."
	def __init__(self):
		self.icon = gtk.StatusIcon()
		
		self.icon.connect("scroll-event", self.scrollEvent)
		self.icon.connect("activate", self.clickEvent)
		
		self.set_visible = self.icon.set_visible
		
		self.update()


	def scrollEvent(self, widget, event, *args):
		"Changes the volume when the user scrolls on the volume applet."
		if event.direction == gtk.gdk.SCROLL_UP:
			subprocess.call([OSSMIX, DEVICE, "--", "+%.1f" % self.getStep()])
			
		elif event.direction == gtk.gdk.SCROLL_DOWN:
			subprocess.call([OSSMIX, DEVICE, "--", "-%.1f" % self.getStep()])

		self.update()


	def clickEvent(self, widget, *args):
		"Starts or closes ossxmix when the volume applet is clicked."
		if not hasattr(self, "ossxmix"):
			self.ossxmix = subprocess.Popen([OSSXMIX, "-S"])
		else:
			if self.ossxmix.poll() == None:
				self.ossxmix.terminate()
			else:
				self.ossxmix = subprocess.Popen([OSSXMIX, "-S"])

		self.update()

		
	def getVolume(self):
		"Returns the current volume in decibels as a float."
		process = subprocess.Popen([OSSMIX, DEVICE], stdout=subprocess.PIPE)
		volume = float(process.communicate()[0].split()[-2])
		process.wait()
		
		return volume

	
	def getStep(self):
		"Returns the next volume step to make in decibels as a float."
		return (MAX_DB - self.getVolume() + 1) * 0.1


	def getIconName(self):
		"Returns the icon name for the current volume level."
		volume = self.getVolume()
		
		for level in LEVELS:
			if level[0] < volume:
				return level[1]


	def update(self, *args):
		"Updates the volume applet's tooltip and icon."
		self.icon.set_tooltip("Volume: %.1f dB" % self.getVolume())
		self.icon.set_from_icon_name(self.getIconName())


if __name__ == "__main__":
	icon = StatusIcon()
	icon.set_visible(True)
	
	try:
		gtk.main()
	except KeyboardInterrupt:
		print("")

