#!/usr/bin/env python
# source http://bit-rot.blogspot.com/2009/07/mplayer-remote-control-using-cellphone.html

import curses.wrapper
pipe_location="/home/lasseb/.mplayer/rc"
KEY_ENTER = 10
def main(stdscr):
	curses.init_pair(1, curses.COLOR_GREEN, curses.COLOR_BLACK)
	curses.init_pair(2, curses.COLOR_CYAN, curses.COLOR_BLACK)
	green = curses.color_pair(1)
	cyan = curses.color_pair(2)

	stdscr.addstr("mPlayer Remote\n", cyan)
	stdscr.addstr("\n"+"Keybindings:"+"\n")
	stdscr.addstr("F - ")
	stdscr.addstr("Fullscreen on/off", green)
	stdscr.addstr("\nN - ")
	stdscr.addstr("Skip file", green)
	stdscr.addstr("\nB - ")
	stdscr.addstr("Previous file", green)
	stdscr.addstr("\nJ - ")
	stdscr.addstr("Jump subtitles", green)
	stdscr.addstr("\nO - ")
	stdscr.addstr("Toggle OSD on/off", green)
	stdscr.addstr("\nP - ")
	stdscr.addstr("Pause", green)
	stdscr.addstr("\nQ - ")
	stdscr.addstr("Quit player", green)
	stdscr.addstr("\n. - ")
	stdscr.addstr("Seek 600", green)
	stdscr.addstr("\n, - ")
	stdscr.addstr("Seek -600", green)
	stdscr.addstr("\nLeft Arrow - ")
	stdscr.addstr("Seek -10", green)
	stdscr.addstr("\nRight Arrow - ")
	stdscr.addstr("Seek +10", green)   
	stdscr.addstr("\nUp Arrow - ")
	stdscr.addstr("Volume +", green)
	stdscr.addstr("\nDown Arrow - ")
	stdscr.addstr("Volume -", green)
	while True:
		pipe=open(pipe_location,'w')
		c=stdscr.getch()
		if c==ord('f'):
			pipe.write("vo_fullscreen"+"\n")
		elif c==curses.KEY_RIGHT:
			pipe.write("seek 10"+"\n")
		elif c==curses.KEY_LEFT:
			pipe.write("seek -10"+"\n")
		elif c==ord('.'):
			pipe.write("seek 600"+"\n")
		elif c==ord(','):
			pipe.write("seek -600"+"\n")
		elif c==ord('n'):
			pipe.write("pt_step 1"+"\n")
		elif c==ord('b'):
			pipe.write("pt_step -1"+"\n")
		elif c==curses.KEY_UP:
			pipe.write("volume +1"+"\n")
		elif c==curses.KEY_DOWN:
			pipe.write("volume -1"+"\n")
		elif c==ord('j'):
			pipe.write("sub_select"+"\n")
		elif c==ord('o'):
			pipe.write("osd"+"\n")
		elif c==ord('p') or c==ord(' ') or int(c)==KEY_ENTER:
			pipe.write("pause"+"\n")
		elif c==ord('q'):
			pipe.write("quit"+"\n")
			break
		pipe.close()

curses.wrapper(main)
