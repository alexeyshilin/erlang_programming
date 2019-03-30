
import sys
import getopt

from py_interface import erl_term
from py_interface import erl_node
from py_interface import erl_opts
from py_interface import erl_common
from py_interface import erl_eventhandler

mb = None

def factorial(n):
	if n == 0:
		return 1
	else:
		return n * factorial(n-1)

def factorial_(n):return reduce(lambda x,y:x*y,[1]+range(1,n+1))

deliberate_fail = erl_term.ErlAtom("do_deliberately_fail")
as_you_wish = erl_term.ErlAtom("ok_deliberately_failed_as_you_wished")

def __TestMBoxCallback(msg, *k, **kw):
	global mb
	
	print("[*]")

	txt = "Incoming msg=%s (k=%s, kw=%s)" % (repr(msg), repr(k), repr(kw))
	print(txt.encode("ascii", errors="backslashreplace"))

	if type(msg) == tuple:
		if len(msg) == 2:
			if erl_term.IsErlPid(msg[0]):
				dest = msg[0]
				if msg[1] == deliberate_fail:
					reply = as_you_wish
				else:
					reply = factorial(int(msg[1]))
				print("[res=%s]" % (reply))
				#mb.Send(dest, (mb.Self(), reply))
				mb.Send(dest, (erl_term.ErlAtom("ok"), reply))


def __FlushStdout():
	input = sys.stdin.readline()
	if input == "": # master died
		sys.exit(0)
	print("-FLUSH-")
	sys.stdout.flush()

def main(argv):
	global mb

	try:
		opts, args = getopt.getopt(argv[1:], "?dn:c:q")
	except getopt.error as info:
		print(info)
		sys.exit(1)

	print("[start]")

	n = erl_node.ErlNode("bar@Simon-Thompsons-Computer-2", erl_opts.ErlNodeOpts(cookie="cookie-value"))
	n.Publish()

	mb = n.CreateMBox(__TestMBoxCallback)
	mb.RegisterName("facserver")

	evhand = erl_eventhandler.GetEventHandler()
	#evhand.PushReadEvent(sys.stdin, __FlushStdout)

	#sys.stdout.flush()

	print("[start loop]")

	evhand.Loop()

	print("[ok]")


try:
	main(sys.argv)
except KeyboardInterrupt:
	print("Interrupted. Exiting.")
	sys.exit(1)


# sudo hostname Simon-Thompsons-Computer-2
# echo "127.0.0.1       Simon-Thompsons-Computer-2" >> /etc/hosts

# python3 server_node.py


# erl -sname foo
# erlang:set_cookie(node(), 'cookie-value').

# erl v.1
#  myrpc:f(12).

# erl v.2
# {facserver, 'bar@Simon-Thompsons-Computer-2'} ! {self(), 12}.
# flush().
