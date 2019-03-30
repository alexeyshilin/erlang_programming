
#https://github.com/Pyrlang/Pyrlang
#https://github.com/Pyrlang/Term
#https://pyrlang.github.io/Pyrlang/

import logging

from term import Atom
from pyrlang import Node, Process
# from pyrlang import GeventEngine as Engine
from pyrlang import AsyncioEngine as Engine
from colors import color

LOG = logging.getLogger(color("EXAMPLE2", fg='lime'))
logging.getLogger("").setLevel(logging.DEBUG)

def factorial(n):
	if n == 0:
		return 1
	else:
		return n * factorial(n-1)

def factorial_(n):return reduce(lambda x,y:x*y,[1]+range(1,n+1))

class MyProcess(Process):
	def __init__(self, node) -> None:
		Process.__init__(self, node_name=node.node_name_)
		#node.register_name(self, Atom('my_process'))  # optional
		node.register_name(self, Atom('facserver'))  # optional
		#LOG.info("Registering process - 'my_process'")
		LOG.info("Registering process - 'facserver'")
		self.node = node

	def handle_one_inbox_message(self, msg):
		print(msg)
		LOG.info("Incoming %s", msg)
		From = msg[0]
		Data = msg[1]
		Res = factorial(int(Data))
		self.node.send(sender=self.pid_, receiver=From, message=(Atom("ok"), Res))


def main():
	event_engine = Engine()
	#node = Node(node_name="bar@127.0.0.1", cookie="cookie-value", engine=event_engine)
	node = Node(node_name="bar@Simon-Thompsons-Computer-2", cookie="cookie-value", engine=event_engine)
	MyProcess(node)
	event_engine.run_forever()


if __name__ == "__main__":
	main()

# sudo hostname Simon-Thompsons-Computer-2
# echo "127.0.0.1       Simon-Thompsons-Computer-2" >> /etc/hosts

# python3 node_test.py


# erl -sname foo
# erlang:set_cookie(node(), 'cookie-value').

# erl v.1
# {facserver, 'bar@Simon-Thompsons-Computer-2'} ! {self(), 12}.
# flush().

# erl v.2
# myrpc:f(12).
