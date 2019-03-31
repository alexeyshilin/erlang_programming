
// https://github.com/takayuki/Erlang.NET
// https://github.com/takayuki/Erlang.NET.git
// 3964e8419cf4caa112637daf0bf01e2eb4775ec0

using System;
using System.Collections.Generic;
using System.Reflection;
using System.Threading;
using Erlang.NET;
using log4net;
using log4net.Config;

namespace Erlang.NET.Test
{
	public class TestFactorial
	{
		static TestFactorial()
		{
			XmlConfigurator.Configure();
		}

		static long factorial(long number)
		{
			if (number <= 1)
				return 1;
			else
				return number * factorial(number - 1);
		}

		public static void Main(string[] args)
		{
			OtpNode b = new OtpNode("bar@Simon-Thompsons-Computer-2", "cookie-value");

			OtpMbox mb = b.createMbox("facserver", true);

			int i = 0;
			while(true)
			{
				System.Console.Out.WriteLine("[try]\n");
				//OtpErlangObject msg = mb.receive();
				OtpMsg msg =  mb.receiveMsg();

				//System.Console.Out.WriteLine("IN msg: " + msg.ToString() + "\n");
				System.Console.Out.WriteLine("[received]\n");
				System.Console.Out.WriteLine(msg);

				OtpErlangTuple t = (OtpErlangTuple)msg.getMsg();
				OtpErlangPid sender = (OtpErlangPid)t.elementAt(0);
				
				//OtpErlangObject[] v = { mb.Self, t.elementAt(1) };

				OtpErlangLong val = (OtpErlangLong)t.elementAt(1);
				System.Console.Out.WriteLine(val.longValue());
				
				long res = TestFactorial.factorial( val.longValue() );
				

				//OtpErlangObject[] v = { mb.Self, new OtpErlangLong(res) };
				OtpErlangObject[] v = { new OtpErlangAtom("ok"), new OtpErlangLong(res) };

				mb.send(sender, new OtpErlangTuple(v));
				
				++i;
				
				if( i>1000 )
					break;
			}

			b.close();
		}
	}
}

// sudo hostname Simon-Thompsons-Computer-2
// echo "127.0.0.1       Simon-Thompsons-Computer-2" >> /etc/hosts

// #1
// make
// mono test.exe


// #2
// erl -sname foo
// erlang:set_cookie(node(), 'cookie-value').

/// v.1
// {facserver, 'bar@Simon-Thompsons-Computer-2'} ! {self(), 12}.
// flush().

/// v.2
// myrpc:f(12).
