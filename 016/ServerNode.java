import com.ericsson.otp.erlang.*;
import java.math.BigInteger;

public class ServerNode {

	public static void main (String[] args) throws Exception{

		//OtpNode bar = new OtpNodeC("bar");
		OtpNode bar = new OtpNode("bar", "cookie-value");
		OtpMbox mbox = bar.createMbox("facserver");

		OtpErlangObject o;
		OtpErlangTuple msg;
		OtpErlangPid from;
		BigInteger n;
		OtpErlangAtom ok = new OtpErlangAtom("ok");


		while(true) try {
			o = mbox.receive();
			msg = (OtpErlangTuple)o;
			from = (OtpErlangPid)(msg.elementAt(0));
			n = ((OtpErlangLong)(msg.elementAt(1))).bigIntegerValue();
			OtpErlangObject[] reply = new OtpErlangObject[2];

			reply[0] = ok;
			reply[1] = new OtpErlangLong(Factorial.factorial(n));
			OtpErlangTuple tuple = new OtpErlangTuple(reply);
			mbox.send(from,tuple);
		}catch(OtpErlangExit e) { break; }

	}
}

class Factorial{

	static int factorial(int n)
	{    
		if (n == 0)
			return 1;
		else
			return(n * factorial(n-1));
	}

	public static BigInteger factorial(BigInteger n) {
		BigInteger result = BigInteger.ONE;

		while (!n.equals(BigInteger.ZERO)) {
			result = result.multiply(n);
			n = n.subtract(BigInteger.ONE);
		}

		return result;
	}
}

// java --version
// javac --version

// sudo dnf install java-9-openjdk
// sudo dnf install java-9-openjdk-devel

// sudo dnf install java-1.8.0-openjdk
// sudo dnf install java-1.8.0-openjdk-devel

//# javac -classpath ".:/usr/lib64/erlang/lib/jinterface-1.8.1/priv/OtpErlang.jar" ServerNode.java
//# java -classpath ".:/usr/lib64/erlang/lib/jinterface-1.8.1/priv/OtpErlang.jar" ServerNode
