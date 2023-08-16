//! Server Node Example.
//!
//! The node registers specified name to the EPMD and waits messages from other (connected) nodes.
//! If this node receives a message, it will print the message and discard it.
//!
//! # Usage Examples
//!
//! ```bash
//! $ cargo run --example recv_msg -- --help
//! $ cargo run --example recv_msg -- --local bar@localhost --cookie erlang_cookie
//!
//! # On another shell
//! $ erl -sname foo
//! > {bar, bar@localhost} ! hello.
//! ```
use clap::Parser;
use futures::stream::StreamExt;

#[derive(Debug, Parser)]
#[clap(name = "recv_msg")]
struct Args {
	#[clap(long = "local", default_value = "bar@localhost")]
	local_node: erl_dist::node::NodeName,

	#[clap(long, default_value = "WPKYDIOSJIMJUURLRUHV")]
	cookie: String,

	/// Add `PUBLISHED` distribution flag to the node (otherwise, the node runs as a hidden node).
	#[clap(long)]
	published: bool,
}

impl Args {
	async fn local_epmd_client(
		&self,
	) -> anyhow::Result<erl_dist::epmd::EpmdClient<smol::net::TcpStream>> {
		let addr = (self.local_node.host(), erl_dist::epmd::DEFAULT_EPMD_PORT);
		let stream = smol::net::TcpStream::connect(addr).await?;
		Ok(erl_dist::epmd::EpmdClient::new(stream))
	}
}

fn main() -> anyhow::Result<()> {
	let args = Args::parse();
	smol::block_on(async {
		let listener = smol::net::TcpListener::bind("0.0.0.0:0").await?;
		let listening_port = listener.local_addr()?.port();
		println!("Listening port: {}", listening_port);

		let local_node_entry = if args.published {
			erl_dist::epmd::NodeEntry::new(args.local_node.name(), listening_port)
		} else {
			erl_dist::epmd::NodeEntry::new_hidden(args.local_node.name(), listening_port)
		};

		let (keepalive_connection, creation) = args
			.local_epmd_client()
			.await?
			.register(local_node_entry)
			.await?;
		println!("Registered self node: creation={:?}", creation);

		let mut incoming = listener.incoming();
		while let Some(stream) = incoming.next().await {
			let stream = stream?;
			let mut local_node = erl_dist::node::LocalNode::new(args.local_node.clone(), creation);
			if args.published {
				local_node.flags |= erl_dist::DistributionFlags::PUBLISHED;
			}
			let cookie = args.cookie.clone();
			smol::spawn(async move {
				match handle_client(local_node, cookie, stream).await {
					Ok(()) => {
						println!("Client disconnected");
					}
					Err(e) => {
						println!("Error: {}", e);
					}
				};
			})
			.detach();
		}

		std::mem::drop(keepalive_connection);
		Ok(())
	})
}

async fn handle_client(
	local_node: erl_dist::node::LocalNode,
	cookie: String,
	stream: smol::net::TcpStream,
) -> anyhow::Result<()> {
	let mut handshake =
		erl_dist::handshake::ServerSideHandshake::new(stream, local_node.clone(), &cookie);
	let status = if handshake.execute_recv_name().await?.is_some() {
		erl_dist::handshake::HandshakeStatus::Ok
	} else {
		// Dynamic name.
		erl_dist::handshake::HandshakeStatus::Named {
			name: "generated_name".to_owned(),
			creation: erl_dist::node::Creation::random(),
		}
	};
	let (stream, peer_node) = handshake.execute_rest(status).await?;
	println!("Connected: {:?}", peer_node);

	let (mut tx, rx) = erl_dist::message::channel(stream, local_node.flags & peer_node.flags);
	let mut timer = smol::Timer::after(std::time::Duration::from_secs(30));
	let mut msg_future = Box::pin(rx.recv_owned());
	loop {
		let result = futures::future::select(
			msg_future,
			smol::Timer::after(std::time::Duration::from_secs(10)),
		)
		.await;
		match result {
			futures::future::Either::Left((result, _)) => {
				let (msg, rx) = result?;
				println!("Recv: {:?}", msg);
				
				match msg {
			erl_dist::message::Message::RegSend(param1) =>
			//println!("param1: {:?}", param1),
			/*
			match param1 {
				erl_dist::message::Message::RegSend::RegSend(from, to, msg) => {
					println!("from: {:?}", from);
					println!("to: {:?}", to);
					println!("message: {:?}", msg);
					}
				_ => {}
			},
			*/
			{
			println!("from: {:?}", param1.from_pid);
			println!("to: {:?}", param1.to_name);
			println!("message: {:?}", param1.message);

			match param1.message {
				erl_dist::term::Term::Tuple(val) => {println!("message: {:?}", val);
					let msg_pid = &val.elements[0];
					let msg_srv = &val.elements[1];
					let msg_msg = &val.elements[2];
					println!("pid: {:?}", msg_pid);
					println!("srv: {:?}", msg_srv);

					/*
					let node_name = match msg_pid {
						erl_dist::term::Term::Pid(val) => {
							println!("node name: {:?}", val.node.name);
							val.node.name.clone();
						},
						_ => {}
					};
					*/
					let node_name = match msg_pid {
						erl_dist::term::Term::Pid(val) => val.node.name.clone(),
						_ => "test".to_string()
					};
					println!("node name: {:?}", node_name);

					match msg_srv {
					erl_dist::term::Term::Atom(val) => {

							let creation = erl_dist::node::Creation::random();
							//let local_node = erl_dist::node::LocalNode::new("bar@localhost".parse()?, creation);
							//let local_node = erl_dist::node::LocalNode::new("blah@COLOSUPERBOSS".parse()?, creation);
							let local_node = erl_dist::node::LocalNode::new(node_name.parse()?, creation);
							let from_pid = erl_dist::term::Pid::new(local_node.name.to_string(), 0, 0, local_node.creation.get());

							let msg = erl_dist::message::Message::reg_send(
								from_pid,
								//eetf::Atom::from("srv1"),
								val.clone(),
								eetf::Atom::from("[test message]").into(),
							);
							let res = tx.send(msg).await?;
					},
					_ => {}
					};


					},
				_ => {}
			}


			let creation = erl_dist::node::Creation::random();
			//let local_node = erl_dist::node::LocalNode::new("bar@localhost".parse()?, creation);
			let local_node = erl_dist::node::LocalNode::new("blah@COLOSUPERBOSS".parse()?, creation);
			let from_pid = erl_dist::term::Pid::new(local_node.name.to_string(), 0, 0, local_node.creation.get());

			let msg = erl_dist::message::Message::reg_send(
				from_pid,
				eetf::Atom::from("srv1"),
				eetf::Atom::from("[args.message]").into(),
			);
			let res = tx.send(msg).await?;

			}
			_ => {}
				}

				msg_future = Box::pin(rx.recv_owned());
			}
			futures::future::Either::Right((_, f)) => {
				msg_future = f;
			}
		}

/*
		let pid = eetf::Pid::new(args.local_node.to_string(), 0, 0, creation.get());
		let msg = erl_dist::message::Message::reg_send(
			pid,
			eetf::Atom::from(args.destination),
			eetf::Atom::from(args.message).into(),
		);
		tx.send(msg).await?;
*/

		if smol::future::poll_once(&mut timer).await.is_some() {
			tx.send(erl_dist::message::Message::Tick).await?;
			timer.set_after(std::time::Duration::from_secs(30));
		}
	}
}

//$1> /usr/local/share/libs/erlang_otp_26.0.2/bin/epmd -daemon

//$1> cargo build
//$1> /pathto/target/debug/someport --cookie testcookie --local bar1@localhost
//$2> /pathto/target/debug/someport --cookie testcookie --local bar2@COLOSUPERBOSS

//$3> /pathto/erl -fname "blahblah" -sname "blah" -setcookie "testcookie"
//(blah@COLOSUPERBOSS)1>

//$4> /pathto/erl -fname testtest -sname test -setcookie "testcookie"
//(test@COLOSUPERBOSS)1>

//$3> register(srv1,self()).
//$4> register(srv2,self()).

//$3> {srv2, test@COLOSUPERBOSS} ! hello.
//$4> {srv1, blah@COLOSUPERBOSS} ! hello.

//$3> flush().
//$4> flush().

//$3> {param1, bar1@localhost} ! {self(), srv1, {2233,447799}}.
//$4> {param1, bar1@localhost} ! {self(), srv2, {2233,447799}}.

//$3> {param1, bar2@COLOSUPERBOSS} ! {self(), srv1, {2233,447799}}.
//$4> {param1, bar2@COLOSUPERBOSS} ! {self(), srv2, {2233,447799}}.

//$3> flush().
//$4> flush().
