
fn lower(mut s: String) -> Result<String, String> {
    s.make_ascii_uppercase();
    Ok(s)
}

fn main() {
   use erlang_port::{PortReceive, PortSend};

  let mut port = unsafe {
      use erlang_port::PacketSize;
      erlang_port::nouse_stdio(PacketSize::Four)
  };

  for string_in in port.receiver.iter() {
      let result = lower(string_in);

      port.sender.reply(result);
  }
}
