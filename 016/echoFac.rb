# gem install mojombo-electricity -s http://github.com/mojombo/electricity
# gem install erlectricity
# gem install mojombo-erlectricity -s http://gems.github.com

require 'rubygems'
require 'erlectricity'
require 'stringio'

def fac n
	if (n<=0) then 1 else n*(fac (n-1)) end
end

receive do |f|
	f.when([:fac, String]) do |text|
		n = text.to_i
		f.send ! ( [:result, "#{n}!=#{(fac n)}"])
		f.receiveloop
	end
end
