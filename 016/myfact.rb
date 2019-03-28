require 'rubygems'
require 'stringio'

def fac n
	if (n<=0) then 1 else n*(fac (n-1)) end
end

inp = $stdin.gets

res = fac(inp.to_i)

$stdout.print "Ruby language. #{res}\n"
