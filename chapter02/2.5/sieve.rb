# ruby ./sieve.rb

def get_max
  print "Enter a number: "
  gets.to_i
end

def compute(max, prime)
  limit = Integer(Math.sqrt(max))
  prime[0] = false
  prime[1] = false
  (2..limit).each do |n|
    if prime[n]
      m = n * n
      while m <= max
        prime[m] = false
        m += n
      end
    end
  end
end

max = 25 || get_max
prime = Array.new(max + 1, true)
require 'pry';binding.pry
compute(max, prime)
(2..max).each do |n|
  puts n if prime[n]
end
