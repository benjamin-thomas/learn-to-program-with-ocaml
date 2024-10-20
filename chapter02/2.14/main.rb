require "immutable/set"

require 'awesome_print'
require 'pry'

def solve(cols, d1, d2)
  if cols.empty?
    1
  else
    cols.difference(d1).difference(d2).reduce(0) do |acc, n|
      acc +
        solve(cols.delete(n),
              d1.add(n).map { |x| x + 1 },
              d2.add(n).map { |x| x - 1 }
             )
    end
  end
end

=begin

The performance is abysimal in comparaison to Haskell or OCaml

[2.14]$ time ruby ./main.rb
365596

real    9m29,389s
user    9m27,560s
sys     0m0,993s

=end
p(solve(Immutable::Set[*(1..14).to_a], Immutable::Set.empty, Immutable::Set.empty))
