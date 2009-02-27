#!/usr/bin/env ruby

lines = File.read(ARGV[0]).split("\n")

parsedLines = lines.collect do |l|
  l =~ /^NOTICE:\s*(start|end) ([A-Za-z_0-9]+)\s*\d\d\d\d-\d\d-\d\d (\d\d):(\d\d):(\d\d)\.(\d+)\+\d\d\s*/
  if $& == nil
    nil
  else
    h = $3.to_i
    m = $4.to_i
    s = $5.to_i
    f = "0.#{$6}".to_f
    t = ((h * 60) + m) * 60 + s + f
    [$1,$2,t]
  end
end.compact

stack = []
contextStack = []
parsedLines.each do |l|
  if l[0] == "start"
    stack.push l
    contextStack.push l[1]
  elsif l[0] == "end"
    s = stack.pop
    if s[1] != l[1]
      raise "error: mismatched start and end: #{s[1]} - #{l[1]}"
    end
    t = l[2] - s[2]
    puts "#{t.to_s[0..6]}: #{contextStack.join(".")}\n"
    contextStack.pop
  end
end
