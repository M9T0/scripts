# encoding: utf-8

require 'set'
require 'csv'
require 'tempfile'

def toTagString(arg)
  str = "未実施"
  cls = "warning"
  case arg.encode("UTF-8")
  when "OK" then
    str = "OK"
    cls = "success"
  when "NG" then
    str = "NG"
    cls = "danger"
  when "保留" then
    str = "保留"
    cls = "default"
  when "未実施" then
    str = "未実施"
    cls = "warning"
  end
  "<span class=\"label label-#{cls}\">#{str}</span>"
end

#temp = File.new("temp.md", "w+")
temp = Tempfile.new("temp", "./")
index = 1

ARGV.map do |sourcepath|
  funcs = Hash.new
  open(sourcepath, "rb:Shift_JIS:UTF-8", undef: :replace) do |f|
    CSV.new(f, headers: :first_row).each do |row|
      mkdRow = { :index => index,
                 :subject => row[1],
                 :content => row[2],
                 :result => row[3].to_s(),
                 :remarks => row[4] }
      if !(funcs.key? row[0]) then
        funcs.store(row[0], [mkdRow])
      else
        funcs[row[0]].push(mkdRow)
      end
      index = index + 1
    end
  end

  temp.puts "# " + File.basename(sourcepath, ".*").encode("UTF-8") + "\n"
  funcs.each do |k, v|
    temp.puts "## " + k.to_s() + "\n"
    temp.puts "\n"
    temp.puts "|番号|機能|確認内容|結果|備考|\n"
    temp.puts "|---:|----|--------|----|----|\n"
    v.each do |s|
      temp.puts "|#{s[:index]}|#{s[:subject]}|#{s[:content]}|#{toTagString(s[:result])}|#{s[:remarks]}|\n"
    end
    temp.puts "\n"
  end
end
temp.fsync()

system("pandoc",
       "--table-of-contents",
       "--output=" + File.basename(ARGV[0], ".*") + ".html",
       "--to=html5",
       "--from=markdown",
       "--highlight-style=tango",
       "--smart",
       "--standalone",
       "--self-contained",
       "--css=http://jasonm23.github.com/markdown-css-themes/markdown7.css",
       "--css=https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css", temp.path)
