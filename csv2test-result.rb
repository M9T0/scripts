# encoding: utf-8

require 'set'
require 'csv'
require 'tempfile'

sourcepath = ARGV[0]
funcs = Hash.new
index = 1

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

temp = Tempfile::new("temp", "./")
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

system("pandoc", "--table-of-contents", "--output=" + File.basename(sourcepath, ".*") + ".html",
    "--to=html", "--from=markdown", "--highlight-style=tango",
    "--css=http://jasonm23.github.com/markdown-css-themes/markdown7.css",
    "--css=https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css", "-s", temp.path())
