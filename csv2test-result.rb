require 'set'
require 'csv'

funcs = Hash.new
index = 0

def toTagString(result)
    case str
    when "OK" then
        '<span class="label label-success">OK</span>'
    when "NG" then
        '<span class="label label-danger">NG</span>'
    when "保留" then
        '<span class="label label-default">保留</span>'
    when "未実施" then
        '<span class="label label-warning">未実施</span>'
    end
end

CSV.table(ARGV[0], encoding: "Shift_JIS:UTF-8", header_converters: nil).each do |row|
#CSV.foreach(ARGV[0], headers: :first_row, endoding: :) do |row|
    mkdRow = { :index => index, :subject => row[1], :content => row[2], :result => row[3], :remarks => row[4] }
    if !(funcs.key? row[0]) then
        funcs.store(row[0], [mkdRow])
    else
        funcs[row[0]].push(mkdRow)
    end
    index = index + 1
end

puts '# ' + File.basename(ARGV[0], ".csv")
funcs.each do |k, v|
    puts '## ' + k
    puts ''
    puts "|番号|機能|確認内容|結果|備考|".encode("cp932")
    puts "|---:|----|--------|----|----|".encode("cp932")
    v.each do |s|
        puts "|#{s[:index]}|#{s[:subject]}|#{s[:content]}|#{toTagString(s[:result])}|#{s[:remarks]}|".encode("cp932")
    end
    puts ''
end
