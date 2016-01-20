require 'csv'

# 行
class Row
    def initialize(s, c, r, m)
        @subject = s
        @content = c
        @result  = r
        @remark  = m
    end

    def toString()
        return "|" + s + "|" + c + "|" + r + "|" + m + "|\n"
    end
end

class Node
end

# テーブル
class Leaf extend Node
    def initialize(n, rows)
        @name = n
        @rows = rows
    end

    def toString()
        result = "|機能|確認内容|結果|備考|\n"
        result += "|----|------|----|----|\n"
        for row in rows
            result += row.toString()
        end
        return result
    end
end

class Branch extend Node
    def initialize(n, child)
        @name = n
        @children = child
    end

    def toString()
        result = '# ' + @name + "\n"
        for c in @children
            result += c.toString()
        end
    end
end
