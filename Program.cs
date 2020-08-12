using Antlr4.Runtime;
using System.Text;
using HtmlAgilityPack;
using System.Linq;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System;
using Antlr4.Runtime.Tree;
using System.Runtime.ExceptionServices;
using System.Diagnostics.CodeAnalysis;
using System.Net.Mime;

namespace ReadJavaGrammar
{
    class Program
    {
        public static string addr = "https://docs.oracle.com/javase/specs/jls/se8/html/jls-19.html";

        static void Rec(StringBuilder sb, string lhs, List<HtmlNode> rhs_node_children, bool alt = false)
        {
            bool first = true;
            for (int j = 0; j < rhs_node_children.Count; ++j)
            {
                var rhs_s = rhs_node_children[j];
                var rhs = rhs_s.InnerText;
                if (rhs.Trim() == "(one of)")
                {
                    alt = true;
                    ++j;
                    for (; j < rhs_node_children.Count; ++j)
                    {
                        if (rhs_node_children[j].Name == "br")
                            continue;
                        else break;
                    }
                }
                else if (rhs_s.Name == "br")
                {
                    if (j + 1 == rhs_node_children.Count
                        || (rhs_node_children[j + 1].InnerText.Replace(" ", "").Replace("\n", "").Trim() == "" && j+2 == rhs_node_children.Count))
                    {

                    }
                    else
                        sb.Append("| ");
                    // It appears after visiting the spec, a <br> can be in "(one of)"
                    // rules. But, the <br> has no meaning, so continue to consider anything
                    // as alt.
                    // Also, do not add "|" if the last thing on the children list
                    // as in "ConditionalExpression:"
                }
                else if (rhs_s.Name == "#text")
                {
                    sb.Append(rhs);
                }
                else if (rhs_s.Name == "a")
                {
                    if (alt && !first)
                    {
                        sb.Append("| ");
                    }
                    first = false;
                    sb.Append(lc(rhs));
                }
                else if (rhs_s.Name == "code")
                {
                    if (alt && !first)
                    {
                        sb.Append("| ");
                    }
                    first = false;
                    var str = rhs.Trim();
                    str = str.Replace("&lt;", "<");
                    str = str.Replace("&gt;", ">");
                    str = str.Replace("&amp;", "&");
                    rhs = "'" + str + "'";
                    sb.Append(rhs);
                }
                else if (rhs_s.Name == "pre")
                {
                    Rec(sb, lhs, rhs_s.ChildNodes.ToList(), true);
                }
                else
                {
                    sb.Append(rhs);
                }
            }
        }

        static void Main(string[] args)
        {
            StringBuilder sb = new StringBuilder();
            HtmlAgilityPack.HtmlDocument doc = null;
            string local_file_name = "c:/temp/java.gra";
            if (! System.IO.File.Exists(local_file_name))
            {
                HttpWebRequest request = (HttpWebRequest)WebRequest.Create(addr);
                HttpWebResponse response = (HttpWebResponse)request.GetResponse();
                if (response.StatusCode == HttpStatusCode.OK)
                {
                    Stream receiveStream = response.GetResponseStream();
                    StreamReader readStream = null;
                    if (String.IsNullOrWhiteSpace(response.CharacterSet))
                        readStream = new StreamReader(receiveStream);
                    else
                        readStream = new StreamReader(receiveStream, Encoding.GetEncoding(response.CharacterSet));
                    string data = readStream.ReadToEnd();
                    System.IO.File.WriteAllText(local_file_name, data);
                    response.Close();
                    readStream.Close();
                }
            }
            doc = new HtmlAgilityPack.HtmlDocument();
            string htmlString = System.IO.File.ReadAllText(local_file_name);
            doc.LoadHtml(htmlString);

            var lhs_list = doc.DocumentNode.SelectNodes("//div[@class='lhs']").ToList();
            var rhs_list = doc.DocumentNode.SelectNodes("//div[@class = 'rhs']").ToList();
            for (int i = 0; i < lhs_list.Count; ++i)
            {
                var lhs = lhs_list[i].InnerText;
                var rhs_node = rhs_list[i];
                List<HtmlNode> rhs_node_children = rhs_node.ChildNodes.ToList();
                if (i >= 4)
                {
                    sb.AppendLine(lc(lhs));
                    Rec(sb, lhs, rhs_node_children);
                    sb.AppendLine(";");
                }
            }
            var input = sb.ToString();
            var str = new AntlrInputStream(input);
            var lexer = new JavaSpecLexer(str);
            var tokens = new CommonTokenStream(lexer);
            var parser = new JavaSpecParser(tokens);
            var tree = parser.rules();
            var walker = new ParseTreeWalker();
            var listener = new Listen();
            walker.Walk(listener, tree);
            System.Console.WriteLine(listener.sb.ToString());
        }

        static string lc(string id)
        {
            if (id != string.Empty && char.IsUpper(id[0]))
            {
                id = char.ToLower(id[0]) + id.Substring(1);
            }
            return id;
        }
    }

    class Listen : JavaSpecBaseListener
    {
        public StringBuilder sb = new StringBuilder();

        public Listen()
        {
            sb.AppendLine("grammar Java"
                + (Program.addr.Contains("se8") ? "8" : "unknown")
                + ";");
            sb.AppendLine();
        }

        public override void ExitRule([Antlr4.Runtime.Misc.NotNull] JavaSpecParser.RuleContext context)
        {
            sb.AppendLine(" ;");
        }

        public override void ExitLhs([Antlr4.Runtime.Misc.NotNull] JavaSpecParser.LhsContext context)
        {
            sb.Append(context.ID().GetText() + " :");
        }

        public override void ExitRhs_sym([Antlr4.Runtime.Misc.NotNull] JavaSpecParser.Rhs_symContext context)
        {
            if (context.ID() != null) sb.Append(" " + context.ID().GetText());
            else if (context.PIPE() != null) sb.Append(" |");
            else if (context.SL() != null) sb.Append(" " + context.SL().GetText());
        }

        public override void EnterZero_or_one([Antlr4.Runtime.Misc.NotNull] JavaSpecParser.Zero_or_oneContext context)
        {
            var rhs = context.rhs();
            if (rhs.ChildCount == 0 || rhs.ChildCount >= 2)
            {
                sb.Append(" (");
            }
        }

        public override void ExitZero_or_one([Antlr4.Runtime.Misc.NotNull] JavaSpecParser.Zero_or_oneContext context)
        {
            var rhs = context.rhs();
            if (rhs.ChildCount == 0 || rhs.ChildCount >= 2)
            {
                sb.Append(" )");
            }
            sb.Append("?");
        }

        public override void EnterZero_or_more([Antlr4.Runtime.Misc.NotNull] JavaSpecParser.Zero_or_moreContext context)
        {
            var rhs = context.rhs();
            if (rhs.ChildCount == 0 || rhs.ChildCount >= 2)
            {
                sb.Append(" (");
            }
        }

        public override void ExitZero_or_more([Antlr4.Runtime.Misc.NotNull] JavaSpecParser.Zero_or_moreContext context)
        {
            var rhs = context.rhs();
            if (rhs.ChildCount == 0 || rhs.ChildCount >= 2)
            {
                sb.Append(" )");
            }
            sb.Append("*");
        }
    }
}
