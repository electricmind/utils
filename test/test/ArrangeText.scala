package test
import ru.wordmetrix.webcrawler.{ Vector, TreeApproximator, debug, CFG }
import TreeApproximator._
import java.io._
object ArrangeText extends App {
    implicit lazy val cfg = CFG(List("-d"))
    type Word = Int
    implicit lazy val accuracy: Double = 0.01
    
    object string2word {
        var map = Map[String,Word]()
        val words = Iterator.from(10)
        def apply(s : String) = {
            val word = map.getOrElse(s, words.next)
            map = map + (s -> word)
            word
        }
    }
    
    def arrange(tree: Tree[Word, File], path: File): Unit = tree match {
        case node: Node[Word, File] => {
            arrange(node.child1, new File(path, "1"))
            arrange(node.child2, new File(path, "2"))
        }
        case leaf: Leaf[Word, File] => {
            path.mkdirs()
            println("%s -> %s".format(leaf.value, path))
            val fin = new FileInputStream(leaf.value)
            val buf = new Array[Byte](fin.available())
            fin.read(buf)
            val fout = new FileOutputStream(new File(path, leaf.value.getName()))
            fout.write(buf)
            fout.close()
            fin.close()
        }
    }

    override def main(args: Array[String]) {
        val delimiter = """\W+""".r

        val Array(target, files @ _*) = args

        def vectors = files.toIterator.map(x => {
            ((Vector(
                io.Source.fromFile(x).getLines().map(delimiter.split).flatten
                    .toList.groupBy(x =>                         x.toLowerCase())
                    .map({ case (x, y) => (x, y.length.toDouble) })
                    .filter(_._2 > 5)
                    .map({case (x,y) => string2word(x) -> y })
                    .toList),
                new File(x))
            )
        })
        
        val t = System.currentTimeMillis()
        def tree = vectors.foldLeft(TreeApproximator(vectors.next))({
            case (tree, (vector, filename)) => {
                debug.time("%s %d %s tree(%s).energy => %4.3f, length = %d / %d".format(
                    (System.currentTimeMillis() - t)/10,
                    tree.n,
                    string2word.map.size,
                    filename,
                    tree.energy2,
                    vector.size,
                    tree.average.size)
                ) {
                    (tree + (vector, filename)).asInstanceOf[TreeApproximator.Node[Word,File]].rectify(2)
                }

            }
        })

        def tree_opt = (1 to 5).foldLeft(tree.asInstanceOf[Node[Word, File]])({
            case (tree, n) =>
                debug.time("Rectifying #%3d = %4.3f %d".format(
                    n, tree.energy2, tree.average.size)
                ) {
                    tree.rectify(tree.n)
                }
        })

        arrange(tree_opt.align()._1, new File(target))
    }
}
