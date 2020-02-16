package io.loustler.graph

final case class GraphEdge[E, N](source: Graph[E, N], target: Graph[E, N], value: E)

class Graph[E, N](var value: N = null.asInstanceOf[N]) {
  import scala.collection.immutable.Queue

  var inEdges: List[GraphEdge[E, N]] = Nil

  var outEdges: List[GraphEdge[E, N]] = Nil

  def succs: List[Graph[E, N]] = outEdges.map(_.target)

  def preds: List[Graph[E, N]] = inEdges.map(_.target)

  def connect(from: N, via: E, to: N): (Graph[E, N], Graph[E, N]) = {
    val fromGraph: Graph[E, N] = if (value == null) {
      value = from
      this
    } else Graph.one(from)

    val toGraph: Graph[E, N] = hop(to) match {
      case Some(g) => g
      case None    => Graph.one(to)
    }

    fromGraph.outEdges = GraphEdge(fromGraph, toGraph, via) :: fromGraph.outEdges
    toGraph.inEdges = GraphEdge(fromGraph, toGraph, via) :: toGraph.inEdges

    fromGraph -> toGraph
  }

  def hop(n: N): Option[Graph[E, N]] = graphsByDepth.find(_.value == n)

  /**
    * Node value.,From DFS
    * @return
    */
  def nodesByDepth: List[N] = graphsByDepth.map(_.value)

  /**
    * Nodes value, from BFS
    *
    * @return
    */
  def nodesByBreadth: List[N] = graphsByBreadth.map(_.value)

  /**
    * DFS(Depth First Search)
    *
    * @return
    */
  def graphsByDepth: List[Graph[E, N]] = {
    def loop(graph: Graph[E, N], set: Set[Graph[E, N]]): Set[Graph[E, N]] =
      if (!set(graph)) {
        val ss = graph.succs.foldLeft(set + graph) {
          case (acc, g) => loop(g, acc)
        }

        graph.preds.foldLeft(ss) {
          case (acc, g) => loop(g, acc)
        }
      } else set

    loop(this, Set.empty).toList
  }

  /**
    * BFS(Breadth First Search)
    *
    * @return
    */
  def graphsByBreadth: List[Graph[E, N]] = {
    @scala.annotation.tailrec
    def loop(queue: Queue[Graph[E, N]], set: Set[Graph[E, N]]): Set[Graph[E, N]] =
      if (queue.nonEmpty && !set(queue.head)) {
        val queue2 = queue.head.succs.foldLeft(queue.tail) {
          case (acc, g) => acc :+ g
        }

        loop(
          queue.head.preds.foldLeft(queue2) {
            case (acc, g) => acc :+ g
          },
          set + queue.head
        )
      } else set

    loop(Queue(this), Set.empty).toList
  }

  def size: Int = graphsByDepth.size
}

final class WeightedGraph[N](n: N) extends Graph[Double, N](n) {

  def dijkstra(from: N, to: N): List[N] = {
    var distances = nodesByDepth.map((_, Double.PositiveInfinity)).toMap + (from -> 0.0)

    var spt: Map[N, N] = Map()

    while (distances.nonEmpty) {
      val (v, _) = distances.minBy(_._2)

      distances = distances - v

      val vv = hop(v).get

      /**
        * 인접 노드에서 가장 최소거리 계산
        */
      for (e <- vv.outEdges) {
        val u = e.target.value

        if (distances(u) < distances(v) + e.value) {
          distances = distances + (u -> (distances(v) + e.value))

          spt = spt + (u -> v)
        }
      }
    }

    var step = to
    var path = List(step)

    while (spt.contains(step)) {
      step = spt(step)
      path = step :: path
    }

    path.reverse
  }
}

object Graph {
  def empty[E, N]: Graph[E, N] = new Graph[E, N]

  def one[E, N](n: N): Graph[E, N] = new Graph(n)

  def apply[E, N](tuples: (N, E, N)*): Graph[E, N] = {
    val graph = Graph.empty[E, N]

    for ((from, via, to) <- tuples) {
      graph.connect(from, via, to)
    }

    graph
  }
}
