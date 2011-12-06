package scalahamburg.graph
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import org.scalatest.mock.EasyMockSugar
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class GraphSpec extends FlatSpec with ShouldMatchers {
  "An empty Graph" should "not be very useful" in {
    val empty = Graph()
    // does not compile:
    //    empty.hasNeighbours("blah") should be (false)
  }

  "A Graph" should "be constructed as an immutable object" in {
    val graph = Graph().add(("one", "two"), 3).add(("two", "three"), 5)
    graph.hasNeighbours("one") should be(true)
    graph.add(("blah", "three"), 3)
    graph.hasNeighbours("blah") should be(false)
  }

  //	it should "contain at least two nodes" in{
  //		val graph = Graph().add(("one", "one"), 100)
  //		graph.hasNeighbours("one") should be (false)
  //	}

  it should "join the added nodes" in {
    val graph = Graph().add(("one", "two"), 1).add(("three", "four"), 99)
    graph.hasNeighbours("one") should be(true)
    graph.neighbours("one") should be(Set("two"))
  }

  it should "use the weight in both directions" in {
    val graph = Graph().add(("one", "two"), 77)
    graph.weight("one", "two") should be(Some(77))
    graph.weight("two", "one") should be(Some(77))
  }

  it should "be able to remove nodes" in {
    val graph = Graph().add(("one", "two"), 1).remove("two")
    graph.hasNeighbours("one") should be(false)
  }

  it should "not complain about nonexisting nodes" in {
    val graph = Graph().add(("one", "two"), 1).remove("both")
    graph.hasNeighbours("nix") should be(false)
    graph.neighbours("nix") should be('empty) // call 'isEmpty' on result
    graph.shortestPath("nix", "niemals") should be(None)
    graph.weight("nix", "niemals") should be(None)
  }

  it should "provide a 'shortestPath' method" in {
    val graph = Graph() add
      (("one", "two"), 1) add
      (("two", "three"), 1) add
      (("one", "three"), 3)

    val shortest = graph.shortestPath("one", "three")
    shortest.get.weight should be(2)

  }
}
