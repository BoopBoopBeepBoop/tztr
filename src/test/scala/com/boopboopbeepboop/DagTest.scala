package com.boopboopbeepboop

import com.boopboopbeepboop.util.Dag
import com.boopboopbeepboop.util.Dag.{Continue, DagChange, Halt}
import org.scalatest.{FunSpec, Matchers}

class DagTest extends FunSpec with Matchers {

  val root = Dag("root")

  val split1 = Dag("split1", Seq(root))
  val split2 = Dag("split2", Seq(root))

  val join = Dag("join", Seq(split1, split2))

  describe("Dag") {
    it("visits every node") {

      var acc = Map.empty[String, Int].withDefaultValue(0)
      Dag.visit(join) { node => acc += (node -> (acc(node) + 1)); true }

      acc shouldEqual Map(
        "root" -> 1,
        "split1" -> 1,
        "split2" -> 1,
        "join" -> 1
      )
    }

    it("visits some nodes") {

      var acc = Map.empty[String, Int].withDefaultValue(0)
      val out = Dag.visit(join) { node =>
        acc += (node -> (acc(node) + 1))

        if (node.contains("split")) false
        else true
      }

      // now we shouldn't make it to the root node; condition above stops us.
      acc shouldEqual Map(
        "split1" -> 1,
        "split2" -> 1,
        "join" -> 1
      )
      out shouldEqual Set(split1, split2, join).map(_.node)
    }

    it("transforms a graph") {
      val (newDag, _) = Dag.transform(join) { n => DagChange(n + "_1") }

      val collected = Dag.visit(newDag)(_ => true)
      collected shouldEqual Set("root_1", "split1_1", "split2_1", "join_1")
    }

    it("partially transforms a graph") {
      val (newDag, visited) = Dag.transform(join) { n =>
        val go = if (n.contains("split")) Halt else Continue
        DagChange(n + "_1", go)
      }
      visited shouldEqual Set("split1_1", "split2_1", "join_1")

      val collected = Dag.visit(newDag)(_ => true)
      // this time, root should not be changed
      collected shouldEqual Set("root", "split1_1", "split2_1", "join_1")
    }
  }
}
