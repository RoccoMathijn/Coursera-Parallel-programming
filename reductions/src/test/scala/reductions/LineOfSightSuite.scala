package reductions

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner]) 
class LineOfSightSuite extends FunSuite {
  import LineOfSight._
  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }


  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }


  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("upsweep") {
    val input = Array[Float](0f, 1f, 8f, 9f)
    val t1 = upsweep(input, 0, input.length, 1)
    assert(t1 == Node(Node(Leaf(0, 1, 0), Leaf(1, 2, 1)), Node(Leaf(2, 3, 4), Leaf(3, 4, 3))))
  }

  test("upsweep should correctly compute the tree on the indices 1 until 5 of a 5 element array for threshold 2") {
    val input = Array[Float](0f, 1f, 14f, 36f, 8f)
    val t1 = upsweep(input, 1, input.length, 2)
    assert(t1 == Node(Leaf(1,3,7.0f),Leaf(3,5,12.0f)))
  }

  test("downsweep") {
    val input = Array[Float](0f, 1f, 8f, 9f)
    val output = new Array[Float](4)
    val t1 = upsweep(input, 1, input.length, 1)
    downsweep(input, output, 0, t1)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("downsweep startingMax") {
    val input = Array[Float](0f, 1f, 8f, 21f)
    val output = new Array[Float](4)
    val t1 = upsweep(input, 1, input.length, 1)
    downsweep(input, output, 6f, t1)
    assert(output.toList == List(0f, 6f, 6f, 7f))
  }

  test("downsweep should correctly compute the output for a tree with 4 leaves when the starting angle is zero") {
    val input = Array[Float](0f, 7f, 3f, 33f, 48f)
    val output = new Array[Float](5)
    val t1 = upsweep(input, 1, input.length, 1)
    downsweep(input, output, 0, t1)
    assert(output.toList == List(0f, 7f, 7f, 11f, 12f))
  }
}

