package se.mjukomp.scones

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Shrink
import org.scalacheck.Shrink.shrink

object Generators {

  // Dangerous to use. - will fail.
  val garbageTextLeafGen: Gen[Leaf] = for {
    text <- Arbitrary.arbString.arbitrary
  } yield Leaf(text)

  val asciiLeafGen: Gen[Leaf] = for {
    text: String <- Gen.asciiPrintableStr
  } yield Leaf(text)

  val groupGen: Gen[Group] =
    Gen.lzy(sconesGen.map(lst => Group(lst)))

  val sconesGen: Gen[List[Scone]] =
    Gen.choose(0, 1000).flatMap(size =>
      Gen.listOfN(size, weightedSconeGen(size)))

  def weightedSconeGen(size: Int): Gen[Scone] =
    Gen.frequency(
      (size + (size >> 4) + 1, asciiLeafGen),
      (1, groupGen))

  def sconeGen: Gen[Scone] =
    Gen.oneOf(asciiLeafGen, groupGen)

  implicit def shrinkScone: Shrink[Scone] =
    Shrink { s: Scone =>
      s match {
        case Leaf(data) =>
          shrink(data).map(Leaf(_))
        case Group(children) =>
          children.toStream.append(
            shrink(children).map(Group(_)))
      }
    }

  implicit def shrinkLeaf(): Shrink[Leaf] =
    Shrink { leaf: Leaf =>
      for (d <- shrink(leaf.data)) yield Leaf(d)
    }

  implicit def shrinkGroup(): Shrink[Group] =
    Shrink { group: Group =>
      for (c <- shrink(group.children)) yield Group(c)
    }
}
