package streams

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Bloxorz._
import scala.collection.immutable.BitSet

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin
      
    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  trait Level2 extends SolutionChecker with BridgeStateTerrain
  {
    val level =
    """------oooo--ooo
      |oooo--oo!o--oTo
      |oo!o--oooo--ooo
      |oooo--oooo--ooo
      |oSoo++oooo++ooo
      |oooo--oooo-----""".stripMargin
      
    val switches : List[AbstractSwitch] = List(
      new SlightSwitch(Pos(2,2), List(Pos(4,4), Pos(4, 5))),
      new VerticalSwitch(Pos(1,8), List(Pos(4,10), Pos(4, 11)))
    )

    val state = new State()
  }
  
  trait Level3 extends SolutionChecker
  {
    val level =
    """------ooooooo--
      |oooo--ooo--oo--
      |ooooooooo--oooo
      |oSoo-------ooTo
      |oooo-------oooo
      |------------ooo""".stripMargin
  }
  
  trait Level4 extends SolutionChecker with BridgeStateTerrain
  {
    val level =
    """---RRRRRRRR----
      |---RRRRRRRR----
      |oooo------ooo--
      |ooo--------oo--
      |ooo--------oo--
      |oSo---ooooRRRRR
      |ooo---ooooRRRRR
      |------oTo--RRoR
      |------ooo--RRRR""".stripMargin
    
    val switches : List[AbstractSwitch] = List()

    val state = new State()
  }

  trait Level5 extends SolutionChecker with BridgeStateTerrain
  {
    val level =
    """-----------oooo
      |-oooo++o!ooooSo
      |-oooo-------ooo
      |-oo!o----------
      |-oooo----------
      |---ooo!oooooo--
      |----------oooo!
      |ooo-------ooooo
      |oTooo++oooooo--
      |oooo-----------""".stripMargin
    
    val switches : List[AbstractSwitch] = List(
      new SlightSwitch(Pos(1,8), List(Pos(1,6), Pos(1, 5))),
      new OpenCompoundSwitch(Pos(3,3), List(Pos(8,5), Pos(8, 6)), BitSet(1, 2, 3)),
      new CloseCompoundSwitch(Pos(5,6), List(Pos(8,5), Pos(8, 6)), BitSet(1, 2, 3)),
      new SlightCompundSwitch(Pos(6,14), List(Pos(8,5), Pos(8, 6)), BitSet(1, 2, 3), BitSet(1, 2, 3))
    )

    val state = new State(BitSet(0,1,2,3))
  }
  
  trait Level6 extends SolutionChecker
  {
    val level =
    """-----oooooo----
      |-----o--ooo----
      |-----o--ooooo--
      |Sooooo-----oooo
      |----ooo----ooTo
      |----ooo-----ooo
      |------o--oo----
      |------ooooo----
      |------ooooo----
      |-------ooo-----""".stripMargin
  }

  trait Level7 extends SolutionChecker with BridgeStateTerrain
  {
    val level =
    """--------oooo---
      |--------oooo---
      |ooo-----o--oooo
      |oSooooooo---oTo
      |ooo----oo!--ooo
      |ooo----ooo--ooo
      |-oo+---o-------
      |--oooooo-------
      |---------------
      |---------------""".stripMargin
    
    val switches : List[AbstractSwitch] = List(
      new VerticalSwitch(Pos(4,9), List(Pos(6, 3)))
    )

    val state = new State(BitSet())
  }
  
  trait Level8 extends SolutionChecker with BridgeStateTerrain
  {
    val level =
    """---------------
      |-------ooo-----
      |-------odo-----
      |-------ooo-----
      |oooooo-oooooo--
      |oSoodo-ooooTo--
      |oooooo-oooooo--
      |-------ooo-----
      |-------odo-----
      |-------ooo-----""".stripMargin
      
    val switches : List[AbstractSwitch] = List()

    val state = new State(BitSet())
    
    override val splitter: List[(Pos, Pos, Pos)] = List((Pos(5, 4), Pos(2, 8), Pos(8, 8)))
  }
  
  trait Level9 extends SolutionChecker with BridgeStateTerrain
  {
    val level =
    """---------------
      |---------------
      |---------------
      |---------------
      |oooo---o---oooo
      |oSdo---o---oddo
      |ooooooooooooooo
      |------oTo------
      |------ooo------
      |---------------""".stripMargin
      
    val switches : List[AbstractSwitch] = List()

    val state = new State(BitSet())
    
    override val splitter: List[(Pos, Pos, Pos)] = List((Pos(5, 13), Pos(5, 12), Pos(5, 2)))
  }

  trait Level10 extends SolutionChecker with BridgeStateTerrain
  {
    val level =
    """ooo-----oooooo-
      |oTo++o++oSoodo-
      |ooo-----oooo+--
      |---------ooo+--
      |-----------oo--
      |------------o--
      |------------o--
      |-----------oo--
      |----ooooo--oo--
      |----o!--ooooo--""".stripMargin
      
    val switches : List[AbstractSwitch] = List(
      new SlightSwitch(Pos(9,5), List(Pos(1,3), Pos(1, 4))),
      new VerticalSwitch(Pos(9,11), List(Pos(1,6), Pos(1, 7), Pos(2, 12), Pos(3, 12)))
    )

    val state = new State(BitSet())
    
    override val splitter: List[(Pos, Pos, Pos)] = List((Pos(1, 12), Pos(1, 12), Pos(1, 9)))
  }

  trait Level11 extends SolutionChecker with BridgeStateTerrain
  {
    val level =
    """-ooo+----------
      |-oTo+----------
      |-ooo-----------
      |-o---oooooo----
      |-o---oo--oo----
      |Soooooo--ooo---
      |-----o!----o---
      |-----oooo--o---
      |-----ooooooo---
      |--------ooo----""".stripMargin
      
    val switches : List[AbstractSwitch] = List(
      new CloseSlightSwitch(Pos(6, 6), List(Pos(0,4), Pos(1, 4)))
    )

    val state = new State(BitSet(0))
  }

  trait Level12 extends SolutionChecker with BridgeStateTerrain
  {
    val level =
    """--------------!
      |-------ooo--ooo
      |-------o!ooooo+
      |-----ooooo--oo-
      |-----oTo+---oo-
      |---ooooo---oooo
      |--ooSo-----oooo
      |--oooo--ooooo--
      |-------ooo-----
      |-------ooo-----""".stripMargin
      
    val switches : List[AbstractSwitch] = List(
      new VerticalSwitch(Pos(2, 8), List(Pos(2, 14))),
      new VerticalSwitch(Pos(0, 14), List(Pos(4, 8)))
    )

    val state = new State(BitSet())
  }
  
  trait Level13 extends SolutionChecker with BridgeStateTerrain
  {
    val level =
    """oooRooooRoooo--
      |oo--------ooo--
      |oo---------ooo-
      |ooo---ooo--oSo-
      |oooRRRoTo--ooo-
      |ooo--Rooo--o---
      |--o--RRRRRoo---
      |--oooRRoRRR----
      |---ooRRRRRR----
      |---ooo--oo-----""".stripMargin
      
    val switches : List[AbstractSwitch] = List()

    val state = new State(BitSet())
  }

  trait Level14 extends SolutionChecker with BridgeStateTerrain
  {
    val level =
    """---------ooo---
      |----ooo--ooo---
      |-o++oSooooooooo
      |-o++ooo------!o
      |-o-----------oo
      |-o-----------oo
      |-o-------oooooo
      |-ooooo---ooo---
      |--ooTo---ooo---
      |---ooo---ooooo!""".stripMargin
      
    val switches : List[AbstractSwitch] = List(
      new VerticalSwitch(Pos(3, 13), List(Pos(2, 2), Pos(2, 3))),
      new VerticalSwitch(Pos(9, 14), List(Pos(3, 2), Pos(3, 3)))
    )

    val state = new State(BitSet())
  }
  
  trait Level15 extends SolutionChecker with BridgeStateTerrain
  {
    val level =
    """-------ooo--ooo
      |----o++ooo++!oo
      |oo++o--ooo--ooo
      |ooooo---!------
      |oo-------------
      |-o-----d-------
      |-o-----o-------
      |ooo---ooo--!oo-
      |oSooooooo++oTo-
      |ooo---ooo--!oo-""".stripMargin
      
    val switches : List[AbstractSwitch] = List(
      new CloseCompoundSwitch(Pos(7, 11), List(Pos(8, 9), Pos(8, 10)), BitSet(0, 1)),
      new CloseCompoundSwitch(Pos(9, 11), List(Pos(8, 9), Pos(8, 10)), BitSet(0, 1)),
      new VerticalSwitch(Pos(1, 12), List(Pos(2, 2), Pos(2, 3))),
      new VerticalCompoundSwitch(Pos(1, 12), List(Pos(1, 5), Pos(1, 6)), BitSet(3, 4), BitSet(3, 4)),
      new SlightCompundSwitch(Pos(3, 8), List(Pos(1, 5), Pos(1, 6)), BitSet(3, 4), BitSet(3, 4)),
      new SlightSwitch(Pos(3, 8), List(Pos(1, 10), Pos(1, 11)))
    )

    val state = new State(BitSet(0, 1, 3, 4))
    
    override val splitter: List[(Pos, Pos, Pos)] = List((Pos(5, 7), Pos(1, 13), Pos(8, 1)))
  }

  trait Level16 extends SolutionChecker with BridgeStateTerrain
  {
    val level =
    """---d--------ooo
      |--dod++!!o++oTo
      |---d--------ooo
      |---------------
      |-----ooo---ooo-
      |-----oSooooodo-
      |-----ooo---ooo-
      |---------------
      |---------------
      |---------------""".stripMargin
      
    val switches : List[AbstractSwitch] = List(
      new OpenVerticalSwitch(Pos(1, 7), List(Pos(1, 5), Pos(1, 6))),
      new OpenVerticalSwitch(Pos(1, 8), List(Pos(1, 10), Pos(1, 11)))
    )

    val state = new State(BitSet())
    
    override val splitter: List[(Pos, Pos, Pos)] = List(
      (Pos(1, 2), Pos(0, 3), Pos(1, 4)),
      (Pos(2, 3), Pos(2, 3), Pos(1, 2)),
      (Pos(0, 3), Pos(1, 7), Pos(1, 9)),
      (Pos(1, 4), Pos(1, 4), Pos(1, 2)),
      (Pos(5, 12), Pos(0, 3), Pos(1, 2))
    )
  }

  trait Level17 extends SolutionChecker with NewTerrain
  {
    val level =
    """ooo------------
      |oSooooooo+--ooo
      |ooo----+oooooTo
      |ooo---------!!o
      |ooo------------
      |ooo------------
      |ooo---+ooooo!--
      |oooooooo+--oo--
      |o!o--------oo--
      |ooo--------o!--""".stripMargin
      
    val switches : List[AbstractSwitch] = List(
      new ChangeSlightSwitch(Pos(8, 1), List(Pos(7, 8)), (id => id ^ BitSet(0))),
      new ChangeVerticalSwitch(Pos(6, 12), List(Pos(2, 7)), (id => id + 1)),
      new ChangeVerticalSwitch(Pos(9, 12), List(), (id => id - 0)),
      new ChangeVerticalSwitch(Pos(3, 12), List(Pos(6, 6)), (id => id - 3)),
      new ChangeVerticalSwitch(Pos(3, 13), List(), (id => id + 3)),
      new ChangeVerticalSwitch(Pos(9, 12), List(Pos(1, 9)), (id => id + 5 - 0))
    )

    val state = new State(BitSet())
  }

  trait Level18 extends SolutionChecker with NewTerrain
  {
    val level =
    """-------!-------
      |oo!o---o-------
      |ooooo--o-------
      |o!Sooooo++oo++o
      |ooooo+--o---o--
      |oo!o----o---o--
      |o-------!--ooo-
      |o---------ooTo-
      |o++!------oooo-
      |---------------""".stripMargin
      
    val switches : List[AbstractSwitch] = List(
      new ChangeSlightSwitch(Pos(0, 7), List(Pos(3, 8), Pos(3, 9)), (id => id + 0)),
      new ChangeSlightSwitch(Pos(3, 1), List(), (id => id - 0)),
      
      new ChangeSlightSwitch(Pos(1, 2), List(Pos(3, 12), Pos(3, 13), Pos(8, 1), Pos(8, 2)), (id => id - 2)),
      new ChangeSlightSwitch(Pos(5, 2), List(), (id => id - 2)),
      
      new ChangeSlightSwitch(Pos(6, 8), List(), (id => id + 2)),
      new ChangeVerticalSwitch(Pos(8, 3), List(Pos(4, 5)), (id => id ^ BitSet(5)))
    )

    val state = new State(BitSet())
  }

  trait Level19 extends SolutionChecker with NewTerrain
  {
    val level =
    """-Soooooooo!oooo
      |-----oo------oo
      |-----oo------oo
      |-------------oo
      |-------------oo
      |ooo--oo++o!oooo
      |oTo--oo--------
      |ooo--oo--------
      |-oo--oo--------
      |-o++oooooo!ooo-""".stripMargin
      
    val switches : List[AbstractSwitch] = List(
      new ChangeSlightSwitch(Pos(0, 10), List(Pos(5, 7), Pos(5, 8)), (id => id ^ BitSet(0))),
      new ChangeSlightSwitch(Pos(5, 10), List(Pos(9, 2), Pos(9, 3)), (id => id - 1)),
      new ChangeSlightSwitch(Pos(9, 10), List(), (id => id + 1))
    )

    val state = new State(BitSet(1))
  }

  trait Level20 extends SolutionChecker with NewTerrain
  {
    val level =
    """------------ooo
      |--ooo++ooo++ooo
      |--ooo--!So--ooo
      |--ooo--ooo-----
      |--o!o--do!-----
      |--ooo--ooo-----
      |oooo---ooo++!oo
      |o!----------ooo
      |------------oTo
      |------------ooo""".stripMargin
      
    val switches : List[AbstractSwitch] = List(
      new ChangeSlightSwitch(Pos(2, 7), List(Pos(1, 5), Pos(1, 6)), (id => id - 0)),
      new ChangeSlightSwitch(Pos(4, 9), List(), (id => id - 0)),
      new ChangeSlightSwitch(Pos(6, 12), List(Pos(6, 10), Pos(6, 11)), (id => id ^ BitSet(2))),
      new ChangeSlightSwitch(Pos(4, 3), List(), (id => id - 0)),
      new ChangeSlightSwitch(Pos(7, 1), List(Pos(1, 10), Pos(1, 11)), (id => id ^ BitSet(4)))
    )

    val state = new State(BitSet(0))
    
    override val splitter: List[(Pos, Pos, Pos)] = List((Pos(4, 7), Pos(1, 13), Pos(7, 13)))
  }

  trait Level21 extends SolutionChecker with NewTerrain
  {
    val level =
    """--------oo-----
      |-------ooo-----
      |oo--oooooo-----
      |oSoooo--o------
      |oooo----o---ooo
      |-oo-----!ooooTo
      |--o-----!o--ooo
      |--ooo+--oo-----
      |---ooo--oo-----
      |---+oooooo-----""".stripMargin
      
    val switches : List[AbstractSwitch] = List(
      new ChangeVerticalSwitch(Pos(5, 8), List(Pos(9, 3)), (id => id ^ BitSet(0))),
      new ChangeVerticalSwitch(Pos(6, 8), List(Pos(7, 5)), (id => id ^ BitSet(1)))
    )

    val state = new State(BitSet())
  }

  trait Level22 extends SolutionChecker with NewTerrain
  {
    val level =
    """------oo----ooo
      |----oooooo--oTo
      |-oooooo!ooooooo
      |-oSoo!--ooooo+-
      |-ooo------ooo--
      |--o--------o---
      |--o--------o---
      |--o+------oo---
      |--oo------oo---
      |---!------!----""".stripMargin
      
    val switches : List[AbstractSwitch] = List(
      new ChangeVerticalSwitch(Pos(9, 10), List(Pos(7, 3)), (id => id ^ BitSet(0))),
      new ChangeVerticalSwitch(Pos(9, 3), List(Pos(3, 13)), (id => id ^ BitSet(1))),
      new ChangeSlightSwitch(Pos(2, 7), List(), (id => id - 0 - 1)),
      new ChangeSlightSwitch(Pos(3, 5), List(), (id => id - 0 - 1))
    )

    val state = new State(BitSet())
  }
  
  trait Level23 extends SolutionChecker with NewTerrain
  {
    val level =
    """-ooo--------ooo
      |-o!o--------o!o
      |-ooo---ooo++ooo
      |+ooo+--oTo--oo!
      |o---o--ooo----o
      |!---o--RRR----o
      |o++oooRRRRRooo+
      |---oSoRRRRRodo-
      |---oooRRRRRooo-
      |---ooooo+------""".stripMargin
      
    val switches : List[AbstractSwitch] = List(
      new Switch(List(Pos(2, 10), Pos(2, 11))),
      new Switch(List(Pos(6, 14))),
      
      new Switch(List(Pos(9, 8))),
      new Switch(List(Pos(6, 1), Pos(6, 2))),
      new Switch(List(Pos(3, 0))),
      new Switch(List(Pos(3, 4))),
      
      //     new ChangeSlightSwitch(Pos(5, 0), List(), (id => (id - 3) + 4)),
      new ChangeSlightSwitch(Pos(5, 0), List(), (id => BitSet(4))),
      new ChangeSlightSwitch(Pos(1, 13), List(), (id => id + 3 ^ BitSet(2))),
      new ChangeSlightSwitch(Pos(3, 14), List(), (id => id - 0 - 1)),
      new ChangeVerticalSwitch(Pos(1, 2), List(), (id => id + 5))
    )

    val state = new State(BitSet(0, 1))
    
    override val splitter: List[(Pos, Pos, Pos)] = List((Pos(7, 12), Pos(7, 12), Pos(2, 2)))
  }
  
  trait Level24 extends SolutionChecker with NewTerrain
  {
    val level =
    """-----------oooo
      |----+ooooooo!od
      |--S++o!o---oooo
      |-!o--oo------o-
      |-oo--o-------o-
      |-ooooo-----ooo-
      |-ooo--ooo++oTo-
      |------!o+--ooo-
      |---------------
      |---------------""".stripMargin
      
    val switches : List[AbstractSwitch] = List(
      new ChangeVerticalSwitch(Pos(1, 12), List(Pos(2, 3), Pos(2, 4)), (id => id + 0)),
      new ChangeVerticalSwitch(Pos(3, 1), List(Pos(1, 4)), (id => id + 1)),
      new ChangeVerticalSwitch(Pos(2, 6), List(Pos(7, 8)), (id => id + 2)),
      new ChangeVerticalSwitch(Pos(7, 6), List(Pos(6, 9), Pos(6, 10)), (id => id + 3))
    )

    val state = new State(BitSet())
    
    override val splitter: List[(Pos, Pos, Pos)] = List((Pos(1, 14), Pos(6, 6), Pos(6, 8)))
  }
  
  trait Level25 extends SolutionChecker with NewTerrain
  {
    val level =
    """---oo----------
      |---ooo---------
      |---oo!-----ooo+
      |----oooo+--oTo+
      |-------oo++ooo-
      |--oo---oo------
      |-oo!o++oo------
      |-oSo---oo---ooo
      |-ooo---oo!ooooo
      |------------ooo""".stripMargin
      
    val switches : List[AbstractSwitch] = List(
      new ChangeVerticalSwitch(Pos(6, 3), List(Pos(4, 9), Pos(4, 10)), (id => id + 0)),
      new ChangeSlightSwitch(Pos(2, 5), List(Pos(2, 14), Pos(3, 14)), (id => id ^ BitSet(1, 0))),
      new ChangeSlightSwitch(Pos(8, 9), List(Pos(3, 8)), (id => id + 2 - 3)),
      new Switch(List(Pos(6, 5), Pos(6, 6)))
    )

    val state = new State(BitSet(3))
  }
  
  trait Level26 extends SolutionChecker with NewTerrain
  {
    val level =
    """------oooo----d
      |------oo!ooo--o
      |-----ooooooo--o
      |-oo++oooo--oooo
      |-ooo+--o---oo--
      |-ooo---o---S---
      |--o----ooo-----
      |--!----oTo+----
      |-------ooo-----
      |---------------""".stripMargin
      
    val switches : List[AbstractSwitch] = List(
      new ChangeVerticalSwitch(Pos(7, 2), List(Pos(4, 4), Pos(7, 10)), (id => id + 0)),
      new ChangeSlightSwitch(Pos(1, 8), List(Pos(3, 3), Pos(3, 4)), (id => id - 1))
    )
  
    override val splitter: List[(Pos, Pos, Pos)] = List((Pos(0, 14), Pos(3, 13), Pos(5, 11)))

    val state = new State(BitSet(1))
  }

  trait Level27 extends SolutionChecker with NewTerrain
  {
    val level =
    """ooo----oooooooo
      |oSooooooooo--oo
      |ooo----oo----oo
      |------------o!o
      |------------oo-
      |ooo--RRRRo--!!-
      |oToRRRRRRR--ooo
      |oooRRRRRRRRRooo
      |-----RRRRRRRooo
      |------+oo+-----""".stripMargin
      
    val switches : List[AbstractSwitch] = List(
      new ChangeVerticalSwitch(Pos(3, 13), List(), (id => id - 1 - 2)),
      new ChangeSlightSwitch(Pos(5, 13), List(Pos(9, 9)), (id => id - 1)),
      new ChangeSlightSwitch(Pos(5, 12), List(Pos(9, 6)), (id => id - 2))
    )

    val state = new State(BitSet(1, 2))
  }

  trait Level28 extends SolutionChecker with NewTerrain
  {
    val level =
    """-oo++oo--------
      |-oo--ooo-------
      |RRS--oooo------
      |RR-----ooo-----
      |RR------ooo----
      |Rooo-----ood---
      |-oTo------ooooo
      |-oooooo---o!ooo
      |--o--oo---ooo--
      |--o--ooo++ooo--""".stripMargin
      
    val switches : List[AbstractSwitch] = List(
      new ChangeSlightSwitch(Pos(7, 11), List(Pos(0, 3), Pos(0, 4), Pos(9, 8), Pos(9, 9)), (id => id - 0))
    )
    
    val state = new State(BitSet(0))
    override val splitter: List[(Pos, Pos, Pos)] = List((Pos(5, 11), Pos(6, 14), Pos(8, 12)))
  }

  trait Level29 extends SolutionChecker with NewTerrain
  {
    val level =
    """--!++o---o++!--
      |-----o---o-----
      |-----ooooo-----
      |!++ooooSoooo++!
      |-----ooooo-----
      |-----+o--o-----
      |-----+o--o++!--
      |ooo--oo--o-----
      |oTo++o---o-----
      |ooo+-----o++!--""".stripMargin
      
    val switches : List[AbstractSwitch] = List(
      new Switch(List(Pos(0, 3), Pos(0, 4))),
      new Switch(List(Pos(0, 10), Pos(0, 11))),
      new Switch(List(Pos(3, 1), Pos(3, 2))),
      new Switch(List(Pos(3, 12), Pos(3, 13))),
      new Switch(List(Pos(6, 10), Pos(6, 11))),
      new Switch(List(Pos(9, 10), Pos(9, 11))),
      new Switch(List(Pos(9, 3))),
      new Switch(List(Pos(5, 5), Pos(6, 5))),
      new Switch(List(Pos(8, 3), Pos(8, 4))),
      
      new ChangeSlightSwitch(Pos(0, 2), List(), (id => id + 1 - 4)),
      new ChangeSlightSwitch(Pos(9, 12), List(), (id => id + 3 - 0 - 1 - 4)),
      new ChangeSlightSwitch(Pos(6, 12), List(), (id => id + 2)),
      new ChangeVerticalSwitch(Pos(3, 14), List(), (id => id + 6)),
      new ChangeVerticalSwitch(Pos(0, 12), List(), (id => id + 7)),
      new ChangeVerticalSwitch(Pos(3, 0), List(), (id => id + 8 - 5))
    )
    
    val state = new State(BitSet(0, 4, 5))
  }

  trait Level30 extends SolutionChecker with NewTerrain
  {
    val level =
    """---oooooRRoooo-
      |---oToo-----Ro-
      |---ooo------Ro!
      |-------Roo++ooo
      |--S----RR-----o
      |-!oR---RR-----o
      |RRRR---oo+--+oo
      |RRRoRoRRoR--!o+
      |oRRRRRRRRRRRo--
      |-RoRRR--RRRRo--""".stripMargin
      
    val switches : List[AbstractSwitch] = List(
      new ChangeVerticalSwitch(Pos(5, 1), List(Pos(3, 10), Pos(3, 11)), (id => id + 0)),
      new ChangeVerticalSwitch(Pos(7, 12), List(Pos(7, 14)), (id => id ^ BitSet(1))),
      new ChangeVerticalSwitch(Pos(2, 14), List(), (id => id - 0 + 3)),
      new Switch(List(Pos(6, 12), Pos(6, 9)))
    )
    
    val state = new State(BitSet(0))
  }

  trait Level31 extends SolutionChecker with NewTerrain
  {
    val level =
    """------------ooo+
      |--ooo----!--oTo+
      |--ooo++ooo++ooo+
      |--ooo--ooo---o--
      |--RRR--!oo---R--
      |---R---ooo--RRR-
      |---o---ooo--ooo-
      |-+ooo++o!o++oSo-
      |-+o!o--!----ooo-
      |-+ooo-----------""".stripMargin
      
    val switches : List[AbstractSwitch] = List(
      new Switch(List(Pos(9, 1), Pos(8, 1), Pos(7, 1))),
      new Switch(List(Pos(2, 5), Pos(2, 6))),
      new Switch(List(Pos(2, 10), Pos(2, 11))),
      new Switch(List(Pos(7, 5), Pos(7, 6))),
      new Switch(List(Pos(7, 10), Pos(7, 11))),
      new Switch(List(Pos(0, 15), Pos(1, 15), Pos(2, 15))),
           
      new ChangeVerticalSwitch(Pos(1, 9), List(), (id => id ^ BitSet(2))),
      new ChangeSlightSwitch(Pos(4, 7), List(), (id => id - 1 - 2 - 3 - 4)),
      new ChangeSlightSwitch(Pos(7, 8), List(), (id => id - 1 - 2 - 3 - 4)),
      new ChangeVerticalSwitch(Pos(8, 7), List(), (id => id ^ BitSet(3))),
      new ChangeVerticalSwitch(Pos(8, 3), List(), (id => id - 1 + 5))
    )
    
    val state = new State(BitSet(0, 1, 4))
  }

  trait Level32 extends SolutionChecker with NewTerrain
  {
    val level =
    """-------------o!
      |---oo++oo---ooo
      |--ooo++oo--o!oo
      |--oTo---ooooo--
      |--ooo----ooo---
      |----------oo---
      |-----ooo--oS---
      |-oo++o!o--oo---
      |-oo++ooooooo---
      |---------------""".stripMargin
      
    val switches : List[AbstractSwitch] = List(
      new Switch(List(Pos(1, 5), Pos(1, 6))),
      new Switch(List(Pos(8, 3), Pos(8, 4))),
      new Switch(List(Pos(7, 3), Pos(7, 4))),
      new Switch(List(Pos(2, 5), Pos(2, 6))),
      
      new ChangeVerticalSwitch(Pos(2, 12), List(), (id => id ^ BitSet(1))),
      new ChangeVerticalSwitch(Pos(0, 14), List(), (id => id ^ BitSet(0, 2))),
      new ChangeVerticalSwitch(Pos(7, 6), List(), (id => id ^ BitSet(3)))
    )
    
    val state = new State(BitSet(0))
  }

  trait Level33 extends SolutionChecker with NewTerrain
  {
    val level =
    """-----oo!ooo----
      |-----oooooo+---
      |ooo--!oo!ooooo-
      |oSooooooo!!oo!-
      |-----oo!oo!ooo-
      |-----oooooo!oo-
      |ooo--oooooo!ooo
      |oTo++o!o--ooo!!
      |ooo--ooo---oooo
      |ooo---------ooo""".stripMargin
      
    val switches : List[AbstractSwitch] = List(
      new Switch(List(Pos(7, 3), Pos(7, 4))),
      new Switch(List(Pos(1, 11))),
      
      new ChangeVerticalSwitch(Pos(7, 14), List(), (id => id + 1)),

      new ChangeSlightSwitch(Pos(7, 13), List(), (id => id - 0)),
      new ChangeSlightSwitch(Pos(3, 13), List(), (id => id - 0)),
      
      new ChangeSlightSwitch(Pos(6, 11), List(), (id => id - 0)),
      new ChangeSlightSwitch(Pos(5, 11), List(), (id => id - 0)),

      new ChangeSlightSwitch(Pos(4, 10), List(), (id => id - 0)),
      new ChangeSlightSwitch(Pos(3, 10), List(), (id => id - 0)),
      new ChangeSlightSwitch(Pos(3, 9), List(), (id => id - 0)),
      new ChangeSlightSwitch(Pos(2, 8), List(), (id => id - 0)),
      new ChangeSlightSwitch(Pos(0, 7), List(), (id => id - 0)),
      new ChangeSlightSwitch(Pos(4, 7), List(), (id => id - 0)),
      new ChangeSlightSwitch(Pos(7, 6), List(), (id => id - 0)),
      new ChangeSlightSwitch(Pos(2, 5), List(), (id => id - 0))
    )
    
    val state = new State(BitSet(0))
  }
  

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Block(Pos(0,0), Pos(0, 0)), new State()), "0,0")
      assert(!terrain(Block(Pos(4,11), Pos(4, 11)), new State()), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
  
  test("level1")
  	{
	  new Level1 {
	    println("level1")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}
  
  test("level2")
  	{
	  new Level2 {
	    println("level2")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}
    
  test("level3")
  	{
	  new Level3 {
 	    println("level3")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}
  
  test("level4")
  	{
	  new Level4{
	    println("level4")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}
 
  test("level5")
  	{
	  new Level5{
	    println("level5")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}

  test("level6")
  	{
	  new Level6{
	    println("level6")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}

  test("level7")
  	{
	  new Level7{
	    println("level7")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}
  
  test("level8")
  	{
	  new Level8{
	    println("level8")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}

  test("level9")
  	{
	  new Level9{
	    println("level9")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}

  test("level10")
  	{
	  new Level10{
	    println("level10")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}

  test("level11")
  	{
	  new Level11{
	    println("level11")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}

  test("level12")
  	{
	  new Level12{
	    println("level12")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}

  test("level13")
  	{
	  new Level13{
	    println("level13")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}

  test("level14")
  	{
	  new Level14{
	    println("level14")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}
  	
  test("level15")
  	{
	  new Level15{
	    println("level15")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}

  test("level16")
  	{
	  new Level16{
	    println("level16")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}

  test("level17")
  	{
	  new Level17{
	    println("level17")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}

  test("level18")
  	{
	  new Level18{
	    println("level18")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}

  test("level19")
  	{
	  new Level19{
	    println("level19")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}

  test("level20")
  	{
	  new Level20{
	    println("level20")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}

  test("level21")
  	{
	  new Level21{
	    println("level21")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}

  test("level22")
  	{
	  new Level22{
	    println("level22")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}

  test("level23")
  	{
	  new Level23{
	    println("level23")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}

  test("level24")
  	{
	  new Level24{
	    println("level24")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}
  
  test("level25")
  	{
	  new Level25{
	    println("level25")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}

  test("level26")
  	{
	  new Level26{
	    println("level26")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}
  	
  test("level27")
  	{
	  new Level27{
	    println("level27")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}

  test("level28")
  	{
	  new Level28{
	    println("level28")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}
  
  test("level29")
  	{
	  new Level29{
	    println("level29")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}

  test("level30")
  	{
	  new Level30{
	    println("level30")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}

  test("level31")
  	{
	  new Level31{
	    println("level31")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}

  test("level32")
  	{
	  new Level32{
	    println("level32")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}
  	
  test("level33")
  	{
	  new Level33{
	    println("level33")
	    println(solution)
	    assert(solution.length > 0)
	  }
  	}
}
