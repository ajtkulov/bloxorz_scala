package streams

import scala.collection.immutable._


class State(val state:BitSet)
{
  override def equals(other: Any) = other match {
    case that: State => this.state == that.state
    case _ => false
  }

  override def hashCode = state.hashCode

  def this() = this(BitSet())

  override def toString = state.toString
}

trait BridgeStateTerrain extends StringParserTerrain {
  val state:State
  val switches: List[AbstractSwitch]
  
  override def startBlock: Block = ExtBlock(startPos, startPos, state)

  lazy override val terrain1: Terrain1 = (block: Block, state: State) =>
  {
    val bitSet = state.state
    val r = for (i <- 0 to switches.length - 1; sw = switches.apply(i) if sw.isStepOn(block) ) yield
    {
      (i, sw.stepOn(block, state.state.apply(i)), sw)
    }
    
    val plus = r.filter(p => p._2).map(p => p._1).toList
    val minus = r.filter(p => !p._2).map(p => p._1).toList
    
    val some:(BitSet, BitSet) =
      if (r.length > 0)
      {
        r.apply(0)._3 match
        {
          case x:CompoundSwitch => if (r.apply(0)._2) (x.addState, BitSet()) else (BitSet(), x.minusState)
          case _ => (BitSet(), BitSet())
        }
      }
      else (BitSet(), BitSet())

    val some2:(BitSet, BitSet) =
      if (r.length > 1)
      {
        r.apply(1)._3 match
        {
          case x:CompoundSwitch => if (r.apply(1)._2) (x.addState, BitSet()) else (BitSet(), x.minusState)
          case _ => (BitSet(), BitSet())
        }
      }
      else (BitSet(), BitSet())
    
    
    new State((((bitSet ++ plus) -- minus) ++ some._1 ++ some2._1) -- some._2 -- some2._2)
  }

  def switchOn(x: Pos, state: State): Boolean =
  {
    state.state.toList.filter(p =>
      {
        if (switches.length > p)
        {
          val sw = switches.apply(p); sw.values.contains(x)
        }
        else false}).length > 0
  }
  
  override def terrainFunction(levelVector: Vector[Vector[Char]], state: State): (Block, State) => Boolean =
  {
    import CellType._
    
    def some1(x: Pos, state: State) : CellType =
      if (x.x < 0 || x.x >= levelVector.size || x.y < 0 || x.y >= levelVector.apply(0).size)
        CellType.Empty
      else
      {
        val ch = levelVector.apply(x.x).apply(x.y)
        if (ch == '-')
          CellType.Empty
        else if (ch == 'R')
          CellType.Red
        else if (ch == '+')
          CellType.Switch
        else CellType.Normal
      }
    
    (x, state) =>
    {
      val r1 = some1(x.b1, state)
      val r2 = some1(x.b2, state)
      
      if ((r1 == Empty) || (r2 == Empty))
        false
      else
        if ((r1 == Red) && (r2 == Red) && x.isStanding)
        false
      else
      {
        val res1 = if (r1 == Switch)
          switchOn(x.b1, state)
        else true
        
        val res2 = if (r2 == Switch)
          switchOn(x.b2, state)
        else true
        
        res1 && res2
      }
    }
  }

  lazy override val terrain: Terrain = terrainFunction(vector, state)

  trait AbstractSwitch {
    def isStepOn(block: Block) : Boolean
    val values: List[Pos]
    def stepOn(block: Block, curState: Boolean) : Boolean
  }
  
  trait CompoundSwitch{
    def addState: BitSet
    def minusState: BitSet
  }
  
  trait ChangeStateFunc{
    def change(bitSet: BitSet) : BitSet
  }
  
  class SlightSwitch(val pos: Pos, val values: List[Pos]) extends AbstractSwitch
  {
    def isStepOn(block: Block) : Boolean = pos == block.b1 || pos == block.b2
    def stepOn(block: Block, curState: Boolean) : Boolean = !curState
  }
  
  class ChangeSlightSwitch(pos: Pos, values: List[Pos], func: (BitSet => BitSet)) extends SlightSwitch(pos, values) with ChangeStateFunc
  {
    def change(bitSet: BitSet): BitSet = func(bitSet)
  }

  class Switch(values: List[Pos]) extends SlightSwitch(Pos(-1, -1), values) with ChangeStateFunc
  {
    def change(bitSet: BitSet): BitSet = bitSet
  }
  
  class CloseSlightSwitch(override val pos: Pos, override val values: List[Pos]) extends SlightSwitch(pos, values)
  {
    override def stepOn(block: Block, curState: Boolean) : Boolean = false
  }

  class SlightCompundSwitch(pos: Pos, values: List[Pos], val addState: BitSet, val minusState: BitSet) extends SlightSwitch(pos, values) with CompoundSwitch
  {
  }
  
  class CloseCompoundSwitch(pos: Pos, values: List[Pos], minusState: BitSet) extends SlightCompundSwitch(pos, values, BitSet(), minusState)
  {
    override def stepOn(block: Block, curState: Boolean): Boolean = false
  }
  
  class OpenCompoundSwitch(pos: Pos, values: List[Pos], addState: BitSet) extends SlightCompundSwitch(pos, values, addState, BitSet())
  {
    override def stepOn(block: Block, curState: Boolean): Boolean = true
  }

  class VerticalSwitch(val pos: Pos, val values: List[Pos]) extends AbstractSwitch
  {
    def isStepOn(block: Block) : Boolean = pos == block.b1 && block.isStanding
    def stepOn(block: Block, curState: Boolean) : Boolean = !curState
  }
  
  class ChangeVerticalSwitch(pos: Pos, values: List[Pos], func: (BitSet => BitSet)) extends VerticalSwitch(pos, values) with ChangeStateFunc
  {
    def change(bitSet: BitSet): BitSet = func(bitSet)
  }

  class OpenVerticalSwitch(pos: Pos, values: List[Pos]) extends VerticalSwitch(pos, values)
  {
    override def stepOn(block: Block, curState: Boolean) : Boolean = true
  }

  class CloseVerticalSwitch(pos: Pos, values: List[Pos]) extends VerticalSwitch(pos, values)
  {
    override def stepOn(block: Block, curState: Boolean) : Boolean = false
  }

  class VerticalCompoundSwitch(pos: Pos, values: List[Pos], val addState: BitSet, val minusState: BitSet) extends VerticalSwitch(pos, values) with CompoundSwitch
  {
  }
  
  class CloseVerticalCompoundSwitch(pos: Pos, values: List[Pos], val addState: BitSet, val minusState: BitSet) extends VerticalSwitch(pos, values) with CompoundSwitch
  {
    override def stepOn(block: Block, curState: Boolean) : Boolean = false
  }
  
  class OpenVerticalCompoundSwitch(pos: Pos, values: List[Pos], val addState: BitSet, val minusState: BitSet) extends VerticalSwitch(pos, values) with CompoundSwitch
  {
    override def stepOn(block: Block, curState: Boolean) : Boolean = true
  }
}




