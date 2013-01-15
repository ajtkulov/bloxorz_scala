package streams

import common._

/**
 * This trait represents the layout and building blocks of the game
 */
trait GameDef {

    object CellType extends Enumeration
  {
    type CellType = Value
    val Empty, Switch, Red, Normal = Value
  }
  
  /**
   * The case class `Pos` encodes positions in the terrain.
   * 
   * IMPORTANT NOTE
   *  - The `x` coordinate denotes the position on the vertical axis
   *  - The `y` coordinate is used for the horizontal axis
   *  - The coordinates increase when moving down and right
   * 
   * Illustration:
   *
   *     0 1 2 3   <- y axis
   *   0 o o o o
   *   1 o o o o
   *   2 o # o o    # is at position Pos(2, 1)
   *   3 o o o o
   *  
   *   ^
   *   |
   *  
   *   x axis
   */
  case class Pos(x: Int, y: Int) {
    
      override def equals(other: Any) = other match {
    case that: Pos => this.x == that.x && this.y == that.y
    case _ => false
  }
    
    /** The position obtained by changing the `x` coordiante by `d` */
    def dx(d: Int) = copy(x = x + d, y)

    /** The position obtained by changing the `y` coordiante by `d` */
    def dy(d: Int) = copy(x, y = y + d)
    
    override def hashCode = 31 * (37 + x) + y

  }

  /**
   * The position where the block is located initially.
   *
   * This value is left abstract, it will be defined in concrete
   * instances of the game.
   */
  val startPos: Pos

  /**
   * The target position where the block has to go.
   * This value is left abstract.
   */
  val goal: Pos

  /**
   * The terrain is represented as a function from positions to
   * booleans. The function returns `true` for every position that
   * is inside the terrain.
   *
   * As explained in the documentation of class `Pos`, the `x` axis
   * is the vertical one and increases from top to bottom.
   */
  type Terrain = (Block, State) => Boolean
  type Terrain1 = (Block, State) => State
  
  /**
   * The terrain of this game. This value is left abstract.
   */
  val terrain: Terrain
  val terrain1: Terrain1
  val splitter: List[(Pos, Pos, Pos)]
  

  /**
   * In Bloxorz, we can move left, right, Up or down.
   * These moves are encoded as case objects.
   */
  sealed abstract class Move
  case object Left  extends Move
  case object Right extends Move
  case object Up    extends Move
  case object Down  extends Move

  case object FLeft  extends Move
  case object FRight extends Move
  case object FUp    extends Move
  case object FDown  extends Move
  case object SLeft  extends Move
  case object SRight extends Move
  case object SUp    extends Move
  case object SDown  extends Move
  /**
   * This function returns the block at the start position of
   * the game.
   */
  def startBlock: Block = Block(startPos, startPos)

  /**
   * A block is represented by the position of the two cubes that
   * it consists of. We make sure that `b1` is lexicographically
   * smaller than `b2`.
   */
  
  case class Block(val b1: Pos, val b2: Pos) {

    def req(b1: Pos, b2: Pos): Boolean = b1.x <= b2.x && b1.y <= b2.y 
    // checks the requirement mentioned above
    require(req(b1, b2), "Invalid block position: b1=" + b1 + ", b2=" + b2)

    override def equals(other: Any) = other match {
      case that: Block => this.b1 == that.b1 && this.b2 == that.b2
      case _ => false
    }

    override def hashCode = 41 * (41 + b1.hashCode) + b2.hashCode

    /**
     * Returns a block where the `x` coordinates of `b1` and `b2` are
     * changed by `d1` and `d2`, respectively.
     */
    def dx(d1: Int, d2: Int) = Block(b1.dx(d1), b2.dx(d2))

    /**
     * Returns a block where the `y` coordinates of `b1` and `b2` are
     * changed by `d1` and `d2`, respectively.
     */
    def dy(d1: Int, d2: Int) = Block(b1.dy(d1), b2.dy(d2))


    /** The block obtained by moving left */
    def left = if (isStanding)         dy(-2, -1)
               else if (b1.x == b2.x)  dy(-1, -2)
               else                    dy(-1, -1)

    /** The block obtained by moving right */
    def right = if (isStanding)        dy(1, 2)
                else if (b1.x == b2.x) dy(2, 1)
                else                   dy(1, 1)

    /** The block obtained by moving up */
    def up = if (isStanding)           dx(-2, -1)
             else if (b1.x == b2.x)    dx(-1, -1)
             else                      dx(-1, -2)

    /** The block obtained by moving down */
    def down = if (isStanding)         dx(1, 2)
               else if (b1.x == b2.x)  dx(1, 1)
               else                    dx(2, 1)


    /**
     * Returns the list of blocks that can be obtained by moving
     * the current block, together with the corresponding move.
     */
    def neighbors: List[(Block, Move)] = List((this.left, Left), (this.right, Right), (this.up, Up), (this.down, Down))

    /**
     * Returns the list of positions reachable from the current block
     * which are inside the terrain.
     */
    
    def legalNeighbors: List[(Block, Move)] = neighbors.filter(x => x._1.isLegal)

    /**
     * Returns `true` if the block is standing.
     */
    def isStanding: Boolean = (b1.x == b2.x && b1.y == b2.y)

    /**
     * Returns `true` if the block is entirely inside the terrain.
     */
    def isLegal: Boolean = terrain(this, new State())
  }
  
  case class ExtBlock(override val b1: Pos, override val b2: Pos, val state: State) extends Block(b1, b2)
  {
    override def isLegal: Boolean = terrain(this, state)
    
    override def dx(d1: Int, d2: Int) =
    {
      val n1 = b1.dx(d1)
      val n2 = b2.dx(d2)
      ExtBlock(n1, n2, terrain1(Block(n1, n2), state))
    }
    
    override def dy(d1: Int, d2: Int) =
    {
      val n1 = b1.dy(d1)
      val n2 = b2.dy(d2)
      ExtBlock(n1, n2, terrain1(Block(n1, n2), state))
    }
    
    override def equals(other: Any) = other match {
      case that: ExtBlock => this.b1 == that.b1 && this.b2 == that.b2 && this.state == that.state
      case _ => false
    }

    override def hashCode = 41 * (41 + b1.hashCode) + b2.hashCode + 37 * state.hashCode()
    
    override def legalNeighbors: List[(Block, Move)] = neighbors.filter(x => x._1.isLegal).map(f => func(f))
    
    def func(value: (Block, Move)) : (Block, Move) =
    {
      val filter = splitter.filter(p => p._1 == value._1.b1 && value._1.isStanding)
      if (filter.length == 0)
        value
      else
      {
        val f = filter.apply(0)
        value._1 match
        {
          case x:ExtBlock => (SeparateBlock(f._2, f._3, x.state), value._2)
          case _ => value
        }
      }
    }

  }
  
  def createSeparateBlock(b1: Pos, b2: Pos, state: State) : Block =
    if (Math.abs(b1.x - b2.x) + Math.abs(b1.y - b2.y) == 1)
      createExt(b1, b2, terrain1(MyBlock(b1, b2), state))
    else
      SeparateBlock(b1, b2, terrain1(MyBlock(b1, b2), state))
  
  def createExt(b1: Pos, b2: Pos, state: State) : ExtBlock =
    if (b1.x <= b2.x && b1.y <= b2.y)
      ExtBlock(b1, b2, state)
    else
      ExtBlock(b2, b1, state)

  case class MyBlock(override val b1:Pos, override val b2: Pos) extends Block(b1, b2)
  {
    override def req(b1: Pos, b2: Pos): Boolean = true
  }
  
  case class SeparateBlock(override val b1:Pos, override val b2: Pos, override val state: State) extends ExtBlock(b1, b2, state)
  {
    override def req(b1: Pos, b2: Pos): Boolean = true

    
    override def isStanding: Boolean = false
    
    override def neighbors: List[(Block, Move)] = List((this.fleft, FLeft), (this.fright, FRight), (this.fup, FUp), (this.fdown, FDown),
      (this.sleft, SLeft), (this.sright, SRight), (this.sup, SUp), (this.sdown, SDown))

    def fleft = createSeparateBlock(Pos(b1.x, b1.y - 1), b2, state)
    
    def fdown = createSeparateBlock(Pos(b1.x + 1, b1.y), b2, state)
    
    def fup = createSeparateBlock(Pos(b1.x - 1, b1.y), b2, state)
    
    def fright = createSeparateBlock(Pos(b1.x, b1.y + 1), b2, state)
    

    def sleft = createSeparateBlock(b1, Pos(b2.x, b2.y - 1), state)
    
    def sdown = createSeparateBlock(b1, Pos(b2.x + 1, b2.y), state)
    
    def sup = createSeparateBlock(b1, Pos(b2.x - 1, b2.y), state)
    
    def sright = createSeparateBlock(b1, Pos(b2.x, b2.y + 1), state)

    override def equals(other: Any) = other match {
      case that: SeparateBlock => this.b1 == that.b1 && this.b2 == that.b2 && this.state == that.state
      case _ => false
    }

    override def hashCode = 31 * (37 + b1.hashCode) + b2.hashCode + 31 * state.hashCode()
  }
}
