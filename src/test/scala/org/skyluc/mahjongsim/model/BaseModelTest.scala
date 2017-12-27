package org.skyluc.mahjongsim.model

import org.scalatest.FlatSpec

import BaseModel._

class BaseModelSpec extends FlatSpec {

  "isMahjong" should "detect majhong in D1-D2-D3 D4-D5-D6 D7-D8-D9 D4-D4-D4 WN WN" in {
    val combinations = List(
      Chow(DotTile(1, 1), DotTile(2, 2), DotTile(3, 3)),
      Chow(DotTile(4, 4), DotTile(5, 1), DotTile(6, 2)),
      Chow(DotTile(7, 3), DotTile(8, 4), DotTile(9, 1)),
      Pung(DotTile(4, 1), DotTile(4, 2), DotTile(4, 3))
    )
    assert(isMahjong(List(
        WindTile('N', 1),
        WindTile('N', 2)
      ) ++ combinations.flatMap(_.tiles),
      combinations) == true)
  }

  it should "detect majhong in RG-RG-RG-RG C4-C4-C4 B7-B7-B7 C7-C8-C9 B1 B1" in {
    val combinations = List(
      Kong(DragonTile('G',1), DragonTile('G',2), DragonTile('G',3), DragonTile('G',4)),
      Pung(CharacterTile(4, 1), CharacterTile(4, 2), CharacterTile(4, 3)),
      Pung(BambooTile(7, 4), BambooTile(7, 1), BambooTile(7, 2)),
      Chow(CharacterTile(7, 3), CharacterTile(8, 4), CharacterTile(9, 1)),
    )
    assert(isMahjong(List(
        BambooTile(1, 2),
        BambooTile(1, 3)
      ) ++ combinations.flatMap(_.tiles),
      combinations) == true)
  }

  it should "detect majhong in RG-RG-RG-RG C4 C4 C4 B7 B7 B7 C7 C8 C9 B1 B1" in {
    val combinations = List(
      Kong(DragonTile('G',1), DragonTile('G',2), DragonTile('G',3), DragonTile('G',4)),
    )
    assert(isMahjong(List(
        CharacterTile(4, 1),
        CharacterTile(4, 2),
        CharacterTile(4, 3),
        BambooTile(7, 4),
        BambooTile(7, 1),
        BambooTile(7, 2),
        CharacterTile(7, 3),
        CharacterTile(8, 4),
        CharacterTile(9, 1),
        BambooTile(1, 2),
        BambooTile(1, 3)
      ) ++ combinations.flatMap(_.tiles),
      combinations) == true)
  }

  it should "detect majhong in B1-B2-B3 D5-D5-D5 C2-C3-C4 B5-B5-B5 D9 D9" in {
    val combinations = List(
      Chow(BambooTile(1, 1), BambooTile(2, 2), BambooTile(3, 3)),
      Pung(DotTile(5, 4), DotTile(5, 1), DotTile(5, 2)),
      Chow(CharacterTile(2, 3), CharacterTile(3, 4), CharacterTile(4, 1)),
      Pung(BambooTile(5, 2), BambooTile(5, 3), BambooTile(5, 4))
    )
    assert(isMahjong(List(
        DotTile(9, 1),
        DotTile(9, 2)
      ) ++ combinations.flatMap(_.tiles),
      combinations) == true)
  }

  it should "detect majhong in B1-B2-B3 D5 D5 D5 C2 C3 C4 B5 B5 B5 D9 D9" in {
    assert(isMahjong(List(
        BambooTile(1, 1),
        BambooTile(2, 2),
        BambooTile(3, 3),
        DotTile(5, 4),
        DotTile(5, 1),
        DotTile(5, 2),
        CharacterTile(2, 3),
        CharacterTile(3, 4),
        CharacterTile(4, 1),
        BambooTile(5, 2),
        BambooTile(5, 3),
        BambooTile(5, 4),
        DotTile(9, 1),
        DotTile(9, 2)
      ),
      Nil) == true)
  }

  it should "not detect majhong in WS WE B1 B3 B4 B6 B7 RW RW C2 C8 C9 D1 D2" in {
    assert(isMahjong(List(
      WindTile('S', 1),
      WindTile('E', 2),
      BambooTile(1, 3),
      BambooTile(3, 4),
      BambooTile(4, 1),
      BambooTile(6, 2),
      BambooTile(7, 3),
      DragonTile('W', 4),
      DragonTile('W', 1),
      CharacterTile(2, 2),
      CharacterTile(8, 3),
      CharacterTile(9, 4),
      DotTile(1, 1),
      DotTile(2, 2)),
      Nil) == false)
  }

  it should "not detect majhong in WS WE WE B3 B4 B6 RG RW RW C2 C8 C9 D1 D2" in {
    assert(isMahjong(List(
      WindTile('S', 1),
      WindTile('E', 2),
      WindTile('E', 3),
      BambooTile(3, 4),
      BambooTile(4, 1),
      BambooTile(6, 2),
      DragonTile('G', 3),
      DragonTile('W', 4),
      DragonTile('W', 1),
      CharacterTile(2, 2),
      CharacterTile(8, 3),
      CharacterTile(9, 4),
      DotTile(1, 1),
      DotTile(2, 2)),
      Nil) == false)
  }
}