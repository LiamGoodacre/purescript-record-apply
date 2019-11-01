module Data.Record.Apply where

import Type.Row

import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.RowList (class RowToList, kind RowList, Nil, Cons)
import Record (get, insert, delete)
import Type.RowList (class ListToRow, RLProxy(..))
import Prim.Row as Row

class ApplyRecord
  (io :: # Type)
  (i :: # Type)
  (o :: # Type)
  | io -> i o
  , i -> io o
  , o -> io i where
    applyRecord ::
      Record io ->
      Record i ->
      Record o

instance applyRecordImpl ::
  ( RowToList io lio
  , RowToList i li
  , RowToList o lo
  , ApplyRowList lio li lo io i o
  , ListToRow lio io
  , ListToRow li i
  , ListToRow lo o ) =>
  ApplyRecord io i o where
    applyRecord io i =
      applyRowList
        (RLProxy :: RLProxy lio)
        (RLProxy :: RLProxy li)
        (RLProxy :: RLProxy lo)
        io
        i

class 
  ( ListToRow io ior
  , ListToRow i ir
  , ListToRow o or ) <=
  ApplyRowList
    (io :: RowList)
    (i :: RowList)
    (o :: RowList)
    (ior :: # Type)
    (ir :: # Type)
    (or :: # Type)
    | io -> i o ior ir or
    , i -> io o ior ir or
    , o -> io i ior ir or where
      applyRowList ::
        RLProxy io ->
        RLProxy i ->
        RLProxy o ->
        Record ior ->
        Record ir ->
        Record or

instance applyRowListNil ::
  ApplyRowList Nil Nil Nil () () () where
    applyRowList a b c d e = e

rltail :: forall k v t. RLProxy (Cons k v t) -> RLProxy t
rltail _ = RLProxy

instance applyRowListCons ::
  ( Row.Cons k (i -> o) tior ior
  , Row.Cons k i tir ir
  , Row.Cons k o tor or
  , Row.Lacks k tior
  , Row.Lacks k tir
  , Row.Lacks k tor
  , ListToRow tio tior
  , ListToRow ti tir
  , ListToRow to tor
  , ApplyRowList tio ti to tior tir tor
  , IsSymbol k ) =>
  ApplyRowList
    (Cons k (i -> o) tio)
    (Cons k i ti)
    (Cons k o to)
    ior
    ir
    or
  where
    applyRowList io i o ior ir =
      let key = SProxy :: SProxy k   
          f = get key ior
          x = get key ir
          tior = delete key ior :: Record tior
          tir = delete key ir :: Record tir
          tor = applyRowList (rltail io) (rltail i) (rltail o) tior tir
      in insert key (f x) tor
