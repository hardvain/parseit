module Practice

namespace HList
  data HList : List Type -> Type where
    HNil : HList []
    HCons : a -> HList xs -> HList (a :: xs)

