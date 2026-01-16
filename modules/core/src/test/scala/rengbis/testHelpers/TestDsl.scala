package rengbis.testHelpers

import rengbis.Schema.{ BinaryConstraint, BoundConstraint, BoundOp, ListConstraint, NumericConstraint, TextConstraint }

object textLength:
    def >=(v: Int): TextConstraint.Size  = TextConstraint.Size(BoundConstraint(BoundOp.MinInclusive, v))
    def >(v: Int): TextConstraint.Size   = TextConstraint.Size(BoundConstraint(BoundOp.MinExclusive, v))
    def <=(v: Int): TextConstraint.Size  = TextConstraint.Size(BoundConstraint(BoundOp.MaxInclusive, v))
    def <(v: Int): TextConstraint.Size   = TextConstraint.Size(BoundConstraint(BoundOp.MaxExclusive, v))
    def ===(v: Int): TextConstraint.Size = TextConstraint.Size(BoundConstraint(BoundOp.Exact, v))

object listSize:
    def >=(v: Int): ListConstraint.Size  = ListConstraint.Size(BoundConstraint(BoundOp.MinInclusive, v))
    def >(v: Int): ListConstraint.Size   = ListConstraint.Size(BoundConstraint(BoundOp.MinExclusive, v))
    def <=(v: Int): ListConstraint.Size  = ListConstraint.Size(BoundConstraint(BoundOp.MaxInclusive, v))
    def <(v: Int): ListConstraint.Size   = ListConstraint.Size(BoundConstraint(BoundOp.MaxExclusive, v))
    def ===(v: Int): ListConstraint.Size = ListConstraint.Size(BoundConstraint(BoundOp.Exact, v))

object numValue:
    def >=(v: Int): NumericConstraint.Value         = NumericConstraint.Value(BoundConstraint(BoundOp.MinInclusive, BigDecimal(v)))
    def >(v: Int): NumericConstraint.Value          = NumericConstraint.Value(BoundConstraint(BoundOp.MinExclusive, BigDecimal(v)))
    def <=(v: Int): NumericConstraint.Value         = NumericConstraint.Value(BoundConstraint(BoundOp.MaxInclusive, BigDecimal(v)))
    def <(v: Int): NumericConstraint.Value          = NumericConstraint.Value(BoundConstraint(BoundOp.MaxExclusive, BigDecimal(v)))
    def ===(v: Int): NumericConstraint.Value        = NumericConstraint.Value(BoundConstraint(BoundOp.Exact, BigDecimal(v)))
    def >=(v: BigDecimal): NumericConstraint.Value  = NumericConstraint.Value(BoundConstraint(BoundOp.MinInclusive, v))
    def >(v: BigDecimal): NumericConstraint.Value   = NumericConstraint.Value(BoundConstraint(BoundOp.MinExclusive, v))
    def <=(v: BigDecimal): NumericConstraint.Value  = NumericConstraint.Value(BoundConstraint(BoundOp.MaxInclusive, v))
    def <(v: BigDecimal): NumericConstraint.Value   = NumericConstraint.Value(BoundConstraint(BoundOp.MaxExclusive, v))
    def ===(v: BigDecimal): NumericConstraint.Value = NumericConstraint.Value(BoundConstraint(BoundOp.Exact, v))

object binBytes:
    def >=(v: Int): BinaryConstraint.Size  = BinaryConstraint.Size(BoundConstraint(BoundOp.MinInclusive, v), BinaryConstraint.BinaryUnit.bytes)
    def >(v: Int): BinaryConstraint.Size   = BinaryConstraint.Size(BoundConstraint(BoundOp.MinExclusive, v), BinaryConstraint.BinaryUnit.bytes)
    def <=(v: Int): BinaryConstraint.Size  = BinaryConstraint.Size(BoundConstraint(BoundOp.MaxInclusive, v), BinaryConstraint.BinaryUnit.bytes)
    def <(v: Int): BinaryConstraint.Size   = BinaryConstraint.Size(BoundConstraint(BoundOp.MaxExclusive, v), BinaryConstraint.BinaryUnit.bytes)
    def ===(v: Int): BinaryConstraint.Size = BinaryConstraint.Size(BoundConstraint(BoundOp.Exact, v), BinaryConstraint.BinaryUnit.bytes)
