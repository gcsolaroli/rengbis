package rengbis.testHelpers

import rengbis.Schema.{ BinaryConstraint, BoundConstraint, BoundOp, ListConstraint, NumericConstraint, TextConstraint }

// DSL for creating TextConstraint.SizeRange
object textLength:
    def >=(v: Int): TextConstraint.SizeRange  = TextConstraint.SizeRange.minInclusive(v)
    def >(v: Int): TextConstraint.SizeRange   = TextConstraint.SizeRange.minExclusive(v)
    def <=(v: Int): TextConstraint.SizeRange  = TextConstraint.SizeRange.maxInclusive(v)
    def <(v: Int): TextConstraint.SizeRange   = TextConstraint.SizeRange.maxExclusive(v)
    def ===(v: Int): TextConstraint.SizeRange = TextConstraint.SizeRange.exact(v)

// DSL for creating ListConstraint.SizeRange
object listSize:
    def >=(v: Int): ListConstraint.SizeRange  = ListConstraint.SizeRange.minInclusive(v)
    def >(v: Int): ListConstraint.SizeRange   = ListConstraint.SizeRange.minExclusive(v)
    def <=(v: Int): ListConstraint.SizeRange  = ListConstraint.SizeRange.maxInclusive(v)
    def <(v: Int): ListConstraint.SizeRange   = ListConstraint.SizeRange.maxExclusive(v)
    def ===(v: Int): ListConstraint.SizeRange = ListConstraint.SizeRange.exact(v)

// DSL for creating NumericConstraint.ValueRange
object numValue:
    def >=(v: Int): NumericConstraint.ValueRange         = NumericConstraint.ValueRange.minInclusive(BigDecimal(v))
    def >(v: Int): NumericConstraint.ValueRange          = NumericConstraint.ValueRange.minExclusive(BigDecimal(v))
    def <=(v: Int): NumericConstraint.ValueRange         = NumericConstraint.ValueRange.maxInclusive(BigDecimal(v))
    def <(v: Int): NumericConstraint.ValueRange          = NumericConstraint.ValueRange.maxExclusive(BigDecimal(v))
    def ===(v: Int): NumericConstraint.ValueRange        = NumericConstraint.ValueRange.exact(BigDecimal(v))
    def >=(v: BigDecimal): NumericConstraint.ValueRange  = NumericConstraint.ValueRange.minInclusive(v)
    def >(v: BigDecimal): NumericConstraint.ValueRange   = NumericConstraint.ValueRange.minExclusive(v)
    def <=(v: BigDecimal): NumericConstraint.ValueRange  = NumericConstraint.ValueRange.maxInclusive(v)
    def <(v: BigDecimal): NumericConstraint.ValueRange   = NumericConstraint.ValueRange.maxExclusive(v)
    def ===(v: BigDecimal): NumericConstraint.ValueRange = NumericConstraint.ValueRange.exact(v)

// DSL for creating BinaryConstraint.SizeRange
object binBytes:
    def >=(v: Int): BinaryConstraint.SizeRange  = BinaryConstraint.SizeRange.minInclusive(v)
    def >(v: Int): BinaryConstraint.SizeRange   = BinaryConstraint.SizeRange.minExclusive(v)
    def <=(v: Int): BinaryConstraint.SizeRange  = BinaryConstraint.SizeRange.maxInclusive(v)
    def <(v: Int): BinaryConstraint.SizeRange   = BinaryConstraint.SizeRange.maxExclusive(v)
    def ===(v: Int): BinaryConstraint.SizeRange = BinaryConstraint.SizeRange.exact(v)
