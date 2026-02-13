package parser

object ParserOps {

  /** Parser after a Unit parser: run p1 (discard result), then p2 and take p2's result. */
  final implicit class P0[A <: Unit](p1: Parser[A]) {
    def take[B](p2: Parser[B]): Parser[B] = p1.zip(p2).map(_._2)
  }

  final implicit class P2[A, B](p1: Parser[(A, B)]) {
    def take[C](p2: Parser[C]): Parser[(A, B, C)] =
      p1.zip(p2).map { case ((a, b), c) => (a, b, c) }
  }

  final implicit class P3[A, B, C](p1: Parser[(A, B, C)]) {
    def take[D](p2: Parser[D]): Parser[(A, B, C, D)] =
      p1.zip(p2).map { case ((a, b, c), d) => (a, b, c, d) }
  }

  final implicit class P4[A, B, C, D](p1: Parser[(A, B, C, D)]) {
    def take[E](p2: Parser[E]): Parser[(A, B, C, D, E)] =
      p1.zip(p2).map { case ((a, b, c, d), e) => (a, b, c, d, e) }
  }

  final implicit class P5[A, B, C, D, E](
      p1: Parser[(A, B, C, D, E)]
  ) {
    def take[F](p2: Parser[F]): Parser[(A, B, C, D, E, F)] =
      p1.zip(p2).map { case ((a, b, c, d, e), f) => (a, b, c, d, e, f) }
  }

  final implicit class P6[A, B, C, D, E, F](
      p1: Parser[(A, B, C, D, E, F)]
  ) {
    def take[G](p2: Parser[G]): Parser[(A, B, C, D, E, F, G)] =
      p1.zip(p2).map { case ((a, b, c, d, e, f), g) => (a, b, c, d, e, f, g) }
  }

  final implicit class P7[A, B, C, D, E, F, G](
      p1: Parser[(A, B, C, D, E, F, G)]
  ) {
    def take[H](p2: Parser[H]): Parser[(A, B, C, D, E, F, G, H)] =
      p1.zip(p2).map {
        case ((a, b, c, d, e, f, g), h) =>
          (a, b, c, d, e, f, g, h)
      }
  }

  final implicit class P8[A, B, C, D, E, F, G, H](
      p1: Parser[(A, B, C, D, E, F, G, H)]
  ) {
    def take[I](p2: Parser[I]): Parser[(A, B, C, D, E, F, G, H, I)] =
      p1.zip(p2).map {
        case ((a, b, c, d, e, f, g, h), i) =>
          (a, b, c, d, e, f, g, h, i)
      }
  }

  final implicit class P9[A, B, C, D, E, F, G, H, I](
      p1: Parser[(A, B, C, D, E, F, G, H, I)]
  ) {
    def take[J](p2: Parser[J]): Parser[(A, B, C, D, E, F, G, H, I, J)] =
      p1.zip(p2).map {
        case ((a, b, c, d, e, f, g, h, i), j) =>
          (a, b, c, d, e, f, g, h, i, j)
      }
  }

  final implicit class P10[A, B, C, D, E, F, G, H, I, J](
      p1: Parser[(A, B, C, D, E, F, G, H, I, J)]
  ) {
    def take[K](p2: Parser[K]): Parser[(A, B, C, D, E, F, G, H, I, J, K)] =
      p1.zip(p2).map {
        case ((a, b, c, d, e, f, g, h, i, j), k) =>
          (a, b, c, d, e, f, g, h, i, j, k)
      }
  }

  final implicit class P11[A, B, C, D, E, F, G, H, I, J, K](
      p1: Parser[(A, B, C, D, E, F, G, H, I, J, K)]
  ) {
    def take[L](p2: Parser[L]): Parser[(A, B, C, D, E, F, G, H, I, J, K, L)] =
      p1.zip(p2).map {
        case ((a, b, c, d, e, f, g, h, i, j, k), l) =>
          (a, b, c, d, e, f, g, h, i, j, k, l)
      }
  }

  final implicit class P12[A, B, C, D, E, F, G, H, I, J, K, L](
      p1: Parser[(A, B, C, D, E, F, G, H, I, J, K, L)]
  ) {
    def take[M](
        p2: Parser[M]
    ): Parser[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
      p1.zip(p2).map {
        case ((a, b, c, d, e, f, g, h, i, j, k, l), m) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m)
      }
  }

  final implicit class P13[A, B, C, D, E, F, G, H, I, J, K, L, M](
      p1: Parser[(A, B, C, D, E, F, G, H, I, J, K, L, M)]
  ) {
    def take[N](
        p2: Parser[N]
    ): Parser[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
      p1.zip(p2).map {
        case ((a, b, c, d, e, f, g, h, i, j, k, l, m), n) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
      }
  }

  final implicit class P14[A, B, C, D, E, F, G, H, I, J, K, L, M, N](
      p1: Parser[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)]
  ) {
    def take[O](
        p2: Parser[O]
    ): Parser[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
      p1.zip(p2).map {
        case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n), o) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
      }
  }

  final implicit class P15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](
      p1: Parser[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)]
  ) {
    def take[P](
        p2: Parser[P]
    ): Parser[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
      p1.zip(p2).map {
        case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o), p) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
      }
  }

  final implicit class P16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](
      p1: Parser[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)]
  ) {
    def take[Q](
        p2: Parser[Q]
    ): Parser[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
      p1.zip(p2).map {
        case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p), q) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
      }
  }

  final implicit class P17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](
      p1: Parser[
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)
      ]
  ) {
    def take[R](
        p2: Parser[R]
    ): Parser[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
      p1.zip(p2).map {
        case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q), r) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
      }
  }
}
