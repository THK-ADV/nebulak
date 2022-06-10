package printer

object PrinterOps {
  implicit class P0[A >: Unit](p1: Printer[A]) {
    def take[B](p2: Printer[B]): Printer[B] =
      p1.zip(p2).contraMapSuccess(b => ((), b))

    def map[B](f: A => B): Printer[B] = Printer { case (b, input) =>
      val b0 = f(())
      if (b0 == b) p1.print((), input)
      else Left(PrintingError(b0.toString, b.toString))
    }
  }

  implicit class P2[A, B](p1: Printer[(A, B)]) {
    def take[C](p2: Printer[C]): Printer[(A, B, C)] =
      p1.zip(p2).contraMapSuccess { case (a, b, c) => ((a, b), c) }
  }

  implicit class P3[A, B, C](p1: Printer[(A, B, C)]) {
    def take[D](p2: Printer[D]): Printer[(A, B, C, D)] =
      p1.zip(p2).contraMapSuccess { case (a, b, c, d) => ((a, b, c), d) }
  }

  implicit class P4[A, B, C, D](p1: Printer[(A, B, C, D)]) {
    def take[E](p2: Printer[E]): Printer[(A, B, C, D, E)] =
      p1.zip(p2).contraMapSuccess { case (a, b, c, d, e) =>
        ((a, b, c, d), e)
      }
  }

  implicit class P5[A, B, C, D, E](p1: Printer[(A, B, C, D, E)]) {
    def take[F](p2: Printer[F]): Printer[(A, B, C, D, E, F)] =
      p1.zip(p2).contraMapSuccess { case (a, b, c, d, e, f) =>
        ((a, b, c, d, e), f)
      }
  }

  implicit class P6[A, B, C, D, E, F](p1: Printer[(A, B, C, D, E, F)]) {
    def take[G](p2: Printer[G]): Printer[(A, B, C, D, E, F, G)] =
      p1.zip(p2).contraMapSuccess { case (a, b, c, d, e, f, g) =>
        ((a, b, c, d, e, f), g)
      }
  }

  implicit class P7[A, B, C, D, E, F, G](p1: Printer[(A, B, C, D, E, F, G)]) {
    def take[H](p2: Printer[H]): Printer[(A, B, C, D, E, F, G, H)] =
      p1.zip(p2).contraMapSuccess { case (a, b, c, d, e, f, g, h) =>
        ((a, b, c, d, e, f, g), h)
      }
  }

  implicit class P8[A, B, C, D, E, F, G, H](
      p1: Printer[(A, B, C, D, E, F, G, H)]
  ) {
    def take[I](p2: Printer[I]): Printer[(A, B, C, D, E, F, G, H, I)] =
      p1.zip(p2).contraMapSuccess { case (a, b, c, d, e, f, g, h, i) =>
        ((a, b, c, d, e, f, g, h), i)
      }
  }

  implicit class P9[A, B, C, D, E, F, G, H, I](
      p1: Printer[(A, B, C, D, E, F, G, H, I)]
  ) {
    def take[J](p2: Printer[J]): Printer[(A, B, C, D, E, F, G, H, I, J)] =
      p1.zip(p2).contraMapSuccess { case (a, b, c, d, e, f, g, h, i, j) =>
        ((a, b, c, d, e, f, g, h, i), j)
      }
  }

  implicit class P10[A, B, C, D, E, F, G, H, I, J](
      p1: Printer[(A, B, C, D, E, F, G, H, I, J)]
  ) {
    def take[K](p2: Printer[K]): Printer[(A, B, C, D, E, F, G, H, I, J, K)] =
      p1.zip(p2).contraMapSuccess { case (a, b, c, d, e, f, g, h, i, j, k) =>
        ((a, b, c, d, e, f, g, h, i, j), k)
      }
  }

  implicit class P11[A, B, C, D, E, F, G, H, I, J, K](
      p1: Printer[(A, B, C, D, E, F, G, H, I, J, K)]
  ) {
    def take[L](p2: Printer[L]): Printer[(A, B, C, D, E, F, G, H, I, J, K, L)] =
      p1.zip(p2).contraMapSuccess { case (a, b, c, d, e, f, g, h, i, j, k, l) =>
        ((a, b, c, d, e, f, g, h, i, j, k), l)
      }
  }

  implicit class P12[A, B, C, D, E, F, G, H, I, J, K, L](
      p1: Printer[(A, B, C, D, E, F, G, H, I, J, K, L)]
  ) {
    def take[M](
        p2: Printer[M]
    ): Printer[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
      p1.zip(p2).contraMapSuccess { case (a, b, c, d, e, f, g, h, i, j, k, l, m) =>
        ((a, b, c, d, e, f, g, h, i, j, k, l), m)
      }
  }

  implicit class P13[A, B, C, D, E, F, G, H, I, J, K, L, M](
      p1: Printer[(A, B, C, D, E, F, G, H, I, J, K, L, M)]
  ) {
    def take[N](
        p2: Printer[N]
    ): Printer[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
      p1.zip(p2).contraMapSuccess { case (a, b, c, d, e, f, g, h, i, j, k, l, m, n) =>
        ((a, b, c, d, e, f, g, h, i, j, k, l, m), n)
      }
  }

  implicit class P14[A, B, C, D, E, F, G, H, I, J, K, L, M, N](
      p1: Printer[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)]
  ) {
    def take[O](
        p2: Printer[O]
    ): Printer[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
      p1.zip(p2).contraMapSuccess {
        case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) =>
          ((a, b, c, d, e, f, g, h, i, j, k, l, m, n), o)
      }
  }

  implicit class P15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](
      p1: Printer[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)]
  ) {
    def take[P](
        p2: Printer[P]
    ): Printer[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
      p1.zip(p2).contraMapSuccess {
        case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) =>
          ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o), p)
      }
  }

  implicit class P16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](
      p1: Printer[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)]
  ) {
    def take[Q](
        p2: Printer[Q]
    ): Printer[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
      p1.zip(p2).contraMapSuccess {
        case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) =>
          ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p), q)
      }
  }
}
