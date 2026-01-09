package object rengbis:
    def sequenceEithers[E, A](list: Seq[Either[E, A]]): Either[E, Seq[A]] =
        list.foldLeft[Either[E, Seq[A]]](Right(Seq.empty)) { (acc, current) =>
            for
                accumulator <- acc
                value       <- current
            yield accumulator :+ value
        }

    def optionEither[E, A](value: Option[Either[E, A]]): Either[E, Option[A]] = value match
        case None         => Right(None)
        case Some(either) => either.map(Some(_))
