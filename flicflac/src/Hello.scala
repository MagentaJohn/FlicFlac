package hello

import cats.effect.*, org.http4s.*, org.http4s.dsl.io.*

val root = HttpRoutes.of[IO] { case request @ GET -> Root =>
  StaticFile.fromResource("/index.html", Some(request)).getOrElseF(NotFound())
}

val helloWorldService = HttpRoutes.of[IO] { case GET -> Root / "api" / "hello" / name =>
  Ok(s"Hello, $name.")
}
