package hello

import cats.effect.IO

import cats.syntax.all.*
import cats.data.Kleisli

import org.http4s.HttpRoutes

import cats.effect.*
import cats.implicits.*

def makeRoutes: HttpRoutes[IO] =

  val allFrontendRoutes = io.github.quafadas.sjsls.defaultFrontendRoutes[IO]()

  root <+> helloWorldService <+> allFrontendRoutes
end makeRoutes
